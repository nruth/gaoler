-module(acceptor_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("acceptor_state.hrl").

-define(ACCEPTOR_STORAGE, acceptor_state_test).
-define(INITIAL_STATE, #state{elections=?ACCEPTOR_STORAGE,
                              ready_to_gc=[]}).
-define(ADD_ONE_ELECTION(E), statestore:add(?ACCEPTOR_STORAGE, E)).
-define(NO_PROMISES, #election{}).
-define(PROMISED(N), #election{id=1, promised=N}).
-define(PROMISED_AND_ACCEPTED(N, A), ?PROMISED_AND_ACCEPTED(1, N, A)).
-define(PROMISED_AND_ACCEPTED(ID, N, A), #election{id=ID, promised=N, accepted=A}).


%%%=
setup() ->
    statestore:create(?ACCEPTOR_STORAGE).

teardown(Name) ->
    ets:delete(Name).
    
promise_requests_test_() -> 
    {foreach, 
     fun setup/0,
     fun teardown/1,     
     [
      fun should_send_promise_when_no_higher_promises_made/0,
      fun should_send_highest_promise_when_lower_prepare_received/0,
      fun should_not_change_state_when_receiving_lower_prepare_request/0
     ]
    }.

should_send_promise_when_no_higher_promises_made() ->
    Sender = nil,
    {reply, Reply, _} = acceptor:handle_call({prepare, {1,5}}, Sender, 
                                             ?INITIAL_STATE),
    ?assertEqual({promised, 5, no_value}, Reply).

should_send_highest_promise_when_lower_prepare_received() ->
        % since the proposer should know someone else has already prevented 
        % its round from succeeding and reuse of the same promised message
        % allows simpler acceptor
    Sender = nil,
    ?ADD_ONE_ELECTION(?PROMISED(6)),
    {reply, Reply, _} = acceptor:handle_call({prepare, {1,5}}, Sender, ?INITIAL_STATE),
    ?assertEqual({promised, 6, no_value}, Reply).

should_not_change_state_when_receiving_lower_prepare_request() ->
    Sender = nil,
    ?ADD_ONE_ELECTION(?PROMISED(6)),
    acceptor:handle_call({prepare, {1,5}}, Sender, ?INITIAL_STATE),
    {1, CurrentElection} = statestore:find(?ACCEPTOR_STORAGE, 1),
    ?assertMatch(#election{promised=6}, CurrentElection).


accept_request_state_changes_test_() -> 
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun should_update_accepted_when_higher_round_accept_requested/0,
      fun should_not_change_accepted_when_lower_round_accept_requested/0
     ]
    }.

should_update_accepted_when_higher_round_accept_requested() ->
    Sender = nil, 
    ?ADD_ONE_ELECTION(?PROMISED_AND_ACCEPTED(4, prev)),
    Proposal = {accept, {1,5}, v},
    acceptor:handle_call(Proposal, Sender, ?INITIAL_STATE),
    ?assertMatch({1, #election{accepted = {5, v}}}, 
                 statestore:find(?ACCEPTOR_STORAGE, 1)).

should_not_change_accepted_when_lower_round_accept_requested() ->
    Sender = nil, 
    ?ADD_ONE_ELECTION(?PROMISED_AND_ACCEPTED(6, prev)),
    Proposal = {accept, {1,5}, v},
    acceptor:handle_call(Proposal, Sender, ?INITIAL_STATE),
    ?assertMatch({1, #election{accepted = prev}}, 
                 statestore:find(?ACCEPTOR_STORAGE, 1)).

accept_request_replies_test_() -> 
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun should_reply_accept_for_promised_round/0,
      fun should_reply_accept_for_round_higher_than_promise/0,
      fun should_reply_reject_to_lower_round_than_promised/0
     ]
    }.

should_reply_accept_for_promised_round() ->
    Sender = nil,
    ?ADD_ONE_ELECTION(?PROMISED(5)),
    Proposal = {accept, {1,5}, v},
    {reply, Reply, _} = acceptor:handle_call(Proposal, Sender, ?INITIAL_STATE),
    ?assertEqual({accepted, 5, v}, Reply).

should_reply_accept_for_round_higher_than_promise() ->
    Sender = nil,
    ?ADD_ONE_ELECTION(?PROMISED(3)),
    Proposal = {accept, {1,5}, v},
    {reply, Reply, _} = acceptor:handle_call(Proposal, Sender, ?INITIAL_STATE),
    ?assertEqual({accepted, 5, v}, Reply).

should_reply_reject_to_lower_round_than_promised() ->
    Sender = nil,
    ?ADD_ONE_ELECTION(?PROMISED(6)),
    Proposal = {accept, {1,5}, v},
    {reply, Reply, _} = acceptor:handle_call(Proposal, Sender, ?INITIAL_STATE),
    ?assertEqual({reject, 5}, Reply).


% see van Renesse's "Paxos Made Moderately Complex" sec 4.2
%%
%% -- TODO: Needs updating to use statestore instead of lists
%%
acceptor_garbage_collection_test_() ->
    {foreach, 
     fun setup/0,
     fun teardown/1,      
     [
      fun should_record_last_gc_performed/0,
      fun should_remove_older_elements_from_state/0,
      fun should_retain_newer_elements_in_state/0
     ]
    }.

should_record_last_gc_performed() ->
    {noreply, NewState} = acceptor:handle_cast({gc_older_than, 14},
                                              ?INITIAL_STATE),
    ?assertEqual(14, NewState#state.oldest_remembered_state).

should_remove_older_elements_from_state() ->
    ?ADD_ONE_ELECTION(?PROMISED_AND_ACCEPTED(1, 1, a)),
    ?ADD_ONE_ELECTION(?PROMISED_AND_ACCEPTED(2, 1, b)),
    ?ADD_ONE_ELECTION(?PROMISED_AND_ACCEPTED(3, 1, c)),
    {noreply, NewState} = acceptor:handle_cast({gc_older_than, 4}, 
                                               ?INITIAL_STATE),
    ?assertEqual(false, statestore:find(NewState#state.elections, 1)),
    ?assertEqual(false, statestore:find(NewState#state.elections, 2)),
    ?assertEqual(false, statestore:find(NewState#state.elections, 3)).

should_retain_newer_elements_in_state() ->
    ?ADD_ONE_ELECTION(?PROMISED_AND_ACCEPTED(1, 1, a)),
    ?ADD_ONE_ELECTION(?PROMISED_AND_ACCEPTED(2, 1, b)),
    ?ADD_ONE_ELECTION(?PROMISED_AND_ACCEPTED(3, 1, c)),
    {noreply, NewState} = acceptor:handle_cast({gc_older_than, 2}, 
                                               ?INITIAL_STATE),
    ?assertEqual(false, statestore:find(NewState#state.elections, 1)).

should_add_gc_request_to_state_when_issued_test() ->
    Setup = setup(),    
    {noreply, NewState} = acceptor:handle_cast({ready_to_gc, replica1, 20}, 
                                               ?INITIAL_STATE),
    ?assertEqual([{replica1, 20}], NewState#state.ready_to_gc), 
    teardown(Setup).
    

%% should_leave_no_elections_intact() ->
%%     {noreply, NewState} = acceptor:handle_cast({gc_older_than, 2}, 
%%                                                ?INITIAL_STATE),
%%     ?assertEqual([], NewState#state.elections).
