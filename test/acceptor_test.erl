-module(acceptor_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("acceptor_state.hrl").

-define(NO_ELECTIONS, #state{}).
-define(ONE_ELECTION(E), #state{elections = statestore:add(E)}).
-define(NO_PROMISES, #election{}).
-define(PROMISED(N), #election{id=1, promised=N}).
-define(PROMISED_AND_ACCEPTED(N, A), #election{id=1, promised=N, accepted=A}).

%%%=
setup() ->
    statestore:create().

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
    InitialState = ?NO_ELECTIONS,
    Result = acceptor:handle_call({prepare, {1,5}}, Sender, InitialState),
    ?assertMatch({reply, {promised, 5, no_value}, _}, Result).

should_send_highest_promise_when_lower_prepare_received() ->
        % since the proposer should know someone else has already prevented 
        % its round from succeeding and reuse of the same promised message
        % allows simpler acceptor
    Sender = nil,
    InitialState = ?ONE_ELECTION(?PROMISED(6)),
    Result = acceptor:handle_call({prepare, {1,5}}, Sender, InitialState),
    ?assertMatch({reply, {promised, 6, no_value}, _}, Result).

should_not_change_state_when_receiving_lower_prepare_request() ->
    Sender = nil,
    InitialState = ?ONE_ELECTION(?PROMISED(6)),
    {_,_,State} = acceptor:handle_call({prepare, {1,5}}, Sender, InitialState),
    ?assertEqual(InitialState, State).


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
    InitialState = ?ONE_ELECTION(?PROMISED_AND_ACCEPTED(4, prev)),
    Proposal = {accept, {1,5}, v},
    acceptor:handle_call(Proposal, Sender, InitialState),
    ?assertMatch({1, #election{accepted = {5, v}}}, statestore:find(1)).

should_not_change_accepted_when_lower_round_accept_requested() ->
    Sender = nil, 
    InitialState = ?ONE_ELECTION(?PROMISED_AND_ACCEPTED(6, prev)),
    Proposal = {accept, {1,5}, v},
    acceptor:handle_call(Proposal, Sender, InitialState),
    ?assertMatch({1, #election{accepted = prev}}, statestore:find(1)).

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
    InitialState = ?ONE_ELECTION(?PROMISED(5)),
    Proposal = {accept, {1,5}, v},
    Result = acceptor:handle_call(Proposal, Sender, InitialState),
    ?assertMatch({reply, {accepted, 5, v}, _}, Result).

should_reply_accept_for_round_higher_than_promise() ->
    Sender = nil,
    InitialState = ?ONE_ELECTION(?PROMISED(3)),
    Proposal = {accept, {1,5}, v},
    Result = acceptor:handle_call(Proposal, Sender, InitialState),
    ?assertMatch({reply, {accepted, 5, v}, _}, Result).

should_reply_reject_to_lower_round_than_promised() ->
    Sender = nil,
    InitialState = ?ONE_ELECTION(?PROMISED(6)),
    Proposal = {accept, {1,5}, v},
    Result = acceptor:handle_call(Proposal, Sender, InitialState),
    ?assertMatch({reply, {reject, 5}, _}, Result).


% see van Renesse's "Paxos Made Moderately Complex" sec 4.2
acceptor_garbage_collection_test_() ->
    [
        fun should_record_last_gc_performed/0,
        fun should_remove_older_elements_from_state/0,
        fun should_retain_newer_elements_in_state/0,
        fun should_leave_no_elections_intact/0
    ].

should_record_last_gc_performed() ->
    {noreply, NewState} = acceptor:handle_cast({gc_older_than, 14}, ?NO_ELECTIONS),
    ?assertEqual(14, NewState#state.oldest_remembered_state).

should_remove_older_elements_from_state() ->
    State = #state{elections=[{3, a}, {2,z}, {1, a}]},
    {noreply, NewState} = acceptor:handle_cast({gc_older_than, 4}, State),
    ?assertEqual([], NewState#state.elections).

should_retain_newer_elements_in_state() ->
    State = #state{elections=[{3, a}, {2,z}, {1, a}]},
    {noreply, NewState} = acceptor:handle_cast({gc_older_than, 2}, State),
    ?assertEqual([{3, a}, {2,z}], NewState#state.elections).

should_leave_no_elections_intact() ->
    State = ?NO_ELECTIONS,
    {noreply, NewState} = acceptor:handle_cast({gc_older_than, 2}, State),
    ?assertEqual([], NewState#state.elections).
