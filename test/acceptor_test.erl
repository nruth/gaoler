-module(acceptor_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("acceptor_state.hrl").

-define(NO_ELECTIONS, #state{}).
-define(ONE_ELECTION(E), #state{elections=[{1,E}]}).
-define(NO_PROMISES, #election{}).
-define(PROMISED(N), #election{promised=N}).
-define(PROMISED_AND_ACCEPTED(N, A), #election{promised=N, accepted=A}).

promise_requests_test_() -> 
    [
     fun should_send_promise_when_no_higher_promises_made/0,
     fun should_send_highest_promise_when_lower_prepare_received/0,
     fun should_not_change_state_when_receiving_lower_prepare_request/0
    ].

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
    [
     fun should_update_accepted_when_higher_round_accept_requested/0,
     fun should_not_change_accepted_when_lower_round_accept_requested/0
    ].

should_update_accepted_when_higher_round_accept_requested() ->
    Sender = nil, 
    InitialState = ?ONE_ELECTION(?PROMISED_AND_ACCEPTED(4, prev)),
    Proposal = {accept, {1,5}, v},
    Result = acceptor:handle_call(Proposal, Sender, InitialState),
    ?assertMatch({_, _, ?ONE_ELECTION(#election{accepted = {5,v}})}, Result).

should_not_change_accepted_when_lower_round_accept_requested() ->
    Sender = nil, 
    InitialState = ?ONE_ELECTION(?PROMISED_AND_ACCEPTED(6, prev)),
    Proposal = {accept, {1,5}, v},
    Result = acceptor:handle_call(Proposal, Sender, InitialState),
    ?assertMatch({_, _, ?ONE_ELECTION(#election{accepted = prev})}, Result).


accept_request_replies_test_() -> 
    [
     fun should_reply_accept_for_promised_round/0,
     fun should_reply_accept_for_round_higher_than_promise/0,
     fun should_reply_reject_to_lower_round_than_promised/0
    ].

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
