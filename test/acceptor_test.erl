-module(acceptor_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("acceptor_state.hrl").

-define(NO_PROMISES, #state{}).
-define(PROMISED(N), #state{promised=N}).
-define(PROMISED_AND_ACCEPTED(N, A), #state{promised=N, accepted=A}).


promise_requests_test_() -> [
    fun should_send_promise_when_no_higher_promises_made/0,
    fun should_send_highest_promise_when_lower_prepare_received/0,
    fun should_not_change_state_when_receiving_lower_prepare_request/0
    ].

    should_send_promise_when_no_higher_promises_made() ->
        Sender = nil,
        InitialState = ?NO_PROMISES,
        Result = acceptor:handle_call({prepare, 5}, Sender, InitialState),
        ?assertMatch({reply, {promised, 5, no_value}, _}, Result).

    should_send_highest_promise_when_lower_prepare_received() ->
        % since the proposer should know someone else has already prevented 
        % its round from succeeding and reuse of the same promised message
        % allows simpler acceptor
        Sender = nil,
        InitialState = ?PROMISED(6),
        Result = acceptor:handle_call({prepare, 5}, Sender, InitialState),
        ?assertMatch({reply, {promised, 6, no_value}, _}, Result).

    should_not_change_state_when_receiving_lower_prepare_request() ->
        Sender = nil,
        InitialState = ?PROMISED(6),
        Result = acceptor:handle_call({prepare, 5}, Sender, InitialState),
        ?assertMatch({_, _, #state{promised = 6}}, Result).


accept_request_state_changes_test_() -> [
    fun should_update_accepted_when_higher_round_accept_requested/0,
    fun should_not_change_accepted_when_lower_round_accept_requested/0
].

    should_update_accepted_when_higher_round_accept_requested() ->
        Sender = nil, 
        InitialState = ?PROMISED_AND_ACCEPTED(4, prev),
        Proposal = {accept, 5, v},
        Result = acceptor:handle_call(Proposal, Sender, InitialState),
        ?assertMatch({_, _, #state{accepted = {5,v}}}, Result).

    should_not_change_accepted_when_lower_round_accept_requested() ->
        Sender = nil, 
        InitialState = ?PROMISED_AND_ACCEPTED(6, prev),
        Proposal = {accept, 5, v},
        Result = acceptor:handle_call(Proposal, Sender, InitialState),
        ?assertMatch({_, _, #state{accepted = prev}}, Result).


accept_request_replies_test_() -> [
    fun should_reply_accept_for_promised_round/0,
    fun should_reply_accept_for_round_higher_than_promise/0,
    fun should_reply_reject_to_lower_round_than_promised/0
].

    should_reply_accept_for_promised_round() ->
        Sender = nil,
        InitialState = ?PROMISED(5),
        Proposal = {accept, 5, v},
        Result = acceptor:handle_call(Proposal, Sender, InitialState),
        ?assertMatch({reply, {accepted, 5, v}, _}, Result).

    should_reply_accept_for_round_higher_than_promise() ->
        Sender = nil,
        InitialState = ?PROMISED(3),
        Proposal = {accept, 5, v},
        Result = acceptor:handle_call(Proposal, Sender, InitialState),
        ?assertMatch({reply, {accepted, 5, v}, _}, Result).

    should_reply_reject_to_lower_round_than_promised() ->
        Sender = nil,
        InitialState = ?PROMISED(6),
        Proposal = {accept, 5, v},
        Result = acceptor:handle_call(Proposal, Sender, InitialState),
        ?assertMatch({reply, {reject, 5}, _}, Result).
