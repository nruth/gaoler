-module(acceptor_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("acceptor_state.hrl").

-define(NO_PROMISES, #state{}).
-define(PROMISED(N), #state{promised=N}).
-define(PROMISED_AND_ACCEPTED(N, A), #state{promised=N, accepted=A}).

%% prepare requests / promises
promise_sent_when_no_higher_promises_made_test() ->
  Sender = nil,
  InitialState = ?NO_PROMISES,
  Result = acceptor:handle_call({prepare, 5}, Sender, InitialState),
  ?assertMatch({reply, {promised, 5}, _}, Result).

% since the proposer should know someone else has already prevented 
% its round from succeeding and reuse of the same promised message
% allows simpler acceptor
highest_promise_sent_when_lower_prepare_requested_test() ->
  Sender = nil,
  InitialState = ?PROMISED(6),
  Result = acceptor:handle_call({prepare, 5}, Sender, InitialState),
  ?assertMatch({reply, {promised, 6}, _}, Result).

promise_state_updated_by_higher_prepare_test() ->
  Sender = nil,
  InitialState = ?NO_PROMISES,
  Result = acceptor:handle_call({prepare, 5}, Sender, InitialState),
  ?assertMatch({_, _, #state{promised = 5}}, Result).

promise_state_unchanged_by_lower_prepare_test() ->
  Sender = nil,
  InitialState = ?PROMISED(6),
  Result = acceptor:handle_call({prepare, 5}, Sender, InitialState),
  ?assertMatch({_, _, #state{promised = 6}}, Result).


%% accept requests / votes updates state
proposal_with_higher_round_than_promised_updates_accepted_test() ->
  Sender = nil, 
  InitialState = ?PROMISED_AND_ACCEPTED(4, prev),
  Proposal = {accept, 5, v},
  Result = acceptor:handle_call(Proposal, Sender, InitialState),
  ?assertMatch({_, _, #state{accepted = v}}, Result).

proposal_with_lower_round_than_promised_does_not_change_accepted_test() ->
  Sender = nil, 
  InitialState = ?PROMISED_AND_ACCEPTED(6, prev),
  Proposal = {accept, 5, v},
  Result = acceptor:handle_call(Proposal, Sender, InitialState),
  ?assertMatch({_, _, #state{accepted = prev}}, Result).


%% test acceptors reply to the proposer (no separate learners)
proposal_with_higher_round_than_promised_replies_accept_test() ->
  Sender = nil,
  InitialState = ?PROMISED(3),
  Proposal = {accept, 5, v},
  Result = acceptor:handle_call(Proposal, Sender, InitialState),
  ?assertMatch({reply, {accept, 5}, _}, Result).

proposal_with_lower_round_than_promised_replies_reject_test() ->
  Sender = nil,
  InitialState = ?PROMISED(6),
  Proposal = {accept, 5, v},
  Result = acceptor:handle_call(Proposal, Sender, InitialState),
    ?assertMatch({reply, {reject, 5}, _}, Result).

proposal_with_same_round_as_promised_replies_accept_test() ->
    Sender = nil,
    InitialState = ?PROMISED(5),
    Proposal = {accept, 5, v},
    Result = acceptor:handle_call(Proposal, Sender, InitialState),
    ?assertMatch({reply, {accept, 5}, _}, Result).
