-module(proposer_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("proposer_state.hrl").

-define(PROMISES(N),#state{promises=N}).
-define(ROUND(N),   #state{round=N}).
-define(VALUE(V),   #state{value=V}).

setup() ->
    Mods = [acceptors],
    meck:new(Mods),
    meck:expect(acceptors, accept, 2, ok),
    Mods.

teardown(Mods) ->
    meck:unload(Mods).

on_init_proposer_broadcasts_prepare_test() ->
    Round = 1,
    meck:new(acceptors),
    meck:expect(acceptors, promise, 1, ok),
    proposer:init([Round, val]),
    ?assert(meck:called(acceptors, promise, [Round])),
    meck:unload(acceptors).

%% Test awaiting_promises state
awaiting_promises_test_() ->
    {foreach, 
     fun setup/0, 
     fun teardown/1,
     [
      fun first_promise_received/0,
      fun on_promise_quorum_state_moves_to_accepting/0,
      fun on_promise_quorum_proposer_broadcasts_accept/0,
      fun on_higher_promise_received_proposer_aborts/0
     ]
    }.

% happy case: no other proposers and round 1 succeeds
first_promise_received() ->
    Round = 1,
    InitialState = ?PROMISES(0)?ROUND(Round),
    Result = proposer:awaiting_promises({promised, Round}, InitialState),
    ?assertMatch({next_state, awaiting_promises, ?PROMISES(1)}, Result).

on_promise_quorum_state_moves_to_accepting() ->
    Round = 1,
    InitialState = ?PROMISES(2)?ROUND(Round),
    Result = proposer:awaiting_promises({promised, Round}, InitialState),
    ?assertMatch({next_state, awaiting_accepts, ?PROMISES(3)}, Result).

on_promise_quorum_proposer_broadcasts_accept() ->
    Round = 1,
    InitialState = ?PROMISES(2)?ROUND(Round)?VALUE(foo),
    proposer:awaiting_promises({promised, Round}, InitialState),
    ?assert(meck:called(acceptors, accept, '_')).

% sad case: someone else has been promised a higher round
on_higher_promise_received_proposer_aborts() ->
    Round = 1,
    InitialState = ?PROMISES(0)?ROUND(Round),
    Result = proposer:awaiting_promises({promised, 100}, InitialState),
    ?assertMatch({next_state, aborted, _}, Result).
