-module(proposer_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("proposer_state.hrl").
-include_lib("accepted_record.hrl").

-define(PROMISES(N),#state{promises=N}).
-define(ACCEPTS(N), #state{accepts=N}).
-define(ROUND(N),   #state{round=N}).
-define(VALUE(V),   #state{value=V}).

%% Meck stub modules, used to enable decoupled unit tests
setup() ->
    Mods = [acceptors, gaoler],
    meck:new(Mods),
    meck:expect(gaoler, deliver, 1, ok),
    meck:expect(acceptors, send_accept_request, 2, ok),
    meck:expect(acceptors, send_promise_request, 1, ok),
    Mods.

teardown(Mods) ->
    meck:unload(Mods).


% happy case: no other proposers and round 1 succeeds
first_promise_received_test() ->
    Round = 1,
    AcceptedValue = no_value,
    InitialState = ?PROMISES(0)?ROUND(Round),    
    Result = proposer:awaiting_promises({promised, Round, AcceptedValue}, 
					InitialState),
    ?assertMatch({next_state, awaiting_promises, ?PROMISES(1)}, Result).

on_promise_containing_no_accepted_value_no_past_accept_added_test() ->
    PrepareRound = 300,
    AcceptedValue = no_value,
    InitialState = ?PROMISES(1)?ROUND(PrepareRound)?VALUE(bar),
    Result = proposer:awaiting_promises({promised, PrepareRound, AcceptedValue}, InitialState),
    ?assertMatch({next_state, awaiting_promises, #state{past_accepts=[]}}, Result).

on_promise_containing_accepted_value_past_accept_added_test() ->
    PrepareRound = 300,
    AcceptedValue = #accepted{round=10, value=foo},
    InitialState = ?PROMISES(1)?ROUND(PrepareRound)?VALUE(bar),
    Result = proposer:awaiting_promises({promised, PrepareRound, AcceptedValue}, InitialState),
    ?assertMatch({next_state, awaiting_promises, #state{past_accepts=[AcceptedValue]}}, Result).

%% Testing proposer accept request broadcasts
promise_quorum_broadcast_test_() -> {foreach, fun setup/0, fun teardown/1, [  
    fun on_promise_quorum_state_moves_to_accepting/0, 
    fun on_promise_quorum_proposer_broadcasts_accept/0
]}.

on_promise_quorum_state_moves_to_accepting() ->
    Round = 1,
    AcceptedValue = no_value,
    InitialState = ?PROMISES(2)?ROUND(Round),
    Result = proposer:awaiting_promises({promised, Round, AcceptedValue},
					InitialState),
    ?assertMatch({next_state, awaiting_accepts, ?PROMISES(3)}, Result).

on_promise_quorum_proposer_broadcasts_accept() ->
    Round = 1,
    AcceptedValue = no_value,
    InitialState = ?PROMISES(2)?ROUND(Round)?VALUE(foo),
    proposer:awaiting_promises({promised, Round, AcceptedValue}, 
			       InitialState),
    ?assert(meck:called(acceptors, send_accept_request, '_')).


%% Testing proposer prepare request broadcasts
proposer_broadcast_prepare_test_() -> {foreach, fun setup/0, fun teardown/1, [  
    fun on_init_proposer_broadcasts_prepare/0, 
    fun on_higher_promise_received_proposer_increments_round/0
]}.

% initialising proposer broadcasts prepare
on_init_proposer_broadcasts_prepare() ->
    Round = 1,
    proposer:init([Round, val]),
    ?assert(meck:called(acceptors, send_promise_request, [Round])).

% sad case: someone else has been promised a higher round
on_higher_promise_received_proposer_increments_round() ->
    Round = 1,
    AcceptedValue = no_value,
    InitialState = ?PROMISES(0)?ROUND(Round),
    Result = proposer:awaiting_promises({promised, 100, AcceptedValue}, 
					InitialState),
    ?assertMatch({next_state, awaiting_promises, ?ROUND(101)}, Result).


%% TestCase: awaiting_accepts state
awaiting_accepts_test_() ->
    {foreach, 
     fun setup/0, 
     fun teardown/1,
     [
      fun first_accept_received/0,
      fun on_accept_quorum_proposer_delivers_value/0,
      fun on_accept_quorum_state_moves_to_accepted/0
     ]
    }.

first_accept_received() ->
    Round = 1,
    InitialState = ?PROMISES(3)?ACCEPTS(0)?ROUND(Round)?VALUE(foo),
    Result = proposer:awaiting_accepts({accepted, Round}, InitialState),
    ?assertMatch({next_state, awaiting_accepts, ?ACCEPTS(1)}, Result).

on_accept_quorum_proposer_delivers_value() ->
    Round = 1,
    Value = foo,
    InitialState = ?PROMISES(3)?ACCEPTS(2)?ROUND(Round)?VALUE(Value),
    proposer:awaiting_accepts({accepted, Round}, InitialState),
    ?assert(meck:called(gaoler, deliver, [Value])).

on_accept_quorum_state_moves_to_accepted() ->
    Round = 1,
    Value = 1,
    InitialState = ?PROMISES(3)?ACCEPTS(2)?ROUND(Round)?VALUE(Value),
    Result = {_, _, AcceptedState} = 
	proposer:awaiting_accepts({accepted, Round}, InitialState),
    ?assertMatch({next_state, accepted, _}, Result),
    ?assertEqual(InitialState?ACCEPTS(3), AcceptedState).
