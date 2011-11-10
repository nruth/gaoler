-module(learner_test).
-include_lib("eunit/include/eunit.hrl").

-define (STATE (Round, Value, AcceptCount), [{{Round, Value}, AcceptCount}]).
-define (JOIN_STATE (State1, State2), lists:append(State1, State2)).
-define (ASSERT_SAME_ELEMENTS (List1, List2), ?assertEqual(lists:keysort(1,List1), lists:keysort(1,List2))).

first_accept_received_for_round_value_pair_counts_1_test() ->
    Round = 100, Value = foo, AcceptCount = 0,
    InitialState = [],
    Result = learner:handle_event({accepted, Round, Value}, InitialState),
    ?assertEqual({ok, ?STATE(Round, Value, AcceptCount + 1)}, Result).

accept_received_for_known_value_increases_count_test() ->
    Round = 1, Value = foo, AcceptCount = 1,
    InitialState = ?STATE(Round, Value, AcceptCount),
    Result = learner:handle_event({accepted, Round, Value}, InitialState),
    ?assertEqual({ok, ?STATE(Round, Value, AcceptCount + 1)}, Result).

accept_received_only_increases_its_own_value_test() ->
    Round = 1, AcceptCount = 1,
    InitialState = ?STATE(Round, first_value, AcceptCount),
    {ok, NewState} = learner:handle_event({accepted, Round, second_value}, InitialState),
    ?ASSERT_SAME_ELEMENTS(NewState, ?JOIN_STATE(InitialState, ?STATE(Round, second_value, 1))).

% Tests with Meck stub and cleanup of learners broadcast proxy
broadcast_result_test_() ->
    {foreach,
    fun() ->
        meck:new(learners),
        meck:expect(learners, broadcast_result, 1, ok),
        learners
    end, 
    fun(Modules) -> meck:unload(Modules) end,
     [
      fun dont_sent_multiple_quorum_broadcasts_on_4th_5th_accept/0,
      fun on_accept_quorum_proposer_broadcasts_value/0
     ]
    }.

on_accept_quorum_proposer_broadcasts_value() ->
    Round = 20, Value = foo, AcceptCount = 2,
    InitialState = ?STATE(Round, Value, AcceptCount),
    learner:handle_event({accepted, Round, Value}, InitialState),
    ?assert(meck:called(learners, broadcast_result, [Value])).

dont_sent_multiple_quorum_broadcasts_on_4th_5th_accept() ->
    Round = 20, Value = foo,
    learner:handle_event({accepted, Round, Value}, ?STATE(Round, Value, 3)),
    ?assert(not meck:called(learners, broadcast_result, [Value])),
    learner:handle_event({accepted, Round, Value}, ?STATE(Round, Value, 4)),
    ?assert(not meck:called(learners, broadcast_result, [Value])).