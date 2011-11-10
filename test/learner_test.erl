-module(learner_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("decided_record.hrl").

-define (NOSTATE, []).
-define (DECIDED (Value), #decided{value=Value}).
-define (STATE (Round, Value, AcceptCount), [{{Round, Value}, AcceptCount}]).
-define (JOIN_STATE (State1, State2), lists:append(State1, State2)).
-define (ASSERT_SAME_ELEMENTS (List1, List2), ?assertEqual(lists:keysort(1,List1), lists:keysort(1,List2))).

%% LEARNING A DECIDED VALUE BY COUNTING ACCEPTS
behaviour_while_counting_accept_votes_test_() -> 
    [
    fun should_count_first_round_value_pair_acceptance_messages/0,
    fun should_count_further_round_value_pair_acceptance_messages/0,
    fun should_change_only_the_count_of_the_round_value_pair_accepted/0
    ].

    should_count_first_round_value_pair_acceptance_messages() ->
        Round = 100, Value = foo, AcceptCount = 0,
        InitialState = ?NOSTATE,
        Result = learner:handle_event({accepted, Round, Value}, InitialState),
        ?assertEqual({ok, ?STATE(Round, Value, AcceptCount + 1)}, Result).

    should_count_further_round_value_pair_acceptance_messages() ->
        Round = 1, Value = foo, AcceptCount = 1,
        InitialState = ?STATE(Round, Value, AcceptCount),
        Result = learner:handle_event({accepted, Round, Value}, InitialState),
        ?assertEqual({ok, ?STATE(Round, Value, AcceptCount + 1)}, Result).

    should_change_only_the_count_of_the_round_value_pair_accepted() ->
        Round = 1, AcceptCount = 1,
        InitialState = ?STATE(Round, first_value, AcceptCount),
        {ok, NewState} = learner:handle_event({accepted, Round, second_value}, InitialState),
        ?ASSERT_SAME_ELEMENTS(NewState, ?JOIN_STATE(InitialState, ?STATE(Round, second_value, 1))).

% Tests with Meck stub and cleanup of learners broadcast proxy
behaviour_on_reaching_quorum_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
      fun should_broadcast_decided_value_when_quorum_observed/0,
      fun should_not_broadcast_decided_value_again_for_4th_5th_accept/0,
      fun should_record_quorum_value/0,
      fun should_not_change_state_if_accept_received_after_decision_made/0
    ]}.

    should_broadcast_decided_value_when_quorum_observed() ->
        Round = 20, Value = foo, AcceptCount = 2,
        InitialState = ?STATE(Round, Value, AcceptCount),
        learner:handle_event({accepted, Round, Value}, InitialState),
        ?assert(meck:called(learners, broadcast_result, [Value])).

    should_not_broadcast_decided_value_again_for_4th_5th_accept() ->
        Round = 20, Value = foo,
        learner:handle_event({accepted, Round, Value}, ?STATE(Round, Value, 3)),
        ?assert(not meck:called(learners, broadcast_result, [Value])),
        learner:handle_event({accepted, Round, Value}, ?STATE(Round, Value, 4)),
        ?assert(not meck:called(learners, broadcast_result, [Value])).

    should_record_quorum_value() ->
        Round = 20, Value = foo, AcceptCount = 2,
        InitialState = ?STATE(Round, Value, AcceptCount),
        Result = learner:handle_event({accepted, Round, Value}, InitialState),
        ?assertEqual({ok, ?DECIDED(Value)}, Result).

    should_not_change_state_if_accept_received_after_decision_made() ->
        Round = 20, Value = foo, AcceptCount = 2,
        InitialState = ?STATE(Round, Value, AcceptCount),
        {ok, NewState1} = learner:handle_event({accepted, Round, Value}, InitialState),
        ?assertEqual(NewState1, ?DECIDED(Value)),
        {ok, NewState2} = learner:handle_event({accepted, Round, Value}, NewState1),
        ?assertEqual(NewState2, ?DECIDED(Value)).

%% END LEARNING A DECIDED VALUE BY COUNTING ACCEPTS


%% LEARNING A DECISION BY NOTIFICATION FROM OTHER LEARNER

behaviour_on_receving_decision_notification_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
      fun should_rebroadcast_decision_exactly_once/0,
      fun should_store_value_when_decision_notification_received/0
    ]}.

    should_store_value_when_decision_notification_received() ->
        Value = v,
        ?assertEqual({ok, ?DECIDED(Value)}, learner:handle_event({result, Value}, ?NOSTATE)),
        ?assertEqual({ok, ?DECIDED(Value)}, learner:handle_event({result, Value}, ?DECIDED(Value))).

    should_rebroadcast_decision_exactly_once() ->
        Value = foo,
        {ok, NewState} = learner:handle_event({result, Value}, ?NOSTATE),
        ?assertEqual(1, meck:num_calls(learners, broadcast_result, [Value])),
        learner:handle_event({result, Value}, NewState),
        ?assertEqual(1, meck:num_calls(learners, broadcast_result, [Value])).

%% END LEARNING A DECISION BY NOTIFICATION FROM OTHER LEARNER


%% meck stubs
setup() ->
    meck:new(learners),
    meck:expect(learners, broadcast_result, 1, ok),
    [learners].
    
teardown (Modules) ->
    meck:unload(Modules).
