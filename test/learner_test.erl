-module(learner_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("decided_record.hrl").
-include_lib("learner_state.hrl").

-define (NOSTATE, #learner{}).
-define (DECIDED (Value), #learner{learned=#decided{value=Value}}).
-define (ACCEPTCOUNT (Round, Value, Count), {{Round, Value}, Count}).
-define (STATE (Round, Value, AcceptCount), #learner{
    accepts = [ ?ACCEPTCOUNT(Round, Value, AcceptCount) ]
}).
-define (CALLBACKS(Callbacks), #learner{callbacks=Callbacks}).

%% LEARNING A DECIDED VALUE BY COUNTING ACCEPTS
behaviour_while_counting_accept_votes_test_() -> [
    fun should_count_first_received_round_value_pair_acceptance_messages/0,
    fun should_count_further_round_value_pair_acceptance_messages/0,
    fun should_change_only_the_count_of_the_round_value_pair_accepted/0
].

    should_count_first_received_round_value_pair_acceptance_messages() ->
        Round = 100, Value = foo, AcceptCount = 0,
        InitialState = ?NOSTATE,
        Result = learner:handle_cast({accepted, Round, Value}, InitialState),
        ?assertEqual({noreply, ?STATE(Round, Value, AcceptCount + 1)}, Result).

    should_count_further_round_value_pair_acceptance_messages() ->
        Round = 1, Value = foo, AcceptCount = 1,
        InitialState = ?STATE(Round, Value, AcceptCount),
        Result = learner:handle_cast({accepted, Round, Value}, InitialState),
        ?assertEqual({noreply, ?STATE(Round, Value, AcceptCount + 1)}, Result).

    should_change_only_the_count_of_the_round_value_pair_accepted() ->
        Round = 1, AcceptCount = 1,
        InitialState = ?STATE(Round, first_value, AcceptCount),
        {noreply, NewState} = learner:handle_cast({accepted, Round, second_value}, InitialState),
        ?assertEqual(1, proplists:get_value({Round, first_value}, NewState#learner.accepts)),
        ?assertEqual(1, proplists:get_value({Round, second_value}, NewState#learner.accepts)).

% Tests with Meck stub and cleanup of learners broadcast proxy
behaviour_on_reaching_quorum_test_() -> 
    {foreach, fun setup/0, fun teardown/1, [
    fun should_notify_other_learners_when_quorum_observed/0,
    fun should_not_broadcast_decided_value_again_for_4th_5th_accept/0,
    fun should_record_quorum_value/0,
    fun should_not_change_state_if_accept_received_after_decision_made/0,
    fun should_perform_callbacks/0
]}.

    should_notify_other_learners_when_quorum_observed() ->
        Round = 20, Value = foo, AcceptCount = 2,
        InitialState = ?STATE(Round, Value, AcceptCount),
        learner:handle_cast({accepted, Round, Value}, InitialState),
        ?assert(meck:called(learners, broadcast_result, [Value])).

    should_not_broadcast_decided_value_again_for_4th_5th_accept() ->
        Round = 20, Value = foo,
        learner:handle_cast({accepted, Round, Value}, ?STATE(Round, Value, 3)),
        ?assert(not meck:called(learners, broadcast_result, [Value])),
        learner:handle_cast({accepted, Round, Value}, ?STATE(Round, Value, 4)),
        ?assert(not meck:called(learners, broadcast_result, [Value])).

    should_record_quorum_value() ->
        Round = 20, Value = foo, AcceptCount = 2,
        InitialState = ?STATE(Round, Value, AcceptCount),
        Result = learner:handle_cast({accepted, Round, Value}, InitialState),
        ?assertEqual({noreply, ?DECIDED(Value)}, Result).

    should_not_change_state_if_accept_received_after_decision_made() ->
        Round = 20, Value = foo, AcceptCount = 2,
        InitialState = ?STATE(Round, Value, AcceptCount),
        {noreply, NewState1} = learner:handle_cast({accepted, Round, Value}, InitialState),
        ?assertEqual(NewState1, ?DECIDED(Value)),
        {noreply, NewState2} = learner:handle_cast({accepted, Round, Value}, NewState1),
        ?assertEqual(NewState2, ?DECIDED(Value)).

    should_perform_callbacks() ->
        CallMe = nspy:mock(),
        Round = 20, Value = foo, AcceptCount = 2,
        InitialState = ?STATE(Round, Value, AcceptCount)?CALLBACKS([CallMe]),
        learner:handle_cast({accepted, Round, Value}, InitialState),
        nspy:assert_message_received(CallMe, {result, Value}).

%% END LEARNING A DECIDED VALUE BY COUNTING ACCEPTS


%% LEARNING A DECISION BY NOTIFICATION FROM OTHER LEARNER

behaviour_on_receving_decision_notification_test_() -> 
    {foreach, fun setup/0, fun teardown/1, [
    fun should_learn_the_value_sent_by_another_learner/0,
    fun should_rebroadcast_learned_value_exactly_once/0
]}.

    should_learn_the_value_sent_by_another_learner() ->
        Value = v,
        ?assertEqual({noreply, ?DECIDED(Value)}, learner:handle_cast({result, Value}, ?NOSTATE)),
        ?assertEqual({noreply, ?DECIDED(Value)}, learner:handle_cast({result, Value}, ?DECIDED(Value))).

    should_rebroadcast_learned_value_exactly_once() ->
        Value = foo,
        {noreply, NewState} = learner:handle_cast({result, Value}, ?NOSTATE),
        ?assertEqual(1, meck:num_calls(learners, broadcast_result, [Value])),
        learner:handle_cast({result, Value}, NewState),
        ?assertEqual(1, meck:num_calls(learners, broadcast_result, [Value])).

%% END LEARNING A DECISION BY NOTIFICATION FROM OTHER LEARNER


behaviour_on_receicing_a_learned_value_query_test_() ->[
    fun should_respond_with_learned_value/0,
    fun should_respond_unknown_before_value_learned/0,
    fun should_not_change_state_on_query/0
].

    should_respond_with_learned_value() ->
        ?assertMatch(
            {reply, {learned, value}, _},
            learner:handle_call(get_learned, from, ?DECIDED(value))
        ).

    should_respond_unknown_before_value_learned() ->
        ?assertMatch(
            {reply, unknown, _},
            learner:handle_call(get_learned, from, ?NOSTATE)
        ),
        ?assertMatch(
            {reply, unknown, _},
            learner:handle_call(get_learned, from, ?STATE(1, v, 1))
        ).

    should_not_change_state_on_query() ->
        State = ?STATE(1, v, 1),
        ?assertMatch(
            {_, _, State},
            learner:handle_call(get_learned, from, State)
        ).


await_result_test() ->
    % no msg sent, timeout
    ?assertEqual(timeout, learner:await_result(1)),
    % receive well-formed result notification
    self() ! {result, v},
    ?assertEqual({learned, v}, learner:await_result(10)).

%% meck stubs
setup() ->
    meck:new(learners),
    meck:expect(learners, broadcast_result, 1, ok),
    [learners].
    
teardown (Modules) ->
    meck:unload(Modules).
