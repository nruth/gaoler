-module(learner_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("decided_record.hrl").
-include_lib("learner_state.hrl").

-define (NOSTATE, #learner{}).
-define (DECIDED (Value), #learner{learned=#decided{value=Value}}).

%% LEARNING A DECISION BY NOTIFICATION

behaviour_on_receving_decision_notification_test_() -> 
    {foreach, fun setup/0, fun teardown/1, [
    fun should_learn_the_value_sent_by_another_learner/0,
    fun should_not_rebroadcast_learned_value/0
]}.

    should_learn_the_value_sent_by_another_learner() ->
        Value = v,
        ?assertEqual({noreply, ?DECIDED(Value)}, learner:handle_cast({result, Value}, ?NOSTATE)),
        ?assertEqual({noreply, ?DECIDED(Value)}, learner:handle_cast({result, Value}, ?DECIDED(Value))).

    should_not_rebroadcast_learned_value() ->
        Value = foo,
        {noreply, NewState} = learner:handle_cast({result, Value}, ?NOSTATE),
        ?assertEqual(1, meck:num_calls(learners, broadcast_result, [Value])),
        learner:handle_cast({result, Value}, NewState),
        ?assertEqual(1, meck:num_calls(learners, broadcast_result, [Value])).

%% END LEARNING A DECISION BY NOTIFICATION


behaviour_on_receicing_a_learned_value_query_test_() ->[
    fun should_respond_with_learned_value/0,
    fun should_respond_unknown_before_value_learned/0
].

    should_respond_with_learned_value() ->
        ?assertMatch(
            {reply, {learned, value}, _},
            learner:handle_call(get_learned, from, ?DECIDED(value))
        ).

    should_respond_unknown_before_value_learned() ->
        ?assertMatch( {reply, unknown, _},
            learner:handle_call(get_learned, from, ?NOSTATE)
        ).


%% meck stubs
setup() ->
    meck:new(learners),
    meck:expect(learners, broadcast_result, 1, ok),
    [learners].
    
teardown (Modules) ->
    meck:unload(Modules).
