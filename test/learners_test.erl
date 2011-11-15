-module(learners_test).
-include_lib("eunit/include/eunit.hrl").


% Tests with Meck stub and cleanup of learners broadcast proxy
broadcast_learners_test_() ->
    {foreach,
    fun() ->
        meck:new(gaoler),
        [gaoler]
    end, 
    fun(Modules) -> meck:unload(Modules) end,
     [ 
        fun sends_to_each_learner/0, 
         fun no_learners_registered_crashes_broadcast/0
     ]
    }.

sends_to_each_learner() ->
    Value = {value},
    meck:expect(gaoler, learners, 0, [nspy:mock(), nspy:mock(), nspy:mock()]),
    learners:broadcast_result(Value),
    [ ?assertEqual([{notify,{result,Value}}], nspy:get_messages_from_spy(Mock)) || Mock <- gaoler:learners() ].

no_learners_registered_crashes_broadcast() ->
    meck:expect(gaoler, learners, 0, []),
    ?assertError(_, learners:broadcast_result(whatever)).
