-module (lib_stats_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("records.hrl").

cumulative_moving_mean_test_() -> [
    fun base_case/0,
    fun induction_step/0,
    fun chained_calls/0
].

    base_case() ->
        ?assertEqual(#mean{value=10.0, samples=1}, lib_stats:cumulative_moving_mean(#mean{}, 10)),
        ?assertEqual(#mean{value=10.5, samples=1}, lib_stats:cumulative_moving_mean(#mean{}, 10.5)).

    induction_step() ->
        ?assertEqual(#mean{value=10.0, samples=2}, lib_stats:cumulative_moving_mean(#mean{value=10, samples=1}, 10)),
        ?assertEqual(#mean{value=20.0, samples=2}, lib_stats:cumulative_moving_mean(#mean{value=10, samples=1}, 30)),
        ?assertEqual(#mean{value=2.5, samples=2}, lib_stats:cumulative_moving_mean(#mean{value=0, samples=1}, 5)).

    chained_calls() ->
        Result = lib_stats:cumulative_moving_mean(
                    lib_stats:cumulative_moving_mean(
                        lib_stats:cumulative_moving_mean(#mean{}, 0)
                    , 10)
                 , 20),
        ?assertEqual(#mean{value=10.0, samples=3}, Result).