-module (measurer_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("records.hrl").

cumulative_mean_latency_test() ->
    Mean_latency = measurer:cumulative_mean_latency(5, 
        fun() -> timer:sleep(10) end
    ),
    io:format("~p",[Mean_latency]),
    ?assert(
        is_abs_diff_less_than(10,
            Mean_latency#mean.value/1000,
            1
        )
    ).

time_once_test() ->
    ?assert(
        is_abs_diff_less_than(10,
            measurer:time_once(
                fun() -> timer:sleep(10) end
            )/1000,
            1
        )
    ).


is_abs_diff_less_than_test() ->
    ?assert(is_abs_diff_less_than(10, 12, 4)),
    ?assertNot(is_abs_diff_less_than(10, 14, 1)).

is_abs_diff_less_than(X, Y, Diff) ->
    abs(X-Y) < Diff.