-module (round_lib_test).
-include_lib("eunit/include/eunit.hrl").

max_round_test_() -> [
    fun should_give_round_highest_priority/0,
    fun should_give_node_secondary_priority/0,
    fun should_give_pid_third_priority/0
].

should_give_round_highest_priority() ->
    Min = round_lib:round(1, 4, 5), Max= round_lib:round(2, 1, 1),
    ?assertEqual(Max, round_lib:max(Min, Max)),
    ok.

should_give_node_secondary_priority() ->
    Min = round_lib:round(1, 2, 50), Max = round_lib:round(1, 5, 1),
    ?assertEqual(Max, round_lib:max(Min, Max)),
    ok.

should_give_pid_third_priority() ->
    Min = round_lib:round(1, 1, 50), Max= round_lib:round(1, 1, 60),
    ?assertEqual(Max, round_lib:max(Min, Max)),
    ok.
