-module (round_lib).
-export ([max/2, round/3]).

max (A, B) ->
    erlang:max(A, B).

round(Round, Node, Pid) ->
    {round, Round, Node, Pid}.
