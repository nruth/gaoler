-module(learners).

-export ([broadcast_result/1]).

broadcast_result(Value) ->
    [spawn(fun() -> gen_event:notify(Learner, {result, Value}) end) ||
	Learner <- gaoler:get_learners()],
    ok.
