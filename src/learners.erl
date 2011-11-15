-module(learners).

-export ([broadcast_result/1]).

broadcast_result(Value) ->
    case gaoler:learners() of
        [] -> erlang:errors(no_learners);
        Learners ->
            [spawn(fun() -> gen_event:notify(Learner, {result, Value}) end) ||
        	Learner <- Learners]
    end,
    ok.
