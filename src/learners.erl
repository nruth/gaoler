-module(learners).
-include_lib("accepted_record.hrl").

-export ([
    broadcast_result/1,
    broadcast_accept/1
]).

broadcast_accept({reject, _Round}) -> ok;
broadcast_accept({accept, _Round, _Value}=Accepted) ->
    case gaoler:learners() of
        [] -> erlang:errors(no_learners);
        Learners ->
            [spawn(fun() -> gen_event:notify(Learner, Accepted) end) ||
        	Learner <- Learners]
    end,
    ok.

broadcast_result(Value) ->
    case gaoler:learners() of
        [] -> erlang:errors(no_learners);
        Learners ->
            [spawn(fun() -> gen_event:notify(Learner, {result, Value}) end) ||
        	Learner <- Learners]
    end,
    ok.
