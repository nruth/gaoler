-module(learners).
-include_lib("accepted_record.hrl").

-export ([
    broadcast_result/1
]).

broadcast_result(Value) ->
    gen_server:abcast(learner, {result, Value}).
