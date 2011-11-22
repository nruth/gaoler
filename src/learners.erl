-module(learners).
-include_lib("accepted_record.hrl").

-export ([
    broadcast_result/1,
    broadcast_accept/1
]).

broadcast_accept({reject, _Round}) -> ok;
broadcast_accept({accepted, _Round, _Value}=Accepted) ->
    gen_server:abcast(learner, Accepted).

broadcast_result(Value) ->
    gen_server:abcast(learner, {result, Value}).
