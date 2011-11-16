-module (coordinator).

-export([get/0, put/1]).

%% will return either:
%%  - the agreed value
%%  - unknown, for no value chosen
get() ->
    case learner:get() of
        {learned, Value} -> Value;
        unknown -> unknown
    end.

%% will return either:
%%  - the proposed value
%%  - a previously agreed value
put(Proposal) ->
    case learner:get() of
        {learned, Value} -> Value;
        unknown -> paxos(Proposal).
            
    end.

paxos(Proposal) ->
    proposer:propose(Proposal).
