-module (coordinator).

-export([get/0, put/2]).

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
%% Timeout specifies in ms the maximum time to block waiting for a result
put(Proposal, Timeout) ->
    case learner:get() of
        {learned, Value} -> Value;
        unknown -> paxos(Proposal, Timeout)
    end.

%% makes a proposal and waits for the outcome
paxos(Proposal, Timeout) ->
    % register callback first to avoid race-condition
    % where paxos may finish before we insert our callback
    learner:register_callback(self()),
    proposer:propose(Proposal),
    learner:await_result(Timeout).
