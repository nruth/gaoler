-module (coordinator).

-export([get/0, put/2]).

%% will return either:
%%  - the agreed value
%%  - unknown, for no value chosen
%%
%% User: "What was decided?"
%% Coordinator: "Do we have a value?"
%% if Learner replies "Yes": 
%%      return the value to user
%% else Learner replies "No":
%%      tell the user we don't know
get() ->
    case learner:get() of
        {learned, Value} -> Value;
        unknown -> unknown
    end.

%% will return either:
%%  - the proposed value
%%  - a previously agreed value
%% Timeout specifies in ms the maximum time to block waiting for a result
%%
%% User: "I want X"
%% Coordinator: "do we have a value?"
%% if Learner replies "Yes": 
%%      return the value to user
%% else Learner replies "No": no value has been decided, or it's been forgotten
%%      Coordinator -> Learner: tell me when a value is decided (callback)
%%      Coordinator -> Proposer: propose X
%%      Proposer: runs Paxos for X,
%%          Consequently:
%%          - Acceptor persists the (highest) round number and chosen value
%%          - Learner -> Coordinator the chosen value
put(Proposal, Timeout) ->
    case learner:get() of
        {learned, Value} -> Value;
        unknown -> paxos(Proposal, Timeout);
        Else -> error({undefined, Else})
    end.

%% makes a proposal and waits for the outcome
paxos(Proposal, Timeout) ->
    % register callback first to avoid race-condition
    % where paxos may finish before we insert our callback
    registered = learner:register_callback(),
    proposer:propose(Proposal),
    learner:await_result(Timeout).
