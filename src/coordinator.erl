-module (coordinator).

-export([get/0, put/3]).

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
        {learned, Value} -> {ok, Value};
        unknown -> unknown
    end.

%% will return either:
%%  - the proposed value
%%  - a previously agreed value
%% Timeout specifies in ms the maximum time to block waiting for a result
%%
%% Caller: "I want X"
%% Coordinator: "do we have a value?"
%% if Learner replies "Yes": 
%%      return the value
%% else Learner replies "No": no value has been decided, or it's been forgotten
%%      Coordinator -> Proposer: propose X
%%      Proposer: runs Paxos for X, 
%%                  informs X of result
%%          Consequently:
%%          - Acceptor persists relevant info for fault tolerance re: this election
%%          - Learners cache the result
put(Election, Proposal, Timeout) ->
    case learner:get() of
        {learned, Value} -> {ok, Value};
        unknown -> paxos(Election, Proposal, Timeout);
        Else -> error({undefined, Else})
    end.

%% makes a proposal and waits for the outcome
paxos(Election, Proposal, Timeout) ->
    ProposerPid = proposer:propose(Election, Proposal),
    {ok, _} = timer:kill_after(Timeout, ProposerPid),
    receive {learned, Value} -> 
        {ok, Value}
    after Timeout -> 
        {error, timeout}
    end.
