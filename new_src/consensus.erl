-module (consensus).
-export ([propose/3]).

%% will return either:
%%  - the proposed value
%%  - a previously agreed value
%% Timeout specifies in ms the maximum time to block waiting for a result
propose (Ticket, Proposal, Timeout) ->
    % check if a learned result is cached
    case learner_cache:get(Ticket) of
        {learned, Value} -> {ok, Value};
        % no agreed value, or the cache forgot, so run Paxos
        unknown -> paxos(Ticket, Proposal, Timeout);
        Else -> error({undefined, Else})
    end.

% %% will return either:
% %%  - the agreed value
% %%  - unknown, for no value chosen
% get() -> % TODO: run paxos prepare phase to discover any value lost from cache
%     case learner_cache:get() of
%         {learned, Value} -> {ok, Value};
%         unknown -> unknown
%     end.

%% makes a proposal and waits for the outcome
paxos(Election, Proposal, Timeout) ->
    ProposerPid = proposer:propose(Election, Proposal),
    {ok, _} = timer:kill_after(Timeout, ProposerPid),
    receive {learned, Value} -> 
        {ok, Value}
    after Timeout -> 
        {error, timeout}
    end.
