-module(proposer).
-export([
	 prepare/3
	]).

-define(MAJORITY, 3).

%% propose(Round, Value) ->
%%     spawn (
%%     Proposal = prepare(Round, Value),
%%     Result = ballot(Round, Proposal),
%%       from ! result ),
%%     receive 
%% 	Result  ->
%% 	    Result;
%%     after ?TIMEOUT ->
%% 	    dead
%%     end.

prepare(Acceptors, Round, Value) ->
    Message = {prepare, Round},
    broadcast(Acceptors, self(), Message),

    case wait_for_promises(Round, 0, no_previous_value) of
	no_previous_value ->
	    Value;
	PreviousValue ->
	    PreviousValue
    end,

    {Round, Value}.

wait_for_promises(_Round, ?MAJORITY, Votes) -> 
% votes = previously accepted values for an acceptor    
    Votes;
wait_for_promises(Round, NumberOfReplies, PreviousVote) ->
    receive 
	{promised, Round, LatestVote} ->
	    NewVote = add_latest_vote(LatestVote, PreviousVote),
	    wait_for_promises(Round, NumberOfReplies+1, NewVote);
	_Other ->
	    wait_for_promises(Round, NumberOfReplies, PreviousVote)
    end.
	    

ballot(Round, Proposal) ->
    % send ballot to acceptors
    Ballot = msg,
    
    Result = wait_for_ballot(Round, Proposal, 0, no_previous_value),
    % wait for quorum
    % return 
    Result.

wait_for_ballot(Round, Proposal, ?MAJORITY, Votes) ->
    Votes;
wait_for_ballot(Round, Proposal, NumberOfReplies, PreviousVote) ->
    ok.


broadcast([], _, _) ->
    ok;
broadcast([Target|TargetGroup], From, Message) ->
    Target ! {Message, From},
    broadcast(TargetGroup, From, Message).



add_latest_vote({Round, _Value}=Vote, {OldRound,_OldValue}=OldVote) ->
    case Round > OldRound of
	true ->
	    Vote;
	false ->
	    OldVote
    end;
add_latest_vote(Vote, no_previous_value) ->
    Vote.
