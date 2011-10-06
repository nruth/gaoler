-module(proposer).
-export([
	 prepare/2,
	]).
-define(MAJORITY, 3).

prepare(Round, Value) ->
    % send requests to acceptors

    MyProposal = 
	case wait_for_promises(Round, 0, no_previous_value) of
	    no_previous_value ->
		Value;
	    PreviousValue ->
		PreviousValue
	end,
    ballot(Round, MyProposal).


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
	    

ballot() ->
    % send ballot to acceptors
    % wait for quorum
    % return 


add_latest_vote({Round, _Value}=Vote, {OldRound,_OldValue}=OldVote) ->
    case Round > OldRound of
	true ->
	    Vote;
	false ->
	    OldVote
    end;
add_latest_vote(Vote, no_previous_value) ->
    Vote.
