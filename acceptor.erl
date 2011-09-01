-module (acceptor).
-export ([start/1]).

  start(Name) ->
    spawn(fun() -> init(Name) end).

  init(Name) ->
    Promise = order:null(), %comparison lower than any real value
    PreviouslyVotedInRound = order:null(), %comparison lower than any real value
    PreviouslyAcceptedValue = na,
    acceptor(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue).


  acceptor(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue) ->
    receive
      {prepare, Proposer, Round} -> 
        % io:format("ACC ~w <- PREPARE for round ~w~n", [Name, Round]),
        process_prepare_message(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue, {prepare, Proposer, Round});
      {accept, Proposer, Round, Proposal} -> 
        % io:format("ACC ~w <- ACCEPT for value ~w in round ~w~n", [Name, Proposal, Round]),
        process_accept_message(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue, Proposer, Round, Proposal);
      stop -> ok
  end.

  process_prepare_message(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue, {prepare, Proposer, Round}) ->
    case order:gr(Round, Promise) of %   prepare request's round > any previously issued promise
      false -> 
        % io:format("ACC ~w SORRY -> ~w and keeping same promise ~n", [Name, Round]),
        Proposer ! {sorry, Round},
        acceptor(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue);
      true ->
        % io:format("ACC ~w PROMISE -> ~w with previous vote ~w in ~w and UPDATING MY PROMISE ~n", [Name, Round, PreviouslyVotedInRound, PreviouslyAcceptedValue]),
        Proposer ! {promise, Round, PreviouslyVotedInRound, PreviouslyAcceptedValue},
        acceptor(Name, Round, PreviouslyVotedInRound, PreviouslyAcceptedValue)
    end.


  process_accept_message(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue, Proposer, Round, Proposal) ->
    case order:goe(Round, Promise) of % accept request's round > promise
      true -> % no promise conflict, I can vote for the proposal!
        % io:format("ACC ~w VOTE -> ~w~n", [Name, Round]),
        Proposer ! {vote, Round},
        recurse_acceptor_with_updated_accept_value(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue, Round, Proposal);
      false -> % I promised not to vote in a round this low, sorry!
        % io:format("ACC ~w SORRY VOTE -> ~w < promised ~w~n", [Name, Round, Promise]),
        Proposer ! {sorry, Round},
        acceptor(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue)
    end.

  recurse_acceptor_with_updated_accept_value(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue, Round, Proposal) ->
    case order:gr(Round, PreviouslyVotedInRound) of % current round  > highest round I've accepted a value in prevously
      true ->
        % io:format("ACC ~w UPDATING ACCEPT VALUES to round ~w proposal ~w~n", [Name, Round, Proposal]),
        acceptor(Name, Promise, Round, Proposal); 
      false ->
        % io:format("ACC ~w KEEPING PREVIOUS ACCEPT VALUE to round ~w proposal ~w~n", [Name, PreviouslyVotedInRound, PreviouslyAcceptedValue]),
        acceptor(Name, Promise, PreviouslyVotedInRound, PreviouslyAcceptedValue)
    end.
