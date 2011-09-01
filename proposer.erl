-module (proposer).
-export ([start/4]).

-define (timeout, 2000).
-define (backoff, 10).
-define (delay, 20).

  start (Name, Proposal, Acceptors, Seed) ->
    spawn(fun() -> init(Name, Proposal, Acceptors, Seed) end).

  init(Name, Proposal, Acceptors, Seed) ->
    random:seed(Seed, Seed, Seed),
    Round = order:null(Name),
    round(Name, ?backoff, Round, Proposal, Acceptors).


  % In a round the proposer will wait for accept and vote messages for up to timeout milliseconds. 
  % If it has not received the necessary number of replies it will abort the round. 
  % It will then back-off an increasing number of milliseconds before starting the next round. 
  % It will try its best to get the acceptors to vote for a proposal but as you will see it will be happy if they can agree on anything. 
  % The delay will be used to introduce a slight delay in the system to make simulations more interesting.
  % Each round consist of one ballot attempt. The ballot either succeeds or aborts, in which case a new round is initiated.
  round(Name, Backoff, Round, Proposal, Acceptors) ->
    case ballot(Round, Proposal, Acceptors, Name) of
      {ok, Decision} -> % the ballot is ok so we've agreed on a value and are finished
        io:format("~w decided ~w in round ~w~n~n", [Acceptors, Decision, Round]),
        {ok, Decision};
      abort -> % the ballot was aborted, so I'm going to wait a while, so any other ballots can finish, then propose a new ballot when it is hopefully quieter
        timer:sleep(random:uniform(Backoff)),
        io:format("PRP ~w interrupted, going to sleep~n", [Name]),
        NextRound = order:inc(Round),
        NextBackoff = 2*Backoff, % exponential backoff, c.f. IPX frame collisions
        round(Name, NextBackoff, NextRound, Proposal, Acceptors)
    end.


  % A ballot is initialized by multi-casting a prepare message to all acceptors. 
  % The process then collects all promises and also the accepted value with the highest sequence number so far. 
  % If we receive promises from a quorum (a majority) we start the voting process by multi-casting an accept message to all acceptors in the quorum. 
  % In the accept message we include the value with the highest sequence number accepted by a member on the quorum.
  ballot (Round, Proposal, Acceptors, Name) ->
    io:format("PRP ~w issuing round ~w ballot ~w to ~w~n", [Name, Round, Proposal, Acceptors]),
    prepare(Round, Acceptors),
    Quorum = (length(Acceptors) div 2) + 1,
    
    % start out with my proposal, for the case where no others are sent back during prepare
    case collect(Quorum, Round, order:null(), Proposal) of
      {accepted, ChosenProposal} ->
        accept(Round, ChosenProposal, Acceptors),
        case vote(Quorum, Round) of
          ok -> {ok, ChosenProposal};
          abort -> abort
        end;

      abort -> abort
    end.


  % The collect procedure will simply receive promises and, if no acceptor has any objections, 
  % learn the so far accepted value with the highest ballot number. 
  % Note that we need a time out since acceptors could take forever or simply refuse to reply. 
  % Also note that:
  %   - we have tagged the sent request with the sequence number and only accept replies with the same sequence number
  %   - we need a catch all alternative since there might be delayed messages out there that otherwise would just stack up.

  % 0 case when we have received Quorum (majority) and can accept the proposal!
  collect(0, _, _, Proposal) -> {accepted, Proposal};

  collect(N, Round, HighestBallotSoFar, HighestNumberedProposal) ->
    receive
      {promise, Round, _, na} ->
        % acceptor said ok and I haven't voted for a value yet, so reduce needed ok's by 1 & continue counting towards Quorum
        collect(N-1, Round, HighestBallotSoFar, HighestNumberedProposal);
      {promise, Round, VotedForValueInRound, VotedValue} ->
        % acceptor said ok but I have voted for a value already
        % io:format("PRP ~w received promise for round ~w with vote ~w and value ~w~n", [self(), Round, VotedForValueInRound, VotedValue]),
        case order:gr(HighestBallotSoFar, VotedForValueInRound) of
          true -> % HighestBallotNumber > Voted
            % io:format("PRP keeping same value (~w) (round ~w > ~w)~n", [HighestNumberedProposal, HighestBallotSoFar, VotedForValueInRound]),
            collect(N-1, Round, HighestBallotSoFar, HighestNumberedProposal);
          false -> % HighestBallotNumber < Voted, so replace it
            % io:format("PRP NEW ACCEPT VALUE ~w replacing ~w since its more recent (round ~w < ~w)~n", [VotedValue, HighestNumberedProposal, HighestBallotSoFar, VotedForValueInRound]),
            collect(N-1, Round, VotedForValueInRound, VotedValue)
        end;
      {promise, _, _, _} -> %this is a junk message in the queue so just ignore it
        collect(N, Round, HighestBallotSoFar, HighestNumberedProposal);
      {sorry, Round} -> % I'm being told by the acceptor that it's already voted on a higher proposal
        collect(N, Round, HighestBallotSoFar, HighestNumberedProposal); %dumb approach - just keep counting, assuming there are enough proposers left, timeout will trigger
      {sorry, _} -> %this is a message from a different round so ignore it
        collect(N, Round, HighestBallotSoFar, HighestNumberedProposal)
    after ?timeout ->
      abort
    end.

  vote(0, _) -> ok;
  vote(N, Round) ->
    receive
      {vote, Round} -> %acceptor votes, count the vote
        io:format("PRP ~w +1 VOTE~n", [Round]),
        vote(N-1, Round);
      {vote, _} -> %junk value, ignore
        % io:format("PRP ~w DISCARDING VOTE in round ~w~n", [self(), Round]),
        vote(N, Round);
      {sorry, Round} -> %acceptor votes says "don't count me, I voted on something else already"
        io:format("PRP ~w +0 from SORRY VOTE~n", [Round]),
        vote(N, Round);
      {sorry, _} -> %junk value, ignore
        % io:format("PRP ~w DISCARDING SORRY VOTE in round ~w~n", [self(), Round]),
        vote(N, Round)
    after ?timeout ->
      abort
    end.

  prepare(Round, Acceptors) ->
    Fun = fun(Acceptor) -> send(Acceptor, {prepare, self(), Round}) end,
    lists:map(Fun, Acceptors).
  
  accept(Round, Proposal, Acceptors) ->
    Fun = fun(Acceptor) -> send(Acceptor, {accept, self(), Round, Proposal}) end,
    lists:map(Fun, Acceptors).
  
  send(Name, Message) ->
    Name ! Message.
