-module(acceptor).

%% API
-export([
	 promise_me/2,
	 vote/3
	]).
-export([
	 start/0,
	 stop/0,
	 loop/1
	]).


%% state
-record(state, 
	{
	  highest_round=0,
	  latest_vote=0
	}).

%% API
promise_me(Proposer, Round) ->
    ?MODULE ! {promise, Round, Proposer}.

vote(Proposer, Round, Value) ->
    ?MODULE ! {vote, Round, Value, Proposer}.

start() ->
    Pid = spawn(fun() -> acceptor:loop(#state{}) end),
    {ok, Pid}.

stop() ->
    ?MODULE ! stop.

loop(State) ->
    receive 
	{promise, Round, From} ->
	    NewState = handle_promise(Round, From, State),
	    loop(NewState);
	{vote, Round, Value, From} ->
	    NewState = handle_vote(Round, Value, From, State),
	    loop(NewState);
	stop ->
	    ok;
	_Other ->
	    loop(State)
    end.

handle_promise(Round, From, State) ->
    case Round > State#state.highest_round of
	true ->
	    NewState = State#state{highest_round = Round},
	    From ! {promised, Round},
	    NewState;
	false ->
	    From ! {promised, State#state.highest_round},
	    State
    end.

handle_vote(Round, Value, From, State) ->
    {Status, NewState} = 
	case Round >= State#state.highest_round of
	    true ->
		{ok, State#state{latest_vote = {Round, Value},
				 highest_round = Round}};
	    false ->
		{no, State}
	end,

    LastVote = {voted, NewState#state.latest_vote},
    InRound = {in_round, NewState#state.highest_round},
    Reply = {Status, LastVote, InRound},
    
    From ! Reply, 

    NewState.

