-module(proposer).
-behaviour(gen_fsm).

%% Events
-export([
	 propose/3,
	 accept_promise/3,
	 accept/3
	]).

%% States
-export([
	 idle/2,
	 proposing/2,
	 accepting/2,
	 locked/2
	]).

%% gen_fsm callbacks
-export([
	 start_link/1,
	 init/1, 
	 handle_event/3,
	 handle_sync_event/4, 
	 handle_info/3, 
	 terminate/3, 
	 code_change/4
	]).

-define(SERVER, ?MODULE).

-record(state, 
	{
	  lock,
	  requester,
	  respond_to,
	  promises_received=[],
	  accepts_received=[]
	}).

%% API
start_link(RespondTo) ->
    gen_fsm:start_link(?MODULE, [RespondTo], []).

propose(Proposer, LockID, Requester) ->
    gen_fsm:send_event(Proposer, {propose, {LockID, Requester}}).

accept_promise(Proposer, LockID, Acceptor) ->
    gen_fsm:send_event(Proposer, {accept_promise, LockID, Acceptor}).

accept(Proposer, LockID, Acceptor) ->
    gen_fsm:send_event(Proposer, {accepted, LockID, Acceptor}).

%% States
idle({propose, {LockID, Requester}}, State) ->
    NewState = State#state{lock=LockID,
			   requester=Requester},
    acceptor:promise(self(), LockID),
    {next_state, proposing, NewState};
idle(_Event, State) ->
    {next_state, idle, State}.


proposing({accept_promise, LockID, Acceptor}, 
	  #state{lock=LockID} = State) ->
    Promises = [Acceptor|State#state.promises_received],
    NewState = State#state{promises_received=Promises},
    case is_majority(NewState#state.promises_received) of
	true ->
	    acceptor:accept(self(), LockID),
	    {next_state, accepting, NewState};
	false ->
	    {next_state, proposing, NewState}
    end;
proposing(_Event, State) -> % ignore invalid requests
    {next_state, proposing, State}.


accepting({accepted, LockID, Acceptor}, State) ->
    Accepts = [Acceptor|State#state.accepts_received],
    NewState = State#state{accepts_received = Accepts},
    case is_majority(NewState#state.accepts_received) of
	true ->
	    NewState#state.respond_to ! {got_lock, LockID},
	    {next_state, locked, NewState};
	false ->
	    {next_state, accepting, NewState}
    end;
accepting(_Event, State) ->
    {next_state, accepting, State}.


locked(_Event, State) ->
    {next_state, locked, State}.


init([RespondTo]) ->
    {ok, idle, #state{respond_to=RespondTo}}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%% Helpers
is_majority(Replies) ->
    RepliesCount = length(Replies),
    AcceptorsCount = length(gaoler:get_nodes()),
    if 
	RepliesCount > AcceptorsCount div 2 ->
	    true;
	true ->
	    false
    end.
