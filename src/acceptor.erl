-module(acceptor).
-behaviour(gen_fsm).

%% API
-export([
	 start_link/0,
	 promise/2,
	 accept/2
	]).

%% States
-export([
	 idle/2,
	 promised/2
	]).

%% gen_fsm callbacks
-export([
	 init/1, 
	 handle_event/3,
	 handle_sync_event/4, 
	 handle_info/3, 
	 terminate/3, 
	 code_change/4
	]).

-define(SERVER, ?MODULE).

-record(state, {lock}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

promise(Proposer, Lock) ->
    % impl fsm_abcast()?
    [gen_fsm:send_event(Acceptor, {promise, Proposer, Lock}) ||
	Acceptor <- gaoler:get_nodes()].

accept(Proposer, Lock) ->
    % if accept, send value to all learners, move to idle
    % else nack, move to idle
    [gen_fsm:send_event(Acceptor, {accept, Proposer, Lock}) ||
	Acceptor <- gaoler:get_nodes()].


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([]) ->
    {ok, idle, #state{}}.

idle({promise, Proposer, Lock}, State) ->
    % if promise, move to proposing state
    % else nack, move to idle
    NewState = State#state{lock=Lock},
    proposer:accept_promise(Proposer, Lock, self()), % unique id?
    {next_state, promised, NewState}.

promised({accept, Proposer, Lock}, 
	 #state{lock=Lock}=State) ->
    proposer:accept(Proposer, Lock, self()),
    {next_state, idle, #state{}};
promised(_Event, State) ->
    {next_state, promised, State}.


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

%%%===================================================================
%%% Internal functions
%%%===================================================================
