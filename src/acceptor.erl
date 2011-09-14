-module(acceptor).
-behaviour(gen_fsm).

%% API
-export([
	 start_link/0,
	 promise/0,
	 accept/0
	]).

%% gen_fsm callbacks
-export([init/1, idle/2, idle/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

promise() ->
    % if promise, move to proposing state
    % else nack, move to idle
    ok.

accept() ->
    % if accept, send value to all learners, move to idle
    % else nack, move to idle
    ok.


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([]) ->
    {ok, idle, #state{}}.

idle(_Event, State) ->
    {next_state, idle, State}.

idle(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, idle, State}.

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
