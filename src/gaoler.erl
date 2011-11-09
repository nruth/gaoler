-module(gaoler).
-behaviour(gen_server).

%% API
-export([
	 start_link/0, 
	 join/0,
	 get_acceptors/0,
	 stop/0
	]).

%% gen_server callbacks
-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3
	]).

-define(SERVER, ?MODULE). 
-define(REQUIRED_MAJORITY, 3).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join() ->
    gen_server:abcast(?SERVER, {join, node()}). 

get_acceptors() ->
    gen_server:call(?SERVER, get_acceptors).

stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


handle_call(get_acceptors, _From, State) ->
    Reply = [{acceptor, Node} || Node <- [node()|nodes()]],
    {reply, Reply, State}.


handle_cast({join, Node}, State) ->
    erlang:monitor_node(Node, true),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({nodedown, Node}, State) ->
    io:format("Lost node: ~p~n", [Node]),
    case length(gaoler:get_acceptors()) < ?REQUIRED_MAJORITY of 
	true ->
	    io:format("FATAL: Insufficient majority.~n", []);
	false ->
	    ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

    
