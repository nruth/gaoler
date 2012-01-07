-module(gaoler).
-behaviour(gen_server).

-include_lib("gaoler_state.hrl").

%% API
-export([
	 start_link/0, 
	 get_acceptors/0,
         majority/0,
         acquire/1,
	 join/0,
	 stop/0
	]).

%% gen_server callbacks
-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3,
         handle_operation/1
	]).

-define(SERVER, ?MODULE). 
-define(REQUIRED_MAJORITY, 3).
-define(DEFAULT_MAJORITY, 3).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

acquire(Client) ->
    spawn_link(fun() -> ?MODULE:handle_operation({acquire, Client}) end),
    ok.

handle_operation(Operation) ->            
    {ok, Ticket} = ticket_machine:next(),
    case coordinator:put(Ticket, Operation, 1000) of 
        {ok, Operation} ->
            % deliver lock to all replicas
            % should use the replicated_lock:deliver/1 function
            gen_server:abcast(replicated_lock, {deliver, {Ticket, Operation}});
        {taken, _OperationInSlot} ->
             % someone else got the lock, try again with a higher number
            handle_operation(Operation);
        {error, _}=Error ->
            Error
    end.

majority() ->
    gen_server:call(?SERVER, majority).

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
    
    % read config
    Configuration = 
        case file:consult("gaoler.config") of
            {ok, ReadConfig} ->
                ReadConfig;
            {error, _} ->
                [{majority, ?DEFAULT_MAJORITY}, {nodes, []}]
        end,
    InitialState = #state{configuration = Configuration},

    % ping nodes from configuration
    Nodes = proplists:get_value(nodes, InitialState#state.configuration, []),
    [spawn(fun() -> net_adm:ping(Node) end) || Node <- Nodes],
    {ok, InitialState}.

handle_call(majority, _From, State) ->
    {reply, proplists:get_value(majority, State#state.configuration), State};
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

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
