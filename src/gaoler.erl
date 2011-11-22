-module(gaoler).
-behaviour(gen_server).

-include_lib("gaoler_state.hrl").

%% API
-export([
	 start_link/0, 
	 join/0,
	 get_acceptors/0,
	 stop/0,
	 subscribe/1,
	 unsubscribe/1,
	 deliver/1
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

%% internal child processes
-export([
	 deliver_message/2
	]).

-define(SERVER, ?MODULE). 
-define(REQUIRED_MAJORITY, 3).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

subscribe(Callback) when is_function(Callback, 1) ->
    gen_server:call(?SERVER, {subscribe, Callback});
subscribe({_Module, _Function}=Callback) ->
    gen_server:call(?SERVER, {subscribe, Callback}).

unsubscribe(Callback) ->
    gen_server:call(?SERVER, {unsubscribe, Callback}).

deliver(Value) ->
    gen_server:cast(?SERVER, {deliver, Value}).

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
    {ok, Nodes} = application:get_env(gaoler, nodes),
    % ping nodes from configuration
    [spawn(fun() -> net_adm:ping(Node) end) || Node <- Nodes],
    {ok, #state{}}.


handle_call(get_acceptors, _From, State) ->
    Reply = [{acceptor, Node} || Node <- [node()|nodes()]],
    {reply, Reply, State};
handle_call({subscribe, {Module, Function}=ModFun}, _From, State) ->
    {Reply, NewState} = 
	case erlang:function_exported(Module, Function, 1) of
	    true ->
		{ok, add_subscriber(ModFun, State)};
	    false ->
		{{error, not_exported}, State}
	end,
    {reply, Reply, NewState};
handle_call({subscribe, Callback}, _From, State) ->
    NewState = add_subscriber(Callback, State),
    {reply, ok, NewState};
handle_call({unsubscribe, Callback}, _From, State) ->
    NewState = State#state{subscribers = 
			       lists:delete(Callback, 
					    State#state.subscribers)},
    {reply, ok, NewState}.

handle_cast({join, Node}, State) ->
    erlang:monitor_node(Node, true),
    {noreply, State};
handle_cast({deliver, Value}, State) ->
    spawn_link(fun() -> 
		       ?MODULE:deliver_message(Value, 
					       State#state.subscribers) 
	       end),
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
handle_info({'EXIT', _FromPid, Reason}, State) ->
    io:format("Malformed callback: ~p~n", [Reason]),
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

deliver_message(_, []) -> 
    ok;
deliver_message(Value, [{Module,Function}|Tail]) -> 
    Module:Function(Value),
    deliver_message(Value, Tail);
deliver_message(Value, [Function|Tail]) when is_function(Function) -> 
    Function(Value),
    deliver_message(Value, Tail).

add_subscriber(Callback, State) -> 
    State#state{subscribers = [Callback|State#state.subscribers]}.
