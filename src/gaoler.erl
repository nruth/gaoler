-module(gaoler).
-behaviour(gen_server).

%% API
-export([
	 start_link/0,
	 get_nodes/0,
	 register_new_node/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {nodes=[]}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_new_node(Node) ->
    gen_server:call(?MODULE, {new_node, Node}).

get_nodes() ->
    gen_server:call(?MODULE, get_nodes).

%%%===================================================================
%%% gen_server functions
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(get_nodes, _From, State) ->
    {reply, State#state.nodes, State};
handle_call({new_node, Node}, _From, State) ->
    Nodes = [Node|State#state.nodes],
    NewState = State#state{nodes = Nodes},
    {reply, {ok, Node}, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
