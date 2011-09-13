-module(gaoler).
-behaviour(gen_server).

%% API
-export([
	 start_link/0,
	 get_nodes/0,
	 join/0,
	 stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {acceptors=[]}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join() ->
    {ok, AcceptorPid} = gaoler_sup:add_acceptor(),
    gen_server:abcast(?SERVER, {join, AcceptorPid}).

stop() ->
    gen_server:cast(?SERVER, stop).

get_nodes() ->
    gen_server:call(?MODULE, get_nodes).

%%%===================================================================
%%% gen_server functions
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(get_nodes, _From, State) ->
    {reply, State#state.acceptors, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({join, AcceptorPid}, State) ->
    link(AcceptorPid),
    Acceptors = [AcceptorPid|State#state.acceptors],
    NewState = State#state{acceptors = Acceptors},
    {noreply, NewState};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', Pid, _Reason}, State) ->
    Acceptors = lists:delete(Pid, State#state.acceptors),
    NewState = State#state{acceptors = Acceptors},
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
