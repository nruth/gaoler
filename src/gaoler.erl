-module(gaoler).
-behaviour(gen_server).

%% API
-export([
	 start_link/0, 
	 join/1,
	 get_acceptors/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { is_leader = false,
		 acceptors = [] }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join(Acceptor) ->
    gen_server:abcast(?SERVER, {join, Acceptor, node()}). 

get_acceptors() ->
    gen_server:call(?SERVER, get_acceptors).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, GaolerNodes} = application:get_env(gaoler, nodes),
    
    % establish connection between nodes
    lists:all(fun(Node) -> case net_adm:ping(Node) of
			       pong -> true;
			       _ -> false
			   end
	      end, GaolerNodes), 
    {ok, #state{}}.

handle_call(get_acceptors, _From, State) ->
    Reply = State#state.acceptors,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({current_acceptors, NewAcceptors}, State) ->
    NewState = merge_acceptors(NewAcceptors, State), 
    {noreply, NewState};
handle_cast({join, AcceptorNode, From}, State) ->
    gen_server:cast({?SERVER, From}, 
		    {current_acceptors, State#state.acceptors}),
    NewState = handle_join(AcceptorNode, State), 
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Object, Info}, State) ->
    io:format("Removing ~p since ~p ~n", [Object,Info]),
    NewAcceptors = lists:keydelete(Object, 1, State#state.acceptors),
    NewState = State#state{acceptors = NewAcceptors},
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

merge_acceptors([], State) ->
    State;
merge_acceptors([{AcceptorNode, _Ref}|Tail], State) ->
    NewState = 
	case lists:keymember(AcceptorNode, 1, State#state.acceptors) of 
	    false ->
		handle_join(AcceptorNode, State);
	    true ->
		State
	end,
    merge_acceptors(Tail, NewState).

handle_join(AcceptorNode, State) ->
    Ref = erlang:monitor(process, AcceptorNode),
    State#state{acceptors = [{AcceptorNode, Ref}|State#state.acceptors]}.

    
