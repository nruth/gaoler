-module(gaoler).
-behaviour(gen_server).

-include_lib("gaoler_state.hrl").

%% API
-export([
	 start_link/0, 
	 get_acceptors/0,
         request/2,
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
         do_handle_acquire/3
	]).

-define(SERVER, ?MODULE). 
-define(REQUIRED_MAJORITY, 3).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

request(Resource, Client) ->
    handle_acquire(Resource, Client).

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

handle_acquire(Resource, Client) ->
    Parent = self(),
    spawn(fun() ->
                  ?MODULE:do_handle_acquire(Resource, Client, Parent)
          end),
    receive
        {resource, Resource, _}=Result ->
            Result
    after 1000 ->
            {error, timed_out}
    end.
            

do_handle_acquire(Resource, Client, Parent) ->
    {ok, Ticket} = ticket_machine:next(),
    case coordinator:put(Ticket, Client, 1000) of 
        {ok, Client} ->
            Parent ! {resource, Resource, {Client, Ticket}};
        %% {ok, _OtherNumber} ->
        %%     % someone else got the lock, try again (with backoff time)
        %%     handle_acquire(Resource);
        {error, _}=Error ->
            Error
    end.
