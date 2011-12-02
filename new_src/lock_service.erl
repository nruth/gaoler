-module (lock_service).
-behaviour(gen_server).
%% API
-export ([acquire/3, release/1]).

-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% execute Critical fun when lock for Resource is acquired
% and release the lock when Critical returns
% fun()::Critical is executed by the calling process
acquire(Resource, Critical, Timeout) where is_function(Critical, 0) ->
    % insert into lock queue
    % async await callback
    % run critical code (fun) on receiving callback 
    % release lock
    wait = gen_server:call({join_queue, Resource}),
    receive
        {lock, Resource} -> Critical();
    after
        Timeout -> timeout
    end.


release(Ticket) ->
    gen_server:call({advance_queue, lib_ticket:resource(Ticket)}),
    receive
        {lock, Resource} -> apply(Critical, []);
    after
        Timeout -> timeout
    end.

stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({join_queue, Resource}, From, State) ->
    NewState = State#state{queue = join_queue(State#state.queue, From)},
    Reply = wait,
    {reply, Reply, NewState}.

handle_call({advance_queue, Resource}, _From, State) ->
    NewState = State#state{queue = advance_queue(State#state.queue)},
    Reply = ok,
    {reply, Reply, NewState}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% TODO: persistence / FT with paxos
advance_queue( [Head | Tail] ) -> 
    Tail.

%% TODO: persistence / FT with paxos
join_queue(Queue, Elem) ->
    Queue ++ Elem.
