-module (lock).
-behaviour(gen_server).
-export ([acquire/1, release/1, get_queue/0, stop/0]).
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("lock_state.hrl").
-define (SERVER, ?MODULE).

%%% API
start_link(PersistenceModule, CommsModule) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PersistenceModule, CommsModule], []).

acquire (Client) ->
    gen_server:call(?SERVER, {acquire, Client}).

release (Client) ->
    gen_server:call(?SERVER, {release, Client}).

get_queue() -> 
    gen_server:call(?SERVER, get_queue).
    
stop() ->
    gen_server:cast(?SERVER, stop).

%%% Internals

%% starts with empty data store and sets the lock module
init([PersistenceModule, CommsModule]) ->
    PersistenceModule:init(),
    {ok, #state{
        queue = queue:new(), 
        persistence_module = PersistenceModule,
        comms_module = CommsModule
    }}.

%% gen_server callback
handle_call(get_queue, _From, State) ->
    {reply, State#state.queue, State};
handle_call({acquire, Client}, _From, State) ->
    {reply, ok, handle_acquire_req(Client, State)};
handle_call({release, Client}, _From, State) -> 
    {reply, ok, handle_release_req(Client, State)}.

%% add the client to the lock queue, and
%% give them the lock if nobody else was waiting
handle_acquire_req(Client, #state{queue=Queue}=State) ->
    NewQueue = queue:in(Client, Queue),
    NewState = State#state{queue=NewQueue},

    % if the queue was empty we can send out the lock
    case queue:is_empty(Queue) of
        true -> persistence_callback(lock_granted, [NewQueue], NewState),
                comms(send_lock, [Client], State);
        false -> noop
    end,
    NewState.

%% give the current lock holder from the queue
%%  and give the lock to the next in queue (if any)
handle_release_req(_Client, State) ->
    % release the lock, removing the queue head who held it
    case queue:out(State#state.queue) of
        % the lock was held
        {{value, _Releasing}, NewQueue} ->
            % is someone waiting in the queue?
            case queue:peek(NewQueue) of
                {value, NextLockHolder} -> 
                    % yes: send them the lock
                    persistence_callback(lock_holder_changed, [NewQueue], State),
                    comms(send_lock, [NextLockHolder], State);
                empty -> 
                    % no: wait for the next request
                    persistence_callback(lock_available, [NewQueue], State)
            end,
            State#state{queue=NewQueue};
        {empty, EmptyQueue} -> % no lock is held, do nothing
            State#state{queue=EmptyQueue}
    end.

%% gen_server callback
handle_cast(stop, State) -> 
    {stop, normal, State}.

%% calls a function on the lock module set in State
%% Args should be a list, but will be called as separate params
persistence_callback(FunctionName, Args, State) ->
    apply(State#state.persistence_module, FunctionName, Args).

%% calls a function on the comms module set in State
%% Args should be a list, but will be called as separate params
comms(FunctionName, Args, State) -> 
    apply(State#state.comms_module, FunctionName, Args).

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
