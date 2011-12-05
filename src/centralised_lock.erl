-module (centralised_lock).
-export ([acquire/1, release/1, get_queue/0, stop/0]).

-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record (state, {queue = undefined}).
-define (SERVER, ?MODULE).

%%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

acquire (Client) ->
    gen_server:call(?SERVER, {acquire, Client}).

release (Client) ->
    gen_server:call(?SERVER, {release, Client}).

get_queue() -> 
    gen_server:call(?SERVER, get_queue).
    
stop() ->
    gen_server:cast(?SERVER, stop).

%%% internals

% starts with empty data store
init([]) -> {ok, #state{queue = new_queue()}}.

handle_call({acquire, Client}, _From, State) ->
    {reply, ok, acquire(Client, State)};
handle_call(get_queue, _From, State) ->
    {reply, State#state.queue, State};
handle_call({release, Client}, _From, State) -> 
    {reply, ok, release(Client, State)}.

handle_cast(stop, State) -> 
    {stop, normal, State}.

%% add the client to the lock queue, and
%% give them the lock if nobody else was waiting
acquire(Client, State) ->
    Queue = State#state.queue,
    case is_empty(Queue) of
        true -> send_lock(Client);
        false -> noop
    end,
    NewQueue = append(Client, Queue),
    State#state{queue=NewQueue}.

%% give the current lock holder from the queue
%%  and give the lock to the next in queue (if any)
release(_Client, State) ->
    case pop_head(State#state.queue) of
        {{value, _Releasing}, NewQueue} ->
            %remove lock, send new if any
            case current_lock_holder(NewQueue) of
                {value, NextLockHolder} ->
                    send_lock(NextLockHolder);
                empty -> 
                    noop
            end,
            State#state{queue=NewQueue};
        {empty, EmptyQueue} -> % no lock is held, do nothing
            State#state{queue=EmptyQueue}
    end.


send_lock(Receiver) ->
    Receiver ! lock.

%%% Internals

%% returns true or false 
is_empty(Queue) ->
    queue:is_empty(Queue).

%% obeys queue:peek api
%% returns {value, QueueHead} or empty
current_lock_holder(Queue) ->
    queue:peek(Queue).

new_queue() ->
    queue:new().

append(Elem, Queue) ->
    queue:in(Elem, Queue).

%% mimics queue:out api
%% returns {empty, EmptyQueue} or {{value, Head}, QueueTail}
pop_head(Queue) ->
    queue:out(Queue).

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
