-module (replicated_lock).
-include_lib("replicated_lock_state.hrl").

-export ([acquire/1, release/1, get_queue/0, stop/0]).

-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define (SERVER, ?MODULE).

%%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

acquire (Client) ->
    BroadcastFun = fun() ->
        gen_server:abcast(?SERVER, {acquire, Client}),
        receive
            {ack, acquire} -> ok
        end
    end,
    lib_delegate:run_in_proc(BroadcastFun).

release (Client) ->
    BroadcastFun = fun() ->
        gen_server:abcast(?SERVER, {release, Client}),
        receive
            {ack, release} -> ok
        end
    end,
    lib_delegate:run_in_proc(BroadcastFun).

get_queue() -> 
    gen_server:call(?SERVER, get_queue).
    
stop() ->
    gen_server:cast(?SERVER, stop).

%%% internals

% starts with empty data store
init([]) -> {ok, #state{queue = queue:new()}}.

handle_call(get_queue, _From, State) ->
    {reply, State#state.queue, State};

handle_call({deliver, {SlotNumberOperation, Operation}}, _From,
            #state{slot=SlotNumberOperation}=State) ->
    NewFutureOps = [{SlotNumberOperation, Operation}|State#state.future],
    StateWithOpAdded = State#state{future = NewFutureOps},
    NewState = deliver_to_queue(SlotNumberOperation, StateWithOpAdded),
    {reply, ok, NewState};
handle_call({deliver, {_SlotNumberOperation, _Operation}=FutureOperation}, _From, State) ->
    NewState = State#state{future=[FutureOperation|State#state.future]},
    {reply, ok, NewState}.

% Deliver received operations to the lock replica
deliver_to_queue(Slot, State) ->
    SortedOpsState = State#state{future = lists:sort(State#state.future)},
    aux_deliver_to_queue(Slot, SortedOpsState).

% assumes Operations is sorted
aux_deliver_to_queue(Slot, State) ->
    case State#state.future of
        [{Slot, Op}|Ops] ->
            NewState = deliver(Op, State#state{future=Ops}),
            aux_deliver_to_queue(Slot + 1, NewState);
        _ -> 
            State
    end.

deliver({acquire, Client}, State) -> acquire(Client, State);
deliver({release, Client}, State) -> release(Client, State).

handle_cast({acquire, Client}, State) ->
    Client ! {ack, acquire},
    {noreply, acquire(Client, State)};
handle_cast({release, Client}, State) -> 
    Client ! {ack, release},
    {noreply, release(Client, State)};
handle_cast(stop, State) -> 
    {stop, normal, State}.

%% add the client to the lock queue, and
%% give them the lock if nobody else was waiting
acquire(Client, State) ->
    Queue = State#state.queue,
    case queue:is_empty(Queue) of
        true -> send_lock(Client);
        false -> noop
    end,
    NewQueue = queue:in(Client, Queue),
    State#state{queue=NewQueue}.

%% give the current lock holder from the queue
%%  and give the lock to the next in queue (if any)
release(_Client, State) ->
    case queue:out(State#state.queue) of
        {{value, _Releasing}, NewQueue} ->
            %remove lock, send new if any
            case queue:peek(NewQueue) of
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



%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
