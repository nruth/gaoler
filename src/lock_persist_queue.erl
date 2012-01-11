% persist the lock queue
% implicitly persists the lock holder (queue head)
-module (lock_persist_queue).
-export ([lock_granted/1, lock_available/1, lock_holder_changed/1, init/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% called to perform any start-up procedure for the lock persistence
%% e.g. create dets table
init() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, FileHandle} = file:open(lock_queue_store, [raw, write]), % overwrites, not append
    {ok, FileHandle}.


%% called when the lock is held by the server, 
%% and is being issued to a client
lock_granted(Queue) -> 
    ok = gen_server:call(?MODULE, {write, Queue}).

%% called when the lock is held by the server, ready for issue
lock_available(Queue) -> 
    ok = gen_server:call(?MODULE, {write, Queue}).

%% called when the lock is released by a client
%% and another client is queueing for it
%% so they will be issued it immediately
lock_holder_changed(Queue) -> 
    ok = gen_server:call(?MODULE, {write, Queue}).


%%% INTERNALS

handle_call({write, Term}, _From, FileHandle) ->
    atomic_persist(Term, FileHandle),
    {reply, ok, FileHandle}.

atomic_persist(Term, FileHandle) ->
    file:write(FileHandle, Term),
    file:datasync(FileHandle).

handle_cast(stop, State) -> 
    {stop, normal, State}.

terminate(_Reason, FileHandle) -> 
    file:close(FileHandle).


%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

