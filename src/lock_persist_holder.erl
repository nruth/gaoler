% persist the current lock holder
% when no holder sets to none
-module (lock_persist_holder).
-export ([lock_granted/1, lock_available/1, lock_holder_changed/1, init/0]).

%% called to perform any start-up procedure for the lock persistence
%% e.g. create dets table
init() ->
    {ok, lock_holder} = dets:open_file(lock_holder, []).

%% called when the lock is held by the server, 
%% and is being issued to a client
lock_granted(Queue) -> 
    ok = dets:insert(lock_holder, {lock_holder, queue:peek(Queue)}),
    dets:sync(lock_holder).

%% called when the lock is held by the server, ready for issue
lock_available(_Queue) -> 
    ok = dets:insert(lock_holder, {lock_holder, none}),
    dets:sync(lock_holder).

%% called when the lock is released by a client
%% and another client is queueing for it
%% so they will be issued it immediately
lock_holder_changed(Queue) -> 
    ok = dets:insert(lock_holder, {lock_holder, queue:peek(Queue)}),
    dets:sync(lock_holder).
