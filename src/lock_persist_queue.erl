% persist the lock queue
% implicitly persists the lock holder (queue head)
-module (lock_persist_queue).
-export ([lock_granted/1, lock_available/1, lock_holder_changed/1, init/0]).

%% called to perform any start-up procedure for the lock persistence
%% e.g. create dets table
init() ->
    {ok, lock_queue_storage} = dets:open_file(lock_queue_storage, []).

%% called when the lock is held by the server, 
%% and is being issued to a client
lock_granted(Queue) -> 
    ok = dets:insert(lock_queue_storage, Queue).

%% called when the lock is held by the server, ready for issue
lock_available(Queue) -> 
    ok = dets:insert(lock_queue_storage, Queue).

%% called when the lock is released by a client
%% and another client is queueing for it
%% so they will be issued it immediately
lock_holder_changed(Queue) -> 
    ok = dets:insert(lock_queue_storage, Queue).
