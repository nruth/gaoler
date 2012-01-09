% persist the current lock state
% available / granted
-module (lock_persist_state).
-export ([lock_granted/1, lock_available/1, lock_holder_changed/1, init/0]).

%% called to perform any start-up procedure for the lock persistence
%% e.g. create dets table
init() ->
    {ok, lock_state} = dets:open_file(lock_state, []).

%% called when the lock is held by the server, 
%% and is being issued to a client
lock_granted(_) -> 
    ok = dets:insert(lock_state, {lock,granted}).

%% called when the lock is held by the server, ready for issue
lock_available(_) -> 
    ok = dets:insert(lock_state, {lock,available}).

%% called when the lock is released by a client
%% and another client is queueing for it
%% so they will be issued it immediately
lock_holder_changed(_) -> 
    % no-op; the lock is already in granted state
    ok.
