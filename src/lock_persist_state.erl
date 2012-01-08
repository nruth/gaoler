% persist the current lock state
% available / granted
-module (lock_persist_state).
-export ([lock_granted/1, lock_available/1, lock_holder_changed/1]).

%% called when the lock is held by the server, 
%% and is being issued to a client
lock_granted(_) -> 
    % do nothing, no persistence
    ok.

%% called when the lock is held by the server, ready for issue
lock_available(_) -> 
    % do nothing, no persistence
    ok.

%% called when the lock is released by a client
%% and another client is queueing for it
%% so they will be issued it immediately
lock_holder_changed(_) -> 
    % do nothing, no persistence
    ok.