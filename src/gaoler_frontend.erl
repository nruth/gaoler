-module(gaoler_frontend).

%% User API
-export([
	 get_lock/2,
	 release_lock/1,
	 steal_lock/2
	]).

%% Debugging
-export([
	 current_locks/0
	]).

%% Some random timeout
-define(TIMEOUT, 100).

%% User API
get_lock(LockID, Requester) ->
    % optimization: don't check lock which is already locked

    % spawn a new proposer and let him do the job
    % this should probably be linked and placed in supervisor tree somehow
    FrontEndPid = self(),
    Pid = spawn(fun() -> 
			proposer:start_new_proposal(LockID,
						    Requester,
						    FrontEndPid) 
		end),
    io:format("Started new proposer ~p ~n", [Pid]),

    % wait for the reply
    receive 
	{got_lock, ID} ->
	    io:format("Gave lock (~p) to ~p ~n", [ID, Requester]),
	    {ok, ID}
    after ?TIMEOUT ->
	    failed
    end.

release_lock(_LockID) ->
    ok.

steal_lock(_LockID, _ResourcePid) ->
    ok.


%% Debugging
current_locks() ->
    ok.
