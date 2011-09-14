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

    % start a new proposer and let it work (slavery ftw)
    % this proposer probably be monitored a bit... supervisor tree?
    FrontEndPid = self(),
    {ok, Pid} = proposer:start_link(FrontEndPid),
    proposer:propose(Pid, LockID, Requester),

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
