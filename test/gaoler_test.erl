-module(gaoler_test).
-include_lib("eunit/include/eunit.hrl").

-define(HANDLER, some_handler).

application_start_test() ->
    ok = application:start(gaoler),
    ?assertNot(undefined == whereis(gaoler_sup)),
    gaoler:join(), % acceptor 1

    % add 4 more acceptors
    supervisor:start_child(gaoler_sup, 
			   {acceptor2, {acceptor, start_link, []},
			    permanent, 5000, worker, [acceptor2]}),
    supervisor:start_child(gaoler_sup, 
			   {acceptor3, {acceptor, start_link, []},
			    permanent, 5000, worker, [acceptor3]}),
    supervisor:start_child(gaoler_sup, 
			   {acceptor4, {acceptor, start_link, []},
			    permanent, 5000, worker, [acceptor4]}),
    supervisor:start_child(gaoler_sup, 
			   {acceptor5, {acceptor, start_link, []},
			    permanent, 5000, worker, [acceptor5]}).


get_lock_test() ->
    LockID = ?HANDLER,
    Requester = self(),

    ?assertEqual({ok, LockID},
		gaoler_frontend:get_lock(LockID,
					 Requester)).

release_lock_test() ->
    ?assert(ok == gaoler_frontend:release_lock(?HANDLER)).

steal_lock_test() ->
    ?assert(ok == gaoler_frontend:steal_lock(?HANDLER, self())).

%% experiments - not sure about these
get_nodes_test() ->
    Nodes = gaoler:get_nodes(),
    ?assert(length(Nodes) == 1).

