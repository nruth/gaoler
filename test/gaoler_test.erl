-module(gaoler_test).
-include_lib("eunit/include/eunit.hrl").

-define(HANDLER, some_handler).

application_start_test() ->
    ok = application:start(gaoler),
    ?assertNot(undefined == whereis(gaoler_sup)).

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
register_new_node_test() ->
    Node = node,
    ?assert({ok, Node} == gaoler:register_new_node(Node)).

get_nodes_test() ->
    ?assert([node] == gaoler:get_nodes()).
