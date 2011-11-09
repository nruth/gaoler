-module(gaoler_app_test).
-include_lib("eunit/include/eunit.hrl").


check_house_is_started_on_application_start_test() ->
    ?assertEqual(ok, application:start(gaoler)),
    ?assert(erlang:is_pid(erlang:whereis(house_sup))),
    ?assert(erlang:is_pid(erlang:whereis(acceptor0))),
    application:stop(gaoler).

multiple_houses_on_same_node_test() ->
    application:start(gaoler),
    gaoler_sup:start_house(),
    ?assert(erlang:is_pid(erlang:whereis(house_sup1))),
    ?assert(erlang:is_pid(erlang:whereis(acceptor1))),
    ?assert(erlang:is_pid(erlang:whereis(proposer1))),
    application:stop(gaoler).
    

%% start_house_sup_test() ->
%%     house_sup:start_link(house_sup, 0),
%%     ?assert(erlang:is_pid(erlang:whereis(house_sup_0))).
