-module(gaoler_app_test).
-include_lib("eunit/include/eunit.hrl").


check_house_is_started_on_application_start_test() ->
    ?assertEqual(ok, application:start(gaoler)),
    ?assert(erlang:is_pid(erlang:whereis(house_sup))),
    ?assert(erlang:is_pid(erlang:whereis(acceptor))),
%    ?assert(erlang:is_pid(erlang:whereis(proposer))),
    application:stop(gaoler).
