-module(ticket_machine_test).
-include_lib("eunit/include/eunit.hrl").

next_number_test() ->
    ticket_machine:start_link(),
    ?assertEqual({ok, 1}, ticket_machine:next()),
    ticket_machine:stop().
    
