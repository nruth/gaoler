-module(centralised_lock_integ_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("queue_lib.hrl").

lock_test() ->
    centralised_lock:start_link(),
    ?assert( ?QUEUE_LIB:is_empty(centralised_lock:get_queue()) ),

    Mock = nspy:mock(),
    centralised_lock:acquire(Mock),
    ?assertNot( ?QUEUE_LIB:is_empty(centralised_lock:get_queue()) ),
    nspy:assert_message_received(Mock, lock),

    centralised_lock:release(Mock),
    ?assert( ?QUEUE_LIB:is_empty(centralised_lock:get_queue()) ),
    
    centralised_lock:stop().