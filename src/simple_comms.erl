-module (simple_comms).

-export ([send_lock/1]).

send_lock(Receiver) ->
    Receiver ! lock.