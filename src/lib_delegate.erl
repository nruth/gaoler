% sending work to background pids and getting a response, simple rpc
-module (lib_delegate).
-export ([execute/2, run_in_proc/1]).

run_in_proc(Fun) ->
    spawn_link(fun() -> ?MODULE:execute(self(), Fun) end),
    receive
        Value -> Value
    end.
    
execute(From, Fun) ->
    From ! Fun().

    