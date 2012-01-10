% README: run from erl interactive with: 
% cd(ebin). 
% file:script('../scripts/ndlg_profiling.erl').

%%% PARAMS
PersistenceModules = [
    lock_no_persistence, 
    lock_persist_state, 
    lock_persist_holder, 
    lock_persist_queue
].
Duration = 5000.
Workers = 50.
PauseBetweenRuns = 2000.


%% IMPLEMENTATION

Lock = fun() ->
lock:acquire(self()),
receive lock -> ok end,
lock:release(self())
end.

TmpResultFilename = io_lib:format("throughput_~psec_~pprocs", [Duration, Workers]).

lists:map( 
    fun(PersistenceMod) ->
        io:format('.'),
        timer:sleep(PauseBetweenRuns),
        lock:start_link(PersistenceMod, simple_comms),
        measurer:throughput(Lock, Duration, Workers),
        lock:stop(),
        % dynamically create and exec `mv tmpfile resultfile`
        os:cmd(io_lib:format("mv ~s ~p_~s", [TmpResultFilename, PersistenceMod, TmpResultFilename]))
    end,
    PersistenceModules
).
