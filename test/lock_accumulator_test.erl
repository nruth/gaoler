-module (lock_accumulator_test).
-include_lib("eunit/include/eunit.hrl").

datastore_without_lock_test() ->
    datastore:start(),
    {ok, 0} = datastore:read(),
    TimesToIncrement = 20,
    fork_run_with_barrier(TimesToIncrement, fun perform_unsafe_operation/0),

    % check if all processes has incremented the value
    {ok, Result} = datastore:read(),
    ?assertNotEqual(TimesToIncrement, Result),
    datastore:stop().

accumulator(Persistence) ->
    {timeout, 30, fun() ->
        lock:start_link(Persistence, simple_comms),
        datastore:start(),
        ?assertEqual({ok, 0}, datastore:read()),
        TimesToIncrement = 20,
        fork_run_with_barrier(TimesToIncrement, fun perform_atomic_operation/0),

        % check if all processes has incremented the value
        {ok, Result} = datastore:read(),
        datastore:stop(),
        lock:stop(),
        ?assertEqual(TimesToIncrement, Result)
    end}.

accumulated_consistency_test_() ->
    lists:map(
        fun accumulator/1, 
        [lock_no_persistence, lock_persist_holder, lock_persist_queue, lock_persist_state]
    ).

%% run fun Work on NumWorkers parallel procs
%% returns when all workers finish (barrier)
fork_run_with_barrier(NumWorkers, Work) ->
    Coordinator = self(),
    %% generate a fun wrapping the work to be done
    Worker = fun(F) ->
        fun() ->
            F(),
            Coordinator ! {self(), done} %% signal that work is done
        end
    end,

    % spawn processes on local node to perform operation
    Pids = [spawn_link(Worker(Work)) || _X <- lists:seq(1,NumWorkers)],

    % all processes have to complete before we can check return value
    wait_operations(Pids).

perform_unsafe_operation() ->
    read_and_increment_value().

perform_atomic_operation() ->
    lock:acquire(self()),
    wait_for_lock(),
    perform_unsafe_operation(),
    lock:release(self()).

wait_operations([Pid|Tail]) ->
    receive 
        {Pid, done} ->
            [ok|wait_operations(Tail)]
    end;
wait_operations([]) ->
    ok.

wait_for_lock() ->
    receive 
        lock -> ok
    end.

read_and_increment_value() ->
    {ok, Value} = datastore:read(),
    NewValue = Value + 1,
    ok = datastore:write(NewValue).
