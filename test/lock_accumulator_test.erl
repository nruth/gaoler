-module (lock_accumulator_test).
-include_lib("eunit/include/eunit.hrl").

datastore_without_lock_test() ->
    datastore:start(),
    {ok, 0} = datastore:read(),
    TimesToIncrement = 100,
    Coordinator = self(),
    % spawn processes on local node to perform operation
    Pids = [spawn_link(fun() -> perform_unsafe_operation(Coordinator) end) 
            || _X <- lists:seq(1,TimesToIncrement)],
    % all processes have to complete before we can check return value
    wait_operations(Pids),

    % check if all processes has incremented the value
    {ok, Result} = datastore:read(),
    ?assertNotEqual(TimesToIncrement, Result),
    datastore:stop().

accumulated_consistency_test() ->
    lock:start_link(lock_no_persistence, simple_comms),
    datastore:start(),
    {ok, 0} = datastore:read(),
    TimesToIncrement = 100,
    Coordinator = self(),

    % spawn processes on local node to perform operation
    Pids = [spawn_link(fun() -> perform_atomic_operation(Coordinator) end) 
            || _X <- lists:seq(1,TimesToIncrement)],

    % all processes have to complete before we can check return value
    wait_operations(Pids),

    % check if all processes has incremented the value
    {ok, TimesToIncrement} = datastore:read(),
    datastore:stop().

perform_unsafe_operation(Coordinator) ->
    read_and_increment_value(),
    Coordinator ! {self(), done}.

perform_atomic_operation(Coordinator) ->
    lock:acquire(self()),
    wait_for_lock(),
    perform_unsafe_operation(Coordinator),
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
