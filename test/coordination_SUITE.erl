-module(coordination_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").
-include_lib("persister.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specify a list of all unit test functions
all() -> [
          single_request_test,
          accumulated_consistency_test
         ].

init_per_suite(Config) ->
    % start four slaves
    NodeConfiguration = "-setcookie gaoler",
    Slaves = [slave1, slave2, slave3, slave4],
    [{ok, _} = slave:start(list_to_atom(net_adm:localhost()), 
                           Slave, NodeConfiguration) || 
        Slave <- Slaves],
    [{slaves, Slaves}|Config].


end_per_suite(Config) ->
    [slave:stop(Slave) || Slave <- nodes()],
    Config.

init_per_testcase(_TestCase, Config) ->
    % start gaoler on master
    ok = application:start(gaoler),

    % start gaoler on all slaves
    [ok = rpc:call(Node, application, start, [gaoler]) || 
        Node <- nodes()],
    clean_up_logdirectory(),
    Config.

end_per_testcase(_TestCase, Config) ->
    application:stop(gaoler), 
    [rpc:call(Slave, application, stop, [gaoler]) || Slave <- nodes()],
    clean_up_logdirectory(),
    Config.


clean_up_logdirectory() ->
    file:delete(?LOGFILE), % old
    case file:list_dir(?LOGDIRECTORY) of
        {ok, Files} ->
            [file:delete(?LOGDIRECTORY++"/"++File) || File <- Files];
        _ ->
            ok
    end,
    file:del_dir(?LOGDIRECTORY).

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%
   
single_request_test(_Config) ->
    ok = replica:request(acquire, self()).
    
accumulated_consistency_test(_Config) ->
    datastore:start(),
    {ok, 0} = datastore:read(),
    TimesToIncrement = 5000,
    Self = self(),

    % spawn processes on local node to perform operation
    Pids = [spawn_link(fun() -> perform_atomic_operation(Self) end) 
            || _X <- lists:seq(1,TimesToIncrement)],

    % all processes have to complete before we can check return value
    wait_atomic_operations(Pids),

    % check if all processes has incremented the value
    {ok, TimesToIncrement} = datastore:read(),
    
    datastore:stop().

wait_atomic_operations([Pid|Tail]) ->
    receive 
        {Pid, done} ->
            [ok|wait_atomic_operations(Tail)]
    end;
wait_atomic_operations([]) ->
    [].

perform_atomic_operation(Parent) ->
    Client = self(),
    ok = replica:request(acquire, Client),
    wait_for_lock(),
    read_and_increment_value(),
    ok = replica:request(release, Client),
    Parent ! {self(), done}.

wait_for_lock() ->
    receive 
        lock ->
            ok
    end.

read_and_increment_value() ->
    {ok, Value} = datastore:read(),
    NewValue = Value + 1,
    ok = datastore:write(NewValue).
  

