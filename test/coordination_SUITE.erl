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
          coordinator_no_proposed_value_test,
          coordinator_put_new_proposal_test,
          gaoler_acquire_and_release_lock_test,
          acquire_two_locks_immediately_release_test
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

coordinator_no_proposed_value_test(_Config) ->
    unknown = coordinator:get().


coordinator_put_new_proposal_test(_Config) ->
    Value = my_value,
    {ok, Election} = ticket_machine:next(),
    {ok, Value} = coordinator:put(Election, Value, 1000).

% TODO: add testing for caching

   
gaoler_acquire_and_release_lock_test(_Config) ->
    ok = gaoler:acquire(beer),   
    ok = gaoler:release(beer).


acquire_two_locks_immediately_release_test(_Config) ->
    ok = gaoler:acquire(beer),
    ok = gaoler:release(beer),
    ok = gaoler:acquire(beer),
    ok = gaoler:release(beer).
