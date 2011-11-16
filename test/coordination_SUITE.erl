-module(coordination_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include("ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specify a list of all unit test functions
all() -> [slave_test].

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
    
    Config.

end_per_testcase(_TestCase, Config) ->
    application:stop(gaoler), 
    [rpc:call(Slave, application, stop, [gaoler]) || Slave <- nodes()],
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

slave_test(Config) ->
    'master@archie-laptop' = node().
