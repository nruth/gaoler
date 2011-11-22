-module(coordination_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specify a list of all unit test functions
all() -> [
%          coordinator_no_proposed_value_test,
          coordinator_put_new_proposal_test
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
    
    Config.

end_per_testcase(_TestCase, Config) ->
    application:stop(gaoler), 
    [rpc:call(Slave, application, stop, [gaoler]) || Slave <- nodes()],
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

coordinator_no_proposed_value_test(Config) ->
    unknown = coordinator:get().


coordinator_put_new_proposal_test(Config) ->
    Value = my_value,
    {result, Value} = coordinator:put(Value, 1000),
    Value = coordinator:get(),
    Value = coordinator:put(else, 1000).
   


