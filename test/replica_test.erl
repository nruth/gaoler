-module(replica_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    Mods = [centralised_lock, proposer],
    meck:new(Mods),
    meck:expect(centralised_lock, acquire, 1, ok),
    meck:expect(centralised_lock, release, 1, ok),
    meck:expect(proposer, propose, 
                fun({Slot, Proposal}) ->
                        self() ! {decision, Slot, Proposal} 
                end),
    replica:start(centralised_lock),
    Mods.

teardown(Mods) ->
    replica:stop(),
    meck:unload(Mods).

replica_test_() ->
    {foreach, 
     fun setup/0,
     fun teardown/1,
     [
      fun request_api_acquire/0,
      fun request_api_release/0
     ]
    }.

request_api_acquire() ->
    ?assertEqual(ok, replica:request(acquire, self())),
    timer:sleep(10),
    ?assert(meck:called(proposer, propose, '_')).

request_api_release() ->
    ?assertEqual(ok, replica:request(release, self())),
    timer:sleep(10),
    ?assert(meck:called(proposer, propose, '_')).
