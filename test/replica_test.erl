-module(replica_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    Mods = [lock, proposer, acceptor],
    meck:new(Mods),
    meck:expect(acceptor, gc_this, 3, ok),
    meck:expect(lock, acquire, 1, ok),
    meck:expect(lock, release, 1, ok),
    meck:expect(proposer, propose, 
                fun({Slot, Proposal}) ->
                        self() ! {decision, Slot, Proposal} 
                end),
    replica:start_link(lock),
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
    timer:sleep(100),
    ?assert(meck:called(proposer, propose, '_')).

request_api_release() ->
    ?assertEqual(ok, replica:request(release, self())),
    timer:sleep(100),
    ?assert(meck:called(proposer, propose, '_')).
