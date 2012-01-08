-module(acceptors_test).
-include_lib("eunit/include/eunit.hrl").
-define(NOTEST, true).

setup() ->
    Mods = [proposer, gaoler, learners],
    meck:new(Mods),
    meck:expect(proposer, deliver_promise, 2, ok),
    meck:expect(proposer, deliver_accept, 2, ok),
    meck:new(acceptor, [passthrough]),
    [acceptor|Mods].
    
teardown(Mods) ->
    meck:unload(Mods).

%% TestCase: broadcast messages
proxy_test_() ->
    {foreach, 
     fun setup/0, 
     fun teardown/1,
     [
      fun acceptor_reply_with_promise/0,
      fun acceptor_reply_with_promises_one_acceptor_crash/0,
      fun on_send_accept_acceptors_reply/0
     ]
    }.

acceptor_reply_with_promise() ->
    Round = {1,1},
    
    % start 5 acceptors
    Acceptors = start_acceptors(5),

    % issue broadcast
    acceptors:send_promise_requests(proposer, Round),
    timer:sleep(50),

    % check that all acceptors got called
    Promises = [meck:called(acceptor, prepare, [Acceptor, Round]) ||
     		   Acceptor <- Acceptors],
    ?assert(lists:all(fun(true) -> true;
			 (_) -> false end, Promises)),

    % check that all acceptors returned a value
    ?assertEqual(5, meck:num_calls(proposer, deliver_promise, '_')),

    % clean up
    [acceptor:stop(Acceptor) || Acceptor <- Acceptors].

acceptor_reply_with_promises_one_acceptor_crash() ->
    Round = {1,1},
    Acceptors = start_acceptors(5),
    
    % kill one acceptor
    [FirstAcceptor|LiveAcceptors] = Acceptors,
    unlink(FirstAcceptor), % necessary for test process not to crash
    exit(FirstAcceptor, crash),

    acceptors:send_promise_requests(proposer, Round),
    timer:sleep(10),
    
    ?assertEqual(4, meck:num_calls(proposer, deliver_promise, '_')),
    
    [acceptor:stop(Acceptor) || Acceptor <- LiveAcceptors].


%% TestCase: Accept requests
on_send_accept_acceptors_reply() ->
    Round = {1,1},
    Value = foo,
    Acceptors = start_acceptors(5),

    % issue broadcast
    acceptors:send_accept_requests(proposer, Round, Value),
    timer:sleep(10),

    % check that all acceptors got called
    Accepts = [meck:called(acceptor, accept, [Acceptor, Round, Value]) ||
     		   Acceptor <- Acceptors],
    ?assert(lists:all(fun(true) -> true;
			 (_) -> false end, Accepts)),

    % check that all acceptors accepted or rejected the value
    ?assertEqual(5, meck:num_calls(proposer, deliver_accept, '_')),

    % clean up
    [acceptor:stop(Acceptor) || Acceptor <- Acceptors].


%% Helper functions
start_acceptors(N) ->
    AcceptorServers = [acceptor:start_link(
			 list_to_atom("acceptor" ++ 
					  integer_to_list(X))) 
		       || X <- lists:seq(1,N)],

    Acceptors = [Pid || {ok, Pid} <- AcceptorServers],

    % remove dependency on group membership
    meck:expect(gaoler, get_acceptors, 0, Acceptors),
    Acceptors.

