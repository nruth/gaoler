-module(acceptors_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    Mods = [proposer, gaoler],
    meck:new(Mods),
    meck:expect(proposer, promised, 1, ok),
    Mods.
    
teardown(Mods) ->
    meck:unload(Mods).

proxy_test_() ->
    {foreach, 
     fun setup/0, 
     fun teardown/1,
     [
      fun acceptor_reply_with_promise/0,
      fun acceptor_reply_with_promises_one_acceptor_crash/0      
     ]
    }.

%% TestCase: broadcast messages
acceptor_reply_with_promise() ->
    Round = 1,
    
    % start 5 acceptors
    Acceptors = start_acceptors(5),

    % remove dependency on group membership
    meck:expect(gaoler, get_acceptors, 0, Acceptors),

    % issue broadcast
    acceptors:send_promise_request(Round),
    timer:sleep(10),

    % check that all acceptors returned a promise
    ?assertEqual(5, meck:num_calls(proposer, promised, '_')),

    % clean up
    [acceptor:stop(Acceptor) || Acceptor <- Acceptors].

acceptor_reply_with_promises_one_acceptor_crash() ->
    Round = 1,
    Acceptors = start_acceptors(5),
    meck:expect(gaoler, get_acceptors, 0, Acceptors),
    
    % kill one acceptor
    [FirstAcceptor|LiveAcceptors] = Acceptors,
    unlink(FirstAcceptor), % necessary for test process not to crash
    exit(FirstAcceptor, crash),

    acceptors:send_promise_request(Round),
    timer:sleep(10),
    
    ?assertEqual(4, meck:num_calls(proposer, promised, '_')),
    
    [acceptor:stop(Acceptor) || Acceptor <- LiveAcceptors].

%% TestCase: receive responses
% meck:called(proposer, promised, [Round])


start_acceptors(N) ->
    AcceptorServers = [acceptor:start_link(
			 list_to_atom("acceptor" ++ 
					  integer_to_list(X))) 
		       || X <- lists:seq(1,N)],
    [Pid || {ok, Pid} <- AcceptorServers].
