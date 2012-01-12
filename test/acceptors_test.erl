-module(acceptors_test).
-include_lib("eunit/include/eunit.hrl").

%% TestCase: broadcast messages
proxy_test_() ->
    {foreach, 
     fun setup/0, 
     fun teardown/1,
     [
      fun send_promise_requests/0,
      fun send_accept_requests/0
     ]
    }.

send_promise_requests() ->
    Acceptors = start_acceptors(5),
    acceptors:send_promise_requests(proposer, round),
    timer:sleep(50),
    
    % check that all acceptors got called
    Promises = [ meck:called(acceptor, prepare, [Acceptor, round]) || Acceptor <- Acceptors],
    ?assert(lists:all(fun(B) -> B end, Promises)),
    % check all replies sent to proposer
    Promises = [ meck:called(proposer, deliver_promise, [proposer, promise]) || _ <- lists:seq(1, 5)],
    ?assert(lists:all(fun(B) -> B end, Promises)).


send_accept_requests() ->
    Acceptors = start_acceptors(5),
    acceptors:send_accept_requests(proposer, round, val),
    timer:sleep(50),
    
    % check that all acceptors got called
    Votes = [ meck:called(acceptor, accept, [Acceptor, round, val]) || Acceptor <- Acceptors],
    ?assert(lists:all(fun(B) -> B end, Votes)),
    % check all replies sent to proposer
    Promises = [ meck:called(proposer, deliver_accept, [proposer, accept]) || _ <- lists:seq(1, 5)],
    ?assert(lists:all(fun(B) -> B end, Promises)).



%TODO test in acceptor:accept instead
% acceptor_reply_with_promises_one_acceptor_crash() ->
%     Round = {1,1},
%     Acceptors = start_acceptors(5),
%     
%     % kill one acceptor
%     [FirstAcceptor|LiveAcceptors] = Acceptors,
%     unlink(FirstAcceptor), % necessary for test process not to crash
%     exit(FirstAcceptor, crash),
% 
%     acceptors:send_promise_requests(proposer, Round),
%     timer:sleep(10),
%     
%     ?assertEqual(4, meck:num_calls(proposer, deliver_promise, '_')),
%     
%     [acceptor:stop(Acceptor) || Acceptor <- LiveAcceptors].


% %% Helper functions
start_acceptors(N) ->
    Acceptors = [ nspy:mock() || _ <- lists:seq(1, N)],

    % remove dependency on group membership
    meck:expect(gaoler, get_acceptors, 0, Acceptors),
    Acceptors.

setup() ->
    Mods = [proposer, gaoler, acceptor],
    meck:new(Mods),
    meck:expect(proposer, deliver_promise, 2, ok),
    meck:expect(proposer, deliver_accept, 2, ok),
    meck:expect(acceptor, prepare, 2, promise),
    meck:expect(acceptor, accept, 3, accept),
    Mods.

teardown(Mods) ->
    meck:unload(Mods).
