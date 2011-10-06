-module(acceptor_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    acceptor:start_link().
    
cleanup() ->
    acceptor:stop().

% receive a prepare request for round=n
% return promise never to vote in anything less than n
% always accept prepare request as long as new n is higher than stored n
% reply with previously accepted highest n and value for that n

%% promise_test() ->
%%     acceptor:start_link(),    
%%     ?assert({promised, 10} == acceptor:prepare(10)),
%%     ?assert({promised, 11} == acceptor:prepare(11)),
%%     ?assertNot({promised, 10} == acceptor:prepare(10)),
%%     ?assert({promised, 11} == acceptor:prepare(10)),
%%     cleanup().

simple_promise_test() ->
    Acceptor = acceptor:start(),
    ProposerMock = nspy:mock(),
    Acceptor ! {promise, 10, ProposerMock},
    timer:sleep(10),
    nspy:assert_message_received(ProposerMock, {promised, 10}),
    Acceptor ! stop.
    

simple_vote_test() ->
    Acceptor = acceptor:start(),
    ProposerMock = nspy:mock(),
    Acceptor ! {promise, 10, ProposerMock},
    Acceptor ! {vote, 10, value, ProposerMock},
    timer:sleep(10),
    nspy:assert_message_received(ProposerMock, {promised, 10}),
    nspy:assert_message_received(ProposerMock, {ok, 
						{voted, {10, value}},
						{in_round, 10}
					       }),
    Acceptor ! stop.
    

% get accept request with n and value
% say ok if I haven't promised anything with a higher n
% if ignoring the value it should probably tell the proposer

%% accept_request_test() ->
%%     setup(),

%%     Value = some_value,
%%     Round = 10,

%%     ?assert({promised, Round} == 
%% 		gen_server:call(acceptor, {promise, Round})),

%%     ?assertEqual({ok,
%% 		  {voted, {Round, Value}},
%% 		  {in_round, Round}
%% 		 }, acceptor:vote(Round, Value)),

%%     ?assert({promised, Round+1} == acceptor:prepare(Round+1)),

%%     ?assertEqual({no, 
%% 		  {voted, {Round, Value}},
%% 		  {in_round, Round+1}
%% 		 }, acceptor:vote(Round, Value)),
    
%%     cleanup().

%% consecutive_votes_test() ->
%%     acceptor:start_link(),

    

%%     acceptor:stop().
