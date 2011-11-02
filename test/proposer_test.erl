-module(proposer_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("proposer_state.hrl").

-define(MOCKCOMMS,  #state{acceptors_proxy=nspy:mock()}).
-define(PROMISES(N),#state{promises=N}).
-define(ROUND(N),   #state{round=N}).
-define(VALUE(V),   #state{value=V}).

%% happy case: no other proposers and round 1 succeeds
first_promise_received_test() ->
    Round = 1,
    InitialState = ?MOCKCOMMS?PROMISES(0)?ROUND(Round),
    Result = proposer:awaiting_promises({promised, Round}, InitialState),
    ?assertMatch({next_state, awaiting_promises, ?PROMISES(1)}, Result).

success_case_move_to_next_state_test() ->
    Round = 1,
    InitialState = ?MOCKCOMMS?PROMISES(2)?ROUND(Round),
    Result = proposer:awaiting_promises({promised, Round}, InitialState),
    ?assertMatch({next_state, awaiting_accepts, ?PROMISES(3)}, Result).


%% sad case: someone else has been promised a higher round
higher_promise_received_test() ->
    Round = 1,
    InitialState = ?MOCKCOMMS?PROMISES(0)?ROUND(Round),
    Result = proposer:awaiting_promises({promised, 100}, InitialState),
    ?assertMatch({next_state, aborted, _}, Result).


%% %% happy case: a prepare message is received by the acceptor
%% prepare_message_sent_to_acceptor_test() ->
%%     meck:new(gaoler),

%%     meck:expect(gaoler, get_acceptors, fun() -> [nspy:mock()] end),

%%     [Acceptor] = gaoler:get_acceptors(),

%%     InitialState = ?PROMISES(2)#state{acceptors=[Acceptor],
%% 				      round=1},
    
%%     proposer:awaiting_promises({promised, 1}, InitialState),

%%     MessagesReceived = nspy:get_messages_from_spy(Acceptor),
%%     ?assertEqual(1, length(MessagesReceived)).
    
    
