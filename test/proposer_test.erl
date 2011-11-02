-module(proposer_test).
-include_lib("eunit/include/eunit.hrl").


propose_test() ->
    %{ok, SequenceNumber, Value} = proposer:start_proposal(SequenceNumber, Value).

    % should send prepare messages for some round id

    % should monotonically increase the round id and retry when rejected

    % should issue ballot (accept requests) if majority of promises received
    %  - with highest round-id value returned
    % - proposes new value if no past-value has been accepted

    ok.

prepare_sends_prepare_request_to_acceptors_test() ->
    AcceptorMocks = [nspy:mock() || _ <- lists:seq(1,5)],

    ProposerPid = 
	spawn(fun() -> proposer:prepare(AcceptorMocks, 0, value) end),

    timer:sleep(10),
    [nspy:assert_message_received(AcceptorMock, {{prepare, 0}, ProposerPid}) ||
	AcceptorMock <- AcceptorMocks],

    exit(ProposerPid, kill).

prepare_check_promises_reply_test() ->
    ProposerPid = 
	spawn(fun() -> proposer:prepare([], 0, value) end),
    
    [ProposerPid ! {promised, 0, value} || _ <- lists:seq(3)],

    %% test result value...
    
    exit(ProposerPid, kill).

%prepare_when_no_previous_values_returns_proposed_value_test() ->
%    ?assertEqual({10, value}, proposer:prepare([], 10, value)).

%% prepare_with_previous_value_using_old_round_test() ->
%%     proposer:prepare(10, value),
%%     ?assertEquals({10, value}, proposer:prepare(11, value)).
