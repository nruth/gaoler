-module(proposer_test).
-include_lib("eunit/include/eunit.hrl").

propose_test() ->
    {ok, SequenceNumber, Value} = proposer:start_proposal(SequenceNumber, Value).

    % should send prepare messages for some round id

    % should monotonically increase the round id and retry when rejected

    % should issue ballot (accept requests) if majority of promises received
    %  - with highest round-id value returned
    % - proposes new value if no past-value has been accepted

    
