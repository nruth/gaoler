-module (lib_proposal).
-export ([proposal/2]).
-record (proposal, {round = undefined, value = undefined}).

proposal(Round, Value) ->
    #proposal{round=Round, value=Value}.

round(Proposal) ->
    Proposal#proposal.round.

value(Proposal) ->
    Proposal#proposal.value.
