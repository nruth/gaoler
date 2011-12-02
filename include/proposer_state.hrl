-record(proposal, {
    accepted_in_round = -1, % base-case, less than any round
    value = undefined
}).

-record(state, {
    election  = 0,
    round     = undefined,
    value     = #proposal{},
    promises  = 0,
    accepts   = 0,
    rejects = 0,
    reply_to = undefined
}).
