-record(proposal, {
    accepted_in_round = -1, % base-case, less than any round
    value = undefined
}).

-record(state, 
	{
	  round     = undefined,
	  value     = #proposal{},
	  promises  = 0,
	  past_accepts = [], % TODO: remove
	  accepts   = 0
	}).
