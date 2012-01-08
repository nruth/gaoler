-record(state, {
    oldest_remembered_state = 0,
    elections = undefined
}).

-record(election, {
    id = undefined,
    promised = 0,
    accepted = no_value
}).
