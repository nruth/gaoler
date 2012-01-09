-record(state, {
    oldest_remembered_state = 0,
    ready_to_gc = [],
    elections = undefined
}).

-record(election, {
    id = undefined,
    promised = 0,
    accepted = no_value
}).
