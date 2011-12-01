-record(state, {
  elections = []
}).

-record(election, {
  promised = 0,
  accepted = no_value
}).
