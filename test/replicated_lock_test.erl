-module(replicated_lock_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("replicated_lock_state.hrl").

-define(EMPTY, #state{queue=queue:new()}).

deliver_test_no_future_operation_queued_test() ->
    InitialState = ?EMPTY,
    LockRequester = self(),
    DeliverOperation = {1, {acquire, LockRequester}},
    {reply, ok, ResultState} = replicated_lock:handle_call({deliver, DeliverOperation}, nil, InitialState),
    ?assertEqual([], ResultState#state.future),
    ?assertEqual({value, LockRequester}, queue:peek(ResultState#state.queue)).