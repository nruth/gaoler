-module(replicated_lock_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("replicated_lock_state.hrl").
-include_lib("queue_lib.hrl").

-define(EMPTY, #state{queue=?QUEUE_LIB:new()}).

deliver_test_no_future_operation_queued_test() ->
    InitialState = ?EMPTY,
    LockRequester = self(),
    DeliverOperation = {1, {acquire, LockRequester}},
    {reply, ok, ResultState} = replicated_lock:handle_call({deliver, DeliverOperation}, nil, InitialState),
    ?assertEqual([], ResultState#state.future),
    ?assertEqual({value, LockRequester}, ?QUEUE_LIB:peek(ResultState#state.queue)).