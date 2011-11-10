-module(gaoler_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("gaoler_state.hrl").

-define(SUBSCRIBER(Fun), #state{subscribers = [Fun]}).

client_subscribe_fun_test() ->
    Fun = fun(Value) -> io:format("delivered ~p", [Value]) end,
    Result = gaoler:handle_call({subscribe, Fun}, nil, #state{}),
    ?assertMatch({reply, ok, ?SUBSCRIBER(Fun)}, Result).

client_subscribe_module_function_test() ->
    Module = dummy,
    Function = deliver,
    meck:new(Module),
    meck:expect(Module, Function, 1, ok),
    Result = gaoler:handle_call({subscribe, {Module, Function}}, 
				nil, #state{}),
    ?assertMatch({reply, ok, ?SUBSCRIBER({Module, Function})}, Result),
    meck:unload(Module).
				
client_unsubscribe_test() ->
    Fun = fun(Value) -> io:format("delivered ~p", [Value]) end,
    Result = gaoler:handle_call({unsubscribe, Fun}, nil, ?SUBSCRIBER(Fun)),
    ?assertMatch({reply, ok, #state{subscribers=[]}}, Result).

when_deliver_message_to_client_check_callback_is_called_test() ->
    Module = dummy,
    Function = deliver,
    meck:new(Module),
    meck:expect(Module, Function, 1, ok),
    InitialState = ?SUBSCRIBER({Module, Function}),
    gaoler:handle_cast({deliver, value}, InitialState),
    timer:sleep(10),
    ?assert(meck:called(dummy, deliver, '_')),
    meck:unload(Module).
    
