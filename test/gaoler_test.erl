-module(gaoler_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("gaoler_state.hrl").

-define(SUBSCRIBER(Fun), #state{subscribers = [Fun]}).

subscribe_fun_test() ->
    Fun = fun(Value) -> io:format("delivered ~p", [Value]) end,
    Result = gaoler:handle_call({subscribe, Fun}, nil, #state{}),
    ?assertMatch({reply, ok, ?SUBSCRIBER(Fun)}, Result).

subscribe_module_function_test() ->
    Module = dummy,
    Function = deliver,
    meck:new(Module),
    meck:expect(Module, Function, 1, ok),
    Result = gaoler:handle_call({subscribe, {Module, Function}}, 
				nil, #state{}),
    ?assertMatch({reply, ok, ?SUBSCRIBER({Module, Function})}, Result),
    meck:unload(Module).
				
unsubscribe_test() ->
    Fun = fun(Value) -> io:format("delivered ~p", [Value]) end,
    Result = gaoler:handle_call({unsubscribe, Fun}, nil, ?SUBSCRIBER(Fun)),
    ?assertMatch({reply, ok, #state{subscribers=[]}}, Result).
