-module(dummy).
-export([important_task/0]).

important_task() ->    
    gaoler:acquire(beer),
    drink_beer(),
    gaoler:release(beer).

drink_beer() ->
    io:format("I'm a happy application!", []).
