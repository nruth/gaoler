-module(datastore).
-export([start/0, stop/0, read/0, write/1, loop/1]).

start() ->
    register(?MODULE, spawn(fun() -> loop(0) end)).

stop() ->
    ?MODULE ! stop.

read() ->
    ?MODULE ! {read, self()},
    wait_result().

write(NewValue) ->
    ?MODULE ! {write, NewValue, self()},
    wait_result().

loop(Value) ->                          
    receive 
        {read, From} ->
            From ! {reply, {ok, Value}},
            loop(Value);
        {write, NewValue, From} ->
            From ! {reply, ok},
            loop(NewValue);
        stop ->
            ok
    end.

wait_result() ->
    receive 
        {reply, Reply} -> 
            Reply%; 
        %_ ->
        %    wait_result()
    end.
