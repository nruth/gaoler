-module(gaoler_app).

-behaviour(application).

-export([
         start/0,
	 start/2, 
	 stop/1
	]).

start() ->
    start(nil, nil).

start(_StartType, _StartArgs) ->
%    lock:start_link(lock_no_persistence, simple_comms),
%    replica:start(lock),
    gaoler_sup:start_link().

stop(_State) ->
    % todo: graceful shutdown
    ok.
