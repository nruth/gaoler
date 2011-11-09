-module(gaoler_app).

-behaviour(application).

-export([
	 start/2, 
	 stop/1
	]).

start(_StartType, _StartArgs) ->
    gaoler_sup:start_link().

stop(_State) ->
    % todo: graceful shutdown
    ok.
