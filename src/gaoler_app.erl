-module(gaoler_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Sup = gaoler_sup:start_link(),
    {ok, Acceptor} = gaoler_sup:add_child_acceptor(acceptor),
    register(acceptor, Acceptor),
    gaoler:join({acceptor, node()}),
    Sup.


stop(_State) ->
    ok.
