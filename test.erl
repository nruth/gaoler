-module (test).
-export ([start/1, stop/0]).

  acceptors() -> [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u].

  register_acceptors(Acceptors) ->
    RegisterAcceptor = fun(Acceptor) -> register(Acceptor, acceptor:start(Acceptor)) end,
    lists:foreach(RegisterAcceptor, Acceptors).

  start(Seed) ->
    register_acceptors(acceptors()),
    
    proposer:start(kurtz, green, acceptors(), Seed+1),
    proposer:start(willard, red, acceptors(), Seed+2),
    proposer:start(kilgore, blue, acceptors(), Seed+3),
    proposer:start(clara, yellow, acceptors(), Seed+4),
    proposer:start(hazal, pink, acceptors(), Seed+5),
    proposer:start(fred, prune, acceptors(), Seed+6),
    proposer:start(cedric, maron, acceptors(), Seed+7),
    proposer:start(timmy, gris, acceptors(), Seed+8),
    proposer:start(july, black, acceptors(), Seed+9),
    true.

  stop() -> lists:foreach(fun(Acceptor) -> stop(Acceptor) end, acceptors()).

  stop(Name) ->
    case whereis(Name) of
      undefined -> 
        ok;
      Pid -> Pid ! stop
    end.