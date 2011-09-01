-module(pers).
-export([read/1, store/4, delete/1]).

  read(Id) -> 
    {ok, Id} = dets:open_file(Id, []), 
    case dets:lookup(Id, perm) of
      [{perm, Bn, An, Av}] -> {Bn, An, Av};
      [] -> {order:null(), order:null(), na}
    end.

  store(Id, Bn, An, Av) -> 
    dets:insert(Id, {perm, Bn, An, Av}).

  delete(Id) -> 
    dets:delete(Id, perm), 
    dets:close(Id).