-module(order).
-export([null/0, null/1, gr/2, goe/2, inc/1]).

  null() -> {0,0}.
  null(Id) -> {0, Id}.

  gr({N1,I1}, {N2,I2}) -> 
    if
      N1 > N2 -> true;
      ((N1 == N2) and (I1 > I2)) -> true;
      true -> false
    end.


  goe({N1,I1}, {N2,I2}) -> 
  if
    N1 > N2 -> true;
    ((N1 == N2) and (I1 >= I2)) -> true;
    true -> false
  end.

  inc({N, Id}) -> {N+1, Id}.