-module (replicas_test).
-include_lib("eunit/include/eunit.hrl").

is_replica_test_() -> 
    N1=  'gaoler@lakka-1.net',
    N2=  'gaoler@lakka-2.net',
    N3=  'gaoler@lakka-3.net',
    N4=  'gaoler@lakka-4.net',
    N5=  'gaoler@lakka-5.net',
    Nodes = [N1, N4, N2, N3, N5],

    [
    ?_assertNot(replicas:is_replica(Nodes, N5, 3)),
    ?_assertNot(replicas:is_replica(Nodes, N4, 3)),
    ?_assert(replicas:is_replica(Nodes, N3, 3)),
    ?_assert(replicas:is_replica(Nodes, N2, 3)),
    ?_assert(replicas:is_replica(Nodes, N1, 3)),
    ?_assert(replicas:is_replica(Nodes, N2, 2)),
    ?_assert(replicas:is_replica(Nodes, N1, 2)),
    ?_assert(replicas:is_replica(Nodes, N1, 1)),
    ?_assert(replicas:is_replica(Nodes, N5, 5))
    ].    