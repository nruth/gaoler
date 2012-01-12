-module (replica_election).
-export ([am_i_a_replica/1, is_replica/3]).

am_i_a_replica(HowManyReplicas) ->
    is_replica(nodes(), net_adm:localhost(), HowManyReplicas).

is_replica(Nodes, Node, HowManyReplicas) ->
    Node =< lists:nth(HowManyReplicas, lists:sort(Nodes)).
