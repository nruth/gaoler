-module (replica_election).
-export ([am_i_a_replica/1, is_replica/3]).

am_i_a_replica(HowManyReplicas) ->
    is_replica(nodes(), node(), HowManyReplicas).

is_replica(Nodes, Node, HowManyReplicas) ->
    Node =< lists:nth(HowManyReplicas, lists:sort(Nodes)).
