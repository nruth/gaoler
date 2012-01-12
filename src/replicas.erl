-module (replicas).
-export ([am_i_a_replica/1, is_replica/3, replica_list/1]).


%% predicate for whether the current node() is considered a replica
%% assumes nodes() already connected
am_i_a_replica(HowManyReplicas) ->
    is_replica(nodes(), node(), HowManyReplicas).


%% returns the list of nodes bearing replicas, a sublist of nodes()
%% assumes nodes() already connected
replica_list(HowManyReplicas) ->
    lists:sublist(lists:sort([node()|nodes()]), HowManyReplicas).

%internal but exported for testing
is_replica(Nodes, Node, HowManyReplicas) ->
    Node =< lists:nth(HowManyReplicas, lists:sort(Nodes)).
