-module (lib_queue_list).
-export ([is_empty/1, peek/1, new/0, in/2, out/1]).

%% returns true or false 
is_empty([]) ->
    true;
is_empty(_Queue) ->
    false.

new() -> [].

%% append to queue
in(Elem, Queue) ->
    Queue ++ [Elem].


%% obeys queue:peek api
%% returns {value, QueueHead} or empty
peek([]) -> 
    empty;
peek([Head|_]) -> 
    {value, Head}.


%% mimics queue:out; pops queue head
%% returns {empty, EmptyQueue} or {{value, Head}, QueueTail}
out([]) -> 
    {empty, []};
out([Head|QueueTail]) ->
    {{value, Head}, QueueTail}.
