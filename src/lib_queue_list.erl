-module (lib_queue_list).
-export ([is_empty/1, head/1, new/0, append/2, pop_head/1]).

%% returns true or false 
is_empty([]) ->
    true;
is_empty(_Queue) ->
    false.

new() -> [].

append(Elem, Queue) ->
    Queue ++ [Elem].


%% obeys queue:peek api
%% returns {value, QueueHead} or empty
head([]) -> 
    empty;
head([Head|_]) -> 
    {value, Head}.


%% mimics queue:out api
%% returns {empty, EmptyQueue} or {{value, Head}, QueueTail}
pop_head([]) -> 
    {empty, []};
pop_head([Head|QueueTail]) ->
    {{value, Head}, QueueTail}.


% queue implementation
% %% returns true or false 
% is_empty(Queue) ->
%     queue:is_empty(Queue).
% 
% %% obeys queue:peek api
% %% returns {value, QueueHead} or empty
% current_lock_holder(Queue) ->
%     queue:peek(Queue).
% 
% new_queue() ->
%     queue:new().
% 
% append(Elem, Queue) ->
%     queue:in(Elem, Queue).
% 
% %% mimics queue:out api
% %% returns {empty, EmptyQueue} or {{value, Head}, QueueTail}
% pop_head(Queue) ->
%     queue:out(Queue).
