-module(statestore).

-export([
         create/1,
         find/2,
         replace/2,
         add/2,
         gc/2
        ]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("acceptor_state.hrl").

create(TableName) ->
    ets:new(TableName, [named_table, set, {keypos, #election.id}, public]).

find(TableName, ElectionId) ->
    case ets:lookup(TableName, ElectionId) of
        [] ->
            false;
        [FoundElection] ->
            {ElectionId, FoundElection}
    end.

replace(TableName, NewElection) ->
    add(TableName, NewElection).

add(TableName, ElectionRecord) ->    
    ets:insert(TableName, ElectionRecord).

gc(TableName, OldestNeededId) ->   
    DeleteOlderThanIdSpecification = 
        ets:fun2ms(fun(#election{id = ID}) 
                         when ID < OldestNeededId ->
                           true % used with ets:select_delete/2
                   end),
    ets:select_delete(TableName, DeleteOlderThanIdSpecification).
    
