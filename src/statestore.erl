-module(statestore).
-export([
         create/1,
         find/2,
         replace/2,
         add/2
        ]).
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

                
