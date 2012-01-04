-module(statestore).
-export([
         create/0,
         find/1,
         replace/1,
         add/1
        ]).
-include_lib("acceptor_state.hrl").
-define(TABLE, acceptor_state).

create() ->
    ets:new(?TABLE, [named_table, set, {keypos, #election.id}, public]).

find(ElectionId) ->
    case ets:lookup(?TABLE, ElectionId) of
        [] ->
            false;
        [FoundElection] ->
            {ElectionId, FoundElection}
    end.

replace(NewElection) ->
    add(NewElection).

add(ElectionRecord) ->    
    ets:insert(?TABLE, ElectionRecord).

                
