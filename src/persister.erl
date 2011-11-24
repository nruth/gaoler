-module(persister).
-include_lib("persister.hrl").

-export([
         remember_promise/1,
         remember_vote/2
        ]).

remember_promise(Round) ->
    LogRecord = "{promised,"++integer_to_list(Round)++"}.\n",
    append_to_file(LogRecord),
    ok.

% currently only supports atoms for value
remember_vote(Round, Value) when is_atom(Value) ->
    LogRecord = "{accepted,"++integer_to_list(Round)++
        ","++atom_to_list(Value)++"}.\n",
    append_to_file(LogRecord),
    ok.


%%% Internal functions
append_to_file(Record) ->
    file:write_file(?LOGFILE, Record, [append]).
