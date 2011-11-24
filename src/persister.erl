-module(persister).

-include_lib("persister.hrl").
-include_lib("acceptor_state.hrl").

-export([
         remember_promise/1,
         remember_vote/2,
         load_saved_state/0
        ]).

remember_promise(Round) ->
    LogRecord = "{promised,"++integer_to_list(Round)++"}.\n",
    append_to_file(LogRecord).

% currently only supports atoms for value
remember_vote(Round, Value) when is_atom(Value) ->
    LogRecord = "{accepted,"++integer_to_list(Round)++
        ","++atom_to_list(Value)++"}.\n",
    append_to_file(LogRecord).

% returns an acceptor state record
load_saved_state() ->
    case file:consult(?LOGFILE) of
        {ok, Data} ->   
            ReversedData = lists:reverse(Data),
            #state{promised=find_last_promise(ReversedData), 
                   accepted=find_last_accepted_value(ReversedData)};
        {error, {_,_,_}} ->
            % critical -> could not parse the logfile
            #state{};
        {error, _} ->
            % file does not exists, this is ok, return empty state
            #state{}
    end.

%%% Internal functions
append_to_file(Record) ->
    file:write_file(?LOGFILE, Record, [append]).

% assumes list is reversed
find_last_promise([]) -> 0;
find_last_promise([{promised, Value}|_Tail]) -> Value;
find_last_promise([_|Tail]) -> find_last_promise(Tail).

find_last_accepted_value([]) -> no_value;
find_last_accepted_value([{accepted, Round, Value}|_Tail]) -> {Round, Value};
find_last_accepted_value([_|Tail]) -> find_last_accepted_value(Tail).
    
