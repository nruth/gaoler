-module(persister).

-include_lib("persister.hrl").
-include_lib("acceptor_state.hrl").

-export([
         remember_promise/2,
         remember_vote/3,
         load_saved_state/0,
         read_log_file/2
        ]).

remember_promise(Election, Round) ->
    LogRecord = "{promised,"++integer_to_list(Round)++"}.\n",
    append_to_file(Election, LogRecord).

% currently only supports atoms for value
remember_vote(Election, Round, Value) when is_atom(Value) ->
    LogRecord = "{accepted,"++integer_to_list(Round)++
        ","++atom_to_list(Value)++"}.\n",
    append_to_file(Election, LogRecord).

% returns a acceptor state record with previous elections
load_saved_state() ->
    case file:list_dir(?LOGDIRECTORY) of
        {ok, Files} ->
            Elections = collect_elections(Files),
            #state{elections=Elections};
        _NoHistoryAvailable ->
            #state{}
    end.

collect_elections(Files) ->
    Self = self(),
    Pids = lists:map(fun(File) ->
                             spawn(fun() ->
                                           read_log_file(Self, File)
                                   end) 
                     end, Files),
    gather_elections(Pids).

gather_elections([Pid|Tail]) ->
    receive 
        {Pid, {error, _}} -> % filter corrupt files, is this intended?
            gather_elections(Tail);
        {Pid, ReturnValue} ->
            [ReturnValue|gather_elections(Tail)]
    end;
gather_elections([]) ->
    [].

% returns an acceptor election record for specified logfile
read_log_file(Parent, File) ->
    Result = 
        case file:consult(?LOGDIRECTORY++"/"++File) of
            {ok, [{election, ElectionId}|Data]} ->                
                ReversedData = lists:reverse(Data),
                {ElectionId,
                 #election{promised=find_last_promise(ReversedData), 
                           accepted=find_last_accepted_value(ReversedData)}
                };
            {error, {_,_,_}} ->
                % critical -> could not parse the logfile
                {error, parse_error}
        end,
    io:format("Result is: ~p~n", [Result]),
    Parent ! {self(), Result}.

%%% Internal functions
append_to_file(Election, Record) ->
    file:make_dir(?LOGDIRECTORY),
    case file:read_file_info(?LOG(Election)) of
        {ok, _} ->
            ignore;
        {error, _} ->
            file:write_file(?LOG(Election), "{election, "
                            ++integer_to_list(Election)++"}.\n", [append])
    end,
    file:write_file(?LOG(Election), Record, [append]).

% assumes list is reversed
find_last_promise([]) -> 0;
find_last_promise([{promised, Value}|_Tail]) -> Value;
find_last_promise([_|Tail]) -> find_last_promise(Tail).

find_last_accepted_value([]) -> no_value;
find_last_accepted_value([{accepted, Round, Value}|_Tail]) -> {Round, Value};
find_last_accepted_value([_|Tail]) -> find_last_accepted_value(Tail).
    
