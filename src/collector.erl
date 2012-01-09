-module (collector).
-record (work, {duration = 0, ctime = 0}).
-export ([
    start_link/0,
    collect/0,
    duration_from_result/1, 
    ctime_from_result/1,
    send_result/3,
    halt/2
]).

start_link() ->
    spawn_link(collector, collect, []).

collect() ->
    collect(ets:new(collector_results, [duplicate_bag])).
collect(Results) ->
    receive
        {result, Result} ->
            ets:insert(Results, Result),
            collect(Results);
        {halt, ReplyTo} ->
            % send the results back by transferring ets table ownership
            ets:give_away(Results, ReplyTo, collected_results)
    end.

% ask the collector to halt and reply with its collected results
halt(Collector, ReplyTo) ->
    Collector ! {halt, ReplyTo}.

send_result(Collector, Duration, CTime) ->
    Collector ! {result, result(Duration, CTime)}.

result(Duration, CTime) ->
    #work{duration = Duration, ctime = CTime}.

duration_from_result(Result = #work{}) ->
    Result#work.duration.

ctime_from_result(Result = #work{}) ->
    Result#work.ctime.
