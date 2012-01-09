% worker agent for distributed workload generator
% many spawned to execute user-defined fun
-module (worker).

-export ([
    spawn_n_for_duration/4,
    work_for_duration/3
]).

spawn_n_for_duration(_Work, _Duration, 0, _Collector) -> ok;
spawn_n_for_duration(Work, Duration, Num_Workers, Collector) ->
    spawn(?MODULE, work_for_duration, [Duration, Work, Collector]),
    spawn_n_for_duration(Work, Duration, Num_Workers - 1, Collector).

% execute Work fun repeatedly until duration reached, then halt
% measurements are sent to Collector
work_for_duration(Duration, Work, Collector) ->
    erlang:send_after(Duration, self(), halt),
    work_loop(Work, Collector).

% execute Work fun repeatedly
% measurements are sent to Collector
work_loop(Work, Collector) ->
    % do the work and format for the collector
    % tag with local clock as completion time
    % assumes collector is on the same node, else clocks are not sync
    receive
        halt -> ok
    after 0 ->
        Duration = perform(Work),
        CTime = now(),
        collector:send_result(Collector, Duration, CTime),
        work_loop(Work, Collector)
    end.

perform(Work) when is_function(Work) ->
    {Duration, _} = timer:tc(Work),
    Duration.
