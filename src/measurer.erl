-module (measurer).
-export ([ throughput/3 ]).

%% execute Work repeatedly for Duration on Num_Workers processes
%% returns a list of completion times to use in throughput calculation
throughput(Work, Duration, NumWorkers) ->
    Collector = collector:start_link(),
    StartTimestamp = now(), % used to make time readings relative to 0
    ok = worker:spawn_n_for_duration(Work, Duration, NumWorkers, Collector),
    timer:apply_after(Duration, collector, halt, [Collector, self()]),
    receive
        {'ETS-TRANSFER', ResultsETS, _FromPid, collected_results} ->
            write_results_to_file(ResultsETS, StartTimestamp, io_lib:format("throughput_~psec_~pprocs", [Duration, NumWorkers]))
    end.


write_results_to_file(Results, StartTimestamp, FileName) ->
    {ok, FileHandle} = file:open(FileName, [raw, write]),
    file:write(FileHandle, "CTime,Duration\n"),
    % write each ets entry into the file, on its own line
    ets:foldl(fun (R, _) ->
            CTime = timer:now_diff(collector:ctime_from_result(R), StartTimestamp),
            Duration = collector:duration_from_result(R),
            DataLine = io_lib:format("~p,~p~n", [CTime, Duration]),
            file:write(FileHandle, DataLine)
        end,
        unused_acc0,
        Results
    ),
    file:close(FileHandle).
