% README: run from erl interactive with: 
% cd(ebin). 
% file:script('../scripts/ndlg_profiling.erl').

Lock = fun() ->
lock:acquire(self()),
receive lock -> ok end,
lock:release(self())
end.

io:format('.').
lock:start_link(lock_no_persistence, simple_comms).
measurer:throughput(Lock, 5000, 100).
lock:stop().
os:cmd("mv throughput_5000sec_100procs lock_no_persistence_throughput_5000sec_100procs").

io:format('.').
lock:start_link(lock_persist_holder, simple_comms).
measurer:throughput(Lock, 5000, 100).
lock:stop().
os:cmd("mv throughput_5000sec_100procs lock_persist_holder_throughput_5000sec_100procs").

io:format('.').
lock:start_link(lock_persist_state, simple_comms).
measurer:throughput(Lock, 5000, 100).
lock:stop().
os:cmd("mv throughput_5000sec_100procs lock_persist_state_throughput_5000sec_100procs").

io:format('.').
lock:start_link(lock_persist_queue, simple_comms).
measurer:throughput(Lock, 5000, 100).
lock:stop().
os:cmd("mv throughput_5000sec_100procs lock_persist_queue_throughput_5000sec_100procs").

