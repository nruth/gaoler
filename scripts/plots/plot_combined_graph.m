interval = 1e-1  

no_persist      = bin_ctimes(interval, load_throughput_result( '../../ebin/lock_no_persistence_throughput_5000sec_100procs' ));
persist_state   = bin_ctimes(interval, load_throughput_result( '../../ebin/lock_persist_state_throughput_5000sec_100procs' ));
persist_holder  = bin_ctimes(interval, load_throughput_result( '../../ebin/lock_persist_holder_throughput_5000sec_100procs' ));
persist_queue   = bin_ctimes(interval, load_throughput_result( '../../ebin/lock_persist_queue_throughput_5000sec_100procs' ));

set(0,'DefaultAxesColorOrder',[0 0 0],...
      'DefaultAxesLineStyleOrder','d:|>:|p:|o:')

plot(   no_persist(:,1), no_persist(:,2), ...
        persist_state(:,1), persist_state(:,2), ...
        persist_holder(:,1), persist_holder(:,2),...
        persist_queue(:,1), persist_queue(:,2)...
);

title('Centralised Lock Throughput for Varied Persistence');
xlabel('Time (s)');
ylabel('Throughput (completions/s)');