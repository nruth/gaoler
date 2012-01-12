persistence_modules = {'no_persistence' 'persist_holder' 'persist_queue' 'noop' 'fake_lock'};

figure;
set(0,'DefaultAxesColorOrder',[0 0 0],...
    'DefaultAxesLineStyleOrder','d-|>-|p-|o-|.-|*-'); % with lines
%         'DefaultAxesLineStyleOrder','d|>|p|o|.|*'); % no lines
      


title('Throughput of the Centralised Lock Persistence Strategies');
xlabel('Time (s)');
ylabel('Throughput (completions/s)');

hold all;

for i = 1:length(persistence_modules)
    import_bb_summary(strcat('../../../results/atomic_file_writes/gaoler_centralised_', persistence_modules{i}, '_5_minute_100_workers.csv'));
    throughput = successful ./ window;
    plot(elapsed, throughput);
end

legend(strrep(strrep(persistence_modules, '_', ' '), 'lock ', ''));

hold off;
