persistence_modules = {'no_persistence' 'persist_state' 'persist_holder' 'persist_queue'};

figure;
set(0,'DefaultAxesColorOrder',[0 0 0],...
    'DefaultAxesLineStyleOrder','d:|>:|p:|o:'); % with lines
%         'DefaultAxesLineStyleOrder','d|>|p|o'); % no lines
      


title('Centralised Lock Throughput for Varied Persistence');
xlabel('Time (s)');
ylabel('Throughput (completions/s)');

hold all;

for i = 1:length(persistence_modules)
    import_bb_summary(strcat('../../../results/gaoler_centralised_', persistence_modules{i}, '_5_minute_100_workers.csv'));
    plot(elapsed, successful);
end

legend(strrep(strrep(persistence_modules, '_', ' '), 'lock ', ''));

hold off;
