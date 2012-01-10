persistence_modules = {'lock_no_persistence' 'lock_persist_state' 'lock_persist_holder' 'lock_persist_queue'};
duration = 5000;
workers = 50;
interval = 5e-1
figure;

set(0,'DefaultAxesColorOrder',[0 0 0],...
        'DefaultAxesLineStyleOrder','d|>|p|o'); % no lines
%       'DefaultAxesLineStyleOrder','d:|>:|p:|o:'); % with lines


title('Centralised Lock Throughput for Varied Persistence');
xlabel('Time (s)');
ylabel('Throughput (completions/s)');

hold all;

for i = 1:length(persistence_modules)
    filename = strcat('../../ebin/', persistence_modules{i}, '_throughput_', int2str(duration), 'sec_', int2str(workers), 'procs');
    result = bin_ctimes(interval, load_throughput_result( filename ));
    plot(result(:,1), result(:,2));
end

legend(strrep(strrep(persistence_modules, '_', ' '), 'lock ', ''));

hold off;
