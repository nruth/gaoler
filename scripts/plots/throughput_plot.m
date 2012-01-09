function [ output_args ] = throughput_plot( filename )
  result = load_throughput_result( filename );
  CTimesScaled = result(:, 1);
  
  max_num = max(CTimesScaled);

  interval = 1e-1
  edges = 0:interval:max_num;

  BinnedCTimes = histc(CTimesScaled, edges);
  BinThroughputs = BinnedCTimes / interval;

  
  plot(edges', BinThroughputs);
  title('Throughput plot');
  xlabel('Time (s)');
  ylabel('Throughput (completions/s)');
end