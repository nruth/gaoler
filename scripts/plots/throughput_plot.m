function [ output_args ] = throughput_plot( filename )

% Import the file
  newData1 = importdata(filename);

  % Break the data up into a new structure with one field per column.
  colheaders = genvarname(newData1.colheaders);
  for i = 1:length(colheaders)
      dataByColumn1.(colheaders{i}) = newData1.data(:, i);
  end

  CTime = dataByColumn1.('CTime');
  Duration = dataByColumn1.('Duration');
  
  % Create new variables in the base workspace from those fields.
%   vars = fieldnames(dataByColumn1);
%   for i = 1:length(vars)
%       assignin('caller', vars{i}, dataByColumn1.(vars{i}));
%   end

  % scale times to seconds
  CTimesScaled = CTime / 1e6;

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