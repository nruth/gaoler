function CTimesScaled = load_throughput_result( filename )

% Import the file
  newData1 = importdata(filename);

  % Break the data up into a new structure with one field per column.
  colheaders = genvarname(newData1.colheaders);
  for i = 1:length(colheaders)
      dataByColumn1.(colheaders{i}) = newData1.data(:, i);
  end

  CTime = dataByColumn1.('CTime');
  Duration = dataByColumn1.('Duration');

    % scale times to seconds
  CTimesScaled = CTime / 1e6;
end

