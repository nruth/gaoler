function ResultTable = bin_ctimes( interval, CTimes)
    max_num = max(CTimes);
    edges = 0:interval:max_num
    BinnedCTimes = histc(CTimes, edges)
    BinThroughputs = BinnedCTimes / interval;
    ResultTable = [edges', BinThroughputs]
end

