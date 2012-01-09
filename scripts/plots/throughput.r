# Read values from space-delimited ndlg_result
data <- read.table("ndlg_result", header=T, sep=",")

# normalise to seconds
ctimes = sapply(data$CTime, function(x) {x / 1e6})

# Compute the largest time value observed
max_num <- max(ctimes)

hist(ctimes, breaks=seq(0, max_num+1, by=1))

# cut results into interval bins & count frequency (cf. histogram)
interval = 1e-1
bins = seq(0, max_num, by=interval)
ctimes.cut <- cut(ctimes, bins, right=TRUE)
ctimes.freq = table(ctimes.cut)

scale <- function(x) { 
    x / interval
}
scaled_ctimes <- sapply(ctimes.freq, scale)

plot(scaled_ctimes)
