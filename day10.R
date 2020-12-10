# Make sure to setwd to the right directory
jolts <- read.table("day10.txt", quote="\"", comment.char="")[,1]

# Part 1
jolt_max <- max(jolts) + 3
sorted_jolts <- c(0, sort(jolts), jolt_max)
diffs <- sapply(1:(length(sorted_jolts) - 1), function(i) sorted_jolts[i + 1] - sorted_jolts[i])
counts <- table(diffs)
print(counts[["1"]] * counts[["3"]])

# Part 2
backwards_jolts <- sort(sorted_jolts, decreasing = TRUE)

num_combinations <- function(backwards_jolts) {
  dynamic_combinations <- c(1, rep(NA, length(backwards_jolts) - 1))
  
  for (i in 2:length(backwards_jolts)) {
    lookback <- 0
    for (j in 1:3) {
      if (i - j > 0) {
        diff <- backwards_jolts[i - j] - backwards_jolts[i]
        if (diff <= 3) {
          lookback <- lookback + dynamic_combinations[i - j]
        }
      }
    }
    dynamic_combinations[i] <- lookback
  }
  return(tail(dynamic_combinations, 1))
}

options(digits=13)
print(num_combinations(backwards_jolts))
