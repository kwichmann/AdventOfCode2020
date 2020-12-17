# Make sure to setwd to the right directory
library(stringr)

# Extra line break added to data file to work
busInfo <- readLines(con = "day13.txt")

earliest_time <- as.integer(busInfo[1])
bus_lines <- as.integer(str_extract_all(busInfo[2], "[0-9]+")[[1]])

# Part 1
for (i in 0:min(bus_lines)) {
  departures <- sapply(bus_lines, function(l) (earliest_time + i) %% l == 0)
  if (any(departures)) {
    print(i * bus_lines[departures])
    break
  }
}

# Part 2

# This solution assumes all bus lines are primes,
# and uses the Chinese Remainder Theorem

# Simple search will be fine here, since all the primes are small
solve_mod <- function(n, p) {
  for (x in 0:(p - 1)) {
    if ((n * x) %% p == 1) {
      return(x)
    }
  }
}

chinese_remainder <- function(b, p) {
  N <- prod(p)
  N_i <- N / p
  x_i <- mapply(solve_mod, N_i, p)
  return(sum(x_i * N_i * b) %% N)
}

desired_schedule <- str_split(busInfo[2], ",")[[1]]
times <- which(desired_schedule != "x") - 1

# However, because of numerical precision, my solution was off by 2 :-/
# print(chinese_remainder(-times, bus_lines))

# This is another implementation - probably clearer
current_time <- 0
step <- 1
for (i in 1:length(bus_lines)) {
  print(bus_lines[i])
  while ((current_time + times[i]) %% (bus_lines[i]) != 0) {
    current_time <- current_time + step
  }
  step <- step * bus_lines[i]
}

print(current_time)
