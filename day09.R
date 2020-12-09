# Make sure to setwd to the right directory
xmas <- read.table("day09.txt", quote="\"", comment.char="")[,1]

valid_sum_search <- function(xmas_list, target) {
  for (i in 1:(length(xmas_list) - 1)) {
    for (j in (i+1):length(xmas_list)) {
      if (xmas_list[i] + xmas_list[j] == target) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# Part 1
for (i in 51:(length(xmas) - 50)) {
  if (!valid_sum_search(xmas[i:(i + 49)], xmas[i + 50])) {
    invalid_value <- xmas[i + 50]
    print(invalid_value)
  }
}

# Part 2
contiguous_sum_search <- function(xmas_list, target) {
  for (i in 1:(length(xmas_list) - 1)) {
    for (j in (i+1):length(xmas_list)) {
      if (sum(xmas_list[i:j]) == target) {
        return(xmas_list[i:j])
      }
    }
  }
}

weakness_sequence <- contiguous_sum_search(xmas, invalid_value)
print(min(weakness_sequence) + max(weakness_sequence))
