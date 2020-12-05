# Make sure to setwd to the right directory
nums <- read.table("day01.txt", quote="\"", comment.char="")[,1]

l <- length(nums)

# Part 1
for (i in 1:(l-1)) {
  for (j in (i+1):l) {
    if (nums[i] + nums[j] == 2020) {
      print(nums[i] * nums[j])
    }
  }
}

# Part 2
for (i in 1:(l-2)) {
  for (j in (i+1):(l-1)) {
    for (k in (j+1):l) {
      if (nums[i] + nums[j] + nums[k] == 2020) {
        print(nums[i] * nums[j] * nums[k])
      }
    }
  }
}
