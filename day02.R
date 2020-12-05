# Make sure to setwd to the right directory
pwds <- read.table("day02.txt", quote="\"", comment.char="")
colnames(pwds) <- c("policy", "letter", "password")

library(dplyr)
library(stringr)

# Part 1
valid_pswds <- pwds %>%
  mutate(minimum = as.integer(gsub("(.*)-.*", "\\1", policy)),
         maximum = as.integer(gsub(".*-(.*)", "\\1", policy)),
         letter = gsub("(.*):", "\\1", letter),
         matches = str_count(password, letter),
         valid = (matches >= minimum) & (matches <= maximum)) %>%
  pull(valid) %>%
  sum()

print(valid_pswds)

# Part 2
valid_pswds2 <- pwds %>%
  mutate(first = as.integer(gsub("(.*)-.*", "\\1", policy)),
         second = as.integer(gsub(".*-(.*)", "\\1", policy)),
         letter = gsub("(.*):", "\\1", letter),
         first_match = substr(password, first, first) == letter,
         second_match = substr(password, second, second) == letter,
         valid = xor(first_match, second_match)) %>%
  pull(valid) %>%
  sum()

print(valid_pswds2)
