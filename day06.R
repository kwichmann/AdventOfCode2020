# Make sure to setwd to the right directory
library(readr)
library(stringr)
library(dplyr)

dataString <- read_file("day06.txt")

# Part 1
questionnaires <- str_split(dataString, "\n\n")
  
questionnaires_some <- questionnaires %>%
  sapply(function(q) gsub("\n", "", q))

q_some_df <- as.data.frame(matrix(rep(NA, 26 * length(questionnaires_some)), ncol = 26))
colnames(q_some_df) <- letters

for (i in 1:length(questionnaires_some)) {
  for (letter in letters) {
    q_some_df[[letter]][i] <- str_detect(questionnaires_some[i], letter)
  }
}

print(sum(as.matrix(q_some_df)))

# Part 2
questionnaires_all <- (questionnaires %>%
  lapply(function(q) str_split(q, "\n")))[[1]] %>%
  lapply(function(q) str_split(q, "")) %>%
  lapply(function(q) Reduce(intersect, q))

q_all_df <- as.data.frame(matrix(rep(NA, 26 * length(questionnaires_all)), ncol = 26))
colnames(q_all_df) <- letters

for (i in 1:length(questionnaires_all)) {
  for (letter in letters) {
    q_all_df[[letter]][i] <- letter %in% questionnaires_all[[i]]
  }
}

print(sum(as.matrix(q_all_df)))
