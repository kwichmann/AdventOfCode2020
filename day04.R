# Make sure to setwd to the right directory
library(readr)
library(stringr)
library(dplyr)

dataString <- read_file("day04.txt")

# Data wrangling
passports <- (str_split(dataString, "\n\n") %>%
  lapply(str_split, "\n| "))[[1]]

passports_df <- as.data.frame(matrix(rep(NA, 8 * length(passports)), ncol = 8))
colnames(passports_df) <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")

for (i in 1:length(passports)) {
  for (j in 1:length(passports[[i]])) {
    keyValue <- str_split(passports[[i]][j], ":")[[1]]
    passports_df[[keyValue[1]]][i] <- keyValue[2]
  }
}

# Part 1
valid1 <- sum(complete.cases(select(passports_df, -cid)))

print(valid1)

# Part 2
valid2 <- passports_df %>%
  mutate(height = as.integer(gsub("(\\d+).*", "\\1", hgt)),
         height_unit = str_sub(hgt, -2)) %>%
  filter(as.integer(byr) >= 1920 & as.integer(byr) <= 2002 &
           as.integer(iyr) >= 2010 & as.integer(iyr) <= 2020 &
           as.integer(eyr) >= 2020 & as.integer(eyr) <= 2030 &
           ((height_unit == "cm" & height >= 150 & height <= 193) |
              (height_unit == "in" & height >= 59 & height <= 76)) &
           str_detect(hcl, "^#[\\d|a-f]{6}$") &
           ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") &
           str_detect(pid, "^\\d{9}$")) %>%
  nrow()

print(valid2)
