# Make sure to setwd to the right directory
tickets <- read.table("day05.txt", quote="\"", comment.char="")
colnames(tickets) <- c("ticket")

library(stringr)

# Part 1
tickets <- tickets %>%
  mutate(binary_row = gsub("F", "0", gsub("B", "1", str_sub(ticket, 1, 7))),
         row = strtoi(binary_row, base = 2),
         binary_col = gsub("L", "0", gsub("R", "1", str_sub(ticket, -3))),
         col = strtoi(binary_col, base = 2),
         seat_id = row * 8 + col)

print(max(tickets$seat_id))

# Part 2
all_seats <- min(tickets$seat_id):max(tickets$seat_id)
your_seat <- setdiff(all_seats, tickets$seat_id)

print(your_seat)
