# Make sure to setwd to the right directory
library(dplyr)
library(stringr)

seats_vec <- read.table("day11.txt", quote="\"", comment.char="")[,1]
seats <- seats_vec %>%
  str_split("") %>%
  unlist() %>%
  matrix(ncol = length(seats_vec)) %>%
  t()

neighbors <- function(seats, x, y) {
  neighbor_summary <- list("." = 0, "L" = 0, "#" = 0)
  for (dx in -1:1) {
    for (dy in -1:1) {
      if (dx != 0 | dy != 0) {
        n_x <- x + dx
        n_y <- y + dy
        if (n_x >= 1 & n_x <= dim(seats)[2] &
            n_y >= 1 & n_y <= dim(seats)[1]) {
          neighbor <- seats[n_y, n_x]
          neighbor_summary[[neighbor]] <- neighbor_summary[[neighbor]] + 1
        }
      }
    }
  }
  return(neighbor_summary)
}

next_step <- function(seats, neighbor_function = neighbors, seat_threshold = 4) {
  new_seats <- seats
  for (x in 1:dim(seats)[2]) {
    for (y in 1:dim(seats)[1]) {
      neighbor_summary <- neighbor_function(seats, x, y)
      if (seats[y, x] == "L" & neighbor_summary[["#"]] == 0) {
        new_seats[y, x] <- "#"
      }
      if (seats[y, x] == "#" & neighbor_summary[["#"]] >= seat_threshold) {
        new_seats[y, x] <- "L"
      }
    }
  }
  return(new_seats)
}

# Part 1
game_of_seats <- function(seats, neighbor_function = neighbors, seat_threshold = 4) {
  repeat {
    new_seats <- next_step(seats,
                           neighbor_function = neighbor_function,
                           seat_threshold = seat_threshold)
    if (all(new_seats == seats)) {
      return(seats)
    }
    seats <- new_seats
  }
}

# This will take a while
print(sum(game_of_seats(seats) == "#"))

# Part 2
far_neighbors <- function(seats, x, y) {
  neighbor_summary <- list("L" = 0, "#" = 0)
  for (dx in -1:1) {
    for (dy in -1:1) {
      if (dx != 0 | dy != 0) {
        n_x <- x
        n_y <- y
        found = FALSE
        while (!found) {
          n_x <- n_x + dx
          n_y <- n_y + dy
          if (n_x >= 1 & n_x <= dim(seats)[2] &
              n_y >= 1 & n_y <= dim(seats)[1]) {
            neighbor <- seats[n_y, n_x]
            if (neighbor != ".") {
              neighbor_summary[[neighbor]] <- neighbor_summary[[neighbor]] + 1
              found <- TRUE
            }
          } else {
            found <- TRUE
          }
        }
      }
    }
  }
  return(neighbor_summary)
}

# This will take a while
print(sum(game_of_seats(seats, neighbor_function = far_neighbors, seat_threshold = 5) == "#"))

