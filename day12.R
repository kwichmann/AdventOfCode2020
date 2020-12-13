# Make sure to setwd to the right directory
library(dplyr)
library(stringr)

navigate <- read.table("day12.txt", quote="\"", comment.char="") %>%
  mutate(bearing = str_sub(V1, 1, 1), distance = as.integer(str_sub(V1, 2, nchar(V1)))) %>%
  select(-V1)
         
# Part 1
bearings <- list(E = c(1, 0), S = c(0, -1), W = c(-1, 0), N = c(0, 1))

coord <- c(0, 0)
orientation <- 0

for (i in 1:nrow(navigate)) {
  bearing <- navigate$bearing[i]
  distance <- navigate$distance[i]
  if (bearing == "L") {
    orientation <- orientation + distance
  }
  if (bearing == "R") {
    orientation <- orientation - distance
  }
  if (bearing == "F") {
    radians <- orientation * pi / 180
    coord <- coord + c(cos(radians), sin(radians)) * distance
  }
  if (bearing %in% names(bearings)) {
    coord <- coord + bearings[[bearing]] * distance
  }
}

print(sum(abs(coord)))

# Part 2
coord <- c(0, 0)
waypoint <- c(10, 1)

for (i in 1:nrow(navigate)) {
  bearing <- navigate$bearing[i]
  distance <- navigate$distance[i]
  
  if (bearing == "L") {
    radians <- distance * pi / 180
    rotation_matrix <- matrix(c(cos(radians), sin(radians),
                                -sin(radians), cos(radians)),
                              nrow = 2, ncol = 2)
    waypoint <- as.vector(rotation_matrix %*% waypoint)
  }
  if (bearing == "R") {
    radians <- -distance * pi / 180
    rotation_matrix <- matrix(c(cos(radians), sin(radians),
                                -sin(radians), cos(radians)),
                              nrow = 2, ncol = 2)
    waypoint <- as.vector(rotation_matrix %*% waypoint)
  }
  if (bearing == "F") {
    coord <- coord + waypoint * distance
  }
  if (bearing %in% names(bearings)) {
    waypoint <- waypoint + bearings[[bearing]] * distance
  }
}

print(sum(abs(coord)))
