# Make sure to setwd to the right directory

library(stringr)

# I added a line break at the end of the data to make this work
rules <- readLines("day07.txt")

bag_contents <- list()

color_contents <- str_split(rules, " bags contain ")

for (color_content in color_contents) {
  bags <- str_split(color_content[2], "\\s*bags*[\\.,]")[[1]]
  contents <- list()
  for (bag in bags[1:(length(bags) - 1)]) {
    color <- str_extract(bag, "[a-z]+ [a-z]+")
    count <- as.integer(str_extract(bag, "\\d+"))
    contents[[color]] <- count
  }
  color_contents[[color_content[1]]] <- contents
}
  
# Part 1
contains_bag <- function(color_contents, search_for, search_in) {
  cols_in_bag <- names(color_contents[[search_in]])
  if ("no other" %in% cols_in_bag) {
    return(FALSE)
  }
  if (search_for %in% cols_in_bag) {
    return(TRUE)
  }
  return(any(sapply(cols_in_bag, function(col) {
    contains_bag(color_contents, search_for, col)
  })))
}

contains_shiny_gold <- sapply(names(color_contents),
                              function(col) contains_bag(color_contents,
                                                         search_for = "shiny gold",
                                                         search_in = col))

print(sum(contains_shiny_gold))

# Part 2
total_bags <- function(color_contents, bag_color) {
  bag <- color_contents[[bag_color]]
  if ("no other" %in% names(bag)) {
    return(0)
  }
  return(sum(unlist(bag)) +
           sum(sapply(names(bag),
                      function(bag_col) bag[[bag_col]] * total_bags(color_contents, bag_col))))
}

print(total_bags(color_contents, bag_color = "shiny gold"))
