# Make sure to setwd to the right directory
terrain <- read.table("day03.txt", quote="\"", comment.char="")
colnames(terrain) <- c("trees")

# Part 1
numTrees <- terrain %>% mutate(width = nchar(trees),
                               vertical = 1:nrow(terrain),
                               horizontal = (3 * (vertical - 1)) %% width + 1,
                               isTree = substr(trees, horizontal, horizontal) == "#") %>%
  pull(isTree) %>%
  sum()

print(numTrees)

# Part 2

# Generalize the above
numTreesFct <- function(terrain, horizontal_speed, vertical_speed) {
  vertical <- seq(1, nrow(terrain), vertical_speed)
  data.frame(trees = terrain$trees[vertical]) %>%
    mutate(width = nchar(trees),
           vertical = 1:length(vertical),
           horizontal = (horizontal_speed * (vertical - 1)) %% width + 1,
           isTree = substr(trees, horizontal, horizontal) == "#") %>%
    pull(isTree) %>%
    sum()
}

# Check that answer to part 1 matches
# numTreesFct(terrain, 3, 1) == numTrees

print(numTreesFct(terrain, 1, 1) *
        numTreesFct(terrain, 3, 1) *
        numTreesFct(terrain, 5, 1) *
        numTreesFct(terrain, 7, 1) *
        numTreesFct(terrain, 1, 2))
