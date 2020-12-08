# Make sure to setwd to the right directory
code <- read.table("~/Misc/AoC/day08.txt", quote="\"", comment.char="")
colnames(code) <- c("opcode", "argument")
code$argument <- as.integer(code$argument)

code_step <- function(code, line, accumulator) {
  opcode <- code$opcode[line]
  if (opcode == "nop") {
    return(list(line = line + 1, accumulator = accumulator))
  }
  if (opcode == "acc") {
    return(list(line = line + 1, accumulator = accumulator + code$argument[line]))
  }
  if (opcode == "jmp") {
    return(list(line = line + code$argument[line], accumulator = accumulator))
  }
  print("Illegal opcode")
}

# Part 1
code_loop <- function(code) {
  visited <- c()
  line <- 1
  accumulator <- 0
  
  while (!(line %in% visited)) {
    visited <- c(visited, line)
    next_step <- code_step(code, line, accumulator)
    line <- next_step$line
    accumulator <- next_step$accumulator
    if (line == dim(code)[1] + 1) {
      return(list(infinite_loop = FALSE, accumulator = accumulator))
    }
  }
  return(list(infinite_loop = TRUE, accumulator = accumulator))
}

print(code_loop(code)$accumulator)

# Part 2
for (line in 1:(dim(code)[1])) {
  opcode <- code$opcode[line]
  if (opcode == "nop") {
    modified_code <- code
    modified_code$opcode[line] <- "jmp"
    test_run <- code_loop(modified_code)
    if (!test_run$infinite_loop) {
      print(test_run$accumulator)
    }
  }
  if (opcode == "jmp") {
    modified_code <- code
    modified_code$opcode[line] <- "nop"
    test_run <- code_loop(modified_code)
    if (!test_run$infinite_loop) {
      print(test_run$accumulator)
    }
  }
}
