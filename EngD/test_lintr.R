library(lintr)
lint_result <- lint("R/ff_run.R")

# Print the lint results
print(lint_result)

# Get the number of lints
num_lints <- length(lint_result)
cat("Number of lints:", num_lints, "\n")