regex_pattern <- "(?:([A-Za-z_][A-Za-z0-9_]*)\\s*<-\\s*([0-9]+|\"[^\"]*\"|'[^']*'))|(?:([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([0-9]+|\"[^\"]*\"|'[^']*'))"
Rpath <- "R"

# Get all R script files in the project
r_files <- list.files(path = Rpath, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
cat("r_files:\n", paste(r_files, collapse = "\n"), "\n")

# Search for constant assignments in each file and store filename, line number, and matched line
constant_assignments <- lapply(r_files, function(file) {
  lines <- readLines(file)  # Read file contents
  line_numbers <- grep(regex_pattern, lines)  # Get line numbers with matches

  if (length(line_numbers) > 0) {  # Check if any matches were found
    matched_lines <- lines[line_numbers]  # Extract the matching lines

    # Combine filename, line number, and matched line into a data frame
    data.frame(
      file = file,
      line_number = line_numbers,
      line_content = matched_lines,
      stringsAsFactors = FALSE
    )
  } else {
    return(NULL)  # No matches found, return NULL
  }
})

# Filter out NULL results and combine into one data frame
constant_assignments_df <- do.call(rbind, Filter(Negate(is.null), constant_assignments))

# Print out the results
print(constant_assignments_df)
