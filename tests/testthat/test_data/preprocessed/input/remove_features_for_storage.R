# Define the list of filtered features (these are the top 8 important features for middle_africa 1)
keywords <- c("timesinceloss", "smoothedtotal", "lastmonth", "previoussameseason", "confidence", "patchdensity")

# Get all the folders in the current directory
folders <- list.dirs(path = ".", recursive = FALSE)

# Function to check if any keyword is present in the filename
contains_keyword <- function(filename, keywords) {
  any(sapply(keywords, function(keyword) grepl(keyword, filename, ignore.case = TRUE)))
}

# Loop through each folder
for (folder in folders) {
  cat("Processing folder:", folder, "\n")

  # Get all files in the folder
  files <- list.files(path = folder, full.names = TRUE)

  # Loop through each file
  for (file in files) {
    # Get the file name without the path
    filename <- basename(file)

    # Check if the file contains any of the allowed keywords
    if (!contains_keyword(filename, keywords)) {
      cat("Deleting file:", filename, "\n")
      # Delete the file if no keyword matches
      file.remove(file)
    } else {
      cat("Allowed file found:", filename, "\n")
    }
  }
}

cat("Done.\n")
