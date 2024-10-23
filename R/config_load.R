config_load <- function(config_path = "config.json") {
  # Check if jsonlite is installed
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite")
  }
  library(jsonlite)

  # Check if here is installed. here package ensures the relative path to the root of R project.
  if (!requireNamespace("here", quietly = TRUE)) {
    install.packages("here")
  }
  library(here)

  # Read the config file
  config <- fromJSON(config_path)
  return (config)
}