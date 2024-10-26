library(config)

config_load <- function() {
  config_filename <- "config.yml"

  # Check if the config file exists in the working directory
  if (file.exists(config_filename)) {
    message("Loading config.yml from the working directory.")
    config <- config::get()
  } else {
    # Define the path to the config file in the package directory
    package_config_file <- system.file(config_filename, package = "ForestForesight")
    cat("package_config_file: ")
    cat(package_config_file)
    cat("\n")
    if (file.exists(package_config_file)) {
      message("Loading config.yml from the package directory.")
      config <- config::get(file = package_config_file)
    } else {
      stop("Config file not found in working directory or package directory.")
    }
  }

  return(config)
}
