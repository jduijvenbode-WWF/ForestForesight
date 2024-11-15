config_load <- function() {
  # Load the yaml package
  library(yaml)
  library(here)
  # Put your config.yml in your working directory
  config_file <- here::here("config.yml") # To avoid conflict, make sure VARs in config.yml are different from Sys_Vars

  # Check if the config file exists
  if (file.exists(config_file)) {
    # Read the config.yml file
    config <- yaml.load_file(config_file)

    # Function to set environment variables from a list
    set_env_vars <- function(config_list, prefix = "") {
      for (name in names(config_list)) {
        value <- config_list[[name]]
        var_name <- paste0(prefix, toupper(name))
        if (is.list(value)) {
          set_env_vars(value, paste0(var_name, "_"))
        } else {
          do.call(Sys.setenv, setNames(list(value), var_name))
        }
      }
    }

    # Set environment variables
    set_env_vars(config)
    # Example: Print one of the environment variables to check
    # cat("EARLIEST_DATA_DATE:\n")
    # print(Sys.getenv("DATABASE_HOST"))
  } else {
    # Handle the case where the config file does not exist
    warning("The config file does not exist. Please check the file path.")
  }
}
