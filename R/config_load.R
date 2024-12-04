#' @export
config_load <- function(config_file_path = "") {
  library(here)

  # Locate env.yml in the package
  config_file <- system.file("env.yml", package = "ForestForesight")
  if (config_file_path != "") {
    config_file <- config_file_path
  }

  if (file.exists(config_file)) {
    load_variables(config_file)
    message("\nDefault config, env.yml loaded successfully.")

    # user_config_file is used by users to replace or supplement default configuration
    user_config_file <- here::here("config.yml")
    if (file.exists(user_config_file)) { # optionally load the user config_file
      print("user_config_file, config.yml was found!")
      load_variables(user_config_file)
    }  else {
      message("User config file is not found the working directory")
    }
  } else {
    error("Default config file, env.yml does not exist. Please check the file path....")
  }
}

load_variables <- function(config_file) {
  library(yaml)
  # Load the YML file
  config <- yaml::yaml.load_file(config_file)
  set_env_vars <- function(config_list, prefix = "") {
    for (name in names(config_list)) {
      value <- config_list[[name]]
      var_name <- paste0(prefix, toupper(name))
      if (is.list(value)) {
        set_env_vars(value, paste0(var_name, "_"))
      } else {
        # Only set environment variables if the value is not NULL or an empty string
        if (!is.null(value) && nzchar(value)) {
          # Resolve relative paths to absolute
          if (grepl("^tests/", value)) {
            value <- system.file(value, package = "ForestForesight")
          }
          library(base)
          do.call(Sys.setenv, stats::setNames(list(value), var_name))
          # do.call(Sys.setenv, list(var_name = value))
          # do.call(Sys.setenv, setNames(list(value), var_name))
          # Sys.setenv(var_name = value)  # Directly set the environment variable
        }
      }
    }
  }
  set_env_vars(config)
}
