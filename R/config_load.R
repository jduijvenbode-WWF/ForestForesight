#' @export
config_load <- function(config_file_path = "") {
  library(yaml)
  library(here)

  if (config_file_path == "") {
    # Locate config.yml in the root
    config_file <- here::here("tests/env.yml")
  } else {
    config_file <- config_file_path
  }

  if (file.exists(config_file)) {
    load_variables(config_file)
    message("Default config, env.yml loaded successfully.")

    # user_config_file is used by users to replace or supplement default configuration
    user_config_file <- here::here("config.yml")
    if (file.exists(user_config_file)) { # optionally load the user config_file
      print("user_config_file was found!")
      load_variables(user_config_file)
    }
  } else {
    warning("The config file does not exist. Please check the file path....")
  }
}

load_variables <- function(config_file) {
  # Load the YML file
  config <- yaml::yaml.load_file(config_file)
  set_env_vars <- function(config_list, prefix = "") {
    for (name in names(config_list)) {
      value <- config_list[[name]]
      var_name <- paste0(prefix, toupper(name))
      if (is.list(value)) {
        set_env_vars(value, paste0(var_name, "_"))
      } else {
        # Resolve relative paths to absolute
        if (grepl("^tests/", value)) {
          value <- here::here(value)
        }
        do.call(Sys.setenv, setNames(list(value), var_name))
      }
    }
  }
  set_env_vars(config)
}
