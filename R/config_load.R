config_load <- function() {
  library(yaml)
  library(here)

  # Locate config.yml in the root
  config_file <- here::here("config.yml")

  if (file.exists(config_file)) {
    # Load the YAML file
    config <- yaml.load_file(config_file)

    # Set environment variables
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
  } else {
    warning("The config file does not exist. Please check the file path.")
  }
}