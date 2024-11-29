# helper_config.R
config_file_path <- here::here("env.yml")
if (file.exists(config_file_path)) {
  # normal unit tests run
} else { # the rcmdcheck unit tests
  config_file_path <- file.path(getwd(), "../env.yml")
  print(config_file_path)

  if (!file.exists(config_file_path)) {
    stop("Error: ../env.yml does not exist. Please provide env.yml under tests folder.")
  }
}
config_load(config_file_path)
