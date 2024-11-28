# helper_config.R
config_file_path <- here::here("env.yml")
if (file.exists(config_file_path)) {
  print("wd is in the root => the normal test")
} else {
  print("============probably in rcmdcheck. printing new config_file_path")
  config_file_path <- file.path(getwd(), "../../env.yml")
  print(config_file_path)
  if (file.exists(config_file_path)) {
    print("------../../env.yml exists!")
    message("-------../../env.yml exists!")
  }
}
config_load(config_file_path)
