# helper_config.R
config_load()

# Print a message to indicate the setup file has been executed
print("helper_config.R has been executed")

cat("TEST_FF_PREP_COUNTRY is ")
cat(Sys.getenv("TEST_FF_PREP_COUNTRY"))
