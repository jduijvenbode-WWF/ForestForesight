code_location <- "C:/Kodingan3/ForestForesight/" # Adapt this to your local R project path
library("devtools")
load_all(code_location) # This enables usage of local R code, not the build package of ForestForesight
config <- ff_load_config()
download_folder <- config$DATA_FOLDER

# Choose an identifier (country code, tile ID, or SpatVector)
identifier <- "GAB"

# Call the ff_sync function that use config file to download Gabon model
ff_sync(
  ff_folder = download_folder,
  identifier = identifier,
  download_model = TRUE,
  download_data = FALSE,
  download_predictions = FALSE,
  verbose = TRUE
)
