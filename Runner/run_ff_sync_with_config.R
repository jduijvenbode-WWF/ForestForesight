#library(ForestForesight)
config_load()

code_location <- "C:/Kodingan3/ForestForesight/"
library("devtools")
load_all(code_location) # with this we are using the live R code, not the build package of ForestForesight

download_folder <- Sys.getenv("DATA_FOLDER") #adjust this
# Choose an identifier (country code, tile ID, or SpatVector)
# identifier <- "PER"  # does this work?

identifier <- "GAB"  # does this work?
#identifier <- shape # use this if you want to download the area that you click from "Loading Area of Intereset"

# Call the ff_sync function
ff_sync(
  ff_folder = download_folder,
  identifier = identifier,
  download_model = TRUE, #prediction models
  download_data = FALSE, #pre-processed data
  download_predictions = FALSE, #disable this if you don't want to download previous prediction
  verbose = TRUE
)
