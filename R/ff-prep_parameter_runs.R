library("ForestForesight")

# first sync with the bucket

download_folder <- "C:/Users/maasv/Documents/downloads"

identifier <- "GAB"  # Example: LAO

# Call the ff_sync function
# ff_sync(
#   ff_folder = download_folder,
#   identifier = identifier,
#   download_model = TRUE,
#   download_data = TRUE,
#   download_predictions = TRUE,
#   download_groundtruth = TRUE,
#   verbose = TRUE
# )

# running ff_prep for Gabon by countrybode
#identifier <- "GAB"
#ff_prep(datafolder = download_folder, identifier = identifier)
data_folder <- "C:/Users/maasv/Documents/downloads/preprocessed"
# running on tiles
tiles <- c("00N_080W", "00N_090W")
tile_prepped <- ff_prep(datafolder = data_folder, tiles = tiles, sample_size = 1)

tile_hash <- digest(tile_prepped$data_matrix, algo = "md5")
cat("tile hash ", tile_hash, "\n")

rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])

# running on country
country_prepped <- ff_prep(datafolder = data_folder, country = identifier, sample_size = 1)

country_hash <- digest(country_prepped$data_matrix, algo = "md5")
cat("Country hash", country_hash, "\n")

rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])

## running on shape
countries <- vect(get(data("countries")))
shape <- countries[countries$iso3 == "GAB"]
shape_prepped <- ff_prep(datafolder = data_folder, shape = shape, sample_size = 1)

shape_hash <- digest(shape_prepped$data_matrix, algo = "md5")
cat("Shape hash", shape_hash, "\n")

rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])

features <- c("temperature", "lastmonth", "confidence", "totallossalerts")

# including features
included_features_prepped <- ff_prep(datafolder = data_folder, country = identifier, inc_features = features, sample_size = 1)
inc_features_hash <- digest(included_features_prepped$data_matrix, algo = "md5")
cat("Shape hash", inc_features_hash, "\n")

rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])

# excluding features
excluded_features_prepped <- ff_prep(datafolder = data_folder, country = identifier, exc_features = features, sample_size = 1)
exc_features_hash <- digest(excluded_features_prepped$data_matrix, algo = "md5")
cat("Shape hash", exc_features_hash, "\n")

rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])








