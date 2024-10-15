# download_folder <- "C:/Users/maasv/Documents/downloads"
#
# identifier <- "GAB"  # Example: Gabon
#
# features <- c("temperature", "lastmonth", "confidence", "totallossalerts")
#
# # Change to your own data_folder because we can't store it locally
# data_folder <- "C:/Users/maasv/Documents/downloads/preprocessed"
# # running on tiles
# tiles <- c("00N_080W", "00N_090W")
# tile_prepped <- ff_prep(datafolder = data_folder, tiles = tiles, sample_size = 1, inc_features = features)
#
# tile_hash <- digest(tile_prepped$data_matrix, algo = "md5")
# cat("tile hash ", tile_hash, "\n")
#
# # running on country
# country_prepped <- ff_prep(datafolder = data_folder, country = identifier, sample_size = 1, inc_features = features)
#
# country_hash <- digest(country_prepped$data_matrix, algo = "md5")
# cat("Country hash", country_hash, "\n")
#
# ## running on shape
# countries <- vect(get(data("countries")))
# shape <- countries[countries$iso3 == "GAB"]
# shape_prepped <- ff_prep(datafolder = data_folder, shape = shape, sample_size = 1, inc_features = features)
#
# shape_hash <- digest(shape_prepped$data_matrix, algo = "md5")
# cat("Shape hash", shape_hash, "\n")
#
# combined_tiles <- paste(tiles, collapse = ", ")
# combined_features <- paste(features, collapse = ", ")
#
# # change this to wherever you want to save it
# hash_folder <- "C:/Users/maasv/Documents/downloads/hashes/"
# date = Sys.Date()
# hash_matrix <- data.frame(
#   identifier_or_tiles = c(combined_tiles, identifier, "shape_of_GAB"),
#   sample_size = c(1, 1, 1),
#   included_features = c(combined_features, combined_features, combined_features),
#   hash = c(tile_hash, country_hash, shape_hash),
#   date = c(date, date, date)
# )
#
# file_name <- paste("hash_matrix_", ".csv", sep = "")
# file_location <- paste(hash_folder, file_name, sep = "")
# write.csv(hash_matrix, file = file_location, row.names = FALSE)
#
# print(hash_matrix)
