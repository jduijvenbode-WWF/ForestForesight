test_that("refactored ff_prep has the same output as the original", {
  download_folder <- Sys.getenv("TEST_DATA_FOLDER") # modify this value in tests/testthat/config.yml

  identifier <- Sys.getenv("TEST_FF_PREP_COUNTRY")

  features <- c("initialforestcover", "lastsixmonths", "timesinceloss")

  data_folder <- paste(download_folder, "preprocessed", sep = "/")

  # sample_size=1 in order to remove randomness
  # running on tiles
  tiles <- c("10N_110E")
  tile_prepped_ref <- ff_prep_refactored(datafolder = download_folder, tiles = tiles, sample_size = 1, inc_features = features)
  tile_prepped <- ff_prep(datafolder = data_folder, tiles = tiles, sample_size = 1, inc_features = features)

  tile_hash_ref <- digest::digest(tile_prepped_ref$feature_dataset, algo = "md5")
  cat("tile hash ", tile_hash_ref, "\n")

  tile_hash <- digest::digest(tile_prepped$data_matrix, algo = "md5")
  cat("tile hash ", tile_hash, "\n")

  # running on country
  country_prepped <- ff_prep(datafolder = data_folder, country = identifier, sample_size = 1, inc_features = features)
  country_prepped_ref <- ff_prep_refactored(datafolder = download_folder, country = identifier, sample_size = 1, inc_features = features)

  country_hash <- digest::digest(country_prepped$data_matrix, algo = "md5")
  cat("Country hash", country_hash, "\n")

  country_hash_ref <- digest::digest(country_prepped_ref$feature_dataset, algo = "md5")
  cat("Country hash", country_hash_ref, "\n")

  ## running on shape
  countries <- vect(get(data("countries")))
  shape <- countries[countries$iso3 == "BRN"]
  shape_prepped <- ff_prep(datafolder = data_folder, shape = shape, sample_size = 1, inc_features = features)
  shape_prepped_ref <- ff_prep_refactored(datafolder = download_folder, shape = shape, sample_size = 1, inc_features = features)

  shape_hash <- digest::digest(shape_prepped$data_matrix, algo = "md5")
  cat("Shape hash", shape_hash, "\n")

  shape_hash_ref <- digest::digest(shape_prepped_ref$feature_dataset, algo = "md5")
  cat("Shape hash", shape_hash_ref, "\n")

  combined_tiles <- paste(tiles, collapse = ", ")
  combined_features <- paste(features, collapse = ", ")

  hash_folder <- paste(download_folder, "hashes/", sep = "/")
  date <- Sys.Date()
  hash_matrix <- data.frame(
    identifier_or_tiles = c(combined_tiles, identifier, "shape_of_BRN"),
    sample_size = c(1, 1, 1),
    included_features = c(combined_features, combined_features, combined_features),
    hash = c(tile_hash, country_hash, shape_hash),
    date = c(date, date, date)
  )

  hash_matrix_ref <- data.frame(
    identifier_or_tiles = c(combined_tiles, identifier, "shape_of_BRN"),
    sample_size = c(1, 1, 1),
    included_features = c(combined_features, combined_features, combined_features),
    hash = c(tile_hash_ref, country_hash_ref, shape_hash_ref),
    date = c(date, date, date)
  )


  file_name_original <- paste("hash_matrix", ".csv", sep = "")
  file_name_refactored <- paste("hash_matrix_refactored", ".csv", sep = "")
  file_location_original <- paste(hash_folder, file_name_original, sep = "")
  file_location_refactored <- paste(hash_folder, file_name_refactored, sep = "")

  write.csv(hash_matrix, file = file_location_original, row.names = FALSE)
  write.csv(hash_matrix_ref, file = file_location_refactored, row.names = FALSE)

  print(hash_matrix)
  print(hash_matrix_ref)

  df_original <- read.csv(file_location_original)
  df_refactored <- read.csv(file_location_refactored)

  testthat::expect_equal(hash_matrix, hash_matrix_ref)
})
