library(testthat)
library(terra)

# test_that("load_groundtruth_raster works correctly", {
#   # Mock data
#   selected_files <- c("file1.tif", "groundtruth_file.tif", "file3.tif") # choose files in one tile and one date from test_data
#   groundtruth_pattern <- "groundtruth"
#   first <- TRUE # When is it false?
#   verbose <- FALSE
#   hasgroundtruth <- FALSE # should I put GT in the input folder???

#   # Expected output
#   expected_groundtruth_raster <- terra::rast("groundtruth_file.tif")
#   expected_hasgroundtruth <- TRUE

#   # Run the function
#   result <- load_groundtruth_raster(selected_files, groundtruth_pattern, first, verbose, hasgroundtruth)

#   # Check the results
#   expect_true(result$hasgroundtruth)
#   expect_equal(result$groundtruth_raster, expected_groundtruth_raster)
# })

test_that("load_groundtruth_raster handles no groundtruth file correctly", {
  # Mock data
  datafolder <- paste0(Sys.getenv("TEST_DATA_FOLDER"), "/preprocessed")
  date <- "2024-09-01"
  groundtruth_pattern <- Sys.getenv("DEFAULT_GROUNDTRUTH")
  tiles <- c("00N_000E")
  groundtruth_pattern <- "groundtruth6m"
  verbose <- TRUE
  extent <- ext(0, 10, -10, 0)


  message("\ndatafolder: ")
  message(datafolder)
  allfiles <- list_and_filter_tile_files(datafolder = datafolder, tiles, groundtruth_pattern, verbose)
  selected_files <- filter_files_by_date_and_groundtruth(date, allfiles, groundtruth_pattern)
  first <- TRUE
  verbose <- FALSE
  hasgroundtruth <- FALSE

  checker_raster <- terra::rast(selected_files[1], win = extent)
  checker_raster[] <- 0
  # Run the function
  result <- load_groundtruth_raster(selected_files, groundtruth_pattern, first, verbose, extent, hasgroundtruth)

  # Check the results
  # message("hasgroundtruth is not false?\n")
  expect_false(result$hasgroundtruth)
  # message("checker_raster is not equal?\n")
  expect_equal(dim(result$groundtruth_raster), dim(checker_raster))
  expect_equal(str(result$groundtruth_raster), str(checker_raster))
})
