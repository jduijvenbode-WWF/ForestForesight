library(testthat)

test_that("load_groundtruth_raster handles no groundtruth file of the correct date correctly", {
  # Mock data
  datafolder <- paste0(Sys.getenv("TEST_DATA_FOLDER"), "/preprocessed")
  date <- "2024-09-01" # Missing date for ground truth
  tiles <- c("10N_110E")
  groundtruth_pattern <- "groundtruth6m"
  verbose <- TRUE

  message("\ndatafolder: ")
  message(datafolder)
  allfiles <- list_and_filter_tile_files(datafolder = datafolder, tiles, groundtruth_pattern, verbose)
  selected_files <- filter_files_by_date_and_groundtruth(date, allfiles, groundtruth_pattern)
  extent <- terra::ext(terra::rast(selected_files[1]))
  first <- TRUE
  verbose <- FALSE
  hasgroundtruth <- FALSE

  checker_raster <- terra::rast(selected_files[1], win = extent)
  checker_raster[] <- 0
  # Run the function
  result <- load_groundtruth_raster(selected_files, groundtruth_pattern, first, verbose, extent, hasgroundtruth)

  # Check the results
  expect_false(result$hasgroundtruth)
  expect_equal(dim(result$groundtruth_raster), dim(checker_raster))
  expect_equal(str(result$groundtruth_raster), str(checker_raster))
})
