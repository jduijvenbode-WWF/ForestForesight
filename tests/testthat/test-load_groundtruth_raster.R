library(testthat)
library(terra)

test_that("load_groundtruth_raster works correctly", {
  # Mock data
  selected_files <- c("file1.tif", "groundtruth_file.tif", "file3.tif")
  groundtruth_pattern <- "groundtruth"
  first <- TRUE
  verbose <- FALSE
  hasgroundtruth <- FALSE

  # Expected output
  expected_groundtruth_raster <- terra::rast("groundtruth_file.tif")
  expected_hasgroundtruth <- TRUE

  # Run the function
  result <- load_groundtruth_raster(selected_files, groundtruth_pattern, first, verbose, hasgroundtruth)

  # Check the results
  expect_true(result$hasgroundtruth)
  expect_equal(result$groundtruth_raster, expected_groundtruth_raster)
})

test_that("load_groundtruth_raster handles no groundtruth file correctly", {
  # Mock data
  selected_files <- c("file1.tif", "file2.tif", "file3.tif")
  groundtruth_pattern <- "groundtruth"
  first <- TRUE
  verbose <- FALSE
  hasgroundtruth <- FALSE

  # Run the function
  result <- load_groundtruth_raster(selected_files, groundtruth_pattern, first, verbose, hasgroundtruth)

  # Check the results
  expect_false(result$hasgroundtruth)
  expect_equal(result$groundtruth_raster[], 0)
})
