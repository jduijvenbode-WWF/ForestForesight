
test_that("prepare_raster_data_by_tile works without shrink or window", {
  # Create mock files and raster data
  temp_file1 <- tempfile(fileext = ".tif")
  temp_file2 <- tempfile(fileext = ".tif")

  r1 <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  r2 <- terra::rast(nrows = 10, ncols = 10, xmin = 5, xmax = 15, ymin = 5, ymax = 15)

  values(r1) <- runif(ncell(r1), min = 0, max = 100)
  values(r2) <- runif(ncell(r2), min = 0, max = 100)

  writeRaster(r1, temp_file1, overwrite = TRUE)
  writeRaster(r2, temp_file2, overwrite = TRUE)

  files <- c(temp_file1, temp_file2)
  shape <- vect(matrix(c(0, 0, 15, 15), ncol = 2), type = "polygons")
  verbose <- TRUE

  # Run the function without shrink or window
  result <- prepare_raster_data_by_tile(files, shape, shrink = "none", window = NA, verbose = verbose)
  extent <- result$extent
  rasstack <- result$rasstack

  # Check output
  expect_true(inherits(rasstack, "SpatRaster"))
})

test_that("prepare_raster_data_by_tile works with shrink set to 'crop'", {
  # Create mock files and raster data with identical extents
  temp_file1 <- tempfile(fileext = ".tif")
  temp_file2 <- tempfile(fileext = ".tif")

  # Define a common extent for both rasters to prevent mismatch
  common_extent <- ext(0, 10, 0, 10)

  r1 <- terra::rast(nrows = 10, ncols = 10, ext = common_extent)
  r2 <- terra::rast(nrows = 10, ncols = 10, ext = common_extent)

  # Fill rasters with values
  values(r1) <- runif(ncell(r1), min = 0, max = 100)
  values(r2) <- runif(ncell(r2), min = 0, max = 100)

  writeRaster(r1, temp_file1, overwrite = TRUE)
  writeRaster(r2, temp_file2, overwrite = TRUE)

  files <- c(temp_file1, temp_file2)
  shape <- vect(matrix(c(0, 0, 10, 10), ncol = 2), type = "polygons")
  window <- ext(2, 8, 2, 8)
  verbose <- TRUE

  # Run the function with shrink set to "crop"
  result <- prepare_raster_data_by_tile(files, shape, shrink = "crop", window = window, verbose = verbose)
  extent <- result$extent
  rasstack_crop <- result$rasstack
  # Check output
  expect_true(inherits(rasstack_crop, "SpatRaster"))
  expect_true(ext(rasstack_crop) <= common_extent)
})
