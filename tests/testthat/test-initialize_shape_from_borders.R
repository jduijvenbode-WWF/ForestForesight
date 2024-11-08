test_that("initialize_shape_from_borders works with real data and extract shrink", {
  # Load actual countries data
  data("countries", package = "ForestForesight") # Replace with the actual package name
  borders <- vect(countries) # Assuming countries data is a spatial object

  # Define a test raster
  test_raster <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  values(test_raster) <- runif(ncell(test_raster), min = 0, max = 100)

  # Save raster to a temporary file
  test_file <- tempfile(fileext = ".tif")
  writeRaster(test_raster, test_file, overwrite = TRUE)

  # Null shape to trigger extraction from borders
  shape <- NULL

  # Run function
  result_shape <- initialize_shape_from_borders(shape, shrink = "extract", files = test_file, borders = borders)

  # Verify result is a SpatVector
  expect_true(inherits(result_shape, "SpatVector"))
  expect_false(is.null(result_shape))
})

test_that("initialize_shape_from_borders does not alter shape when shrink is 'none'", {
  # Load actual countries data
  data("countries", package = "ForestForesight") # Replace with the actual package name
  borders <- vect(countries)

  # Provided shape should not change when shrink is 'none'
  mock_shape <- vect(matrix(c(0, 0, 5, 5), ncol = 2), type = "polygons")
  files <- NULL
  shrink <- "none"

  # Run the function
  shape_result <- initialize_shape_from_borders(mock_shape, shrink, files, borders)

  # Expect shape_result to be identical to mock_shape
  expect_identical(shape_result, mock_shape)
})
