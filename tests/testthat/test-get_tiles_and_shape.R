# Load sample data for tilesvect from gfw_tiles to simulate real spatial data
data(gfw_tiles, envir = environment())
tilesvect <- terra::vect(gfw_tiles)
tiles <- NULL
verbose <- TRUE

test_that("get_tiles_and_shape works when country is provided", {
  # Case: Country is provided, should process based on country
  country <- "BRN"
  shape <- NULL

  # Run the function
  result <- get_tiles_and_shape(country, shape, tilesvect, tiles, verbose)

  # Check if tiles are selected based on the given country
  expect_true(!is.null(result$tiles))
  expect_true(length(result$tiles) > 0)
  expect_equal(result$shape$iso3, country)
})

test_that("get_tiles_and_shape works when shape is provided", {
  country <- NULL
  data(countries, envir = environment())
  countries <- vect(get(data("countries")))
  shape_from_countries <- countries[countries$iso3 == "BRN"]

  # Run the function with the shape from countries
  result <- get_tiles_and_shape(NULL, shape_from_countries, tilesvect, tiles, verbose)

  # Check if tiles are selected based on the given shape from countries
  expect_true(!is.null(result$tiles))
  expect_true(length(result$tiles) > 0)
  expect_equal(result$shape, shape_from_countries)
})

test_that("get_tiles_and_shape projects shape to EPSG:4326 if necessary", {
  # Case: Shape is retrieved from the countries package with a non-EPSG:4326 CRS
  country <- NULL
  data(countries, envir = environment())
  countries <- vect(get(data("countries")))
  shape_from_countries <- terra::project(countries[countries$iso3 == "BRN"], "+proj=utm +zone=33 +datum=WGS84")

  # Run the function
  result <- get_tiles_and_shape(country, shape_from_countries, tilesvect, tiles, verbose)

  # Verify the shape has been projected to EPSG:4326
  expect_true(terra::is.lonlat(result$shape))
})

test_that("get_tiles_and_shape returns only NULL if neither country or shape is provided", {
  # Case: Neither country nor shape is provided, function should return null values
  country <- NULL
  shape <- NULL

  # Run the function
  result <- get_tiles_and_shape(country, shape, tilesvect, tiles, verbose)

  expect_null(result$tiles)
  expect_null(result$shape)
})
