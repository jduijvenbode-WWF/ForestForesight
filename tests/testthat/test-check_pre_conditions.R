library(testthat)

# Mock input data
dates <- Sys.getenv("TEST_FF_PREP_QC_DATE")
country <- Sys.getenv("TEST_FF_PREP_COUNTRY")
datafolder <- Sys.getenv("TEST_DATA_FOLDER")
# cat("From test-QC: the country is ")
# cat(country)
shape <- NULL
tiles <- NULL
shrink <- "none"

test_that("check_pre_conditions works as expected", {
  # Test valid input, expecting to return a list with datafolder and shrink
  result <- check_pre_conditions(dates, country, shape, tiles, datafolder, shrink)
  expect_equal(result$datafolder, datafolder)
  expect_equal(result$shrink, shrink)

  # Test missing country and shape, expecting shrink to be set to 'none'
  result <- check_pre_conditions(dates, country, NULL, tiles, datafolder, shrink)
  expect_equal(result$shrink, "none")

  # Test missing dates with NA
  expect_error(
    check_pre_conditions(NA, country, shape, tiles, datafolder, shrink),
    "No dates were given"
  )

  # Test missing dates with NULL
  expect_error(
    check_pre_conditions(NULL, country, shape, tiles, datafolder, shrink),
    "No dates were given"
  )

  # Test missing dates with an empty string
  expect_error(
    check_pre_conditions("", country, shape, tiles, datafolder, shrink),
    "No dates were given"
  )

  # Test missing tiles, country, and shape
  expect_error(
    check_pre_conditions(dates, NULL, NULL, NULL, datafolder, shrink),
    "Unknown what to process since no tiles, country, or shape were given"
  )

  # Test shape is not SpatVector
  expect_error(
    check_pre_conditions(dates, country, "invalid_shape", tiles, datafolder, shrink),
    "Shape should be of class SpatVector"
  )
})
