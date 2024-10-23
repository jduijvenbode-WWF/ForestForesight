library(testthat)
# config <- config_load() # Hasrul: enabling this makes error in my testthat run

# Mock input data
# dates <- config$TEST_FF_PREP_QC_DATE # Hasrul: we should find another way to use config file for our tests
dates <- "2023-01-01"
country <- "GAB"
shape <- NULL
tiles <- NULL
datafolder <- "D:/WWF/preprocessed"
shrink <- "none"

test_that("quality_check works as expected", {
  # Test valid input, expecting to return a list with datafolder and shrink
  result <- quality_check(dates, country, shape, tiles, datafolder, shrink)
  expect_equal(result$datafolder, datafolder)
  expect_equal(result$shrink, shrink)

  # Test missing date before 2021-01-01
  expect_error(
    quality_check("2020-12-31", country, shape, tiles, datafolder, shrink),
    "The earliest date available is 2021-01-01"
  )

  # Test missing country and shape, expecting shrink to be set to 'none'
  result <- quality_check(dates, country, NULL, tiles, datafolder, shrink)
  expect_equal(result$shrink, "none")

  # Test missing dates with NA
  expect_error(
    quality_check(NA, country, shape, tiles, datafolder, shrink),
    "No dates were given"
  )

  # Test missing dates with NULL
  expect_error(
    quality_check(NULL, country, shape, tiles, datafolder, shrink),
    "No dates were given"
  )

  # Test missing dates with an empty string
  expect_error(
    quality_check("", country, shape, tiles, datafolder, shrink),
    "No dates were given"
  )

  # Test missing tiles, country, and shape
  expect_error(
    quality_check(dates, NULL, NULL, NULL, datafolder, shrink),
    "Unknown what to process since no tiles, country, or shape were given"
  )

  # Test shape is not SpatVector
  expect_error(
    quality_check(dates, country, "invalid_shape", tiles, datafolder, shrink),
    "Shape should be of class SpatVector"
  )
})
