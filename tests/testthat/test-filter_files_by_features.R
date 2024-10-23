library(testthat)

# Create a temporary folder structure and files for testing
temp_datafolder <- tempdir()

# Create mock files for input
allfiles <- c(
  file.path(temp_datafolder, "input_file1.tif"),
  file.path(temp_datafolder, "input_file2.tif"),
  file.path(temp_datafolder, "input_featureA.tif"),
  file.path(temp_datafolder, "input_featureB.tif"),
  file.path(temp_datafolder, "groundtruth6m.tif")
)

file.create(allfiles)

# Set exclusion and inclusion features
exc_features <- c("featureA", "featureB")
inc_features <- c("groundtruth6m", "featureB")
groundtruth_pattern <- "groundtruth6m"
verbose <- TRUE


test_that("filter_files_by_features works as expected for excluding and including features in the file", {

  #
  # Case 1: Exclude features "featureA" and "featureB"
  filtered_files <- filter_files_by_features(allfiles, exc_features, NA, groundtruth_pattern, verbose)

  # Check that excluded features are not in the filtered files
  expect_false(any(grepl("featureA", filtered_files)))
  expect_false(any(grepl("featureB", filtered_files)))
  #
  # Case 2: Include only files with "featureB" and "groundtruth6m"
  filtered_files <- filter_files_by_features(allfiles, NA, inc_features, groundtruth_pattern, verbose)

  # Check that only the included features are in the filtered files
  expect_true(all(grepl("featureB|groundtruth6m", filtered_files)))
  #
  # Clean up mock files
  unlink(temp_datafolder, recursive = TRUE)
})

test_that("filter_files_by_features results in empty allfiles when all files are excluded", {
  # Create some mock files for testing
  allfiles <- c(
    file.path(temp_datafolder, "file_featureA.tif"),    # Excluded by exc_features
    file.path(temp_datafolder, "file_featureB.tif")     # Excluded by exc_features
  )

  # Create these files
  file.create(allfiles)

  # After exclusion of all features, all files should be removed,
  # the vector will be empty and it should throw an error
  expect_error(
    filter_files_by_features(allfiles, exc_features, inc_features, groundtruth_pattern, verbose),
    "After including and excluding the requested variables there are no files left"
  )

  # Clean up mock files
  unlink(temp_datafolder, recursive = TRUE)
})