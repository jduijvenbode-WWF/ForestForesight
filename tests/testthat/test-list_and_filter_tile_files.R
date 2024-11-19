test_that("list_and_filter_tile_files function works as expected", {
  tiles <- c("00N_000E", "00N_010E")
  groundtruth_pattern <- "groundtruth6m"
  verbose <- TRUE

  # Create a temporary folder structure and files for testing
  temp_datafolder <- tempdir()

  dir.create(file.path(temp_datafolder, "input", tiles[1]), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(temp_datafolder, "input", tiles[2]), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(temp_datafolder, "groundtruth", tiles[1]), recursive = TRUE, showWarnings = FALSE)

  # Create mock files for input and groundtruth
  input_file1 <- file.path(temp_datafolder, "input", tiles[1], "input_file1.tif")
  input_file2 <- file.path(temp_datafolder, "input", tiles[2], "input_file2.tif")
  groundtruth_file1 <- file.path(temp_datafolder, "groundtruth", tiles[1], "groundtruth6m.tif")

  file.create(input_file1)
  file.create(input_file2)
  file.create(groundtruth_file1)

  # Case 1: Valid input with groundtruth filtering
  allfiles <- list_and_filter_tile_files(temp_datafolder, tiles, groundtruth_pattern, verbose)

  # Check that all expected files are returned
  expect_true(input_file1 %in% allfiles)
  expect_true(input_file2 %in% allfiles)
  expect_true(groundtruth_file1 %in% allfiles)

  # Case 2: Empty data folder, expect error
  empty_datafolder <- tempdir() # Create a new empty temporary directory

  unlink(list.files(file.path(empty_datafolder, "input"), full.names = TRUE), recursive = TRUE)
  unlink(list.files(file.path(empty_datafolder, "groundtruth"), full.names = TRUE), recursive = TRUE)

  # Test error when no files are found
  expect_error(
    list_and_filter_tile_files(empty_datafolder, tiles, groundtruth_pattern, verbose),
    paste("No folders with tif-files found that correspond to the given tile IDs:", paste(tiles, collapse = ","))
  )

  # Clean up mock files
  unlink(temp_datafolder, recursive = TRUE)
  unlink(empty_datafolder, recursive = TRUE)
})
