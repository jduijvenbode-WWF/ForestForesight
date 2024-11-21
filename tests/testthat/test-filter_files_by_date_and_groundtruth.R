test_that("filter_files_by_date_and_groundtruth selects files based on date and exact groundtruth pattern", {
  # Mock files with correct format: tile_YYYY-MM-DD_feature.tif
  files <- c(
    "tile_2023-01-01_groundtruth6m.tif", "tile_2023-01-01_regular.tif",
    "tile_2023-02-01_groundtruth6m.tif", "tile_2023-01-01_groundtruth4m.tif",
    "tile_2023-01-01_otherfeature.tif"
  )

  groundtruth_pattern <- "groundtruth6m"
  date <- "2023-01-01"

  # Apply the function
  selected_files <- filter_files_by_date(date, files, groundtruth_pattern)

  # Expectations
  expect_true("tile_2023-01-01_groundtruth6m.tif" %in% selected_files) # Should be included
  expect_false("tile_2023-02-01_groundtruth6m.tif" %in% selected_files) # Wrong date
})
