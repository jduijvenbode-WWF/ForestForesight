test_that("test main folder structure is wrong, if main folder structure is wrong, ffstructure check should give an error", {
  #arrange
  test_dir <- tempdir()
  on.exit(unlink(test_dir, recursive = TRUE))

  dir.create(file.path(test_dir, "preprocessed", "input", "10N_010E"), recursive = TRUE)
  dir.create(file.path(test_dir, "preprocessed", "groundtruth", "10N_010E"), recursive = TRUE)
  dir.create(file.path(test_dir, "predictions", "BRA"), recursive = TRUE)

  countries <- vect(get(data("countries")))

  shape <- countries[countries$iso3 == "BRN"]

  #act and assert
  expect_error(ff_structurecheck(shape = shape, folder_path = test_dir, check_date = NULL, error_on_issue = TRUE, silent_on_pass = FALSE),
               regexp = "some main folders are missing",
  )

})

