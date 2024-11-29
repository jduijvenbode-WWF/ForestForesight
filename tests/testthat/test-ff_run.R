test_that("check that ff_run can handle ", {
  test_dir <- tempdir()
  tiffile <- file.path(test_dir, "result.tif")
  tiffile2 <- file.path(test_dir, "result2.tif")
  datadir <- file.path(getwd(), Sys.getenv("TEST_DATA_FOLDER"))
  print(datadir)
  print(dir.exists(datadir))
  modelfile <- file.path(test_dir, "model.model")
  testthat::expect_no_error(
    result1 <- ff_run(
      country = "BRN", train_dates = "2023-01-01",
      validation_dates = "2023-02-01", prediction_dates = "2023-03-01",
      ff_folder = datadir,
      ff_prep_params = list(inc_features = c("initialforestcover", "timesinceloss")),
      save_path_predictions = tiffile,
      save_path = modelfile
    )
  )

  testthat::expect_no_error(
    result2 <- ff_run(country = "BRN", prediction_dates = "2023-03-01", ff_folder = datadir, save_path_predictions = tiffile2, trained_model = modelfile)
  )
  testthat::expect_equal(global(rast(tiffile), "sum"), global(rast(tiffile2), "sum"))
  countries <- terra::vect(get(data("countries")))
  countries <- countries[countries$iso3 == "BRN"]
  testthat::expect_no_error(result3 <- ff_run(shape = countries, prediction_dates = "2023-03-01", ff_folder = datadir, save_path_predictions = tiffile, trained_model = modelfile))
  testthat::expect_no_error(result4 <- ff_run(shape = countries, train_dates = "2023-02-01", ff_folder = datadir, save_path = modelfile))
})
