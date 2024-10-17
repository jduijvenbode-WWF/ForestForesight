library(testthat)
library(ForestForesight)

if (Sys.getenv("NOT_CRAN") == "true") {
  test_check("ForestForesight")
}