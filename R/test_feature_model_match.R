#' Test Feature and Model Match
#'
#' This function tests whether the provided feature names match the features in the given XGBoost model.
#' It checks if the model file and the corresponding feature names file exist, and if so, loads them.
#' It then attempts to generate the importance matrix to validate the match.
#'
#' @param model Either a character string representing the path to the XGBoost model file, or an `xgb.Booster` object.
#' @param feature_names A character vector of feature names.
#' This parameter should be provided if `model` is an `xgb.Booster` object.
#' If `model` is a file path, the function will look for the `.rda` file with feature names in the same directory.
#'
#' @return A logical value: `TRUE` if the feature names match the features in the model, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' # Example with model file and feature names file
#' model_file <- "path/to/model.model"
#' feature_names_file <- "path/to/model.rda"
#' load(feature_names_file)
#' test_feature_model_match(model_file)
#'
#' # Example with an xgb.Booster object
#' model <- xgboost::xgb.train(params = list(objective = "binary:logistic"), data = dtrain, nrounds = 10)
#' feature_names <- colnames(dtrain)
#' test_feature_model_match(model, feature_names)
#' }
#'
#' @export
test_feature_model_match <- function(model, feature_names = NULL) {
  if (class(model) == "character") {
    if (!file.exists(model)) {
      stop("model file does not exist")
    }
    modelfile <- model
    if (!file.exists(gsub("\\.model", "\\.rda", model))) {
      stop("feature names were not found as RDA file in same folder as the model")
    } else {
      model <- xgboost::xgb.load(model)
      feature_names <- get(load(gsub("\\.model", "\\.rda", modelfile)))
    }
  } else {
    if (is.null(feature_names)) {
      stop("feature names should be given if model is an xgb.Booster object")
    }
  }

  result <- tryCatch(
    {
      xgb.importance(feature_names = feature_names, model = model)
      return(TRUE)
    },
    error = function(e) {
      # Print the error message
      return(FALSE)
    }
  )

  return(result)
}
