#' Extract Feature Importance from ForestForesight Model
#'
#' This function loads a ForestForesight model (.model file) and its corresponding
#' feature names (.rda file), extracts feature importance, and saves the results to a CSV file.
#'
#' @param model_path xgb.model class object or Character string for Path to the .model file.
#' @param output_csv Character string. Path to the output CSV file.
#' @param name Character string. Name to be given to the model if the model is an xgb.model
#' @param append Logical. If TRUE, append to existing CSV file. If FALSE, overwrite. Default is TRUE
#'
#' @return Invisibly returns the importance dataframe.
#'
#' @details
#' The function expects a .model file and a corresponding .rda file in the same directory
#' with the same name (different extension). The .rda file should contain the feature names.
#'
#' @import xgboost
#' @importFrom utils write.table
#'
#' @references
#' Jonas van Duijvenbode (2023)
#' Zillah Calle (2023)
#'
#' @examples
#' \dontrun{
#' ff_importance("path/to/model.model", "output_importance.csv")
#' ff_importance("path/to/another_model.model", "output_importance.csv", append = TRUE)
#' }
#'
#' @export
ff_importance <- function(model, output_csv, name = NA, append = TRUE) {
  if (!has_value(name)) {
    if (is.character(model)) {
      name <- sub("\\.model$", "", basename(model))
    }
  }
  model <- load_model(model)
  # Load feature names
  feature_names <- model$feature_names
  # Get importance
  importance_matrix <- xgboost::xgb.importance(model = model)
  # create importance dataset structure

  importance_dataframe <- data.frame(
    model_name = rep(name, nrow(importance_matrix)),
    feature = importance_matrix$Feature,
    rank = seq_len(nrow(importance_matrix)),
    importance = importance_matrix$Gain
  )

  # Write to CSV
  if (!file.exists(output_csv)) {
    append <- FALSE
  }
  write.table(importance_dataframe,
    file = output_csv, sep = ",", row.names = FALSE,
    col.names = !append, append = append
  )

  # Return dataframe invisibly
  invisible(importance_dataframe)
}
