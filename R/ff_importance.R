#' Extract Feature Importance from ForestForesight Model
#'
#' This function loads a ForestForesight model (.model file) and its corresponding
#' feature names (.rda file), extracts feature importance, and saves the results to a CSV file.
#'
#' @param model_path Character string. Path to the .model file.
#' @param output_csv Character string. Path to the output CSV file.
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
ff_importance <- function(model_path, output_csv, append = TRUE) {
  # Check if model file exists
  if (!file.exists(model_path)) {
    stop("Model file does not exist: ", model_path)
  }

  # Construct path for .rda file
  rda_path <- sub("\\.model$", ".rda", model_path)

  # Check if .rda file exists
  if (!file.exists(rda_path)) {
    stop("Corresponding .rda file does not exist: ", rda_path)
  }

  # Load model
  model <- xgboost::xgb.load(model_path)

  # Load feature names
  feature_names <- get(load(rda_path))

  # Assign feature names to model


  # Get importance

  importance_matrix <- xgboost::xgb.importance(model = model)

  importance_matrix$fnum <- as.numeric(gsub("f", "", importance_matrix$Feature)) + 1
  importance_matrix$Feature <- feature_names[importance_matrix$fnum]
  # Create dataframe
  df <- data.frame(
    model_name = rep(sub("\\.model$", "", basename(model_path)), nrow(importance_matrix)),
    feature = importance_matrix$Feature,
    rank = seq_len(nrow(importance_matrix)),
    importance = importance_matrix$Gain
  )

  # Write to CSV
  if (!file.exists(output_csv)) {
    append <- FALSE
  }
  write.table(df,
    file = output_csv, sep = ",", row.names = FALSE,
    col.names = !append, append = append
  )

  # Return dataframe invisibly
  invisible(df)
}
