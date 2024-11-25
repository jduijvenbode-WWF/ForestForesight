#' Train an XGBoost Model for ForestForesight
#'
#' This function trains an XGBoost model with optimized default
#' parameters derived from worldwide data analysis.
#'
#' @inheritParams ff_train
#' @return A trained XGBoost model (xgb.Booster object).
#' @import xgboost
#' @export
ff_train <- function(train_matrix,
                     validation_matrix = NULL,
                     nrounds = 200,
                     eta = 0.1,
                     max_depth = 5,
                     subsample = 0.75,
                     eval_metric = "aucpr",
                     early_stopping_rounds = 10,
                     gamma = NULL,
                     maximize = NULL,
                     min_child_weight = 1,
                     verbose = FALSE,
                     xgb_model = NULL,
                     modelfilename = NULL,
                     objective = "binary:logistic") {
  # Validate inputs
  validate_inputs(train_matrix)

  # Convert matrices to DMatrix format
  dtrain <- convert_to_dmatrix(train_matrix)
  watchlist <- create_watchlist(dtrain, validation_matrix)

  # Set up training parameters
  params <- create_params(
    objective = objective,
    eval_metric = eval_metric,
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    gamma = gamma,
    min_child_weight = min_child_weight
  )

  # Train model
  ff_cat("Starting training", verbose = verbose)
  xgbmodel <- train_model(
    params = params,
    dtrain = dtrain,
    watchlist = watchlist,
    nrounds = nrounds,
    early_stopping_rounds = early_stopping_rounds,
    maximize = maximize,
    xgb_model = xgb_model,
    verbose = verbose
  )

  # Save model if filename provided
  if (!is.null(modelfilename)) {
    save_model(xgbmodel, modelfilename, verbose)
  }

  xgbmodel
}

#' Convert input matrix to XGBoost DMatrix format
#' @param matrix Input matrix or DMatrix object
#' @return XGBoost DMatrix object
convert_to_dmatrix <- function(matrix) {
  if (inherits(matrix, "xgb.DMatrix")) {
    return(matrix)
  }

  xgboost::xgb.DMatrix(
    matrix$features,
    label = matrix$label
  )
}

#' Create watchlist for model training
#' @param dtrain Training DMatrix
#' @param validation_matrix Validation matrix or NULL
#' @return Named list of matrices for monitoring
create_watchlist <- function(dtrain, validation_matrix) {
  if (is.null(validation_matrix)) {
    return(list(train = dtrain))
  }

  deval <- convert_to_dmatrix(validation_matrix)
  list(train = dtrain, eval = deval)
}

#' Create parameter list for XGBoost training
#' @param ... Named parameters for XGBoost
#' @return List of parameters
create_params <- function(...) {
  params <- list(...)

  # Remove NULL parameters
  params[!vapply(params, is.null, logical(1))]
}

#' Train XGBoost model
#' @param params Model parameters
#' @param dtrain Training data
#' @param ... Additional parameters passed to xgb.train
#' @return Trained XGBoost model
train_model <- function(params, dtrain, ...) {
  xgboost::xgb.train(
    params = params,
    data = dtrain,
    ...
  )
}

#' Save trained model to file
#' @param model Trained XGBoost model
#' @param filename Output filename
#' @param verbose Whether to print progress messages
save_model <- function(model, filename, verbose = FALSE) {
  ff_cat("Saving model to", filename, verbose = verbose)

  feature_names <- model$feature_names
  rda_filename <- gsub("\\.model$", ".rda", filename)

  tryCatch(
    {
      suppressWarnings({
        saved <- xgboost::xgb.save(model, filename)
        if (saved) {
          save(feature_names, file = rda_filename)
        } else {
          ff_cat("Warning: Failed to save model", color = "yellow")
        }
      })
    },
    error = function(e) {
      ff_cat("Error saving model:", e$message, color = "red")
    }
  )
}

#' Validate input data
#' @param train_matrix Training data
#' @return NULL invisibly
validate_inputs <- function(train_matrix) {
  if (!hasvalue(train_matrix$label)) {
    stop(
      "The input data has no label. ",
      "This likely means no ground truth was available for this date or area."
    )
  }
  invisible(NULL)
}
