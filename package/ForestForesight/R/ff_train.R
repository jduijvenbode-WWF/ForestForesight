#' Train an XGBoost Model
#'
#' This function trains an XGBoost model with default parameters.
#'
#' @param train_matrix The training matrix for XGBoost. should be of type xgb.Dmatrix
#' @param validation_matrix The matrix to run for the model for XGBoost. should be of type xgb.Dmatrix
#' @param nrounds Number of boosting rounds. Default is 200.
#' @param eta Learning rate. Default is 0.1.
#' @param max_depth Maximum tree depth. Default is 5.
#' @param subsample Subsample ratio of the training instances. Default is 0.75.
#' @param eval_metric Evaluation metric. Default is "aucpr".
#' @param early_stopping_rounds Early stopping rounds. Default is 10.
#' @param verbose should the model run verbose. Default is FALSE.
#'
#' @return Trained XGBoost model.
#'
#' @examples
#' # Example usage:
#' train_matrix <- matrix(c(1, 2, 3, 4), ncol = 2)
#' model <- train_xgboost(train_matrix)
#'
#' @import xgboost
#' @export
#'
#' @references
#' Jonas van Duijvenbode (2023)
#'
#' @keywords XGBoost data preparation
#' @rdname ff_train
#' @name ff_train


ff_train <- function(train_matrix, validation_matrix=NA, nrounds = 200, eta = 0.1, max_depth = 5,
                          subsample = 0.75, eval_metric = "aucpr", early_stopping_rounds = 10,verbose=F) {

  # Convert the matrix to a DMatrix object
  dtrain <- train_matrix
  deval= validation_matrix

  # Set default parameters
  params <- list(
    objective = "binary:logistic",
    eval_metric = eval_metric,
    eta = eta,
    max_depth = max_depth,
    subsample = subsample
  )
  if(is.na(validation_matrix)){
    watchlist=list(train = dtrain)
  }else{watchlist=list(train = dtrain,eval= deval)}
  # Train the XGBoost model
  model <- xgboost::xgb.train(
    params = params,
    nrounds=nrounds,
    data = dtrain,
    watchlist = watchlist,
    early_stopping_rounds = early_stopping_rounds,verbose=verbose
  )

  # Return the trained model
  return(model)
}
