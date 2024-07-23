#' Train an XGBoost Model
#'
#' This function trains an XGBoost model with default parameters. The optimal parameters have been derived by running the algorithm worldwide on many tiles for over a year of training data.
#'
#' @param train_matrix The training matrix for XGBoost. should be of type xgb.Dmatrix
#' @param validation_matrix The matrix to run for the model for XGBoost. should be of type xgb.Dmatrix
#' @param nrounds Number of boosting rounds. Default is 200.
#' @param eta Learning rate. Default is 0.1.
#' @param max_depth Maximum tree depth. Default is 5.
#' @param subsample Subsample ratio of the training instances. Default is 0.75.
#' @param eval_metric Evaluation metric. Default is "aucpr". This can also be a custom evaluation metric.
#' @param maximize Default is NULL. Should be True or False in case a custom evaluation metric is used.
#' @param early_stopping_rounds Early stopping rounds. Default is 10.
#' @param gamma The gamma value, should be between 0 and 0.3. Determines level of pruning
#' @param min_child_weight The minimum weight of the child, determines how quickly the tree grows
#' @param verbose should the model run verbose. Default is FALSE.
#' @param xgb_model Previous build model to continue the training from. Could be an object of class "xgb.Booster", its raw data, or a file name. Default = NULL
#' @param modelfilename character where to save the model. should end with the extension model
#' @param features Vector with the feature names of the training dataset. Should be given when modelfilename is given so that the next time the model is loaded the model knows which features were used
#' @param objective Specify the learning task and the corresponding learning objective. Default is "binary:logistic".
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
#' Zillah Calle (2023)
#'
#' @keywords XGBoost data preparation
#' @rdname ff_train
#' @name ff_train


ff_train <- function(train_matrix, validation_matrix=NA, nrounds = 200, eta = 0.1, max_depth = 5,
                     subsample = 0.75, eval_metric = "aucpr", early_stopping_rounds = 10,num_class=NULL,
                     gamma=NULL, maximize=NULL, min_child_weight=1, verbose = F, xgb_model = NULL,
                     modelfilename = NULL, features = NULL, objective="binary:logistic") {

  if (!is.null(modelfilename)) {save(features,file = gsub("\\.model","\\.rda",modelfilename))}
  # Convert the matrix to a DMatrix object
  if (class(train_matrix) == "xgb.DMatrix") {dtrain <- train_matrix
  }else{dtrain <- xgboost::xgb.DMatrix(train_matrix$features, label = train_matrix$label)}

  # Set default parameters
  params <- list(
    objective = objective,
    eval_metric = eval_metric,
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    gamma = gamma,
    min_child_weight = min_child_weight
  )
  if (is.null(params$gamma)) {params$gamma <- NULL}
  if (any(is.na(validation_matrix))) {watchlist <- list(train = dtrain)
  }else{
    if (class(validation_matrix) == "xgb.DMatrix") {deval <- validation_matrix
    }else{deval <- xgboost::xgb.DMatrix(validation_matrix$features, label = validation_matrix$label)}
    watchlist <- list(train = dtrain,eval = deval)}

  # Train the XGBoost model
  if (!is.null(modelfilename)) {
    cat("saving model\n")
    model <- xgboost::xgb.train(
    params = params,
    nrounds = nrounds,
    data = dtrain,
    watchlist = watchlist,
    early_stopping_rounds = early_stopping_rounds
    , maximize = maximize
    , xgb_model = xgb_model
    , verbose = verbose
    , save_name = modelfilename
    , save_period = 0
    )

    }else{

    model <- xgboost::xgb.train(
    params = params
    , nrounds = nrounds
    , data = dtrain
    , watchlist = watchlist
    , early_stopping_rounds = early_stopping_rounds
    , maximize = maximize
    , xgb_model = xgb_model
    , verbose = verbose
    ,num_class = num_class)
}
  # Return the trained model
  return(model)
}

