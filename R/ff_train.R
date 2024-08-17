#' Train an XGBoost Model for ForestForesight
#'
#' This function trains an XGBoost model with optimized default parameters derived from worldwide data analysis.
#'
#' @param train_matrix An xgb.DMatrix object or a list containing 'features' and 'label' for training.
#' @param validation_matrix An xgb.DMatrix object or a list containing 'features' and 'label' for validation. Default is NA.
#' @param nrounds Number of boosting rounds. Default is 200.
#' @param eta Learning rate. Default is 0.1.
#' @param max_depth Maximum tree depth. Default is 5.
#' @param subsample Subsample ratio of the training instances. Default is 0.75.
#' @param eval_metric Evaluation metric. Default is "aucpr". Can be a custom evaluation metric.
#' @param early_stopping_rounds Number of rounds for early stopping. Default is 10.
#' @param gamma Minimum loss reduction required to make a further partition. Default is NULL.
#' @param maximize Boolean indicating whether to maximize the evaluation metric. Required for custom metrics.
#' @param min_child_weight Minimum sum of instance weight needed in a child. Default is 1.
#' @param verbose Boolean indicating whether to display training progress. Default is FALSE.
#' @param xgb_model Previously trained model to continue training from. Can be an "xgb.Booster" object, raw data, or a file name. Default is NULL.
#' @param modelfilename String specifying where to save the model. Should end with ".model" extension.
#' @param features Vector of feature names used in the training dataset. Required when modelfilename is provided.
#' @param objective Learning objective. Default is "binary:logistic".
#'
#' @return A trained XGBoost model (xgb.Booster object).
#'
#' @examples
#' \dontrun{
#' # Prepare your data
#' train_data <- list(features = matrix(runif(1000), ncol = 10),
#'                    label = sample(0:1, 100, replace = TRUE))
#'
#' # Train the model
#' model <- ff_train(train_matrix = train_data,
#'                   nrounds = 100,
#'                   eta = 0.05,
#'                   max_depth = 6,
#'                   modelfilename = "forest_model.model",
#'                   features = colnames(train_data$features))
#' }
#'
#' @import xgboost
#' @export
#'
#' @references
#' Jonas van Duijvenbode (2023)
#' Zillah Calle (2023)
#'
#' @seealso
#' \code{\link{ff_prep}} for preparing data for this function
#' \code{\link{ff_predict}} for making predictions using the trained model
#'
#' @keywords machine-learning xgboost forestry


ff_train <- function(train_matrix, validation_matrix=NA, nrounds = 200, eta = 0.1, max_depth = 5,
                     subsample = 0.75, eval_metric = "aucpr", early_stopping_rounds = 10,
                     gamma=NULL, maximize=NULL, min_child_weight=1, verbose = F, xgb_model = NULL,
                     modelfilename = NULL, features = NULL, objective="binary:logistic") {


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
    if (verbose) {cat("starting training\n")}
    model <- xgboost::xgb.train(
    params = params,
    nrounds = nrounds,
    data = dtrain,
    watchlist = watchlist,
    early_stopping_rounds = early_stopping_rounds
    , maximize = maximize
    , xgb_model = xgb_model
    , verbose = verbose
)

    if (!is.null(modelfilename)) {
    if(verbose) {
      cat("saving model to",modelfilename,"\n")
    }
    suppressWarnings({result<-xgboost::xgb.save(model,modelfilename)})
    if(result){save(features,file = gsub("\\.model","\\.rda",modelfilename))}else{warning("model is not saved")}
    }
  # Return the trained model
  return(model)
}

