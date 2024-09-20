#' Bayesian Optimization for XGBoost Hyperparameters with ForestForesight
#'
#' This function uses Bayesian optimization to tune the hyperparameters of an XGBoost model for a given dataset.
#' It leverages the `ForestForesight` package for data preparation and model training.
#'
#' @param ff_folder `character` The path to the folder containing the ForestForesight data.
#' @param shape `character` (optional) Shape parameter for data preparation, if required by `ff_prep`.
#' @param country `character` (optional) Country parameter for data preparation, if required by `ff_prep`.
#' @param train_dates `character` Dates for training data, formatted as required by `ff_prep`.
#' @param val_dates `character` Dates for validation data, formatted as required by `ff_prep`.
#' @param bounds `list` List of hyperparameter bounds for Bayesian optimization. Default values:
#' \itemize{
#'   \item `eta`: Learning rate, range [0.01, 0.3]
#'   \item `nrounds`: Number of boosting rounds, range [50, 500]
#'   \item `max_depth`: Maximum depth of the trees, range [3, 10]
#'   \item `subsample`: Subsample ratio of the training instances, range [0.5, 1]
#'   \item `gamma`: Minimum loss reduction to make a further partition, range [0.01, 1]
#'   \item `min_child_weight`: Minimum sum of instance weight needed in a child, range [1, 10]
#' }
#' @param init_points `numeric` Number of initial random searches before starting Bayesian optimization. Default is 5.
#' @param n_iter `numeric` Number of iterations for the Bayesian optimization. Default is 25.
#' @param acq `character` Acquisition function to be used by the optimizer. Default is "ucb" (Upper Confidence Bound).
#' @param kappa `numeric` Exploration-exploitation parameter for the UCB acquisition function. Default is 2.576.
#' @param ff_prep_params `list` Additional parameters to be passed to the `ff_prep` function for data preparation.
#' @param ff_train_params `list` Additional parameters to be passed to the `ff_train` function for model training.
#' @param verbose `logical` Whether to print progress messages. Default is `TRUE`.
#'
#' @return A list containing:
#' \itemize{
#'   \item `best_params`: The best hyperparameters found by the optimizer.
#'   \item `final_model`: The final trained model using the best hyperparameters.
#'   \item `optimization_result`: Full result of the Bayesian optimization process.
#' }
#'
#' @details
#' The function first prepares the training and validation datasets using the `ff_prep` function. It then defines
#' an objective function, `xgb_cv_bayes`, which trains the model with given hyperparameters and returns the best
#' score based on the validation set's AUCPR (Area Under the Precision-Recall Curve).
#'
#' The `BayesianOptimization` function from the `rBayesianOptimization` package is used to find the optimal set of
#' hyperparameters within the specified bounds. Once the best parameters are found, the model is trained on the
#' entire training dataset, and the final model is returned.
#'
#' @import rBayesianOptimization
#' @import xgboost
#' @import ForestForesight
#'
#' @export

ff_optimizer <- function(ff_folder, shape = NULL, country = NULL,
                                  train_dates, val_dates,
                                  #prediction_date,
                                  bounds = list(eta = c(0.01, 0.3),
                                                nrounds = c(50, 500),
                                                max_depth = c(3, 10),
                                                subsample = c(0.5, 1),
                                                gamma = c(0.01,1),
                                                min_child_weight = c(1,10)),
                                  init_points = 5, n_iter = 25, acq = "ucb", kappa = 2.576,
                                  ff_prep_params = list(), ff_train_params = list(),
                                  verbose = TRUE) {

  # Prepare data using ff_prep
  prep_params <- c(list(datafolder = ff_folder, shape = shape, country = country,
                        dates = train_dates), ff_prep_params)
  val_params <- c(list(datafolder = ff_folder, shape = shape, country = country,
                                  dates = val_dates), ff_prep_params)
  train_data <- do.call(ff_prep, prep_params)
  val_data <- do.call(ff_prep, val_params)

  # Objective function for Bayesian optimization
  xgb_cv_bayes <- function(eta, nrounds, max_depth, subsample,gamma,min_child_weight) {
    # Prepare the parameter list for ff_train
    train_params <- c(list(train_matrix = train_data$data_matrix,
                           validation_matrix = val_data$data_matrix,
                           nrounds = as.integer(round(nrounds)),
                           eta = eta,
                           max_depth = as.integer(round(max_depth)),
                           subsample = subsample,gamma=gamma,min_child_weight = min_child_weight,
                           verbose = FALSE), ff_train_params)

    # Train the model using ff_train
    model <- do.call(ff_train, train_params)

    # Get the best score (assuming AUCPR is used)
    best_score <- max(model$evaluation_log$eval_aucpr)

    return(list(Score = best_score, Pred = 0))
  }

  # Run Bayesian optimization
  opt_result <- BayesianOptimization(xgb_cv_bayes,
                                     bounds = bounds,
                                     init_points = init_points,
                                     n_iter = n_iter,
                                     acq = acq,
                                     kappa = kappa,
                                     verbose = verbose)

  # Extract best parameters
  best_params <- list(
    eta = opt_result$Best_Par["eta"],
    nrounds = as.integer(round(opt_result$Best_Par["nrounds"])),
    max_depth = as.integer(round(opt_result$Best_Par["max_depth"])),
    subsample = opt_result$Best_Par["subsample"],
    gamma = opt_result$Best_Par["gamma"],
    min_child_weight = opt_result$Best_Par["min_child_weight"]
  )

  # Train final model with best parameters
  final_train_params <- c(list(train_matrix = train_data$data_matrix,
                               validation_matrix = train_data$validation_matrix),
                          best_params,
                          ff_train_params)
  final_model <- do.call(ff_train, final_train_params)

  return(list(best_params = best_params,
              final_model = final_model,
              optimization_result = opt_result))
}

