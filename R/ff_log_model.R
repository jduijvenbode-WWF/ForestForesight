#' Log a Machine Learning Model to MLflow
#'
#' @description
#' Logs a machine learning model, its parameters, and metrics to MLflow. The function includes
#' validation checks for the environment, MLflow connection, and required Python packages.
#'
#' @param region_name Character string specifying the region name. Must be one of the regions
#'        defined by Forest Foresight.
#' @param algorithm Character string specifying the algorithm name (default: "xgboost").
#' @param method_iteration Character string describing the iteration or version of the method.
#' @param model The trained model object to be logged.
#' @param params_list List of model parameters to be logged.
#' @param metrics_list List of model metrics to be logged.
#' @param current_date Date object specifying the training date (default: Sys.Date()).
#' @param flavor Character string specifying the MLflow flavor to use (default: "xgboost").
#'
#' @return A list containing:
#'   \itemize{
#'     \item success: Logical indicating if the logging was successful
#'     \item run_id: Character string of the MLflow run ID if successful
#'     \item metrics: List of logged metrics if successful
#'     \item error: Error message if the logging failed
#'   }
#'
#' @examples
#' \dontrun{
#' params_list <- list(
#'   eta = 0.1,
#'   max_depth = 6,
#'   nrounds = 1000
#' )
#'
#' metrics_list <- list(
#'   auc = 0.85,
#'   precision = 0.76
#' )
#'
#' result <- ff_log_model(
#'   region_name = "Melanesia",
#'   method_iteration = "full model",
#'   model = model,
#'   params_list = params_list,
#'   metrics_list = metrics_list
#' )
#' }
#'
#' @import mlflow
#' @importFrom reticulate py_available py_module_available
#' @export
ff_log_model <- function(
    region_name,
    algorithm = "xgboost",
    method_iteration,
    model,
    params_list,
    metrics_list,
    current_date = Sys.Date(),
    flavor = "xgboost"
) {
  # Check if required Python packages are available
  required_packages <- c("mlflow")
  if (!reticulate::py_available()) {
    stop("Python is not available. Please ensure Python is installed and configured.")
  }

  missing_packages <- required_packages[!sapply(required_packages, reticulate::py_module_available)]
  if (length(missing_packages) > 0) {
    stop(sprintf("Required Python packages missing: %s", paste(missing_packages, collapse = ", ")))
  }

  # Try to connect to MLflow server
  tryCatch({
    mlflow::mlflow_set_tracking_uri("http://ec2-3-255-204-156.eu-west-1.compute.amazonaws.com:5000/")
    # Test connection by listing experiments
    mlflow::mlflow_list_experiments()
  }, error = function(e) {
    stop("Cannot connect to MLflow tracking server: ", toString(e))
  })

  # Validate region name
  regions <- get(data("countries", envir = environment()))$region
  if (!region_name %in% regions) {
    stop("region name is not in the regions defined by Forest Foresight")
  }

  # Check if experiment exists and set it
  existing_experiments <- mlflow::mlflow_list_experiments()
  experiment_exists <- any(existing_experiments$name == region_name)

  if (experiment_exists) {
    experiment <- mlflow::mlflow_get_experiment(name = region_name)
    experiment_id <- experiment$experiment_id
  } else {
    experiment_id <- mlflow::mlflow_create_experiment(region_name)
  }

  mlflow::mlflow_set_experiment(experiment_id = experiment_id)

  # Start mlflow run with tags
  mlflow::mlflow_start_run(
    tags = list(
      "region" = region_name,
      "algorithm" = algorithm,
      "iteration" = method_iteration,
      "training_date" = as.character(current_date)
    )
  )

  tryCatch({
    # Log all parameters one by one
    for (param_name in names(params_list)) {
      mlflow::mlflow_log_param(param_name, params_list[[param_name]])
    }

    # Log current_date as additional parameter
    mlflow::mlflow_log_param("current_date", current_date)

    # Log all metrics one by one
    for (metric_name in names(metrics_list)) {
      mlflow::mlflow_log_metric(metric_name, metrics_list[[metric_name]])
    }

    # Log model if provided
    if (!missing(model) && !is.null(model)) {
      mlflow::mlflow_log_model(
        model = model,
        artifact_path = "model",
        flavor = flavor
      )
    }

    # Register model with version
    model_name <- paste(region_name, algorithm, sep = "_")
    current_run <- mlflow::mlflow_get_run()
    mlflow::mlflow_create_model_version(
      name = model_name,
      source = paste("runs:/", current_run$run_id, "/model", sep = ""),
      run_id = current_run$run_id,
      tags = list(
        "iteration" = method_iteration
      )
    )

    return(list(
      success = TRUE,
      run_id = current_run$run_id,
      metrics = metrics_list
    ))

  }, error = function(e) {
    # Log error
    mlflow::mlflow_log_metric("training_failed", 1)
    mlflow::mlflow_log_param("error_message", toString(e))
    return(list(
      success = FALSE,
      error = toString(e)
    ))
  }, finally = {
    mlflow::mlflow_end_run()
  })
}
