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
    flavor = "xgboost",
    verbose = TRUE
) {
  current_run <- NULL
  run_successful <- FALSE

  # First check/setup Python environment
  env_check <- check_python_env()
  if (!env_check) {
    stop("Python environment setup failed. Cannot proceed with model logging.")
  }

  ff_cat("Python environment verified", verbose = verbose)

  # Connect to MLflow server
  tryCatch({
    mlflow::mlflow_set_tracking_uri("http://ec2-3-255-204-156.eu-west-1.compute.amazonaws.com:5000/")
  }, error = function(e) {
    stop("Cannot connect to MLflow tracking server: ", toString(e))
  })
  ff_cat("connected to server", verbose = verbose)

  # Validate region name
  regions <- get(data("countries", envir = environment()))$group
  if (!region_name %in% regions) {
    stop("region name is not in the regions defined by Forest Foresight")
  }

  # Ensure run is ended even if function exits with error
  tryCatch({
    # Set experiment
    mlflow::mlflow_set_experiment(experiment_name = region_name)
    ff_cat("switched to experiment", verbose = verbose)

    # Start run and register cleanup
    current_run <- mlflow::mlflow_start_run()
    on.exit({
      if (!run_successful) {
        tryCatch({
          # Delete run if it wasn't successful
          mlflow::mlflow_delete_run(current_run$run_id)
          ff_cat("deleted failed run", verbose = verbose)
        }, error = function(e) {
          warning("Failed to delete run: ", toString(e))
        })
      }
      # Always end the run
      tryCatch({
        mlflow::mlflow_end_run()
        ff_cat("ended run", verbose = verbose)
      }, error = function(e) {
        warning("Failed to end run: ", toString(e))
      })
    }, add = TRUE)

    # Log parameters
    for (param_name in names(params_list)) {
      mlflow::mlflow_log_param(param_name, params_list[[param_name]])
    }
    mlflow::mlflow_log_param("current_date", current_date)

    # Log metrics
    for (metric_name in names(metrics_list)) {
      mlflow::mlflow_log_metric(metric_name, metrics_list[[metric_name]])
    }
    ff_cat("logged parameters and metrics", verbose = verbose)

    # Log model if provided
    if (!missing(model) && !is.null(model)) {
      mlflow::mlflow_log_model(
        model = model,
        artifact_path = "model",
        flavor = flavor
      )
      ff_cat("saved model", verbose = verbose)
    }

    # Register model
    model_name <- paste(region_name, algorithm, sep = "_")

    tryCatch({
      mlflow::mlflow_create_registered_model(model_name)
      ff_cat("registered model", verbose = verbose)
    }, error = function(e) {
      # Ignore error - model probably already exists
    })

    # Create model version
    mlflow::mlflow_create_model_version(
      name = model_name,
      source = paste("runs:/", current_run$run_id, "/model", sep = ""),
      run_id = current_run$run_id,
      tags = list(
        "iteration" = method_iteration,
        "algorithm" = algorithm
      )
    )

    mlflow::mlflow_set_tag(key = "algorithm", value = method_iteration, run_id = current_run$run_id)
    ff_cat("created model version", verbose = verbose)

    # Mark run as successful
    run_successful <- TRUE

    return(list(
      success = TRUE,
      run_id = current_run$run_id,
      metrics = metrics_list
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = toString(e),
      run_id = if (!is.null(current_run)) current_run$run_id else NULL
    ))
  })
}
