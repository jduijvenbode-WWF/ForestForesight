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
  regions <- get(data("countries",envir = environment()))$region
  if (!region_name %in% regions) {
    stop("region name is not in the regions defined by Forest Foresight")
  }
  # Set experiment for region
  mlflow::mlflow_set_experiment(region_name)

  # Start mlflow run with tags
  mlflow::mlflow_start_run(
    tags = list(
      "region" = region_name,
      "algorithm" = algorithm,
      "iteration" = method_iteration
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

    # Log model
    if (hasvalue(model)) {
      mlflow::mlflow_log_model(
        model = model,
        artifact_path = "model",
        flavor = flavor
      )
    }

    # Register model with version
    model_name <- paste(region_name, tag_name, sep = "_")
    current_run <- mlflow::mlflow_get_run()

    mlflow::mlflow_create_model_version(
      name = model_name,
      source = paste("runs:/", current_run$run_id, "/model", sep = ""),
      run_id = current_run$run_id,
      tags = list(
        "iteration" = method_iteration,
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

# Example usage:
# params_list <- list(
#   eta = 0.1,
#   gamma = 0,
#   subsample = 0.8,
#   max_depth = 6,
#   lambda = 1,
#   min_child_weight = 1,
#   objective = "binary:logistic",
#   eval_metric = "auc",
#   nrounds = 1000,
#   stopping_rounds = 50,
#   train_start_date = as.Date("2023-01-01"),
#   train_end_date = as.Date("2023-12-31"),
#   validation_start_date = as.Date("2024-01-01"),
#   validation_end_date = as.Date("2024-02-29"),
#   data_sample_fraction = 1.0,
#   validation_sample_fraction = 1.0,
#   threshold = "dynamic"
# )
#
# metrics_list <- list(
#   auc = 0.85,
#   precision = 0.76,
#   recall = 0.89,
#   f0.5 = 0.78
# )
#
# result <- ff_log_model(
#   region_name = "Melanesia",
#   tag_name = "xgb-model",
#   method_iteration = "full model, standard threshold",
#   model = your_model,
#   params_list = params_list,
#   metrics_list = metrics_list
# )
