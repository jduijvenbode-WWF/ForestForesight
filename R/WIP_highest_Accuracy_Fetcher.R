get_f0.5_scores <- function(experiment_name) {
  get_score <- function(metrics){
    metrics <- data.frame(metrics)
    score <- metrics[metrics$key=="F0.5",'value']
    return(score)
  }
  mlflow::mlflow_set_tracking_uri("http://ec2-3-255-204-156.eu-west-1.compute.amazonaws.com:5000/")
  # Get experiment by name
  experiment <- mlflow::mlflow_get_experiment(name = experiment_name)

  if (is.null(experiment)) {
    stop("Experiment not found: ", experiment_name)
  }

  # Get all runs for this experiment
  runs <- mlflow::mlflow_search_runs(
    experiment_ids = experiment$experiment_id,
    order_by = "start_time DESC"
  )
  F0.5 = sapply(runs$metrics, function(x) data.frame(x))
  # Extract F0.5 scores if they exist
  f0.5_scores <- data.frame(
    run_id = runs$run_id,
    status = runs$status,
    F0.5 = sapply(runs$metrics,function(x) get_score(x)), # Assuming the metric is stored as 'f0.5'
    start_time = runs$start_time
  )

  return(f0.5_scores)
}

# Usage example:
groups=unique(a$group)
for(group in groups){
scores <- get_f0.5_scores(group)
mlflow::mlflow_set_tag(key = "algorithm",value="baseline",run_id = scores$run_id)
}
print(scores)
