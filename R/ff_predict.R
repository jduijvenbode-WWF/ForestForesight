#' Make Predictions Using a ForestForesight Model
#'
#' This function makes predictions using a trained ForestForesight model and optionally
#' evaluates the model performance if ground truth data is provided.
#'
#' @param model An xgb.Booster object or path to a saved xgboost model file. Comes out of ff_train
#' @param test_matrix A list containing 'features' matrix for predictions. Comes out of ff_prep
#' @param thresholds Numeric vector of classification thresholds. Default is 0.5.
#' @param groundtruth Optional vector or SpatRaster of actual values for model evaluation.
#' @param indices Optional vector of indices for filling a raster. Default is NA.
#' @param templateraster Optional SpatRaster template for spatial predictions. Default is NA.
#' @param verbose Boolean indicating whether to display progress messages. Default is FALSE.
#' @param certainty Boolean indicating whether to return raw probabilities instead of binary predictions.
#' Default is FALSE.
#'
#' @return A list containing:
#'   \item{threshold}{The threshold(s) used for classification}
#'   \item{precision_vector}{Precision values for each threshold}
#'   \item{recall_vector}{Recall values for each threshold}
#'   \item{F0.5}{F0.5 scores for each threshold}
#'   \item{predicted_raster}{SpatRaster of predictions if templateraster was provided}
#'   \item{predictions}{Raw prediction values}
#'
#' @examples
#' \dontrun{
#' # Load a saved model and make predictions
#' predictions <- ff_predict(
#'   model = "forest_model.model",
#'   test_matrix = test_data,
#'   thresholds = c(0.3, 0.5, 0.7)
#' )
#'
#' # Make predictions with spatial output
#' predictions <- ff_predict(
#'   model = model_object,
#'   test_matrix = test_data,
#'   templateraster = my_raster,
#'   certainty = TRUE
#' )
#' }
#'
#' @import xgboost terra
#' @export

ff_predict <- function(model, test_matrix, thresholds = 0.5, groundtruth = NA, indices = NA,
                       templateraster = NA, verbose = FALSE, certainty = FALSE) {
  # Load and validate model
  model <- load_model(model)

  # Handle feature matching
  if (!is.null(model$feature_names)) {
    test_features <- colnames(test_matrix$features)
    extra_features <- setdiff(test_features, model$feature_names)

    if (length(extra_features) > 0) {
      ff_cat(
        "Removing extra features from the test matrix:",
        paste(extra_features, collapse = ", "),
        color = "yellow"
      )
      test_matrix$features <- test_matrix$features[, setdiff(test_features, extra_features),
        drop = FALSE
      ]
    }
  }

  # Convert to xgb_matrix
  xgb_matrix <- if (hasvalue(test_matrix$label)) {
    xgboost::xgb.DMatrix(test_matrix$features, label = test_matrix$label)
  } else {
    xgboost::xgb.DMatrix(test_matrix$features)
  }

  ff_cat("calculating predictions", verbose = verbose)
  predictions <- stats::predict(model, xgb_matrix)

  # Calculate metrics if groundtruth is provided
  metrics <- if (!is.na(groundtruth[1])) {
    ff_cat("calculating scores", verbose = verbose)
    calculate_metrics(predictions, groundtruth, thresholds)
  } else {
    list(precision = NA, recall = NA, f05 = NA)
  }

  # Handle spatial predictions
  predicted_raster <- if (inherits(templateraster, "SpatRaster")) {
    fill_raster(templateraster, predictions, indices, certainty, thresholds, verbose)
  } else {
    NA
  }

  if (hasvalue(metrics$f05)) {
    ff_cat("F0.5:", metrics$f05,
      "precision:", metrics$precision,
      "recall:", metrics$recall,
      verbose = verbose
    )
  }

  return(list(
    threshold = thresholds,
    precision_vector = metrics$precision,
    recall_vector = metrics$recall,
    "F0.5" = metrics$f05,
    predicted_raster = predicted_raster,
    predictions = predictions
  ))
}

# Helper function to load and validate model
load_model <- function(model) {
  if (is.character(model)) {
    model_filename <- model
    if (!file.exists(model)) {
      stop("Model file does not exist")
    }
    if (!test_feature_model_match(model)) {
      stop("Number of features in model and corresponding feature names RDA file do not match")
    }

    model <- xgboost::xgb.load(model)
    rda_file <- gsub("\\.model", "\\.rda", model_filename)

    if (file.exists(rda_file)) {
      model_features <- get(load(rda_file))
      attr(model, "feature_names") <- model_features
    }
  }
  return(model)
}

# Helper function to calculate performance metrics
calculate_metrics <- function(predictions, groundtruth, thresholds) {
  if (class(groundtruth) == "SpatRaster") {
    groundtruth <- as.numeric(as.matrix(groundtruth))
  }

  precision_vector <- recall_vector <- f05 <- numeric(length(thresholds))

  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    crosstable <- table(2 * (predictions > threshold) + groundtruth)
    precision_vector[i] <- as.numeric(crosstable[4] / (crosstable[4] + crosstable[3]))
    recall_vector[i] <- as.numeric(crosstable[4] / (crosstable[4] + crosstable[2]))
    f05[i] <- 1.25 * precision_vector[i] * recall_vector[i] /
      (0.25 * precision_vector[i] + recall_vector[i])
  }

  return(list(
    precision = precision_vector,
    recall = recall_vector,
    f05 = f05
  ))
}

# Helper function to fill raster with predictions
fill_raster <- function(templateraster, predictions, indices, certainty, thresholds, verbose) {
  result <- templateraster
  result[] <- 0

  if (length(indices) > 1) {
    ff_cat("filling raster", verbose = verbose)
    result[indices] <- if (!certainty) {
      predictions > thresholds
    } else {
      predictions
    }
    return(result)
  } else if (terra::ncell(templateraster) == length(predictions)) {
    ff_cat("filling raster", verbose = verbose)
    result[] <- if (!certainty) {
      predictions > thresholds
    } else {
      predictions
    }
    return(result)
  } else {
    return(NA)
  }
}
