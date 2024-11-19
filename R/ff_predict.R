#' Train an XGBoost Model for ForestForesight
#'
#' This function trains an XGBoost model with optimized default parameters derived from worldwide data analysis.
#'
#' @param train_matrix An xgb.DMatrix object or a list containing 'features' and 'label' for training.
#' @param validation_matrix An xgb.DMatrix object or a list containing 'features' and 'label' for validation.
#' Default is NA.
#' @param nrounds Number of boosting rounds. Default is 200.
#' @param eta Learning rate. Default is 0.1.
#' @param max_depth Maximum tree depth. Default is 5.
#' @param subsample Subsample ratio of the training instances. Default is 0.75.
#' @param eval_metric Evaluation metric. Default is "aucpr". Can be a custom evaluation metric.
#' @param early_stopping_rounds Number of rounds for early stopping. Default is 10.
#' @param num_class Number of classes for multi-class classification. Default is NULL.
#' @param gamma Minimum loss reduction required to make a further partition. Default is NULL.
#' @param maximize Boolean indicating whether to maximize the evaluation metric. Required for custom metrics.
#' @param min_child_weight Minimum sum of instance weight needed in a child. Default is 1.
#' @param verbose Boolean indicating whether to display training progress. Default is FALSE.
#' @param xgb_model Previously trained model to continue training from.
#' Can be an "xgb.Booster" object, raw data, or a file name. Default is NULL.
#' @param modelfilename String specifying where to save the model. Should end with ".model" extension.
#' @param features Vector of feature names used in the training dataset. Required when modelfilename is provided.
#' @param objective Learning objective. Default is "binary:logistic".
#'
#' @return A trained XGBoost model (xgb.Booster object).
#'
#' @examples
#' \dontrun{
#' # Prepare your data
#' train_data <- list(
#'   features = matrix(runif(1000), ncol = 10),
#'   label = sample(0:1, 100, replace = TRUE)
#' )
#'
#' # Train the model
#' model <- ff_train(
#'   train_matrix = train_data,
#'   nrounds = 100,
#'   eta = 0.05,
#'   max_depth = 6,
#'   modelfilename = "forest_model.model",
#'   features = colnames(train_data$features)
#' )
#' }
#'
#' @import xgboost
#' @export
#'
#' @references
#' Jonas van Duijvenbode (2023)
#' Zillah Calle (2023)
#' Badcoder MC ForLoopFace
#'
#' @seealso
#' \code{\link{ff_prep}} for preparing data for this function
#' \code{\link{ff_predict}} for making predictions using the trained model
#'
#' @keywords machine-learning xgboost forestry


ff_predict <- function(model, test_matrix, threshold = 0.5, groundtruth = NA, indices = NA,
                       templateraster = NA, verbose = FALSE, certainty = FALSE) {
  mdlftrs <- model_loader(model, test_matrix, threshold, groundtruth, indices, templateraster, verbose, certainty) #load features from model

  # prep the features
  #convertmodel
  prepFeatures <- FeaturePrepper(model, test_matrix, threshold, groundtruth, indices, templateraster, verbose, certainty, mdlftrs)
  if (!is.na(test_matrix$label[1])) {
    test_matrix <- xgboost::xgb.DMatrix(test_matrix$features, label = test_matrix$label)
  } else {
    test_matrix <- xgboost::xgb.DMatrix(test_matrix$features, label = test_matrix$label)
  }

  ff_cat("calculating predictions", verbose = verbose)

  predictions <- predict(model, test_matrix)
  scores <- calcScores(predictions, threshold)

  if (model != NA) {
  if (class(templateraster) == "SpatRaster") {
    templateraster[] <- 0
    if (length(indices) > 1) {
      ff_cat("filling raster", verbose = verbose)

      if (!certainty) {
        templateraster[indices] <- predictions > threshold
      } else {
        templateraster[indices] <- predictions
      }
    } else {
      if (terra::ncell(templateraster) == length(predictions)) {
        cat("filling raster", verbose = verbose)

        if (!certainty) {
          templateraster[] <- predictions > threshold
        } else {
          templateraster[] <- predictions
        }
      } else {
        templateraster <- NA
      }
    }
  } else {
    templateraster <- NA
  }
  if (!is.na(f05)) {
    ff_cat("F0.5:", f05, "precision:", precision, "recall:", recall, verbose = verbose)
  }
  }

  save_toFile(scores$f05)
  return(list(
    threshold = threshold, "precision" = precision, "recall" = recall, "F0.5" = f05,
    "predicted_raster" = templateraster, "predictions" = predictions
  ))
}

model_loader <- function(model, test_matrix, threshold, groundtruth, indices, templateraster, verbose, certainty) {
  if (class(model) == "character") {
    modelfilename <- model
    if (file.exists(model)) {
      if (!test_feature_model_match(model)) { stop("number of features in model and corresponding feature names RDA file do not match")}
      model <- xgboost::xgb.load(model)
      if (file.exists(gsub("\\.model", "\\.rda", modelfilename))) {
        model_features <- get(load(gsub("\\.model", "\\.rda", modelfilename)))
        attr(model, "feature_names") <- model_features
      }
    }
  } else {
    model_features <- model$feature_names
  }
  return(model_features)
}

FeaturePrepper <- function(model, test_matrix, threshold, groundtruth, indices, templateraster, verbose, certainty, mdlftrs) {
  if (!is.null(model_features)) {
    test_features <- colnames(test_matrix$features)
    # Check for features in the test matrix not present in the model
    extra_features <- setdiff(test_features, mdlftrs)
    # If there are extra features, remove them from the test matrix
    if (length(extra_features) > 0) {
      ff_cat(
        "Removing extra features from the test matrix:",
        paste(extra_features, collapse = ", "),
        color = "yellow"
      )
      test_matrix$features <- test_matrix$features[, setdiff(test_features, extra_features), drop = FALSE]
    }
  }
  return(test_matrix)
}


#TODO; Still gotta test whether this works.
calcScores <- function(prediction, threshold) {
  if (!is.na(groundtruth[1])) {
    if (class(groundtruth) == "SpatRaster") {
      groundtruth <- as.numeric(as.matrix(groundtruth))
    }

    cat("calculationg scores", verbose = verbose)

    precision <- c()
    recall <- c()
    f05 <- c()
    for (thresh in threshold) {
      res <- table(2 * (predictions > thresh) + groundtruth)
      prec <- as.numeric(res[4] / (res[4] + res[3]))
      rec <- as.numeric(res[4] / (res[4] + res[2]))
      precision <- c(precision, prec)
      recall <- c(recall, rec)
      f05 <- c(f05, 1.25 * prec * rec / (0.25 * prec + rec))
    }
  } else {
    precision <- recall <- f05 <- NA
  }
}
save_toFile <- function(f05) {
  write.csv(f05, "C:\home\users\documents\f05forzimbabwe.csv")
}
