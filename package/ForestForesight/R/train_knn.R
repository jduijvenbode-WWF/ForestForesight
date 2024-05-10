library(caret)
library(doParallel)
library(ROCR)
library(e1071)
library(ggplot2)

customPrSummary <- function(data, lev = NULL, model = NULL) {
  # Assuming binary classification with levels
  if (is.null(lev)) {
    lev <- levels(data$obs)
  }
  # Setting the positive class
  reference <- lev[2] # put the focus on deforested

  # Calculate Precision and Recall
  precision <- caret::posPredValue(data$pred, data$obs, positive = reference)
  recall <- caret::sensitivity(data$pred, data$obs, positive = reference)

  # Calculate F0.5 Score
  F05 <- (1 + 0.5^2) * (precision * recall) / ((0.5^2 * precision) + recall)

  # Return a named list including F0.5
  out <- c(Precision = precision, Recall = recall, F0_5 = F05)
  out[is.na(out)] <- 0  # Handling NaN or NA values
  names(out) <- c("Precision", "Recall", "F0.5")
  return(out)
}

train_model <- function(train_data, valid_data, save_model = FALSE) {
  max_cores <- parallel::detectCores(logical = TRUE)
  cl <- makeCluster(max_cores)
  registerDoParallel(cl)
  cat(sprintf("Using %d CPU cores for parallel processing\n", max_cores))

  x_train <- train_data$data_matrix$features
  y_train <- factor(train_data$data_matrix$label, levels = c(0, 1))
  levels(y_train) <- c("not_deforested", "deforested")

  x_validation <- valid_data$validation_matrix$features
  y_validation <- factor(valid_data$validation_matrix$label, levels = c(0, 1))
  levels(y_validation) <- c("not_deforested", "deforested")

  # Preprocess data
  preProcValues <- preProcess(x_train, method=c("center", "scale"))
  x_train <- predict(preProcValues, x_train)
  x_validation <- predict(preProcValues, x_validation)

  ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, allowParallel = TRUE,
                       savePredictions = "final", classProbs = TRUE,
                       summaryFunction = customPrSummary, verboseIter = TRUE,
                       )

  cat("Training model...\n")
  old_time <- Sys.time()
  model <- train(x = x_train, y = y_train, method = "knn",
                 trControl = ctrl, tuneGrid=expand.grid(k=c(7)), metric="F0.5")

  cat("Model trained\n")
  current_time <- Sys.time()
  train_time <- as.numeric(difftime(current_time, old_time, units = "secs"))

  # Convert to hours, minutes, and seconds
  hours <- floor(train_time / 3600)
  minutes <- floor((train_time %% 3600) / 60)
  seconds <- (train_time %% 3600) %% 60

  # Output the results
  sprintf("Training time was Hours: %d, Minutes: %d, Seconds: %f", hours, minutes, seconds)

  if (save_model != FALSE) {
    model_filename <- paste0(save_model, ".rds")
    saveRDS(model, file = model_filename)
    cat(sprintf("Model saved as %s\n", model_filename))
  }

  # Generate predictions on the validation dataset
  pred <- predict(model, newdata = x_validation)

    # Get values out of the confusion matrix
  print(confusionMatrix(y_validation, pred, positive = "deforested"))

  stopCluster(cl)
  registerDoSEQ()
  summary(model)
  return(model)
}
