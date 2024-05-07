library(caret)
library(doParallel)
library(ROCR)
library(e1071)
library(ggplot2)

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

  ctrl <- trainControl(method = "cv", number = 5, allowParallel = TRUE,
                       savePredictions = "final", classProbs = TRUE,
                       summaryFunction = multiClassSummary)


  cat("Training model...\n")
  old_time <- Sys.time()
  model <- train(x = x_train, y = y_train, method = "knn",
                 trControl = ctrl, tuneLength=10, metric='Sensitivity') #, preProc = manual_scale_function)

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
  print(confusionMatrix(table(y_validation, pred)))

  stopCluster(cl)
  registerDoSEQ()

  return(model)
}
