library(caret)
library(doParallel)
library(ROCR)
library(e1071)
library(ggplot2)

# Custom summary function for F0.5 score and Accuracy
customSummary <- function(data, lev = NULL, model = NULL) {
  cm <- confusionMatrix(data$pred, data$obs, positive = as.character(lev[1]))
  precision <- cm$byClass['Precision']
  recall <- cm$byClass['Recall']
  f0.5_score <- (1 + 0.5^2) * (precision * recall) / ((0.5^2 * precision) + recall)
  accuracy <- cm$overall['Accuracy']
  c(Accuracy = accuracy, F0_5 = f0.5_score, Precision = precision, Recall = recall)
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

  ctrl <- trainControl(method = "cv", number = 3, summaryFunction = customSummary,
                       verboseIter = TRUE, sampling = "down", allowParallel = TRUE,
                       savePredictions = "final", classProbs = TRUE)

  cat("Training model...\n")
  old_time <- Sys.time()
  model <- train(x = x_train, y = y_train, method = "svmLinear",
                 trControl = ctrl, preProc = "scale")

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

  if (!is.null(model$finalModel@SVindex)) {
    sv_indices <- model$finalModel@SVindex
    support_vectors <- x_train[sv_indices, ]  # Assuming x_train is a dataframe
    feature_summary <- colSums(support_vectors)

    # Normalize to sum up to 100 for percentage calculation
    feature_importance <- feature_summary / sum(feature_summary) * 100

    # Bar plot with added text labels for percentages
    bp <- barplot(feature_importance, main = "Feature Importance from Support Vectors",
                  las = 2, col = 'blue', ylim = c(0, 100), width = 1)  # Extend ylim for label space

    # Add text labels on top of each bar
    text(x = bp, y = feature_importance + 1, label = sprintf("%.1f%%", feature_importance), pos = 3, cex = 0.8, col = "black")
    plot(text)
  } else {
    cat("No support vector indices available.\n")
  }

  # Generate predictions on the validation dataset
  validation_predictions <- predict(model, newdata = x_validation)

  # Generate the confusion matrix
  cm <- confusionMatrix(validation_predictions, y_validation)
  print(cm)

  return(model)
}
