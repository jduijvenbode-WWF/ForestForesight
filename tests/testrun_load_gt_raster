
# Step 1: Set up the parameters
code_location <- "C:/Kodingan3/ForestForesight/"
ff_folder <- "C:/Kodingan3/FFdata/"
country_code <- "PER"
train_start <- "2023-06-01"
train_end <- "2023-12-31"
validation_start <- "2023-06-01"
validation_end <- "2023-12-01"
prediction_date <- "2024-01-01"

# Step 1.5: Make sure we are using R codes in our local, not ForestForesight package from Github
library("devtools")
load_all(code_location) # with this we are using the live R code, not the build package of ForestForesight


# Step 2: Create date ranges for training and validation
train_dates <- ForestForesight::daterange(train_start, train_end)
validation_dates <- ForestForesight::daterange(validation_start, validation_end)

# Step 3: Set up file paths for saving results
model_save_path <- paste(ff_folder, "peru_deforestation_model.model")
predictions_save_path <- paste(ff_folder, "peru_deforestation_prediction.tif")
accuracy_csv_path <- paste(ff_folder, "peru_deforestation_accuracy.csv")
importance_csv_path <- paste(ff_folder, "peru_feature_importance.csv")

# Step 4: Run the ff_run function
prediction_result <- ff_run(
  country = country_code,
  prediction_dates = prediction_date,
  ff_folder = ff_folder,
  train_dates = train_dates,
  validation_dates = validation_dates,
  save_path = model_save_path,
  save_path_predictions = predictions_save_path,
  accuracy_csv = accuracy_csv_path,
  importance_csv = importance_csv_path,
  verbose = TRUE,
  autoscale_sample = TRUE
)

# Step 5: Plot the prediction result
if (!is.na(prediction_result)) {
  plot(prediction_result, main = "Deforestation Prediction for Peru")
}

# Step 6: Print a summary of the results
cat("Prediction completed. Results saved to disk:\n")
cat("Model:", model_save_path, "\n")
cat("Prediction raster:", predictions_save_path, "\n")
cat("Accuracy CSV:", accuracy_csv_path, "\n")
cat("Feature importance CSV:", importance_csv_path, "\n")

# Optional: Load and print the first few lines of the accuracy CSV
if (file.exists(accuracy_csv_path)) {
  accuracy_data <- read.csv(accuracy_csv_path)
  cat("\nFirst few lines of accuracy data:\n")
  print(head(accuracy_data))
}
