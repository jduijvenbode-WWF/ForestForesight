#' Train a Model and Predict Deforestation on Raster Data
#'
#' This function trains an XGBoost model using historical data and then predicts deforestation
#' on raster data for a specified date and area.
#'
#' @param shape SpatVector object representing the area of interest. Either `shape` or `country` must be provided.
#' @param country ISO3 country code. Either `shape` or `country` must be provided.
#' @param prediction_dates Dates for prediction in "YYYY-MM-DD" format.
#' @param ff_folder Directory containing the input data.
#' @param train_dates The dates for which you want to create the training data.
#' dates should be a vector with the format YYYY-MM-DD with DD being 01
#' @param validation_dates The dates for which you want to create a distinct validation matrix
#' (if any, select validation = T to make a subsample of the same dates as the training data).
#' @param model_save_path Path to save the trained model (with extension ".model"). Default is NULL.
#' @param predictions_save_path Path to save the predictions (with extension ".tif"). Default is NULL.
#' @param pretrained_model_path Pre-trained model object or path to saved model.
#' If NULL, a new model will be trained. Default is NULL.
#' @param ff_prep_parameters List of parameters for data preprocessing. See `ff_prep` function for details.
#' @param ff_train_parameters List of parameters for model training. See `ff_train` function for details.
#' @param certainty_threshold Probability certainty_threshold for binary classification. Default is 0.5.
#' @param filter_features Feature dataset used for pre-filtering for training.
#' Default is initialforestcover. Can be more than one
#' @param filter_conditions The condition with value that is used to filter the training dataset based on mask features.
#'  Default is ">0". Can be more than one
#' @param accuracy_output_path Path to save accuracy metrics in CSV format. Default is NA (no CSV output).
#' @param importance_output_path Path to save feature importance metrics in CSV format. Default is NA (no CSV output).
#' @param verbose Logical; whether to display progress messages. Default is TRUE.
#' @param autoscale_sample Logical; Whether to automatically scale the number of samples
#' based on the size of the area and the length of the training period.
#' @param validation Logical; Whether to add a validation matrix based on the training data,
#' which is set at 0.25 of the training matrix. Should not be set if validation_dates is not NULL.
#'
#' @return A SpatRaster object containing the predicted deforestation probabilities.
#' If multiple prediction dates are given you receive a rasterstack with a raster per date
#'
#' @examples
#' \dontrun{
#' # Predict deforestation for a country
#' prediction <- ff_run(
#'   country = "BRA",
#'   prediction_date = "2024-01-01",
#'   ff_folder = "path/to/forestforesight/data",
#'   train_dates = ForestForesight::daterange("2022-01-01", "2023-12-31"),
#'   model_save_path = "path/to/save/model.model",
#'   accuracy_output_path = "path/to/save/accuracy.csv"
#' )
#'
#' # Plot the prediction
#' plot(prediction)
#' }
#'
#' @importFrom lubridate ymd %m-%
#' @importFrom terra project mask crop vect merge
#' @export
#'
#' @seealso
#' \code{\link{ff_prep}} for data preparation
#' \code{\link{ff_train}} for model training
#' \code{\link{ff_predict}} for making predictions
#' \code{\link{ff_analyze}} for analyzing prediction results
#'
#' @references
#' Jonas van Duijvenbode (2023)
#' Zillah Calle (2023)
#'
#' @keywords machine-learning prediction forestry raster

ff_run <- function(shape = NULL, country = NULL, prediction_dates = NULL,
                   ff_folder,
                   train_dates = NULL,
                   validation_dates = NULL,
                   model_save_path = NULL,
                   predictions_save_path = NULL,
                   pretrained_model_path = NULL,
                   ff_prep_parameters = NULL,
                   ff_train_parameters = NULL,
                   certainty_threshold = 0.5,
                   filter_features = "initialforestcover",
                   filter_conditions = ">0",
                   accuracy_output_path = NULL,
                   importance_output_path = NA,
                   verbose = TRUE,
                   autoscale_sample = FALSE,
                   validation = FALSE,
                   groundtruth_pattern = Sys.getenv("DEFAULT_GROUNDTRUTH")) {
  fixed_sample_size <- 6e6

  corrected_date_input <- check_dates(
    train_dates, validation_dates,
    prediction_dates, validation, pretrained_model_path, groundtruth_pattern
  )
  validation <- corrected_date_input$validation
  prediction_dates <- corrected_date_input$prediction_dates
  pretrained_model_path <- corrected_date_input$pretrained_model_path
  train_dates <- corrected_date_input$train_dates

  shape_and_tiles <- check_folder_and_input(ff_folder, country, shape, train_dates,
                                            prediction_dates,model_save_path, predictions_save_path)
  shape <- shape_and_tiles$shape
  tiles <- shape_and_tiles$tiles

  # Train model if not provided
  if (is.null(pretrained_model_path)) {
    sample_fraction <- determine_sample_fraction(
      autoscale_sample, ff_folder,
      shape, train_dates,
      filter_conditions, filter_features,
      groundtruth_pattern,
      ff_prep_parameters,
      validation, fixed_sample_size, verbose
    )
    ff_cat("Preparing data\nLooking in folder", ff_folder, verbose = verbose, color = "green")

    data_and_parameters <- prepare_training_data(
      ff_folder, shape, train_dates, filter_conditions, filter_features,
      sample_fraction, groundtruth_pattern, validation,
      ff_prep_parameters, verbose
    )
    train_input_data <- data_and_parameters$train_input_data
    ff_prep_params_combined <- data_and_parameters$ff_prep_params_combined

    train_input_data <- prepare_validation_data(
      train_input_data,
      train_dates, validation_dates, ff_prep_params_combined, verbose
    )

    pretrained_model_path <- prepare_and_train(
      train_input_data, model_save_path,
      validation, validation_dates, ff_train_parameters, verbose
    )
  }
  if (has_value(importance_output_path)) {
    if (has_value(model_save_path)) {
      ff_importance(model = model_save_path, output_csv = importance_output_path, append = TRUE)
    } else {
      ff_importance(model = pretrained_model_path, output_csv = importance_output_path, append = TRUE)
    }
  }
  # Predict
  if (prediction_dates[1] == "3000-01-01") {
    # if no prediction dates were given the prediction date was set to 3000 but should not make
    # actual predictions
    return(NA)
  }
  merged_polygons <- NULL
  for (prediction_date in prediction_dates) {
    raster_list <- list()
    for (tile in tiles) {
      # run the predict function if a model was not built but was provided by the function
      prediction_input_data <- prepare_and_run_prediction(
        ff_folder, tile, prediction_date,
        groundtruth_pattern, filter_features, filter_conditions,
        ff_prep_parameters, pretrained_model_path, verbose
      )

      prediction <- ff_predict(
        model = pretrained_model_path, test_matrix = prediction_input_data$feature_dataset,
        indices = prediction_input_data$test_indices,
        templateraster = prediction_input_data$groundtruth_raster,
        verbose = verbose, certainty = TRUE
      )
      raster_list[[tile]] <- prediction$predicted_raster
      # Analyze prediction
      merged_polygons <- analyze_predictions(
        ff_folder, shape, tile, prediction, prediction_date,
        filter_features, filter_conditions, prediction_input_data,
        certainty_threshold, accuracy_output_path, country,
        merged_polygons, verbose
      )
    }
    if (verbose && has_value(merged_polygons)) {
      # print the precision and recall and F0.5
      precision <- sum(merged_polygons$TP, na.rm = TRUE) /
        (sum(merged_polygons$TP, na.rm = TRUE) + sum(merged_polygons$FP, na.rm = TRUE))
      recall <- sum(merged_polygons$TP, na.rm = TRUE) /
        (sum(merged_polygons$TP, na.rm = TRUE) + sum(merged_polygons$FN, na.rm = TRUE))
      ff_cat("date:", prediction_date, "precision:", precision, ",recall:", recall,
        ",F0.5", (1.25 * precision * recall) / (0.25 * precision + recall),
        color = "green"
      )
    }

    merged_prediction <- merge_and_write_raster(
      raster_list, shape,
      prediction_date, predictions_save_path, prediction_dates, verbose
    )
    if (!exists("prediction_timeseries")) {
      prediction_timeseries <- merged_prediction
    } else {
      prediction_timeseries <- c(prediction_timeseries, merged_prediction)
    }
  }
  return(prediction_timeseries)
}


#' Merge Two Named Lists with User Preferences Overwriting Defaults
#'
#' This function merges two named lists, allowing elements from the user-specified list
#' to overwrite those in the default list if they have the same names. If there are no
#' overlapping keys, the function simply combines the lists.
#'
#' @param default_parameters A named list containing the default values.
#' @param user A named list containing the user-specified values that may overwrite the default values.
#'
#' @return A named list with combined elements from both input lists. User-specified elements
#' overwrite default elements with the same names.
#'
#' @examples
#' default_list <- list(a = 1, b = 2, c = 3)
#' user_list <- list(b = 20, d = 4)
#' merged_list <- merge_lists(default_list, user_list)
#' print(merged_list) # Should print: $a [1], $c [3], $b [20], $d [4]
#'
#' @noRd
merge_lists <- function(default_parameters, user_parameters) {
  indices <- -which(names(default_parameters) %in% names(user_parameters))
  if (length(indices) != 0) {
    c(default_parameters[indices], user_parameters)
  } else {
    c(default_parameters, user_parameters)
  }
}

#' Check if Dates Follow YYYY-MM-01 Format
#'
#' Validates that all dates in a vector follow the format YYYY-MM-01, where:
#' - YYYY is a four-digit year
#' - MM is a valid month (01-12)
#' - The day must be 01
#'
#' @param dates A character vector containing dates to validate
#'
#' @return A logical value: TRUE if all dates are valid, FALSE otherwise
#'
#' @examples
#' is_date_format(c("2023-01-01", "2023-12-01")) # TRUE
#' is_date_format(c("2023-13-01")) # FALSE (invalid month)
#' is_date_format(c("2023-01-02")) # FALSE (day must be 01)
#'
#' @noRd
is_date_format <- function(dates) {
  # Regular expression pattern for YYYY-MM-01
  pattern <- "^\\d{4}-\\d{2}-01$"

  # Check if all elements match the pattern
  all_match <- all(grepl(pattern, dates))

  if (!all_match) {
    return(FALSE)
  }

  # Additional validation for valid months (01-12)
  valid_dates <- sapply(dates, function(date) {
    month <- as.numeric(strsplit(date, "-")[[1]][2])
    return(month >= 1 && month <= 12)
  })

  return(all(valid_dates))
}

check_dates <- function(train_dates, validation_dates, prediction_dates,
                        validation, pretrained_model_path, groundtruth_pattern) {
  if (has_value(train_dates) && !is_date_format(train_dates)) {
    stop("the train dates are in the incorrect format and should be in YYYY-MM-01")
  }

  if (has_value(validation_dates) && !is_date_format(validation_dates)) {
    stop("the train dates are in the incorrect format and should be in YYYY-MM-01")
  }

  if (has_value(prediction_dates) && !is_date_format(prediction_dates)) {
    stop("the train dates are in the incorrect format and should be in YYYY-MM-01")
  }
  if (has_value(validation_dates) && validation) {
    ff_cat("validation will be set to FALSE since validation dates
           were already given and will be used instead of a fraction of the input")
    validation <- FALSE
  }
  if (!has_value(prediction_dates) && !is.null(pretrained_model_path)) {
    stop("prediction_date is not given and input model path is given so there is no need to run.")
  }
  if (!has_value(prediction_dates)) {
    prediction_dates <- "3000-01-01"
  }

  if (is.null(pretrained_model_path)) {
    if (!has_value(train_dates)) {
      # Extract the number of months from groundtruth_pattern (e.g., "6" from "groundtruth6m")
      months_back <- as.integer(gsub("\\D", "", groundtruth_pattern))

      # Fallback to 6 months if pattern is invalid or missing
      if (is.na(months_back) || months_back <= 0) {
        months_back <- 6
        ff_cat(paste("Invalid or missing groundtruth_pattern:", groundtruth_pattern, ". Defaulting to 6 months."),
          color = "yellow", verbose = TRUE
        )
      }

      train_dates <- as.character(
        lubridate::ymd(min(prediction_dates)) %m-%
          months(months_back, abbreviate = FALSE)
      )
      ff_cat("No train dates were given though a training was wanted, model will be trained on",
        train_dates,
        color = "yellow"
      )
    }

    if (max(lubridate::ymd(train_dates)) > min(lubridate::ymd(prediction_dates))) {
      ff_cat("(some) training dates are after prediction dates", color = "yellow")
    }
    if ((min(lubridate::ymd(prediction_dates)) - max(lubridate::ymd(train_dates))) < 170) {
      ff_cat("There should be at least 6 months between training and testing/predicting", color = "yellow")
    }
  }
  prediction_dates <- sort(prediction_dates)
  return(list(
    validation = validation, train_dates = train_dates,
    prediction_dates = prediction_dates, pretrained_model_path = pretrained_model_path
  ))
}

check_folder_and_input <- function(ff_folder, country, shape, train_dates, prediction_dates,model_save_path, predictions_save_path) {
  if (!has_value(model_save_path) && !has_value(prediction_dates)) {
    stop("no model is being saved and no predictions are being made (no prediction dates), so there is no point to this")
  }

  if (has_value(predictions_save_path) && !has_value(prediction_dates)) {
    stop("predictions_save_path was given but no prediction_dates were given, unknown for which date to predict")
  }


  if ((has_value(shape) + has_value(country)) != 1) {
    stop("either input shape or country should be given, not neither and not both")
  }
  if (has_value(shape)) {
    ForestForesight::check_spatvector(shape,
      check_size = has_value(train_dates)
    )
  } else {
    data(countries, envir = environment())
    countries <- terra::vect(countries)
    shape <- countries[which(countries$iso3 == country), ]
  }

  # check if all the function parameters have values in the right format

  if (!has_value(ff_folder)) {
    stop("ff_folder is not given")
  }
  if (!dir.exists(ff_folder)) {
    stop(paste(ff_folder, "does not exist as a main ff_folder"))
  }

  tiles <- terra::vect(get(data("gfw_tiles", envir = environment())))[shape, ]$tile_id
  ff_structurecheck(
    shape = shape, folder_path = ff_folder,
    check_date = if (has_value(train_dates)) {
      train_dates[1]
    } else {
      prediction_dates[1]
    },
    error_on_issue = TRUE, silent_on_pass = TRUE
  )

  return(list(shape = shape, tiles = tiles))
}


determine_sample_fraction <- function(autoscale_sample, ff_folder, shape, train_dates,
                                      filter_conditions, filter_features,
                                      groundtruth_pattern,
                                      ff_prep_parameters,
                                      validation, fixed_sample_size, verbose) {
  # ff prep to determine the sample size
  if (autoscale_sample && has_value(filter_conditions)) {
    ff_cat("Finding optimal sample size based on filter conditions",
      color = "green", verbose = verbose
    )
    ff_prep_params_original <- list(
      datafolder = ff_folder, shape = shape, dates = train_dates,
      filter_conditions = filter_conditions, filter_features = filter_features,
      sample_size = 1, shrink = "extract",
      groundtruth_pattern = groundtruth_pattern, label_threshold = 1
    )
    ff_prep_params_combined <- merge_lists(
      default_parameters = ff_prep_params_original,
      user_parameters = ff_prep_parameters
    )
    ff_prep_params_combined <- merge_lists(
      default_parameters = ff_prep_params_combined,
      user_parameters = list(
        "inc_features" = filter_features, "add_date" = FALSE,
        "add_xy" = FALSE, "verbose" = FALSE
      )
    )
    train_input_data <- do.call(ff_prep, ff_prep_params_combined)
    if (validation) {
      sample_fraction <- min(1, 1.33 * fixed_sample_size / length(
        train_input_data$feature_dataset$features
      ))

      ff_cat("adding validation matrix", color = "green", verbose = verbose)
    } else {
      sample_fraction <- min(1, fixed_sample_size / length(
        train_input_data$feature_dataset$features
      ))
    }

    ff_cat("Autoscaled sample size:", round(sample_fraction, 2),
      color = "green", verbose = verbose
    )
    return(sample_fraction)
  }else{
    return(0.3)
  }

}



prepare_training_data <- function(ff_folder, shape, train_dates, filter_conditions, filter_features,
                                  sample_fraction, groundtruth_pattern, validation,
                                  ff_prep_parameters, verbose) {
  ff_prep_params_original <- list(
    datafolder = ff_folder, shape = shape, dates = train_dates,
    filter_conditions = filter_conditions, filter_features = filter_features,
    sample_size = sample_fraction, verbose = verbose, shrink = "extract",
    groundtruth_pattern = groundtruth_pattern, label_threshold = 1
  )
  if (validation) {
    ff_prep_params_original <- c(ff_prep_params_original, list("validation_sample" = 0.25))
  }
  ff_prep_params_combined <- merge_lists(ff_prep_params_original, ff_prep_parameters)

  train_input_data <- do.call(ff_prep, ff_prep_params_combined)
  return(list(train_input_data = train_input_data, ff_prep_params_combined = ff_prep_params_combined))
}

prepare_validation_data <- function(train_input_data, train_dates,
                                    validation_dates, ff_prep_params_combined, verbose) {
  if (has_value(validation_dates)) {
    ff_cat("adding validation matrix for dates", paste(validation_dates, collapse = ", "), "\n",
      color = "green", verbose = verbose
    )

    ff_prep_params_combined["dates"] <- validation_dates
    ff_prep_params_combined["sample_size"] <- 1 / 3 * ff_prep_params_combined$sample_size
    validation_data <- do.call(ff_prep, ff_prep_params_combined)


    if (min(train_dates) < min(validation_dates)) {
      extra_features <- which(!validation_data$features %in% train_input_data$features)
      if (length(extra_features) > 0) {
        validation_data$features <- validation_data$features[-extra_features]
        validation_data$feature_dataset$features <- validation_data$feature_dataset$features[, -extra_features]
      }
    } else {
      extra_features <- which(!train_input_data$features %in% validation_data$features)
      if (length(extra_features) > 0) {
        train_input_data$features <- train_input_data$features[-extra_features]
        train_input_data$feature_dataset$features <- train_input_data$feature_dataset$features[, -extra_features]
      }
    }
    train_input_data$validation_matrix <- validation_data$feature_dataset
  }
  return(train_input_data)
}


prepare_and_train <- function(train_input_data, model_save_path, validation,
                              validation_dates, ff_train_parameters, verbose) {
  ff_train_params_original <- list(
    train_matrix = train_input_data$feature_dataset, verbose = verbose,
    modelfilename = model_save_path
  )
  if (validation || has_value(validation_dates)) {
    ff_train_params_original <- c(
      ff_train_params_original,
      list(validation_matrix = train_input_data$validation_matrix)
    )
  }
  ff_train_params_merged <- merge_lists(
    ff_train_params_original,
    ff_train_parameters
  )

  pretrained_model_path <- do.call(ff_train, ff_train_params_merged)
  return(pretrained_model_path)
}

prepare_and_run_prediction <- function(ff_folder, tile, prediction_date,
                                       groundtruth_pattern, filter_features, filter_conditions,
                                       ff_prep_parameters, pretrained_model_path, verbose) {
  ff_prep_params_original <- list(
    datafolder = ff_folder, tiles = tile, dates = prediction_date,
    verbose = verbose, filter_features = filter_features,
    filter_conditions = filter_conditions, groundtruth_pattern = groundtruth_pattern,
    sample_size = 1, label_threshold = 1, shrink = "none"
  )
  ff_prep_params_combined <- merge_lists(ff_prep_params_original, ff_prep_parameters)
  if (class(pretrained_model_path) == "character") {
    if (file.exists(gsub("\\.model", "\\.rda", pretrained_model_path))) {
      model_features <- list("inc_features" = get(load(gsub("\\.model", "\\.rda", pretrained_model_path))))

      ff_cat("pre-trained model only includes the following features:",
        paste(model_features$inc_features, collapse = ", "),
        color = "green", verbose = verbose
      )

      ff_prep_params_combined <- merge_lists(
        default_parameters = model_features,
        user_parameters = ff_prep_params_combined
      )
    }
  }
  prediction_input_data <- do.call(ff_prep, ff_prep_params_combined)
  return(prediction_input_data)
}

create_forest_mask <- function(ff_folder, tile, prediction_date, filter_features, filter_conditions) {
  if (length(filter_features) == 0) {
    return(NULL)
  }
  for (i in seq_along(filter_features)) {
    filename <- get_raster(
      tile = tile, date = prediction_date,
      datafolder = file.path(ff_folder, "preprocessed", "input"),
      feature = filter_features[i]
    )
    if (!has_value(filename)) {
      stop(paste("Cannot find the file for feature", filter_features[i]))
    }
    current_feature_raster <- terra::rast(filename)
    operator <- gsub("[[:alnum:]]", "", filter_conditions[i])
    filter_value <- as.numeric(gsub("[^0-9.-]", "", filter_conditions[i]))
    current_feature_raster <- switch(operator,
      ">" = current_feature_raster > filter_value,
      "<" = current_feature_raster < filter_value,
      "==" = current_feature_raster == filter_value,
      "!=" = current_feature_raster != filter_value,
      ">=" = current_feature_raster >= filter_value,
      "<=" = current_feature_raster <= filter_value
    )
    if (i == 1) {
      forest_mask <- current_feature_raster
    } else {
      forest_mask <- forest_mask * current_feature_raster
    }
  }
  return(forest_mask)
}

analyze_predictions <- function(ff_folder, shape, tile, prediction, prediction_date,
                                filter_features, filter_conditions, prediction_input_data,
                                certainty_threshold, accuracy_output_path, country,
                                merged_polygons = NULL, verbose) {
  if (prediction_input_data$has_groundtruth) {
    forest_mask <- create_forest_mask(ff_folder, tile, prediction_date, filter_features, filter_conditions)


    analysis_polygons <- terra::intersect(terra::vect(get(data("degree_polygons"))), terra::aggregate(shape))
    polygons <- ff_analyze(prediction$predicted_raster > certainty_threshold,
      groundtruth = prediction_input_data$groundtruth_raster,
      csv_filename = accuracy_output_path, tile = tile, date = prediction_date,
      append = TRUE, country = country,
      verbose = verbose, forest_mask = forest_mask, analysis_polygons = analysis_polygons
    )
    if (verbose) {
      if (!has_value(merged_polygons)) {
        merged_polygons <- polygons
      } else {
        merged_polygons <- rbind(merged_polygons, polygons)
      }
    }
  } else {
    ff_cat("no analysis is done because no groundtruth is available", color = "green", verbose = verbose)
    return(NULL)
  }
  return(merged_polygons)
}

merge_and_write_raster <- function(raster_list, shape, prediction_date, predictions_save_path, prediction_dates, verbose) {
  if (length(raster_list) == 1) {
    merged_prediction <- raster_list[[1]]
  } else {
    merged_prediction <- do.call(terra::merge, unname(raster_list))
  }
  merged_prediction <- terra::mask(merged_prediction, shape)
  merged_prediction <- terra::crop(merged_prediction, shape)
  names(merged_prediction) <- prediction_date
  if (has_value(predictions_save_path)) {
    if (length(prediction_dates) > 1) {
      filename <- paste0(sub("\\.tif$", "", predictions_save_path), "_", prediction_date, ".tif")
    } else {
      filename <- predictions_save_path
    }

    ff_cat("saving result to ", filename, verbose = verbose)

    terra::writeRaster(merged_prediction, filename, overwrite = TRUE)
  }
  return(merged_prediction)
}
