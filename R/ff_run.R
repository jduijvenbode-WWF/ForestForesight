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
#' @param save_path Path to save the trained model (with extension ".model"). Default is NULL.
#' @param save_path_predictions Path to save the predictions (with extension ".tif"). Default is NULL.
#' @param trained_model Pre-trained model object or path to saved model.
#' If NULL, a new model will be trained. Default is NULL.
#' @param ff_prep_params List of parameters for data preprocessing. See `ff_prep` function for details.
#' @param ff_train_params List of parameters for model training. See `ff_train` function for details.
#' @param threshold Probability threshold for binary classification. Default is 0.5.
#' @param filter_features Feature dataset used for pre-filtering for training.
#' Default is initialforestcover. Can be more than one
#' @param filter_conditions The condition with value that is used to filter the training dataset based on mask features.
#'  Default is ">0". Can be more than one
#' @param accuracy_csv Path to save accuracy metrics in CSV format. Default is NA (no CSV output).
#' @param importance_csv Path to save feature importance metrics in CSV format. Default is NA (no CSV output).
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
#'   save_path = "path/to/save/model.model",
#'   accuracy_csv = "path/to/save/accuracy.csv"
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
                   save_path = NULL,
                   save_path_predictions = NULL,
                   trained_model = NULL,
                   ff_prep_params = NULL,
                   ff_train_params = NULL,
                   threshold = 0.5,
                   filter_features = "initialforestcover",
                   filter_conditions = ">0",
                   accuracy_csv = NULL,
                   importance_csv = NA,
                   verbose = TRUE,
                   autoscale_sample = FALSE,
                   validation = FALSE) {
  fixed_sample_size <- 6e6
  sample_size <- 0.3

  if (!hasvalue(shape) && !hasvalue(country)) {
    stop("either input shape or country should be given")
  }
  if (hasvalue(shape)) {
    ForestForesight::check_spatvector(shape,
      check_size = hasvalue(train_dates)
    )
  }
  if (!hasvalue(shape)) {
    data(countries, envir = environment())
    countries <- terra::vect(countries)
    shape <- countries[which(countries$iso3 == country), ]
  }

  # check if all the function parameters have values in the right format
  if (hasvalue(validation_dates)) {
    validation <- FALSE
  }
  if (!hasvalue(ff_folder)) {
    stop("ff_folder is not given")
  }
  if (!dir.exists(ff_folder)) {
    stop(paste(ff_folder, "does not exist"))
  }
  if (!hasvalue(prediction_dates) && !is.null(trained_model)) {
    stop("prediction_date is not given and model is given so there is no need to run.")
  }
  if (!hasvalue(prediction_dates)) {
    prediction_dates <- "3000-01-01"
  }
  prediction_dates <- sort(prediction_dates)

  if (is.null(trained_model)) {
    if (!hasvalue(train_dates)) {
      ff_cat("No train dates were given though a training was wanted")
      # Extract the number of months from groundtruth_pattern (e.g., "6" from "groundtruth6m")
      months_back <- as.integer(gsub("\\D", "", groundtruth_pattern))

      # Fallback to 6 months if pattern is invalid or missing
      if (is.na(months_back) || months_back <= 0) {
        months_back <- 6
        warning("Invalid or missing groundtruth_pattern. Defaulting to 6 months.")
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


  tiles <- terra::vect(get(data("gfw_tiles", envir = environment())))[shape, ]$tile_id
  shape <- check_spatvector(shape)
  ff_structurecheck(
    shape = shape, folder_path = ff_folder,
    check_date = if (hasvalue(train_dates)) {
      train_dates[1]
    } else {
      prediction_dates[1]
    },
    error_on_issue = TRUE, silent_on_pass = TRUE
  )



  # Train model if not provided
  if (is.null(trained_model)) {
    sample_size <- 0.3
    # ff prep to determine the sample size
    if (autoscale_sample && hasvalue(filter_conditions)) {
      ff_cat("Finding optimal sample size based on filter conditions", color = "green", verbose = verbose)
      ff_prep_params_original <- list(
        datafolder = ff_folder, shape = shape, dates = train_dates,
        filter_conditions = filter_conditions, filter_features = filter_features,
        sample_size = 1, shrink = "extract",
        groundtruth_pattern = Sys.getenv("DEFAULT_GROUNDTRUTH"), label_threshold = 1
      )
      ff_prep_params_combined <- merge_lists(default = ff_prep_params_original, user = ff_prep_params)
      ff_prep_params_combined <- merge_lists(
        default = ff_prep_params_combined,
        user = list(
          "inc_features" = filter_features, "add_date" = FALSE,
          "add_xy" = FALSE, "verbose" = FALSE
        )
      )
      traindata <- do.call(ff_prep, ff_prep_params_combined)
      if (validation) {
        sample_size <- min(1, 1.33 * fixed_sample_size / length(traindata$feature_dataset$features))

        ff_cat("adding validation matrix", color = "green", verbose = verbose)
      } else {
        sample_size <- min(1, fixed_sample_size / length(traindata$feature_dataset$features))
      }

      ff_cat("Autoscaled sample size:", round(sample_size, 2), color = "green", verbose = verbose)
    }

    ff_cat("Preparing data\nLooking in folder", ff_folder, verbose = verbose, color = "green")

    ff_prep_params_original <- list(
      datafolder = ff_folder, shape = shape, dates = train_dates,
      filter_conditions = filter_conditions, filter_features = filter_features,
      sample_size = sample_size, verbose = verbose, shrink = "extract",
      groundtruth_pattern = Sys.getenv("DEFAULT_GROUNDTRUTH"), label_threshold = 1
    )
    if (validation) {
      ff_prep_params_original <- c(ff_prep_params_original, list("validation_sample" = 0.25))
    }
    ff_prep_params_combined <- merge_lists(ff_prep_params_original, ff_prep_params)

    traindata <- do.call(ff_prep, ff_prep_params_combined)
    if (hasvalue(validation_dates)) {
      ff_cat("adding validation matrix for dates", paste(validation_dates, collapse = ", "), "\n",
        color = "green", verbose = verbose
      )

      ff_prep_params_combined["dates"] <- validation_dates
      ff_prep_params_combined["sample_size"] <- 1 / 3 * sample_size
      valdata <- do.call(ff_prep, ff_prep_params_combined)


      if (min(train_dates) < min(validation_dates)) {
        extra_features <- which(!valdata$features %in% traindata$features)
        if (length(extra_features) > 0) {
          valdata$features <- valdata$features[-extra_features]
          valdata$feature_dataset$features <- valdata$feature_dataset$features[, -extra_features]
        }
      } else {
        extra_features <- which(!traindata$features %in% valdata$features)
        if (length(extra_features) > 0) {
          traindata$features <- traindata$features[-extra_features]
          traindata$feature_dataset$features <- traindata$feature_dataset$features[, -extra_features]
        }
      }
      traindata$validation_matrix <- valdata$feature_dataset
    }

    ff_train_params_original <- list(
      train_matrix = traindata$feature_dataset, verbose = verbose,
      modelfilename = save_path
    )
    if (validation || hasvalue(validation_dates)) {
      ff_train_params_original <- c(ff_train_params_original, list(validation_matrix = traindata$validation_matrix))
    }
    ff_train_params_original <- merge_lists(ff_train_params_original, ff_train_params)

    trained_model <- do.call(ff_train, ff_train_params_original)
  }
  if (hasvalue(importance_csv)) {
    if (hasvalue(save_path)) {
      ff_importance(save_path, importance_csv, append = TRUE)
    } else {
      ff_importance(trained_model, importance_csv, append = TRUE)
    }
  }
  # Predict
  if (prediction_dates[1] == "3000-01-01") {
    return(NA)
  }
  firstdate <- TRUE
  for (prediction_date in prediction_dates) {
    raslist <- list()
    for (tile in tiles) {
      # run the predict function if a model was not built but was provided by the function
      ff_prep_params_original <- list(
        datafolder = ff_folder, tiles = tile, dates = prediction_date,
        verbose = verbose, filter_features = filter_features,
        filter_conditions = filter_conditions, groundtruth_pattern = Sys.getenv("DEFAULT_GROUNDTRUTH"),
        sample_size = 1, label_threshold = 1, shrink = "none"
      )
      ff_prep_params_combined <- merge_lists(ff_prep_params_original, ff_prep_params)
      if (class(trained_model) == "character") {
        if (file.exists(gsub("\\.model", "\\.rda", trained_model))) {
          model_features <- list("inc_features" = get(load(gsub("\\.model", "\\.rda", trained_model))))

          ff_cat("pre-trained model only includes the following features:",
            paste(model_features$inc_features, collapse = ", "),
            color = "green", verbose = verbose
          )

          ff_prep_params_combined <- merge_lists(default = model_features, user = ff_prep_params_combined)
        }
      }
      predset <- do.call(ff_prep, ff_prep_params_combined)

      prediction <- ff_predict(
        model = trained_model, test_matrix = predset$feature_dataset,
        indices = predset$test_indices,
        templateraster = predset$groundtruth_raster,
        verbose = verbose, certainty = TRUE
      )
      raslist[[tile]] <- prediction$predicted_raster
      # Analyze prediction
      for (i in seq_along(filter_features)) {
        filename <- get_raster(
          tile = tile, date = prediction_date,
          datafolder = file.path(ff_folder, "preprocessed", "input"),
          feature = filter_features[i]
        )
        if (!hasvalue(filename)) {
          stop(paste("Cannot find the file for feature", filter_features[i]))
        }
        curras <- terra::rast(filename)
        operator <- gsub("[[:alnum:]]", "", filter_conditions[i])
        value <- as.numeric(gsub("[^0-9.-]", "", filter_conditions[i]))
        curras <- switch(operator,
          ">" = curras > value,
          "<" = curras < value,
          "==" = curras == value,
          "!=" = curras != value,
          ">=" = curras >= value,
          "<=" = curras <= value
        )
        if (i == 1) {
          forestras <- curras
        } else {
          forestras <- forestras * curras
        }
      }
      if (!hasvalue(forestras)) {
        forestras <- NULL
      }
      if (predset$has_groundtruth) {
        analysis_polygons <- terra::intersect(terra::vect(get(data("degree_polygons"))), terra::aggregate(shape))
        pols <- ff_analyze(prediction$predicted_raster > threshold,
          groundtruth = predset$groundtruth_raster,
          csvfile = accuracy_csv, tile = tile, date = prediction_date,
          return_polygons = verbose, append = TRUE, country = country,
          verbose = verbose, forestmask = forestras, analysis_polygons = analysis_polygons
        )
        if (verbose) {
          if (tile == tiles[1]) {
            allpols <- pols
          } else {
            allpols <- rbind(allpols, pols)
          }
        }
      } else {
        ff_cat("no analysis is done because no groundtruth is available\n", color = "green", verbose = verbose)
      }
    }
    if (verbose && exists("allpols")) {
      precision <- sum(allpols$TP, na.rm = TRUE) / (sum(allpols$TP, na.rm = TRUE) + sum(allpols$FP, na.rm = TRUE))
      recall <- sum(allpols$TP, na.rm = TRUE) / (sum(allpols$TP, na.rm = TRUE) + sum(allpols$FN, na.rm = TRUE))
      ff_cat("date:", prediction_date, "precision:", precision, ",recall:", recall,
        ",F0.5", (1.25 * precision * recall) / (0.25 * precision + recall),
        color = "green"
      )
    }

    if (length(raslist) == 1) {
      fullras <- raslist[[1]]
    } else {
      fullras <- do.call(terra::merge, unname(raslist))
    }
    fullras <- terra::mask(fullras, shape)
    fullras <- terra::crop(fullras, shape)
    names(fullras) <- prediction_date
    if (hasvalue(save_path_predictions)) {
      if (length(prediction_dates) > 1) {
        filename <- paste0(sub("\\.tif$", "", save_path_predictions), "_", prediction_date, ".tif")
      } else {
        filename <- save_path_predictions
      }

      ff_cat("saving result to ", filename, verbose = verbose)

      terra::writeRaster(fullras, filename, overwrite = TRUE)
    }



    if (firstdate) {
      firstdate <- FALSE
      allras <- fullras
    } else {
      allras <- c(allras, fullras)
    }
  }
  return(allras)
}
