#' Train a Model and Predict Deforestation on Raster Data
#'
#' This function trains an XGBoost model using historical data and then predicts deforestation
#' on raster data for a specified date and area.
#'
#' @param shape SpatVector object representing the area of interest. Either `shape` or `country` must be provided.
#' @param country ISO3 country code. Either `shape` or `country` must be provided.
#' @param prediction_dates Dates for prediction in "YYYY-MM-DD" format.
#' @param ff_folder Directory containing the input data.
#' @param train_dates The dates for which you want to create the training data. dates should be a vector with the format YYYY-MM-DD with DD being 01
#' @param validation_dates The dates for which you want to create a distinct validation matrix (if any, select validation = T to make a subsample of the same dates as the training data).
#' @param save_path Path to save the trained model (with extension ".model"). Default is NULL.
#' @param save_path_predictions Path to save the predictions (with extension ".tif"). Default is NULL.
#' @param trained_model Pre-trained model object or path to saved model. If NULL, a new model will be trained. Default is NULL.
#' @param ff_prep_params List of parameters for data preprocessing. See `ff_prep` function for details.
#' @param ff_train_params List of parameters for model training. See `ff_train` function for details.
#' @param threshold Probability threshold for binary classififf_cation. Default is 0.5.
#' @param fltr_features Feature dataset used for pre-filtering for training. Default is initialforestcover. Can be more than one
#' @param fltr_condition The condition with value that is used to filter the training dataset based on mask features. Default is ">0". Can be more than one
#' @param accuracy_csv Path to save accuracy metrics in CSV format. Default is NA (no CSV output).
#' @param importance_csv Path to save feature importance metrics in CSV format. Default is NA (no CSV output).
#' @param verbose Logical; whether to display progress messages. Default is TRUE.
#' @param autoscale_sample Logical; Whether to automatically scale the number of samples based on the size of the area and the length of the training period.
#' @param validation Logical; Whether to add a validation matrix based on the training data, which is set at 0.25 of the training matrix. Should not be set if validation_dates is not NULL.
#'
#' @return A SpatRaster object containing the predicted deforestation probabilities.If multiple prediction dates are given you receive a rasterstack with a raster per date
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
#' @importFrom lubridate ymd months %m-%
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

config <- config_load()

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
                   fltr_features = "initialforestcover",
                   fltr_condition = ">0",
                   accuracy_csv = NULL,
                   importance_csv = NA,
                   verbose = T,
                   autoscale_sample = F,
                   validation = F) {
  fixed_sample_size <- 6e6
  sample_size <- 0.3
  if (!hasvalue(shape) & !hasvalue(country)) {
    stop("either input shape or country should be given")
  }
  if (!hasvalue(shape)) {
    data(countries, envir = environment())
    countries <- terra::vect(countries)
    shape <- countries[which(countries$iso3 == country), ]
  }
  # check if all the function parameters have values in the right format
  if (hasvalue(validation_dates)) {
    validation <- F
  }
  if (!hasvalue(ff_folder)) {
    stop("ff_folder is not given")
  }
  if (!dir.exists(ff_folder)) {
    stop(paste(ff_folder, "does not exist"))
  }
  if (!hasvalue(prediction_dates) & !is.null(trained_model)) {
    stop("prediction_date is not given and model is given so there is no need to run.")
  }
  if (!hasvalue(prediction_dates)) {
    prediction_dates <- "3000-01-01"
  }
  prediction_dates <- sort(prediction_dates)

  if (is.null(trained_model)) {
    if (!hasvalue(train_dates)) {
      train_dates <- as.character(lubridate::ymd(min(prediction_dates)) %m-% months(6, abbreviate = F))
    }

    if (lubridate::ymd(max(train_dates)) > lubridate::ymd(prediction_dates[1])) {
      ff_cat("(some) training dates are after prediction dates", color = "yellow")
    }
    if ((lubridate::ymd(prediction_dates[1]) - lubridate::ymd(max(train_dates))) < 170) {
      ff_cat("There should be at least 6 months between training and testing/predicting", color = "yellow")
    }
  }


  if (!terra::is.lonlat(shape)) {
    shape <- terra::project(shape, "epsg:4326")
  }
  data(gfw_tiles, envir = environment())
  tiles <- terra::vect(gfw_tiles)[shape, ]$tile_id


  prep_folder <- file.path(ff_folder, "preprocessed")
  if (!dir.exists(prep_folder)) {
    stop(paste(prep_folder, "does not exist"))
  }



  # Train model if not provided
  if (is.null(trained_model)) {
    sample_size <- 0.3
    # ff prep to determine the sample size
    if (autoscale_sample & hasvalue(fltr_condition)) {
      if (verbose) {
        ff_cat("Finding optimal sample size based on filter condition\n", color = "green")
      }
      ff_prep_params_original <- list(
        datafolder = prep_folder, shape = shape, dates = train_dates,
        fltr_condition = fltr_condition, fltr_features = fltr_features,
        sample_size = 1, shrink = "extract",
        groundtruth_pattern = config$DEFAULT_GROUNDTRUTH, label_threshold = 1
      )
      ff_prep_params_combined <- merge_lists(default = ff_prep_params_original, user = ff_prep_params)
      ff_prep_params_combined <- merge_lists(default = ff_prep_params_combined, user = list("inc_features" = fltr_features, "adddate" = F, "addxy" = F, "verbose" = F))
      traindata <- do.call(ff_prep, ff_prep_params_combined)
      if (validation) {
        sample_size <- min(1, 1.33 * fixed_sample_size / length(traindata$data_matrix$features))
        if (verbose) {
          ff_cat("adding validation matrix\n", color = "green")
        }
      } else {
        sample_size <- min(1, fixed_sample_size / length(traindata$data_matrix$features))
      }
      if (verbose) {
        ff_cat("autoscaled sample size:", round(sample_size, 2), "\n", color = "green")
      }
    }



    if (verbose) {
      ff_cat("Preparing data\n", color = "green")
      ff_cat("looking in folder", prep_folder, "\n", color = "green")
    }
    ff_prep_params_original <- list(
      datafolder = prep_folder, shape = shape, dates = train_dates,
      fltr_condition = fltr_condition, fltr_features = fltr_features,
      sample_size = sample_size, verbose = verbose, shrink = "extract",
      groundtruth_pattern = config$DEFAULT_GROUNDTRUTH, label_threshold = 1
    )
    if (validation) {
      ff_prep_params_original <- c(ff_prep_params_original, list("validation_sample" = 0.25))
    }
    ff_prep_params_combined <- merge_lists(ff_prep_params_original, ff_prep_params)

    traindata <- do.call(ff_prep, ff_prep_params_combined)
    if (hasvalue(validation_dates)) {
      if (verbose) {
        ff_cat("adding validation matrix for dates", paste(validation_dates, collapse = ", "), "\n", color = "green")
      }
      ff_prep_params_combined["dates"] <- validation_dates
      ff_prep_params_combined["sample_size"] <- 1 / 3 * sample_size
      valdata <- do.call(ff_prep, ff_prep_params_combined)


      if (min(train_dates) < min(validation_dates)) {
        extra_features <- which(!valdata$features %in% traindata$features)
        if (length(extra_features) > 0) {
          valdata$features <- valdata$features[-extra_features]
          valdata$data_matrix$features <- valdata$data_matrix$features[, -extra_features]
        }
      } else {
        extra_features <- which(!traindata$features %in% valdata$features)
        if (length(extra_features) > 0) {
          traindata$features <- traindata$features[-extra_features]
          traindata$data_matrix$features <- traindata$data_matrix$features[, -extra_features]
        }
      }
      traindata$validation_matrix <- valdata$data_matrix
    }

    ff_train_params_original <- list(
      train_matrix = traindata$data_matrix, verbose = verbose,
      modelfilename = save_path
    )
    if (validation | hasvalue(validation_dates)) {
      ff_train_params_original <- c(ff_train_params_original, list(validation_matrix = traindata$validation_matrix))
    }
    ff_train_params_original <- merge_lists(ff_train_params_original, ff_train_params)

    trained_model <- do.call(ff_train, ff_train_params_original)
  }
  if (hasvalue(importance_csv)) {
    if (hasvalue(save_path)) {
      ff_importance(save_path, importance_csv, append = T)
    } else {
      ff_importance(trained_model, importance_csv, append = T)
    }
  }
  # Predict
  if (prediction_dates[1] == "3000-01-01") {
    return(NA)
  }
  firstdate <- T
  for (prediction_date in prediction_dates) {
    raslist <- list()
    for (tile in tiles) {
      # run the predict function if a model was not built but was provided by the function
      ff_prep_params_original <- list(
        datafolder = prep_folder, tiles = tile, dates = prediction_date,
        verbose = verbose, fltr_features = fltr_features,
        fltr_condition = fltr_condition, groundtruth_pattern = config$DEFAULT_GROUNDTRUTH, sample_size = 1, label_threshold = 1, shrink = "crop"
      )
      ff_prep_params_combined <- merge_lists(ff_prep_params_original, ff_prep_params)
      if (class(trained_model) == "character") {
        if (file.exists(gsub("\\.model", "\\.rda", trained_model))) {
          model_features <- list("inc_features" = get(load(gsub("\\.model", "\\.rda", trained_model))))
          if (verbose) {
            ff_cat("pre-trained model only includes the following features:", paste(model_features$inc_features, collapse = ", "), "\n", color = "green")
          }
          ff_prep_params_combined <- merge_lists(default = model_features, user = ff_prep_params_combined)
        }
      }
      predset <- do.call(ff_prep, ff_prep_params_combined)

      prediction <- ff_predict(
        model = trained_model, test_matrix = predset$data_matrix,
        indices = predset$testindices,
        templateraster = predset$groundtruthraster,
        verbose = verbose, certainty = T
      )
      raslist[[tile]] <- prediction$predicted_raster
      # Analyze prediction
      for (i in seq(length(fltr_features))) {
        filename <- get_raster(tile = tile, date = prediction_date, datafolder = paste0(prep_folder, "/input/"), feature = fltr_features[i])
        if (!hasvalue(filename)) {
          stop(paste("Cannot find the file for feature", fltr_features[i]))
        }
        curras <- terra::rast(filename)
        operator <- gsub("[[:alnum:]]", "", fltr_condition[i])
        value <- as.numeric(gsub("[^0-9.-]", "", fltr_condition[i]))
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
      if (predset$hasgroundtruth) {
        analysis_polygons <- terra::intersect(terra::vect(get(data("degree_polygons"))), terra::aggregate(shape))
        pols <- ff_analyze(prediction$predicted_raster > threshold,
          groundtruth = predset$groundtruthraster,
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
        if (verbose) {
          ff_cat("no analysis is done because no groundtruth is available\n", color = "green")
        }
      }
    }
    if (verbose & exists("allpols")) {
      precision <- sum(allpols$TP, na.rm = T) / (sum(allpols$TP, na.rm = T) + sum(allpols$FP, na.rm = T))
      recall <- sum(allpols$TP, na.rm = T) / (sum(allpols$TP, na.rm = T) + sum(allpols$FN, na.rm = T))
      ff_cat("date:", prediction_date, "precision:", precision, ",recall:", recall, ",F0.5", (1.25 * precision * recall) / (0.25 * precision + recall), "\n", color = "green")
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
      if (verbose) {
        ff_cat("saving result to ", filename)
      }
      terra::writeRaster(fullras, filename, overwrite = T)
    }



    if (firstdate) {
      firstdate <- F
      allras <- fullras
    } else {
      allras <- c(allras, fullras)
    }
  }
  return(allras)
}
