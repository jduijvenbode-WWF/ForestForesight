#' Train and Predict Raster
#'
#' This function trains a model using training data and then predicts raster values using the trained model.
#'
#' @param shape Spatial object representing the shapefile. Either `shape` or `country` should be given.
#' @param country ISO3 country code. Either `shape` or `country` should be given.
#' @param prediction_date Date for prediction in "YYYY-MM-DD" format.
#' @param ff_folder Folder directory containing the input data.
#' @param train_start Starting date for training data in "YYYY-MM-DD" format. Default is "2022-07-01".
#' @param train_end Ending date for training data in "YYYY-MM-DD" format. Default is "2023-07-01".
#' @param model_folder Folder directory to save models. If NULL, models will be saved in `ff_folder/models`.
#' @param prediction_folder Folder directory to save predictions. If NULL, predictions will be saved in `ff_folder/predictions`.
#' @param train Logical value indicating whether to train the model. Default is TRUE.
#' @param model Pre-trained model. If NULL, the function will train a model. Default is NULL.
#' @param ff_prep_params Parameters for data preprocessing.
#' @param ff_train_params Parameters for model training.
#' @param accuracy_csv Path to save accuracy metrics in CSV format. Default is NA (no CSV output).
#' @param overwrite Logical value indicating whether to overwrite existing files. Default is FALSE.
#' @param verbose Logical value indicating whether to display progress messages. Default is TRUE.
#'
#' @return A raster object containing the predicted values.
#'
#' @examples
#' # Example usage:
#' train_predict_raster(prediction_date = "2023-01-01", ff_folder = "path/to/folder")
#'
#' @importFrom lubridate ymd months
#' @importFrom terra project mask crop
#' @export

train_predict_raster <- function(shape = NULL, country = NULL, prediction_date,
                                  ff_folder,
                                  train_start="2022-07-01",
                                  train_end="2023-07-01",
                                  model_folder=NULL,
                                  prediction_folder=NULL,
                                  train=TRUE,
                                  model = NULL,
                                  ff_prep_params = NULL, ff_train_params = NULL,
                                  accuracy_csv = NA, overwrite=F, verbose=T) {
  if (!hasvalue(shape) & !hasvalue(country)) {stop("either input shape or country should be given")}
  if (!hasvalue(shape)) {
    data(countries,envir = environment())
    countries <- terra::vect(countries)
    shape <- countries[which(countries$iso3 == country),]
  }
  #check if all the function parameters have values in the right format
  if (!hasvalue(ff_folder)) {stop("ff_folder is not given")}
  if (!dir.exists(ff_folder)) {stop(paste(ff_folder,"does not exist"))}
  if (!hasvalue(prediction_date)) {stop("prediction_date is not given")}
  #check that end is before start
  if (!hasvalue(train_end)) {train_end <- as.character(lubridate::ymd(prediction_date) %m-% months(6,abbreviate = F))}
  if (lubridate::ymd(train_end) < lubridate::ymd(train_start)) {stop("train_end is before train_start")}
  if (lubridate::ymd(train_end) > lubridate::ymd(prediction_date)) {stop("train_end is after prediction_date")}



  shape <- terra::project(shape, "epsg:4326")
  data(gfw_tiles,envir = environment())
  tiles <- terra::vect(gfw_tiles)[shape,]$tile_id


  prep_folder <- file.path(ff_folder,"preprocessed")
  if (!dir.exists(prep_folder)) {stop(paste(prep_folder,"does not exist"))}


  if (!hasvalue(model_folder)) {model_folder <- file.path(ff_folder,"models")}
  if (!dir.exists(model_folder)) {stop(paste(model_folder,"does not exist"))}

  if (!hasvalue(prediction_folder)) {prediction_folder <- file.path(ff_folder,"models")}
  if (!dir.exists(prediction_folder)) {stop(paste(prediction_folder,"does not exist"))}
  # Prepare data
  if(verbose){cat("Preparing data\n");cat("looking in folder",prep_folder,"\n")}
  traindata <- ff_prep(datafolder = prep_folder, shape = shape, start = train_start, end = train_end,
                       fltr_condition = ">0",fltr_features = "initialforestcover",
                       sample_size = 0.3, verbose = verbose, shrink = "extract",
                       groundtruth_pattern = "groundtruth6m",label_threshold = 1)

  # Train model if not provided
  if (is.null(model)) {
    model <- ff_train(traindata$data_matrix, verbose = verbose,
                      modelfilename = file.path(model_folder, "test_model.model"),
                      features = traindata$features)
  }

  # Predict
  raslist <- list()
  for (tile in tiles) {
    #run the predict function if a model was not built but was provided by the function

    predset <- ff_prep(datafolder = prep_folder, tiles = tile, start = prediction_date,
                       verbose = verbose, fltr_features = "initialforestcover",
                       fltr_condition = ">0")

    prediction <- ff_predict(model, test_matrix = predset$data_matrix,
                             indices = predset$testindices,
                             templateraster = predset$groundtruthraster,
                             verbose = verbose,certainty = T)
    raslist[[tile]] <- prediction$predicted_raster
    # Analyze prediction
    # ff_analyze(prediction$predicted_raster, groundtruth = predset$groundtruthraster,
    #            csvfile = accuracy_csv, tile = tile, date = train_start,
    #            return_polygons = FALSE, append = TRUE, country = country_group,
    #            verbose = verbose)
  }
  if (length(raslist) == 1) {fullras <- raslist[[1]]}else{
    fullras <- do.call(terra::merge,unname(raslist))
  }
  fullras <- terra::mask(fullras,shape)
  fullras <- terra::crop(fullras,shape)
  return(fullras)
}




