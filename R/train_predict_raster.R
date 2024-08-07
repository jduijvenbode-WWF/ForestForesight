#' Train a Model and Predict Deforestation on Raster Data
#'
#' This function trains an XGBoost model using historical data and then predicts deforestation
#' on raster data for a specified date and area.
#'
#' @param shape SpatVector object representing the area of interest. Either `shape` or `country` must be provided.
#' @param country ISO3 country code. Either `shape` or `country` must be provided.
#' @param prediction_date Date for prediction in "YYYY-MM-DD" format.
#' @param ff_folder Directory containing the input data.
#' @param train_start Start date for training data in "YYYY-MM-DD" format. Default is NULL.
#' @param train_end End date for training data in "YYYY-MM-DD" format. Default is NULL.
#' @param save_path Path to save the trained model. Default is NULL.
#' @param trained_model Pre-trained model object or path to saved model. If NULL, a new model will be trained. Default is NULL.
#' @param ff_prep_params List of parameters for data preprocessing. See `ff_prep` function for details.
#' @param ff_train_params List of parameters for model training. See `ff_train` function for details.
#' @param threshold Probability threshold for binary classification. Default is 0.5.
#' @param mask_feature Probability threshold for binary classification. Default is initialforestcover.
#' @param accuracy_csv Path to save accuracy metrics in CSV format. Default is NA (no CSV output).
#' @param overwrite Logical; whether to overwrite existing files. Default is FALSE.
#' @param verbose Logical; whether to display progress messages. Default is TRUE.
#'
#' @return A SpatRaster object containing the predicted deforestation probabilities.
#'
#' @examples
#' \dontrun{
#' # Predict deforestation for a country
#' prediction <- train_predict_raster(
#'   country = "BRA",
#'   prediction_date = "2024-01-01",
#'   ff_folder = "path/to/forestforesight/data",
#'   train_start = "2022-01-01",
#'   train_end = "2023-12-31",
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

train_predict_raster <- function(shape = NULL, country = NULL, prediction_date,
                                  ff_folder,
                                  train_start=NULL,
                                  train_end=NULL,
                                  save_path=NULL,
                                  trained_model = NULL,
                                  ff_prep_params = NULL,
                                  ff_train_params = NULL,
                                  threshold = 0.5,
                                 mask_feature = "initialforestcover",
                                 accuracy_csv = NA,
                                 overwrite=F,
                                 verbose=T) {
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
  if (is.null(trained_model)) {
  if (!hasvalue(train_end)) {train_end <- as.character(lubridate::ymd(prediction_date) %m-% months(6,abbreviate = F))}
  if (lubridate::ymd(train_end) < lubridate::ymd(train_start)) {stop("train_end is before train_start")}
  if (lubridate::ymd(train_end) > lubridate::ymd(prediction_date)) {stop("train_end is after prediction_date")}
}


  if (!terra::is.lonlat(shape)) {shape <- terra::project(shape, "epsg:4326")}
  data(gfw_tiles,envir = environment())
  tiles <- terra::vect(gfw_tiles)[shape,]$tile_id


  prep_folder <- file.path(ff_folder,"preprocessed")
  if (!dir.exists(prep_folder)) {stop(paste(prep_folder,"does not exist"))}
  if(is.null(trained_model)){
    if(!is.null(save)){model_folder = dirname(save_path)


    if (!hasvalue(model_folder)) {model_folder <- file.path(ff_folder,"models")}
    if (!dir.exists(model_folder)) {stop(paste(model_folder,"does not exist"))}
    }
  }



  # Train model if not provided
  if (is.null(trained_model)) {
    if (verbose) {cat("Preparing data\n");cat("looking in folder",prep_folder,"\n")}
    ff_prep_params_original = list(datafolder = prep_folder, shape = shape, start = train_start, end = train_end,
                                   fltr_condition = ">0",fltr_features = "initialforestcover",
                                   sample_size = 0.3, verbose = verbose, shrink = "extract",
                                   groundtruth_pattern = "groundtruth6m",label_threshold = 1)
    ff_prep_params_combined = merge_lists(ff_prep_params_original, ff_prep_params)
    if (!is.null(trained_model)) {
      if (file.exists(gsub("\\.model","\\.rda",trained_model))) {
        model_features <- list("inc_features",get(load(gsub("\\.model","\\.rda",trained_model))))
        ff_prep_params_combined <- merge_lists(default = model_features,user = ff_prep_params_combined)
      }
    }
    traindata <- do.call(ff_prep, ff_prep_params_combined)
    ff_train_params_original = list(traindata$data_matrix, verbose = verbose,
                                    modelfilename = save_path,
                                    features = traindata$features)
    ff_train_params_original = merge_lists(ff_train_params_original, ff_train_params)
    trained_model <- do.call(ff_train, ff_train_params_original)
  }

  # Predict
  raslist <- list()
  for (tile in tiles) {
    #run the predict function if a model was not built but was provided by the function
    ff_prep_params_original = list(datafolder = prep_folder, tiles = tile, start = prediction_date,
                                  verbose = verbose, fltr_features = "initialforestcover",
                                  fltr_condition = ">0", groundtruth_pattern = "groundtruth6m", label_threshold = 1)
    ff_prep_params_combined = merge_lists(ff_prep_params_original, ff_prep_params)
    predset <- do.call(ff_prep, ff_prep_params_combined)

    prediction <- ff_predict(model = trained_model, test_matrix = predset$data_matrix,
                             indices = predset$testindices,
                             templateraster = predset$groundtruthraster,
                             verbose = verbose,certainty = T)
    raslist[[tile]] <- prediction$predicted_raster
    # Analyze prediction

    forestras = get_raster(tile = tile,date = prediction_date,datafolder = paste0(prep_folder,"/input/"),feature = mask_feature)
    if(!hasvalue(forestras)){forestras=NULL}
    if(!is.na(accuracy_csv)){
      ff_analyze(prediction$predicted_raster > threshold, groundtruth = predset$groundtruthraster,
                 csvfile = accuracy_csv, tile = tile, date = prediction_date,
                 return_polygons = FALSE, append = TRUE, country = country,
                 verbose = verbose, forestmask = forestras)
    }

  }
  if (length(raslist) == 1) {fullras <- raslist[[1]]}else{
    fullras <- do.call(terra::merge,unname(raslist))
  }
  fullras <- terra::mask(fullras,shape)
  fullras <- terra::crop(fullras,shape)
  return(fullras)
}




