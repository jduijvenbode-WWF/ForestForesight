#' Train an XGBoost Model for ForestForesight
#'
#' This function trains an XGBoost model with optimized default parameters derived from worldwide data analysis.
#'
#' @param train_matrix An xgb.DMatrix object or a list containing 'features' and 'label' for training.
#' @param validation_matrix An xgb.DMatrix object or a list containing 'features' and 'label' for validation. Default is NA.
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
#' @param xgb_model Previously trained model to continue training from. Can be an "xgb.Booster" object, raw data, or a file name. Default is NULL.
#' @param modelfilename String specifying where to save the model. Should end with ".model" extension.
#' @param features Vector of feature names used in the training dataset. Required when modelfilename is provided.
#' @param objective Learning objective. Default is "binary:logistic".
#'
#' @return A trained XGBoost model (xgb.Booster object).
#'
#' @examples
#' \dontrun{
#' # Prepare your data
#' train_data <- list(features = matrix(runif(1000), ncol = 10),
#'                    label = sample(0:1, 100, replace = TRUE))
#'
#' # Train the model
#' model <- ff_train(train_matrix = train_data,
#'                   nrounds = 100,
#'                   eta = 0.05,
#'                   max_depth = 6,
#'                   modelfilename = "forest_model.model",
#'                   features = colnames(train_data$features))
#' }
#'
#' @import xgboost
#' @export
#'
#' @references
#' Jonas van Duijvenbode (2023)
#' Zillah Calle (2023)
#'
#' @seealso
#' \code{\link{ff_prep}} for preparing data for this function
#' \code{\link{ff_predict}} for making predictions using the trained model
#'
#' @keywords machine-learning xgboost forestry

train_predict_raster <- function(shape = NULL, country = NULL, prediction_date,
                                  ff_folder,
                                  train_start=NULL,
                                  train_end=NULL,
                                  save_path=NULL,
                                  trained_model = NULL,
                                  ff_prep_params = NULL,
                                  ff_train_params = NULL,
                                  threshold = 0.5,
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
    forestras = get_raster(tile = tile,date = prediction_date,datafolder = paste0(prep_folder,"/input/"),feature = "initialforestcover")
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




