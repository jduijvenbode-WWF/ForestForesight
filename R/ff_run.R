#' Train a Model and Predict Deforestation on Raster Data
#'
#' This function trains an XGBoost model using historical data and then predicts deforestation
#' on raster data for a specified date and area.
#'
#' @param shape SpatVector object representing the area of interest. Either `shape` or `country` must be provided.
#' @param country ISO3 country code. Either `shape` or `country` must be provided.
#' @param prediction_dates Dates for prediction in "YYYY-MM-DD" format.
#' @param ff_folder Directory containing the input data.
#' @param train_start Start date for training data in "YYYY-MM-DD" format. Default is NULL.
#' @param train_end End date for training data in "YYYY-MM-DD" format. Default is NULL.
#' @param save_path Path to save the trained model (with extension ".model"). Default is NULL.
#' @param save_path_predictions Path to save the predictions (with extension ".tif"). Default is NULL.
#' @param trained_model Pre-trained model object or path to saved model. If NULL, a new model will be trained. Default is NULL.
#' @param ff_prep_params List of parameters for data preprocessing. See `ff_prep` function for details.
#' @param ff_train_params List of parameters for model training. See `ff_train` function for details.
#' @param threshold Probability threshold for binary classification. Default is 0.5.
#' @param fltr_features Feature dataset used for pre-filtering for training. Default is initialforestcover. Can be more than one
#' @param fltr_condition The condition with value that is used to filter the training dataset based on mask features. Default is ">0". Can be more than one
#' @param accuracy_csv Path to save accuracy metrics in CSV format. Default is NA (no CSV output).
#' @param importance_csv Path to save feature importance metrics in CSV format. Default is NA (no CSV output).
#' @param verbose Logical; whether to display progress messages. Default is TRUE.
#' @param autoscale_sample Logical; Whether to automatically scale the number of samples based on the size of the area and the length of the training period.
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

ff_run <- function(shape = NULL, country = NULL, prediction_dates=NULL,
                   ff_folder,
                   train_start=NULL,
                   train_end=NULL,
                   save_path=NULL,
                   save_path_predictions=NULL,
                   trained_model = NULL,
                   ff_prep_params = NULL,
                   ff_train_params = NULL,
                   threshold = 0.5,
                   fltr_features = "initialforestcover",
                   fltr_condition = ">0",
                   accuracy_csv = NA,
                   importance_csv = NA,
                   verbose=T,
                   autoscale_sample = T) {
  fixed_sample_size <- 6e6
  if (!hasvalue(shape) & !hasvalue(country)) {stop("either input shape or country should be given")}
  if (!hasvalue(shape)) {
    data(countries,envir = environment())
    countries <- terra::vect(countries)
    shape <- countries[which(countries$iso3 == country),]
  }
  #check if all the function parameters have values in the right format
  if (!hasvalue(ff_folder)) {stop("ff_folder is not given")}
  if (!dir.exists(ff_folder)) {stop(paste(ff_folder,"does not exist"))}
  if (!hasvalue(prediction_dates) & !is.null(trained_model)) {stop("prediction_date is not given and model is given so there is no need to run.")}
  if (!hasvalue(prediction_dates)) {prediction_dates <- "3000-01-01"}
  prediction_dates <- sort(prediction_dates)
  #check that end is before start
  if (is.null(trained_model)) {
    if (!hasvalue(train_end)) {train_end <- as.character(lubridate::ymd(prediction_dates[1]) %m-% months(6,abbreviate = F))}
    if (lubridate::ymd(train_end) < lubridate::ymd(train_start)) {stop("train_end is before train_start")}
    if (lubridate::ymd(train_end) > lubridate::ymd(prediction_dates[1])) {warning("train_end is after prediction_date")}
    if ((lubridate::ymd(prediction_dates[1]) - lubridate::ymd(train_end)) < 170 ) {warning("There should be at least 6 months between training and testing/predicting")}
  }


  if (!terra::is.lonlat(shape)) {shape <- terra::project(shape, "epsg:4326")}
  data(gfw_tiles,envir = environment())
  tiles <- terra::vect(gfw_tiles)[shape,]$tile_id


  prep_folder <- file.path(ff_folder,"preprocessed")
  if (!dir.exists(prep_folder)) {stop(paste(prep_folder,"does not exist"))}



  # Train model if not provided
  if (is.null(trained_model)) {
    if (verbose) {cat("Preparing data\n");cat("looking in folder",prep_folder,"\n")}
    if(autoscale_sample & hasvalue(fltr_condition)){
      if (verbose) {cat("Finding optimal sample size based on filter condition\n")}
    ff_prep_params_original = list(datafolder = prep_folder, shape = shape, start = train_start, end = train_end,
                                   fltr_condition = fltr_condition,fltr_features = fltr_features,
                                   sample_size = 1, shrink = "extract",
                                   groundtruth_pattern = "groundtruth6m",label_threshold = 1)
    ff_prep_params_combined = merge_lists(default = ff_prep_params_original, user = ff_prep_params)
    ff_prep_params_combined = merge_lists(default = ff_prep_params_combined, user = list("inc_features" = fltr_features, "adddate" = F, "addxy" = F, "verbose" = F))
    traindata <- do.call(ff_prep, ff_prep_params_combined)
    sample_size <- min(1,fixed_sample_size/length(traindata$data_matrix$features))
    if (verbose) {cat("autoscaled sample size:", round(sample_size,2),"\n")}
    }



    if (verbose) {cat("Preparing data\n");cat("looking in folder",prep_folder,"\n")}
    ff_prep_params_original = list(datafolder = prep_folder, shape = shape, start = train_start, end = train_end,
                                   fltr_condition = fltr_condition,fltr_features = fltr_features,
                                   sample_size = sample_size, verbose = verbose, shrink = "extract",
                                   groundtruth_pattern = "groundtruth6m",label_threshold = 1)
    ff_prep_params_combined = merge_lists(ff_prep_params_original, ff_prep_params)

    traindata <- do.call(ff_prep, ff_prep_params_combined)
    ff_train_params_original = list(traindata$data_matrix, verbose = verbose,
                                    modelfilename = save_path)
    ff_train_params_original = merge_lists(ff_train_params_original, ff_train_params)
    trained_model <- do.call(ff_train, ff_train_params_original)
  }
  if (hasvalue(importance_csv)) {
    if (hasvalue(save_path)) { ff_importance(save_path,importance_csv,append = T)}else{
      ff_importance(trained_model,importance_csv,append = T)
    }
  }
  # Predict
  if (prediction_dates[1] == "3000-01-01") {return(NA)}
  firstdate <- T
  for (prediction_date in prediction_dates) {
    raslist <- list()
    for (tile in tiles) {

      #run the predict function if a model was not built but was provided by the function
      ff_prep_params_original = list(datafolder = prep_folder, tiles = tile, start = prediction_date,
                                     verbose = verbose, fltr_features = fltr_features,
                                     fltr_condition = fltr_condition, groundtruth_pattern = "groundtruth6m",sample_size = 1, label_threshold = 1, shrink = "crop")
      ff_prep_params_combined = merge_lists(ff_prep_params_original, ff_prep_params)
      if (class(trained_model)=="character") {
        if (file.exists(gsub("\\.model","\\.rda",trained_model))) {
          model_features <- list("inc_features"=get(load(gsub("\\.model","\\.rda",trained_model))))
          if (verbose) {cat("pre-trained model only includes the following features:",paste(model_features$inc_features,collapse = ", "),"\n") }
          ff_prep_params_combined <- merge_lists(default = model_features,user = ff_prep_params_combined)
        }
      }
      predset <- do.call(ff_prep, ff_prep_params_combined)

      prediction <- ff_predict(model = trained_model, test_matrix = predset$data_matrix,
                               indices = predset$testindices,
                               templateraster = predset$groundtruthraster,
                               verbose = verbose,certainty = T)
      raslist[[tile]] <- prediction$predicted_raster
      # Analyze prediction

      forestras = get_raster(tile = tile,date = prediction_date,datafolder = paste0(prep_folder,"/input/"),feature = fltr_features)
      if (!hasvalue(forestras)) {forestras <- NULL}
      if (!is.na(accuracy_csv)) {
        pols <- ff_analyze(prediction$predicted_raster > threshold, groundtruth = predset$groundtruthraster,
                        csvfile = accuracy_csv, tile = tile, date = prediction_date,
                        return_polygons = verbose, append = TRUE, country = country,
                        verbose = verbose, forestmask = forestras)
        if (verbose) {if (tile == tiles[1]) {allpols <- pols}else{allpols <- rbind(allpols,pols)}}
      }


    }
    if (verbose & exists("allpols")) {
      precision <- sum(allpols$TP,na.rm = T)/(sum(allpols$TP,na.rm = T) + sum(allpols$FP,na.rm = T))
      recall <- sum(allpols$TP,na.rm = T)/(sum(allpols$TP,na.rm = T) + sum(allpols$FN, na.rm = T))
      cat("date:", prediction_date, "precision:", precision,",recall:",recall,",F0.5",(1.25*precision * recall)/(0.25*precision + recall),"\n")
    }

    if (length(raslist) == 1) {fullras <- raslist[[1]]}else{
      fullras <- do.call(terra::merge,unname(raslist))
    }
    fullras <- terra::mask(fullras,shape)
    fullras <- terra::crop(fullras,shape)
    names(fullras) <- prediction_date
    if (hasvalue(save_path_predictions)) {
      if (length(prediction_dates) > 1) {terra::writeRaster(fullras,paste0(sub("\\.tif$", "", save_path_predictions),"_", prediction_date, ".tif" ), overwrite = T)}
      else {terra::writeRaster(fullras, save_path_predictions,overwrite = T)}
    }

    if (firstdate) {firstdate <- F
    allras <- fullras}else{allras <- c(allras, fullras)}
  }
  return(allras)
}




