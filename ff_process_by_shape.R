
train_predict_raster <- function(shape = NULL, country = NULL, prediction_date,
                                  ff_folder,
                                  train_start="2021-01-01",
                                  train_end="2021-05-01",
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
                       fltr_condition = ">0",fltr_features = "landpercentage",
                       sample_size = 0.03, verbose = verbose, shrink = "extract",
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
                       verbose = verbose, fltr_features = "landpercentage",
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




