#' Prepare data for XGBoost training, validating and predicting
#'
#' This function prepares data for the ForestForesight training and predicting algorithm based on specified parameters.
#'
#' @param datafolder Path to the main data directory, which should contain the "input" and "groundtruth" subdirectories.
#' These subdirectories must include the respective degree folders.Default is the system variable ff_datafolder.
#' @param country ISO3 code of the country or countries for which the data is prepared. Optional if either shape or tiles is given.
#' @param shape SpatVector for which the data is prepared. Optional if either country or tiles is given.
#' @param tiles Vector of tiles in the syntax of e.g., "10N_080W" for which the data is prepared. Optional if either shape or country is given.
#' @param groundtruth_pattern Pattern to identify ground truth files. Default is "groundtruth6m" (groundtruth of future six months in binary format).
#' @param dates vector of dates in the format "YYYY-MM-DD". Default is "2021-01-01".
#' @param inc_features Vector of features to exclusively include in the data preparation.
#' @param exc_features Vector of features to exclude from the data preparation.
#' @param fltr_features Vector of features for filtering data. Default is NULL. Example: 'initialforestcover'.
#' @param fltr_condition Vector of conditions corresponding to `fltr_features`. Each entry should consist of an operator and a value.
#' Should be of the same length as `fltr_features`. Default is NULL. Example: '>0'.
#' @param validation_sample Float between 0 and 1 indicating how much of the training dataset should be used for validation. Default is 0. Advised not to set above 0.3.
#' @param sample_size Fraction size of the random sample. Should be > 0 and <= 1. Default is 0.3.
#' @param adddate Boolean indicating whether to add date-related features ("sinmonth", "month","monthssince2019"). Default is TRUE.
#' @param verbose Boolean indicating whether to display progress messages. Default is TRUE.
#' @param shrink Option to modify the input area when a country is selected. This parameter determines how the spatial extent of the data is adjusted based on the selected country.
#' Options are:
#' \describe{
#'   \item{"none"}{No modification to the input area. The entire extent is used as-is. (Default)}
#'   \item{"crop"}{Crops the input area to the boundaries of the selected country using the exact extent of the country's shape.}
#'   \item{"crop-deg"}{Similar to "crop", but the resulting extent is adjusted to the nearest whole degree (latitude/longitude). This ensures the extent aligns with whole degree boundaries.}
#'   \item{"extract"}{Extracts data only within the boundaries of the selected country. The data is limited to the exact extent of the country's shape, and the function `terra::extract` is used to retrieve the data within this area.}
#' }
#' @param window Set the extent on which to process. Default is NA to derive it from the data.
#' @param label_threshold Threshold for labeling the ground truth. Default is 1, meaning a pixel is labeled as deforested if at least one deforestation event has occurred.
#' If `label_threshold = NA`, the ground truth will represent the total number of deforestation events rather than a binary label.
#' @param addxy Boolean indicating whether to add x and y coordinates as features. Default is FALSE.
#'
#' @return A list containing:
#'   \item{data_matrix}{A list with features and labels for training}
#'   \item{validation_matrix}{A list with features and labels for validation (if validation_sample > 0)}
#'   \item{testindices}{Indices of the filtered samples}
#'   \item{groundtruthraster}{A SpatRaster of the ground truth}
#'   \item{features}{A vector of feature names}
#'
#' @export
#'
#' @references
#' Zillah Calle (2023)
#' Jonas van Duijvenbode (2023)
#'
#' @examples
#' \dontrun{
#' prepared_data <- ff_prep(
#'   datafolder = "path/to/data",
#'   country = "BRA",
#'   dates = ForestForesight::daterange("2022-01-01","2022-12-31"),
#'   fltr_features = "initialforestcover",
#'   fltr_condition = ">0"
#' )
#' }
#'
#' @seealso
#' \code{\link{ff_train}} for training a model with the prepared data
#' \code{\link{ff_predict}} for making predictions using a trained model
#'
#' @keywords machine-learning data-preparation forestry

ff_prep <- function(datafolder=NA, country=NA, shape=NA, tiles=NULL, groundtruth_pattern="groundtruth6m", dates="2023-01-01",
                    inc_features=NA, exc_features=NA, fltr_features=NULL, fltr_condition=NULL, sample_size=0.3, validation_sample=0,
                    adddate=T, verbose=T, shrink="none", window=NA, label_threshold=1, addxy=F){
  ########quality check########
  if (as.Date(min(dates)) < as.Date("2021-01-01")) {stop("the earliest date available is 2021-01-01")}
  if (!hasvalue(country) & !hasvalue(shape)) {shrink <- "none"}
  if (!hasvalue(dates)) {stop("no dates were given")}
  if (!hasvalue(tiles) & !hasvalue(country) & !hasvalue(shape)) {stop("unknown what to process since no tiles or country or shape were given")}
  if (hasvalue(shape) & !class(shape) == "SpatVector") {stop("shape should be of class spatVector")}
  if (!hasvalue(datafolder)) {datafolder <- Sys.getenv("ff_datafolder")}
  if (datafolder == "") {stop("No environment variable for ff_datafolder and no datafolder parameter set")}
  inputdatafolder <- file.path(datafolder,"input")
  groundtruthdatafolder <- file.path(datafolder,"groundtruth")
  ########preprocess for by-country processing########
  data(gfw_tiles,envir = environment())
  tilesvect <- terra::vect(gfw_tiles)
  if (hasvalue(country)) {
    if (verbose) {cat("selecting based on country\n")}
    data(countries,envir = environment())
    countries <- terra::vect(countries)
    shape <- countries[which(countries$iso3 %in% country)]
    tilesvect <- tilesvect[shape]$tile_id
    cat(paste("country contains the following tiles that will be processed:",paste(tilesvect,collapse = ", "),"\n"))
    if (is.null(tiles)) {tiles <- tilesvect}
  }else{
    if (hasvalue(shape)) {
      if ( !terra::is.lonlat(shape)) {shape <- terra::project(shape,"epsg:4326")}
      if (verbose) {cat("selecting based on shape\n")}
      tiles <- tilesvect[shape]$tile_id
      if (verbose) {cat("processing tiles:",paste(tiles,collapse = ", "),"\n")}
    }
  }

  ##########list files and exclude features######
  if (verbose) {cat("searching",inputdatafolder,"for tiles",paste(tiles,collapse = ", "),"\n")}
  allfiles <- as.character(unlist(sapply(tiles,function(x) list.files(path = file.path(inputdatafolder,x),full.names = T,recursive = T,pattern = "tif$"))))
  allgroundtruth = as.character(unlist(sapply(tiles,function(x) list.files(path = file.path(groundtruthdatafolder,x),full.names = T,recursive = T,pattern = "tif$"))))
  allgroundtruth = allgroundtruth[endsWith(gsub(".tif","",allgroundtruth),groundtruth_pattern)]
  allfiles = c(allfiles,allgroundtruth)
  if (length(allfiles) == 0) {stop(paste("no folders with tif-files found that correspond to the given tile id's:",paste(tiles,collapse = ",")))}

  #remove features that are not wanted
  if (!is.na(exc_features[1])) {
    if (verbose) {cat("excluding features\n")}
    exc_indices <- unique(unlist(sapply(exc_features,function(x) which(endsWith(gsub(".tif","",basename(allfiles)),x)))))
    if (length(exc_indices) > 0) {allfiles <- allfiles[-exc_indices]}}
  if (!is.na(inc_features[1])) {
    inc_indices <- unique(unlist(sapply(c(inc_features,groundtruth_pattern),function(x) which(endsWith(gsub(".tif","",basename(allfiles)),x)))))
    if (length(inc_indices > 0)) {allfiles <- allfiles[inc_indices]}}
  if (length(allfiles) == 0) {stop("after including and excluding the requested variables there are no files left")}


  if (length(tiles) > 1) {if (verbose) {cat("No grountruth raster will be returned because multiple tiles are processed together \n")}}
  first <- T
  #######load raster data as matrix#########
  for (tile in tiles) {
    if (exists("extent",inherits = F)) {rm(extent)}
    files <- allfiles[grep(tile,allfiles)]
    if (!hasvalue(shape) & (shrink == "extract")) {
      if (!exists("countries",inherits = F)) {data(countries,envir = environment());borders <- terra::vect(countries)}
      shape <- aggregate(intersect(terra::as.polygons(terra::ext(terra::rast(files[1]))),borders))}
    for (i in dates) {
      if (exists("dts",inherits = F)) {rm(dts)}
      if (verbose) {cat(paste("loading tile data from",tile,"for",i," "))}
      selected_files = select_files_date(i, files)
      #remove groundtruth if it is not of the same month
      if (!(grep(groundtruth_pattern,selected_files) %in% grep(i,selected_files))) {selected_files <- selected_files[-grep(groundtruth_pattern,selected_files)]}
      for (file in selected_files) {if (!exists("extent")) {
        extent <- terra::ext(terra::rast(file))}else{extent <- terra::intersect(extent,terra::ext(terra::rast(file)))}}

      if (shrink %in% c("extract","crop")) {extent <- terra::ext(terra::crop(terra::as.polygons(extent),terra::ext(shape)))}

      if (shrink == "crop-deg") {
        extent <- terra::ext(terra::crop(terra::as.polygons(extent),terra::ext(shape)))
        #crop to the nearest degree by changing the extent
        extent[1] <- floor(extent[1])
        extent[2] <- ceiling(extent[2])
        extent[3] <- floor(extent[3])
        extent[4] <- ceiling(extent[4])
      }
      if (!is.na(window[1])) {extent <- terra::intersect(extent,window)}
      if (verbose) {cat(paste("with extent",round(extent[1],5),round(extent[2],5),round(extent[3],5),round(extent[4],5),"\n"))}
      rasstack <- terra::rast(sapply(selected_files,function(x) terra::rast(x,win = extent)))
      if (length(tiles) > 1) {
        groundtruth_raster = NA
        }else{
          if (first) {
          if (length(grep(groundtruth_pattern,selected_files)) > 0) {
            gtfile = selected_files[grep(groundtruth_pattern,selected_files)]
            groundtruth_raster <- terra::rast(gtfile,win = extent)
          }else{
            if (verbose) {cat("no groundtruth raster was found, first regular raster selected as a template raster.\n")}
            groundtruth_raster <- terra::rast(selected_files[1],win = extent)
            groundtruth_raster[] <- 0}

        }}
      if (shrink == "extract") {
        dts <- terra::extract(rasstack,shape,raw = T,ID = F, xy = addxy)
      }else{

        dts <- as.matrix(rasstack)
        if (addxy) {
          coords <- terra::xyFromCell(rasstack,seq(ncol(rasstack)*nrow(rasstack)))
          dts <- cbind(dts,coords)}
      }

      if (adddate) {dts <- cbind(dts,rep(sin((2*pi*as.numeric(format(as.Date(i),"%m")))/12),nrow(dts)),
                                 rep(as.numeric(format(as.Date(i),"%m")),nrow(dts)),
                                 #add the months since 2019
                                 rep(round(as.numeric(lubridate::as.period(as.Date(i) - as.Date("2019-01-01"),"months"),"months")),nrow(dts)))
      }


      dts[is.na(dts)] <- 0
      newcolnames <- c(gsub(".tif","",c(sapply(basename(selected_files),function(x) strsplit(x,"_")[[1]][4]))))
      if (addxy) {newcolnames <- c(newcolnames,"x","y")}
      if (adddate) {newcolnames <- c(newcolnames,"sinmonth", "month","monthssince2019")}
      colnames(dts) <- newcolnames
      dts <- dts[,order(colnames(dts))]

      # filter on filter conditions
      filterresult <- filter_by_feature(fltr_features,fltr_condition,dts,verbose = verbose)
      dts <- filterresult$filtered_matrix
      sf_indices <- filterresult$filtered_indices
      #take a random sample if that was applied

      if (sample_size < 1) {
        sample_indices <- sample(seq(nrow(dts)),max(round(nrow(dts)*sample_size),1))
        dts <- dts[sample_indices,]
        sf_indices <- sf_indices[sample_indices]}
      if (hasvalue(dim(dts))) {
      if (first) {
        fdts <- dts
        allindices = sf_indices
      }else {
        allindices <- c(allindices,sf_indices + length(allindices))
        common_cols <- intersect(colnames(dts), colnames(fdts))
        notin1 <- colnames(dts)[which(!(colnames(dts) %in% common_cols))]
        notin2 <- colnames(fdts)[which(!(colnames(fdts) %in% common_cols))]
        if (length(c(notin1,notin2)) > 0) {warning(paste(i,": the following columns are dropped because they are not present in the entire time series: ",paste(c(notin1,notin2),collapse = ", ")))}
        # Subset matrices based on common column names
        # Merge matrices by column names
        fdts <- rbind(fdts[, common_cols, drop = FALSE], dts[, common_cols, drop = FALSE])
      }
      fdts <- fdts[,order(colnames(fdts))]
      first <- F}}
    if (verbose) {cat(paste("loading finished, features:",paste(newcolnames,collapse = ", "),"\n"))}
  }
  ######filter data based on features#######
  #filter training data on features that have been declared

  #######create groundtruth data#######
  #split data into feature data and label data
  groundtruth_index <- which(colnames(fdts) == groundtruth_pattern)
  if (length(groundtruth_index) == 1) {
    data_label <- fdts[,groundtruth_index]
    if (hasvalue(label_threshold)) {
      data_label <- as.numeric(data_label > label_threshold)
      if (inherits(groundtruth_raster, "SpatRaster")) {groundtruth_raster <- as.numeric(groundtruth_raster > label_threshold)}}

    fdts <- fdts[,-groundtruth_index]
  }else{
    if (verbose) {warning("no groundtruth rasters found")}
    data_label <- NA
  }

  #make sure that label data is binary
  ##########create validation sample#######
  if (validation_sample > 0) {
    sample_indices <- sample(seq(nrow(fdts)),round(validation_sample*nrow(fdts)))
    data_matrix <- list(features = fdts[-sample_indices,], label = data_label[-sample_indices])
    validation_matrix <- list(features = fdts[sample_indices,], label = data_label[sample_indices])
  }else{
    data_matrix <- list(features = fdts, label = data_label)
    validation_matrix <- NA
  }

  ##########output data####
  if (hasvalue(data_matrix$label)) {if (sum(data_matrix$label) == 0) {warning("data contains no actuals, all labels are 0")}}
  return(list("data_matrix" = data_matrix,"validation_matrix" = validation_matrix,"testindices" = allindices,"groundtruthraster" = groundtruth_raster,features = colnames(fdts)))
}

