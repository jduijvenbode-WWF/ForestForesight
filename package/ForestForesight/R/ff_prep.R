#' Prepare data for XGboost training, validating and predicting.
#'
#' This function prepares data for the ForestForesight training and predicting algorithm based on specified parameters.
#'
#' @param datafolder Path to the data folder. Default is the system variable xgboost_datafolder. should contain the degrees folders
#' @param country Country or countries for which the data is prepared. Is optional when tiles are given. Should be the ISO3 code.
#' @param tiles Vector of tiles in the syntax of e.g. 10N_080W.
#' @param groundtruth_pattern Pattern to identify ground truth files. Default is groundtruth6mbin (groundtruth of future six months in binary format).
#' @param start Start date for training data in the format "YYYY-MM-DD". Default is "2021-01-01".
#' @param end End date for training data in the format "YYYY-MM-DD". Default is NA to only process the start month.
#' @param inc_features Vector of included features. States which features to include in the data preparation.
#' @param exc_features Vector of excluded features. States which features to exclude in the data preparation.
#' @param fltr_features vector of features for filtering data. Default is empty. EXAMPLE: 'initialforestcover'. needs to be combined with fltr_condition of the same length
#' @param fltr_condition Vector of filtering conditions. Default is empty. EXAMPLE:'>0'. Should consist of operator and value and needs to be combined with fltr_features of same length vector
#' @param validation_sample float between 0 and 1 that indicates how much of the training dataset should be used for validation. Default is 0. Advised is to not set it above 0.3
#' @param sample_size Fraction size of the random sample. Should be bigger than 0 and smaller or equal to 1. Default is 1
#' @param relativedate Boolean indicating whether the date is relative. Default is \code{TRUE}.
#' @param shrink Option to shrink the input area if a country was selected. Use none to keep all the data within the tile. Use crop to crop the extent, crop-deg to crop to the nearest outer degree and use extract to keep only the values that overlap with the country
#' @param window Set the extent on which to process. normally NA to not use this option and derive it from the data
#'
#' @return A prepared dataset for machine learning.
#' @export
#'
#'
#' @references
#' Jonas van Duijvenbode (2023)
#' Zillah Calle (2023)
#'
#' @keywords XGBoost data preparation
#' @rdname ff_prep
#' @name ff_prep


ff_prep <- function(datafolder=NA, country=NA, tiles=NULL, groundtruth_pattern="groundtruth6m", start="2021-01-01", end=NA,
                 inc_features=NA, exc_features=NA, fltr_features=NULL, fltr_condition=NULL, sample_size=1, validation_sample=0,
                 relativedate=T, sampleraster=T, verbose=F, shrink="none", window=NA, label_threshold=NA, addxy=F){
  ########quality check########
  if (as.Date(start) < as.Date("2021-01-01")) {stop("the earliest date available is 2021-01-01")}
  if (!hasvalue(country)) {shrink <- "none"}
  if (!hasvalue(start)) {stop("no start date given")}
  if (!hasvalue(tiles) & !hasvalue(country)) {stop("unknown what to process since no tiles or country were given")}
  if (!hasvalue(end)) {end <- start}
  if (!hasvalue(datafolder)) {datafolder <- Sys.getenv("xgboost_datafolder")}
  if (datafolder == "") {stop("No environment variable for xgboost_datafolder and no datafolder parameter set")}
  inputdatafolder <- file.path(datafolder,"input")
  groundtruthdatafolder <- file.path(datafolder,"groundtruth")
  if (sampleraster & ((validation_sample > 0) | sample_size < 1)) {
    sampleraster <- F
    if (verbose) {warning("No template raster will be returned because the resulting matrix is sampled by either subsampling or validation sampling")}}
  ########preprocess for by-country processing########
  data(gfw_tiles,envir = environment())
  tilesvect <- terra::vect(gfw_tiles)
  if (hasvalue(country)) {
    if (verbose) {cat("selecting based on country\n")}
    data(countries,envir = environment())
    borders <- terra::vect(countries)
    selected_country <- borders[which(borders$iso3 %in% country)]
    tilesvect <- tilesvect[selected_country]$tile_id
    cat(paste("country contains the following tiles that will be processed:",paste(tilesvect,collapse = ", "),"\n"))
    if (is.null(tiles)) {tiles <- tilesvect}
  }

  ##########list files and exclude features######
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
    inc_indices <- unique(unlist(sapply(inc_features,function(x) which(endsWith(gsub(".tif","",basename(allfiles)),x)))))
    if (length(inc_indices > 0)) {allfiles <- allfiles[inc_indices]}}
  if (length(allfiles) == 0) {stop("after including and excluding the requested variables there are no files left")}
  #create the range between start and end date
  daterange <- daterange(start,end)
  first <- T
  if (length(tiles) > 1) {warning("No template raster will be returned because multiple tiles are processed together")
    sampleraster <- F}
  #######load raster data as matrix#########
  for (tile in tiles) {
    if (exists("extent",inherits = F)) {rm(extent)}
    files <- allfiles[grep(tile,allfiles)]
    if (!hasvalue(country) & (shrink == "extract")) {
      if (!exists("countries",inherits = F)) {data(countries,envir = environment());borders <- terra::vect(countries)}
      selected_country <- aggregate(intersect(terra::as.polygons(terra::ext(terra::rast(files[1]))),borders))}
    for (i in daterange) {
      if (exists("dts",inherits = F)) {rm(dts)}
      if (verbose) {cat(paste("loading tile data from",tile,"for",i," "))}
      selected_files = select_files_date(i, files)
      #remove groundtruth if it is not of the same month
      if (!(grep(groundtruth_pattern,selected_files) %in% grep(i,selected_files))) {selected_files <- selected_files[-grep(groundtruth_pattern,selected_files)]}
      for (file in selected_files) {if (!exists("extent")) {extent <- terra::ext(terra::rast(file))}else{extent <- terra::intersect(extent,terra::ext(terra::rast(file)))}}

      if (shrink %in% c("extract","crop")) {extent <- terra::ext(terra::crop(terra::as.polygons(extent),terra::ext(selected_country)))}

      if (shrink == "crop-deg") {
        extent <- terra::ext(terra::crop(terra::as.polygons(extent),terra::ext(selected_country)))
        #crop to the nearest degree by changing the extent
        extent[1] <- floor(extent[1])
        extent[2] <- ceiling(extent[2])
        extent[3] <- floor(extent[3])
        extent[4] <- ceiling(extent[4])
      }
      if (!is.na(window[1])) {extent <- terra::intersect(extent,window)}
      if (verbose) {cat(paste("with extent",extent[1],extent[2],extent[3],extent[4],"\n"))}
      rasstack <- terra::rast(sapply(selected_files,function(x) terra::rast(x,win = extent)))

      if (first) {
        if (sampleraster) {
          if (length(grep(groundtruth_pattern,selected_files)) > 0) {
            groundtruth_raster <- terra::rast(selected_files[grep(groundtruth_pattern,selected_files)],win = extent)
          }else{
            groundtruth_raster <- terra::rast(selected_files[1],win = extent)
            groundtruth_raster[] <- 0}}else{
              groundtruth_raster <- terra::rast(selected_files[1],win = extent)
              groundtruth_raster[] <- 0}
      }
      if (shrink == "extract") {
        dts <- terra::extract(rasstack,selected_country,raw = T,ID = F, xy = addxy)
      }else{

        dts <- as.matrix(rasstack)
        if (addxy) {
          coords <- terra::xyFromCell(rasstack,seq(ncol(rasstack)*nrow(rasstack)))
          dts <- cbind(dts,coords)}
      }

      if (relativedate) {dts <- cbind(dts,rep(sin((2*pi*as.numeric(format(as.Date(i),"%m")))/12),nrow(dts)), rep(as.numeric(format(as.Date(i),"%m")),nrow(dts)))}

      dts[is.na(dts)] <- 0
      newcolnames <- c(gsub(".tif","",c(sapply(basename(selected_files),function(x) strsplit(x,"_")[[1]][4]))))
      if (addxy) {newcolnames <- c(newcolnames,"x","y")}
      if (relativedate) {newcolnames <- c(newcolnames,"sinmonth", "month")}
      colnames(dts) <- newcolnames

      dts <- dts[,order(colnames(dts))]
      #take a random sample if that was applied
      if (sample_size < 1) {dts <- dts[sample(seq(nrow(dts)),max(round(nrow(dts)*sample_size),1)),]}
      if (first) {
        fdts <- dts
      }else{
        common_cols <- intersect(colnames(dts), colnames(fdts))
        notin1 <- colnames(dts)[which(!(colnames(dts) %in% common_cols))]
        notin2 <- colnames(fdts)[which(!(colnames(fdts) %in% common_cols))]
        if (length(c(notin1,notin2)) > 0) {warning(paste("the following columns are dropped because they are not present in the entire time series: ",paste(c(notin1,notin2),collapse = ", ")))}
        # Subset matrices based on common column names
        # Merge matrices by column names
        fdts <- rbind(fdts[, common_cols, drop = FALSE], dts[, common_cols, drop = FALSE])
      }
      fdts <- fdts[,order(colnames(fdts))]
      first <- F}
    if (verbose) {cat(paste("loading finished, features:",paste(newcolnames,collapse = ", "),"\n"))}
  }
  ######filter data based on features#######
  #filter training data on features that have been declared
  sfa_indices <- c()
  if (length(fltr_features) > 0) {
    if (verbose) {cat(paste("filtering features\n"))}
    for (i in seq(length(fltr_features))) {
      operator <- gsub("[[:alnum:]]", "", fltr_condition[i])
      value <- gsub("[^0-9]", "", fltr_condition[i])
      filtercolumn <- which(colnames(fdts) == fltr_features[i])
      if (operator == ">") {sf_indices <- which(fdts[,filtercolumn] > value)}
      if (operator == "<") {sf_indices <- which(fdts[,filtercolumn] < value)}
      if (operator == "==") {sf_indices <- which(fdts[,filtercolumn] == value)}
      if (operator == "!=") {sf_indices <- which(fdts[,filtercolumn] != value)}
      if (operator == ">=") {sf_indices <- which(fdts[,filtercolumn] >= value)}
      if (operator == "<=") {sf_indices <- which(fdts[,filtercolumn] <= value)}

      if (verbose) {cat(paste("filtering feature",fltr_features[i],"on",fltr_condition[i],"\n"))}
      if (length(sfa_indices) == 0) {sfa_indices <- c(sfa_indices,sf_indices)}else{sfa_indices <- intersect(sfa_indices,sf_indices)}
    }
    sf_indices <- unique(sfa_indices)
  }else{sf_indices <- NULL}
  if (length(sf_indices) > 0) {
    fdts <- fdts[sf_indices,]
  }
  #######create groundtruth data#######
  #split data into feature data and label data
  groundtruth_index <- which(colnames(fdts) == groundtruth_pattern)
  if (length(groundtruth_index) == 1) {
    data_label <- fdts[,groundtruth_index]
    if (hasvalue(label_threshold)) {
      data_label <- as.numeric(data_label > label_threshold)
      groundtruth_raster <- as.numeric(groundtruth_raster > label_threshold)}

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
  if (hasvalue(data_matrix$label)) {if (sum(data_matrix$label) == 0) {stop("data contains no actuals, all labels are 0")}}
  return(list("data_matrix" = data_matrix,"validation_matrix" = validation_matrix,"testindices" = sf_indices,"groundtruthraster" = groundtruth_raster,features = colnames(fdts)))
}

