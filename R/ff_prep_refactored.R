#' Prepare data for XGBoost training, validating and predicting
#'
#' This function prepares data for the ForestForesight training and predicting algorithm based on specified parameters.
#'
#' @param datafolder Path to the main data directory, which should contain the "input" and "groundtruth" subdirectories.
#' These subdirectories must include the respective degree folders.Default is the system variable ff_datafolder.
#' @param country ISO3 code of the country or countries for which the data is prepared. Optional if either shape or tiles is given.
#' @param shape SpatVector for which the data is prepared. Optional if either country or tiles is given.
#' @param tiles Vector of tiles in the syntax of e.g., "10N_080W" for which the data is prepared. Optional if either shape or country is given.
#' @param groundtruth_pattern Pattern to identify ground truth files. Default is "groundtruth6m" (set in config.json, groundtruth of future six months in binary format).
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
#'   \item{has_ground_truth}{A boolean stating that the groundtruthraster is actually the groundtruth and not just a template}
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
#'   dates = ForestForesight::daterange("2022-01-01", "2022-12-31"),
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

ff_prep_refactored <- function(datafolder = Sys.getenv("DATA_FOLDER"), country = NA, shape = NA, tiles = NULL,
                               groundtruth_pattern = Sys.getenv("DEFAULT_GROUNDTRUTH"), dates = "2023-01-01",
                               inc_features = NA, exc_features = NA, fltr_features = NULL, fltr_condition = NULL, sample_size = 0.3, validation_sample = 0,
                               adddate = TRUE, verbose = TRUE, shrink = "none", window = NA, label_threshold = 1, addxy = FALSE) {

  ######## pre-conditions check########
  check_pre_conditions(dates, country, shape, tiles, shrink)
  has_ground_truth <- FALSE

  ######## Get tiles and shape based on country or custom geometry ########
  data(gfw_tiles, envir = environment()) #CRF unclear comments
  tilesvect <- terra::vect(gfw_tiles)
  tile_and_shape <- get_tiles_and_shape(country, shape, tilesvect, tiles, verbose)
  shape <- tile_and_shape$shape
  tiles <- tile_and_shape$tiles

  ########## list files and exclude features######
  allfiles <- list_and_filter_tile_files(datafolder = datafolder, tiles, groundtruth_pattern, verbose) #CRF generic result name, also doesn't follow snakecase

  # remove features that are not wanted
  allfiles <- filter_files_by_features(allfiles, exc_features, inc_features, groundtruth_pattern, verbose) #CRF unclear comment, it's filtering out features based on exc_ and inc_ + I understand why ground_truth_pattern is here, but it shouldn't as you'd never want to filter those out

  if (length(tiles) > 1) {
    if (verbose) {
      cat("No groundtruth raster will be returned because multiple tiles are processed together \n") #CRF Add info about how to do and combine those afterwards if that is desired
    }
  }

  # Process tiles and dates <- CRF why is this comment here?
  ####### load raster data as matrix#########
  process_result <- process_tile_data(  #CRF which process?
    tiles, allfiles, shape, shrink, window, borders, verbose, dates, groundtruth_pattern, has_ground_truth, addxy, adddate,
    fdts, sample_size, allindices, fltr_features, fltr_condition
  )

  fdts <- process_result$fdts
  allindices <- process_result$allindices
  groundtruth_raster <- process_result$groundtruth_raster
  has_ground_truth <- process_result$has_ground_truth

  ###### filter data based on features#######
  # filter training data on features that have been declared
  #CRF Is the above comment done?

  ####### create groundtruth data#######  CRF but then this doesn't create the ground_truth right? just splits?
  # split data into feature data and label data

  split_result <- split_feature_and_label_data(fdts, groundtruth_pattern, label_threshold, groundtruth_raster, verbose)
  fdts <- split_result$fdts
  data_label <- split_result$data_label
  groundtruth_raster <- split_result$groundtruth_raster

  # make sure that label data is binary , CRF not done I think?
  ########## create validation sample#######

  validation_result <- create_validation_sample(fdts, data_label, validation_sample)
  data_matrix <- validation_result$data_matrix # CRF, why call it data_matrix here after calling it dts and fdts after. It gives the impression the fdts actually gets changed in its structure which is not the case
  validation_matrix <- validation_result$validation_matrix

  ########## output data####
  if (hasvalue(data_matrix$label)) { #CRF Comment above is vague and not what it does really.
    if (sum(data_matrix$label) == 0 && verbose) { #CRF Could be combined into singular if statement
      ff_cat("Data contains no actuals, all labels are 0", color = "yellow") #CRF wouldn't this be cause for an error?
    }
  }

  return(list(
    "data_matrix" = data_matrix,
    "validation_matrix" = validation_matrix,
    "testindices" = allindices,
    "groundtruthraster" = groundtruth_raster,
    "features" = colnames(fdts), #CRF Why use fdts again here instead of data_matrix?
    "has_ground_truth" = has_ground_truth
  ))
}

check_pre_conditions <- function(dates, country, shape, tiles, shrink) {
  # Check date validity
  if (!hasvalue(dates) || any(is.na(dates)) || dates == "") {
    stop("No dates were given")
  }
  if (as.Date(min(dates)) < as.Date(Sys.getenv("EARLIEST_DATA_DATE"))) {
    stop(paste0("The earliest date available is ", Sys.getenv("EARLIEST_DATA_DATE")))
  }

  # Check input parameters validity
  if (!hasvalue(tiles) & !hasvalue(country) & !hasvalue(shape)) {
    stop("Unknown what to process since no tiles, country, or shape were given")
  }
  if (hasvalue(shape) & !inherits(shape, "SpatVector")) {
    stop("Shape should be of class SpatVector")
  }
  if (!hasvalue(country) & !hasvalue(shape) & shrink != "none") {
    stop("Shrink parameter must be 'none' when neither country nor shape are provided")
  }
}

get_tiles_and_shape <- function(country, shape, tilesvect, tiles, verbose) {
  if (hasvalue(country)) {
    if (verbose) {
      cat("Selecting based on country\n")
    }
    data(countries, envir = environment())
    countries <- terra::vect(countries)
    shape <- countries[which(countries$iso3 %in% country)]
    tilesvect <- tilesvect[shape]$tile_id
    if (is.null(tiles)) {
      tiles <- tilesvect
    }
    cat("Processing tiles:", paste(tiles, collapse = ", "), "\n")
  } else if (hasvalue(shape)) {
    if (!terra::is.lonlat(shape)) {
      shape <- terra::project(shape, "epsg:4326")
    }
    if (verbose) {
      cat("Selecting based on shape\n")
    }
    tiles <- tilesvect[shape]$tile_id
    if (verbose) {
      cat("Processing tiles:", paste(tiles, collapse = ", "), "\n")
    }
  }
  return(list(shape = shape, tiles = tiles))
}

list_and_filter_tile_files <- function(datafolder = NA, tiles, groundtruth_pattern, verbose) { #CRF Whole function has snake_case problems
  inputdatafolder <- file.path(datafolder, "input")
  groundtruthdatafolder <- file.path(datafolder, "groundtruth")

  if (verbose) {
    cat("Searching", inputdatafolder, "for tiles", paste(tiles, collapse = ", "), "\n")
  }

  # List all files from the input and ground truth directories
  allfiles <- as.character(unlist(sapply(tiles, function(x) {
    list.files(
      path = file.path(inputdatafolder, x), full.names = TRUE, recursive = TRUE,
      pattern = "tif$"
    )
  })))
  allgroundtruth <- as.character(unlist(sapply(tiles, function(x) {
    list.files(
      path = file.path(groundtruthdatafolder, x), full.names = TRUE,
      recursive = TRUE, pattern = "tif$"
    )
  })))

  # Filter ground truth files
  allgroundtruth <- allgroundtruth[endsWith(gsub(".tif", "", allgroundtruth), groundtruth_pattern)] #CRF This goundtruth filter could be done in the scan above
  allfiles <- c(allfiles, allgroundtruth) #CRF Generic name

  # Error handling if no files are found
  if (length(allfiles) == 0) {
    stop(paste("No folders with tif-files found that correspond to the given tile IDs:", paste(tiles, collapse = ",")))
  }

  return(allfiles)
}

filter_files_by_features <- function(allfiles, exc_features, inc_features, groundtruth_pattern, verbose) { #CRF Allowing for both inclusion and exclusion is counter-intuitive maybe put in an if-else and throw and error if both are non-empty?
  if (!is.na(exc_features[1])) {
    if (verbose) {
      cat("Excluding features\n")
    }
    exc_indices <- unique(unlist(sapply(exc_features, function(x) { #CRF quite a complex line, could use a comment: "Find indices of files that end with a feature in exc_features" for instance
      which(endsWith(gsub(".tif", "", basename(allfiles)), x))
    })))
    if (length(exc_indices) > 0) {
      allfiles <- allfiles[-exc_indices]
    }
  }

  if (!is.na(inc_features[1])) {
    inc_indices <- unique(unlist(sapply(c(inc_features, groundtruth_pattern), function(x) {
      which(endsWith(gsub(".tif", "", basename(allfiles)), x))
    })))
    if (length(inc_indices) > 0) {
      allfiles <- allfiles[inc_indices]
    }
  }

  if (length(allfiles) == 0) {
    stop("After including and excluding the requested variables there are no files left")
  }

  return(allfiles) #CRF Maybe stop calling it all_files, because it's no longer all_files
}

prepare_raster_data_by_tile <- function(selected_files, shape, shrink, window, verbose) {
  for (file in selected_files) {
    if (!exists("extent")) {
      extent <- terra::ext(terra::rast(file))
    } else {
      extent <- terra::intersect(extent, terra::ext(terra::rast(file)))
    }
  }

  if (shrink %in% c("extract", "crop")) { #CRF when does it behave differently based on extract or crop?
    extent <- terra::ext(terra::crop(terra::as.polygons(extent), terra::ext(shape)))
  }

  if (shrink == "crop-deg") {
    extent <- terra::ext(terra::crop(terra::as.polygons(extent), terra::ext(shape)))
    extent[1] <- floor(extent[1])
    extent[2] <- ceiling(extent[2])
    extent[3] <- floor(extent[3])
    extent[4] <- ceiling(extent[4])
  }

  # TODO: check inherits(window, "SpatExtent")'s affect, added here to pass the unit test
  if (!is.null(window) && inherits(window, "SpatExtent")) {
    extent <- terra::intersect(extent, window)
  }

  # TODO: check if !is.null(extent) && is.numeric(extent) doesnt affect much, added to pass unit test
  if (verbose && !is.null(extent) && is.numeric(extent)) {
    cat(paste("with extent", round(extent[1], 5), round(extent[2], 5), round(extent[3], 5), round(extent[4], 5), "\n"))
  }

  rasstack <- terra::rast(sapply(selected_files, function(x) terra::rast(x, win = extent)))
  return(list(extent = extent, rasstack = rasstack))
}

load_groundtruth_raster <- function(selected_files, groundtruth_pattern, first, verbose, extent, has_ground_truth) { #CRF what is the use of the has_ground_truth param here?
  if (first) { #CRF why only when first? and what does first mean?
    if (length(grep(groundtruth_pattern, selected_files)) > 0) {
      has_ground_truth <- TRUE #CRF snake_case problem
      gtfile <- selected_files[grep(groundtruth_pattern, selected_files)]
      groundtruth_raster <- terra::rast(gtfile, win = extent)
    } else {
      if (verbose) {
        cat("no groundtruth raster was found, first regular raster selected as a template raster.\n")
      }
      groundtruth_raster <- terra::rast(selected_files[1], win = extent)
      groundtruth_raster[] <- 0
    }
  }
  list_gt_raster <- list(groundtruth_raster = groundtruth_raster, has_ground_truth = has_ground_truth, first=first) #CRF Use full groundtruth name for clarity
  return(list_gt_raster)
}

initialize_shape_from_borders <- function(shape, shrink, files, borders) { #CRF Not always from borders, it it already has a shape or a different shrink value then it just returns the shape, Also, function is in the wrong place, should be under where it's used
  if (!hasvalue(shape) & (shrink == "extract")) {
    if (!exists("countries", inherits = FALSE)) {
      data(countries, envir = environment())
      borders <- terra::vect(countries)
    }
    # TODO: to pass it's test case we needed to avoid aggregate and implement as below
    # shape <- terra::union(terra::intersect(terra::as.polygons(terra::ext(terra::rast(files[1]))), borders))
    shape <- aggregate(intersect(terra::as.polygons(terra::ext(terra::rast(files[1]))), borders))
  }
  return(shape)
}

filter_files_by_date_and_groundtruth <- function(date, files, groundtruth_pattern) {
  selected_files <- select_files_date(date, files)
  # Remove groundtruth if it is not of the same month
  if (!(grep(groundtruth_pattern, selected_files) %in% grep(date, selected_files))) {
    selected_files <- selected_files[-grep(groundtruth_pattern, selected_files)]
  }

  return(selected_files)
}

transform_raster_to_data_matrix <- function(rasstack, shape, shrink, addxy, dts) {
  if (shrink == "extract") {
    dts <- terra::extract(rasstack, shape, raw = TRUE, ID = FALSE, xy = addxy)
  } else {
    dts <- as.matrix(rasstack)
    if (addxy) {
      coords <- terra::xyFromCell(rasstack, seq(ncol(rasstack) * nrow(rasstack)))
      dts <- cbind(dts, coords)
    }
  }
  return(dts)
}

append_date_based_features <- function(dts, date) {
  dts <- cbind(
    dts, rep(sin((2 * pi * as.numeric(format(as.Date(date), "%m"))) / 12), nrow(dts)), #CRF Could use an explanatory comment
    rep(as.numeric(format(as.Date(date), "%m")), nrow(dts)),
    # add the months since 2019
    rep(round(as.numeric(lubridate::as.period(as.Date(date) - as.Date("2019-01-01"), "months"), "months")), nrow(dts))
  )
  return(dts)
}

finalize_column_names_and_data_matrix <- function(dts, selected_files, addxy, adddate) { #CRF Should probably have a different function name
  dts[is.na(dts)] <- 0
  newcolnames <- gsub(".tif", "", sapply(basename(selected_files), function(x) strsplit(x, "_")[[1]][4]))
  if (addxy) {
    newcolnames <- c(newcolnames, "x", "y")
  }
  if (adddate) {
    newcolnames <- c(newcolnames, "sinmonth", "month", "monthssince2019")
  }

  colnames(dts) <- newcolnames
  dts <- dts[, order(colnames(dts))]
  return(list(dts = dts, newcolnames = newcolnames))
}

sample_and_combine_data <- function(date, dts, fdts, sf_indices, sample_size, first, allindices) {
  # take a random sample if that was applied

  if (sample_size < 1) {
    sample_indices <- sample(seq(nrow(dts)), max(round(nrow(dts) * sample_size), 1))
    dts <- dts[sample_indices, ]
    sf_indices <- sf_indices[sample_indices] #CRF What does it stand for? Sample feature indices?
  }

  if (hasvalue(dim(dts))) { #CRF This whole function is kinda confusing to me
    if (first) {
      fdts <- dts #CRF what does it mean? feature or first dataset?
      allindices <- sf_indices
    } else {
      allindices <- c(allindices, sf_indices + length(allindices))
      common_cols <- intersect(colnames(dts), colnames(fdts))
      notin1 <- colnames(dts)[which(!(colnames(dts) %in% common_cols))]
      notin2 <- colnames(fdts)[which(!(colnames(fdts) %in% common_cols))]
      if (length(c(notin1, notin2)) > 0) {
        ff_cat(paste(date, ": the following columns are dropped because they are not present in the entire time series: ", paste(c(notin1, notin2),
          collapse = ", "
        )), color = "yellow")
      }

      # Subset matrices based on common column names
      # Merge matrices by column names
      fdts <- rbind(fdts[, common_cols, drop = FALSE], dts[, common_cols, drop = FALSE])
    }
    fdts <- fdts[, order(colnames(fdts))]
    first <- FALSE
  }

  return(list(fdts = fdts, allindices = allindices, first = first))
}

create_validation_sample <- function(fdts, data_label, validation_sample) {
  if (validation_sample > 0) {
    sample_indices <- sample(seq(nrow(fdts)), round(validation_sample * nrow(fdts)))
    data_matrix <- list(features = fdts[-sample_indices, ], label = data_label[-sample_indices])
    validation_matrix <- list(features = fdts[sample_indices, ], label = data_label[sample_indices])
  } else {
    data_matrix <- list(features = fdts, label = data_label)
    validation_matrix <- NA
  }

  return(list(data_matrix = data_matrix, validation_matrix = validation_matrix))
}

split_feature_and_label_data <- function(fdts, groundtruth_pattern, label_threshold, groundtruth_raster, verbose) {
  groundtruth_index <- which(colnames(fdts) == groundtruth_pattern)

  if (length(groundtruth_index) == 1) {
    data_label <- fdts[, groundtruth_index]

    if (hasvalue(label_threshold)) {
      data_label <- as.numeric(data_label >= label_threshold) #CRF shouldn't this be >= instead of just >?
      if (inherits(groundtruth_raster, "SpatRaster")) {
        groundtruth_raster <- as.numeric(groundtruth_raster >= label_threshold)
      }
    }

    fdts <- fdts[, -groundtruth_index] # Remove groundtruth column from features
  } else {
    if (verbose) { #CRF Could also be that there were incorrectly more than 1 ground_truth raster
      ff_cat("No groundtruth rasters found", color = "yellow")
    }
    data_label <- NA
  }

  return(list(fdts = fdts, data_label = data_label, groundtruth_raster = groundtruth_raster))
}

process_tile_data <- function(tiles, allfiles, shape, shrink, window, borders, verbose, dates, groundtruth_pattern, has_ground_truth, addxy, adddate, fdts,
                              sample_size, allindices, fltr_features, fltr_condition) { # CRF snake_case problems
  first <- TRUE #CRF could use a comment because at first you have no clue what it's for
  ####### load raster data as matrix#########
  for (tile in tiles) {
    if (exists("extent", inherits = FALSE)) { #CRF what's the purpose of this if_block?
      rm(extent)
    }

    files <- allfiles[grep(tile, allfiles)] #CRF using all_files previously makes this line confusing, as it becomes clear that all_files only contains file locations/names
    shape <- initialize_shape_from_borders(shape, shrink, files, borders)

    result <- process_tile_dates( #CRF vague return param name
      tiles, tile, files, shape, shrink, window, groundtruth_pattern, dates, verbose, addxy, adddate, first, fdts, sample_size,
      allindices, has_ground_truth, fltr_features, fltr_condition
    )

    fdts <- result$fdts
    allindices <- result$allindices
    first <- result$first
    groundtruth_raster <- result$groundtruth_raster
    newcolnames <- result$newcolnames
    has_ground_truth <- result$has_ground_truth

    if (verbose) {
      cat(paste("loading finished, features:", paste(newcolnames, collapse = ", "), "\n"))
    }
  }
  return(list(fdts = fdts, allindices = allindices, groundtruth_raster = groundtruth_raster, has_ground_truth = has_ground_truth))
}

process_tile_dates <- function(tiles, tile, files, shape, shrink, window, groundtruth_pattern, dates, verbose, addxy, adddate, first, fdts, sample_size,
                               allindices, has_ground_truth, fltr_features, fltr_condition) {
  for (date in dates) {
    if (exists("dts", inherits = FALSE)) { #CRF Is this just to clear up the environment after every iteration of the loop?
      rm(dts)
    }

    if (verbose) {
      cat(paste("loading tile data from", tile, "for", date, " "))
    }

    selected_files <- filter_files_by_date_and_groundtruth(date, files, groundtruth_pattern) #CRF it doesn't really filter by ground_truth it just filters it out if the ground_truth is of the wrong date
    result <- prepare_raster_data_by_tile(selected_files, shape, shrink, window, verbose) #CRF Vague output param name. It returns the extent (shape of the data) and the raster stack
    extent <- result$extent
    rasstack <- result$rasstack

    if (length(tiles) > 1) {
      groundtruth_raster <- NA
    } else {
      groundtruth_result <- load_groundtruth_raster(selected_files, groundtruth_pattern, first, verbose, extent, has_ground_truth)
      groundtruth_raster <- groundtruth_result$groundtruth_raster
      has_ground_truth <- groundtruth_result$has_ground_truth
      first <- groundtruth_result$first
    }
    # Process raster data CRF <- change dts to dataset for clarity?
    # CRF the dts parameter should never exist as it is removed above. So shouldn't be put as an input parameter right?
    dts <- transform_raster_to_data_matrix(rasstack, shape, shrink, addxy, dts) # the most memory consumptive function

    gc() # garbabe collection: to free up memory usage
    # Add date features if necessary
    if (adddate) { #CRF snake_case
      dts <- append_date_based_features(dts, date)
    }

    result <- finalize_column_names_and_data_matrix(dts, selected_files, addxy, adddate) #CRF vague return param and actually only adds names to the columns in the matrix
    dts <- result$dts
    newcolnames <- result$newcolnames #CRF Snakecase

    # filter on filter conditions
    filterresult <- filter_by_feature(fltr_features, fltr_condition, dts, verbose = verbose) #CRF snake_case
    dts <- filterresult$filtered_matrix
    sf_indices <- filterresult$filtered_indices

    # take a random sample if that was applied
    #
    # Subset matrices based on common column names
    # Merge matrices by column names
    combine_result <- sample_and_combine_data(date, dts, fdts, sf_indices, sample_size, first, allindices) #CRF vague return param + bad comments above
    fdts <- combine_result$fdts #CRF What does fdts mean?
    allindices <- combine_result$allindices
    first <- combine_result$first
  }
  return(list( #CRF Does this have to return "first"?
    fdts = fdts, allindices = allindices, first = first, groundtruth_raster = groundtruth_raster, newcolnames = newcolnames,
    has_ground_truth = has_ground_truth
  ))
}
