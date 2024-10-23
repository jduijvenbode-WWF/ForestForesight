#' Calculate Scores
#'
#' This function calculates scores based on predictions, ground truth, and optional parameters.
#'
#' @param predictions A character vector or raster object representing the predictions.
#' @param groundtruth A character vector or raster object representing the ground truth.
#' @param forestmask An optional character vector or raster object representing the forest mask.
#' @param csvfile An optional CSV file to which the results will be written.
#' @param append Logical. If TRUE, results will be appended to the existing CSV file.
#' @param country Character. If NULL all the overlapping polygons will be processed. Otherwise the ISO3 code should be given and the analysis_polygons dataset should contain a column called iso3
#' @param analysis_polygons Optional vector or character vector representing analysis polygons.
#' @param return_polygons Logical. If TRUE, the polygons with calculated scores will be returned.
#' @param remove_empty Logical. If TRUE, empty rows (with all scores being zero) will be removed from the output.
#' @param date character. should be in format (YYYY-MM-DD). Optional if either groundtruth or predictions is a character to the tiffile.
#' @param tile character. should be in format AA{N-S}_BBB{W-E}. Optional if either groundtruth or predictions is a character to the tiffile with a directory name.
#' @param method character. the shorthand for the method used, which should also be included in the separate csv file for storing methods
#' @param verbose Logical. Whether the steps taken in the function should be verbose.
#'
#' @return A vector dataset containing calculated scores for each polygon.
#'
#' @export

ff_analyze <- function(predictions, groundtruth, forestmask = NULL, csvfile = NULL, country = NULL, append = T, analysis_polygons = NULL, return_polygons = T, remove_empty = T, date = NULL, tile = NULL, method = NA, verbose = F) {
  if (!(class(predictions) %in% c("character", "SpatRaster"))) {
    stop("predictions is not a raster or path to a raster")
  }
  if (!(class(groundtruth) %in% c("character", "SpatRaster"))) {
    stop("predictions is not a raster or path to a raster")
  }
  if (!is.null(csvfile)) {
    if (append == TRUE & !file.exists(csvfile)) {
      append <- F
      cat("CSV file did not yet exist, creating empty one\n")
    }
  }
  if (is.null(date)) {
    if (class(predictions) == "character") {
      date <- substr(basename(predictions), 10, 19)
    } else {
      if (class(groundtruth) == "character") {
        date <- substr(basename(predictions), 10, 19)
      } else {
        stop("no method to derive date from filename")
      }
    }
  }

  if (class(predictions) == "character") {
    predictions <- terra::rast(predictions)
  }
  if (class(groundtruth) == "character") {
    groundtruth <- terra::rast(groundtruth, win = terra::ext(predictions))
  }
  if (verbose) {
    cat("rasters loaded\n")
  }
  groundtruth[is.na(groundtruth)] <- 0
  groundtruth <- groundtruth > 0
  if (!is.null(forestmask)) {
    if (verbose) {
      cat("using forest mask\n")
    }
    if (class(forestmask) == "character") {
      forestmask <- terra::rast(forestmask)
    }
    forestmask <- terra::crop(forestmask, groundtruth)
    cross <- (2 * groundtruth + predictions) * (forestmask > 0)
  } else {
    cross <- 2 * groundtruth + predictions
  }
  if (is.null(analysis_polygons)) {
    data(degree_polygons, envir = environment())
    pols <- terra::vect(degree_polygons)
  } else {
    if (class(analysis_polygons) == "character") {
      pols <- terra::vect(analysis_polygons)
    } else {
      pols <- analysis_polygons
    }
  }
  if (!is.null(country)) {
    pols <- pols[which(pols$iso3 == country)]
  }
  if (verbose) {
    cat("summarizing statistics\n")
  }
  pols$FP <- terra::extract(cross == 1, pols, fun = "sum", na.rm = T, touches = F)[, 2]
  pols$FN <- terra::extract(cross == 2, pols, fun = "sum", na.rm = T, touches = F)[, 2]
  pols$TP <- terra::extract(cross == 3, pols, fun = "sum", na.rm = T, touches = F)[, 2]
  pols$TN <- terra::extract(cross == 0, pols, fun = "sum", na.rm = T, touches = F)[, 2]

  if (verbose) {
    cat("calculating F0.5 score\n")
    pr <- sum(pols$TP, na.rm = T) / (sum(pols$TP, na.rm = T) + sum(pols$FP, na.rm = T))
    re <- sum(pols$TP, na.rm = T) / (sum(pols$TP, na.rm = T) + sum(pols$FN, na.rm = T))
    cat(paste("F0.5 score is:", 1.25 * pr * re / (0.25 * pr + re), "\n"))
  }
  if (verbose) {
    cat("adding metadata\n")
  }
  pols$date <- date
  pols$method <- method
  if (remove_empty) {
    empty_indices <- which(rowSums(as.data.frame(pols[, c("FP", "FN", "TP")]), na.rm = T) == 0)
    if (length(empty_indices) > 0) {
      if (verbose) {
        ff_cat("removing", length(empty_indices), "empty records\n")
      }
      pols <- pols[-empty_indices, ]
    }
  }
  if (!is.null(csvfile)) {
    if (append & file.exists(csvfile)) {
      if (verbose) {
        cat("appending to existing dataset\n")
      }
      pastdata <- read.csv(csvfile)
      pastdata$X <- NULL
      write.csv(rbind(pastdata, as.data.frame(pols)), csvfile)
    } else {
      if (!file.exists(csvfile) & append & verbose) {
        ff_cat("the given file does not exist, while append was set to TRUE\n", color = "yellow")
      }
      write.csv(as.data.frame(pols), csvfile)
    }
  }
  if (return_polygons) {
    return(pols)
  }
}
