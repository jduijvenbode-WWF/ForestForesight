#' Calculate Scores
#'
#' This function calculates scores based on predictions, ground truth, and optional parameters.
#'
#' @param predictions A character vector or raster object representing the predictions.
#' @param groundtruth A character vector or raster object representing the ground truth.
#' @param forest_mask An optional character vector or raster object representing the forest mask.
#' @param csv_filename An optional CSV file to which the results will be written.
#' @param append Logical. If TRUE, results will be appended to the existing CSV file.
#' @param country Character. If NULL all the overlapping polygons will be processed.
#' Otherwise the ISO3 code should be given and the analysis_polygons dataset should contain a column called iso3
#' @param analysis_polygons Optional vector or character vector representing analysis polygons.
#' @param remove_empty Logical. If TRUE, empty rows (with all scores being zero) will be removed from the output.
#' @param date character. should be in format (YYYY-MM-DD).
#' Optional if either groundtruth or predictions is a character to the tiffile.
#' @param tile character. should be in format AA{N-S}_BBB{W-E}.
#' Optional if either groundtruth or predictions is a character to the tiffile with a directory name.
#' @param method character. The shorthand for the method used,
#' which should also be included in the separate csv file for storing methods
#' @param add_wkt Logical.  Whether the WKT should be added as a column to the CSV output
#' @param verbose Logical. Whether the steps taken in the function should be verbose.
#'
#' @return A vector dataset containing calculated scores for each polygon.
#'
#' @export

ff_analyze <- function(predictions, groundtruth, forest_mask = NULL, csv_filename = NULL, country = NULL,
                       append = TRUE, analysis_polygons = NULL, remove_empty = TRUE,
                       date = NULL, tile = NULL, method = NA, add_wkt = FALSE, verbose = FALSE) {
  if (!inherits(predictions, c("SpatRaster", "character"))) {
    stop("predictions is not a raster or path to a raster")
  }
  if (!inherits(groundtruth, c("SpatRaster", "character"))) {
    stop("predictions is not a raster or path to a raster")
  }
  if (hasvalue(csv_filename)) {
    if (append == TRUE && !file.exists(csv_filename)) {
      append <- FALSE
      ff_cat("CSV file did not yet exist, creating empty one", verbose = verbose)
    }
  }
  if (is.null(date)) {
    if (inherits(predictions, "character")) {
      date <- substr(basename(predictions), 10, 19)
    } else {
      if (inherits(groundtruth, "character")) {
        date <- substr(basename(groundtruth), 10, 19)
      } else {
        stop("no method to derive date from filename")
      }
    }
  }

  if (inherits(predictions, "character")) {
    if (!file.exists(predictions)) {
      stop("predictions file does not exist")
    }
    predictions <- terra::rast(predictions)
  }
  if (terra::global(predictions,fun = "max", na.rm = T) < 1) {
    ff_cat("The raster seems to be not classified, automatically reclassifying raster based on 0.5 threshold.
           If this is not wanted, please load the raster before using ff_analyze and classify it according
           to the wanted threshold", color = "yellow", verbose = TRUE)
    predictions <- predictions > 0.5
  }
  if (inherits(groundtruth, "character")) {
    if (!file.exists(groundtruth)) {
      stop("groundtruth file does not exist")
    }
    groundtruth <- terra::rast(groundtruth, win = terra::ext(predictions))
  }
  ff_cat("rasters loaded", verbose = verbose)
  groundtruth[is.na(groundtruth)] <- 0
  groundtruth <- groundtruth > 0
  if (!is.null(forest_mask)) {
    ff_cat("using forest mask", verbose = verbose)
    if (inherits(forest_mask, "character")) {
      if (!file.exists(forest_mask)) {
        stop("forest mask file does not exist")
      }
      forest_mask <- terra::rast(forest_mask)
    }
    forest_mask <- terra::crop(forest_mask, groundtruth)
    crosstable_raster <- (2 * groundtruth + predictions) * (forest_mask > 0)
  } else {
    crosstable_raster <- 2 * groundtruth + predictions
  }
  if (is.null(analysis_polygons)) {
    polygons <- terra::vect(get(data("degree_polygons", envir = environment())))
  } else {
    if (inherits(analysis_polygons, "character")) {
      polygons <- terra::vect(analysis_polygons)
    } else {
      polygons <- analysis_polygons
    }
  }
  if (hasvalue(country) && ("iso3" %in% names(polygons))) {
    polygons <- polygons[which(polygons$iso3 == country)]
  }
  polygons = polygons[terra::ext(crosstable_raster),]
  ff_cat("summarizing statistics", verbose = verbose)
  polygons$FP <- terra::extract(crosstable_raster == 1, polygons, fun = "sum", na.rm = TRUE, touches = FALSE)[, 2]
  polygons$FN <- terra::extract(crosstable_raster == 2, polygons, fun = "sum", na.rm = TRUE, touches = FALSE)[, 2]
  polygons$TP <- terra::extract(crosstable_raster == 3, polygons, fun = "sum", na.rm = TRUE, touches = FALSE)[, 2]
  polygons$TN <- terra::extract(crosstable_raster == 0, polygons, fun = "sum", na.rm = TRUE, touches = FALSE)[, 2]

  if (verbose) {
    ff_cat("calculating F0.5 score", verbose = verbose)
    precision <- sum(polygons$TP, na.rm = TRUE) / (sum(polygons$TP, na.rm = TRUE) + sum(polygons$FP, na.rm = TRUE))
    recall <- sum(polygons$TP, na.rm = TRUE) / (sum(polygons$TP, na.rm = TRUE) + sum(polygons$FN, na.rm = TRUE))
    ff_cat("F0.5 score is:", 1.25 * precision * recall / (0.25 * precision + recall), verbose = verbose)
  }
  ff_cat("adding metadata", verbose = verbose)
  polygons$date <- date
  polygons$method <- method
  if (remove_empty) {
    empty_indices <- which(rowSums(as.data.frame(polygons[, c("FP", "FN", "TP")]), na.rm = TRUE) == 0)
    if (length(empty_indices) > 0) {
      ff_cat("removing", length(empty_indices), "empty records", verbose = verbose)
      polygons <- polygons[-empty_indices, ]
    }
  }
  if (add_wkt) {
    polygons_dataframe <- as.data.frame(polygons, geom = "wkt")
  } else {
    polygons_dataframe <- as.data.frame(polygons)
  }
  if (hasvalue(csv_filename)) {
    if (append && file.exists(csv_filename)) {
      ff_cat("appending to existing dataset", verbose = verbose)
      previous_data <- read.csv(csv_filename)
      previous_data$X <- NULL
      write.csv(rbind(previous_data, polygons_dataframe), csv_filename)
    } else {
      if (!file.exists(csv_filename) && append && verbose) {
        ff_cat("the given file does not exist, while append was set to TRUE", color = "yellow", verbose = verbose)
      }
      write.csv(polygons_dataframe, csv_filename)
    }
  }
  invisible(polygons)
}
