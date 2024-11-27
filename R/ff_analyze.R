#' Extract date from filename
#'
#' @param predictions Path to predictions file or SpatRaster object
#' @param groundtruth Path to groundtruth file or SpatRaster object
#' @return Character string containing the date in YYYY-MM-DD format
#' @noRd
get_date_from_files <- function(predictions, groundtruth) {
  if (inherits(predictions, "character")) {
    return(substr(basename(predictions), 10, 19))
  } else if (inherits(groundtruth, "character")) {
    return(substr(basename(groundtruth), 10, 19))
  }
  stop("No method to derive date from filename")
}

#' Validate input data types and load rasters
#'
#' @param predictions Path to predictions file or SpatRaster object
#' @param groundtruth Path to groundtruth file or SpatRaster object
#' @param forest_mask Optional path to forest mask file or SpatRaster object
#' @return List containing loaded SpatRaster objects
#' @noRd
validate_and_load_data <- function(predictions, groundtruth, forest_mask = NULL) {
  if (!inherits(predictions, c("SpatRaster", "character"))) {
    stop("predictions is not a raster or path to a raster")
  }
  if (!inherits(groundtruth, c("SpatRaster", "character"))) {
    stop("groundtruth is not a raster or path to a raster")
  }

  # Load predictions
  if (inherits(predictions, "character")) {
    if (!file.exists(predictions)) {
      stop("predictions file does not exist")
    }
    predictions <- terra::rast(predictions)
  }

  # Check and reclassify predictions if needed
  if (terra::global(predictions, fun = "max", na.rm = TRUE) < 1) {
    ff_cat("The raster seems to be not classified, automatically reclassifying raster based on 0.5 threshold.
           If this is not wanted, please load the raster before using ff_analyze and classify it according
           to the wanted threshold", color = "yellow")
    predictions <- predictions > 0.5
  }

  # Load groundtruth
  if (inherits(groundtruth, "character")) {
    if (!file.exists(groundtruth)) {
      stop("groundtruth file does not exist")
    }
    groundtruth <- terra::rast(groundtruth, win = terra::ext(predictions))
  }

  # Process groundtruth
  groundtruth[is.na(groundtruth)] <- 0
  groundtruth <- groundtruth > 0

  # Load forest mask if provided
  if (!is.null(forest_mask)) {
    if (inherits(forest_mask, "character")) {
      if (!file.exists(forest_mask)) {
        stop("forest mask file does not exist")
      }
      forest_mask <- terra::rast(forest_mask)
    }
    forest_mask <- terra::crop(forest_mask, groundtruth)
  }

  list(predictions = predictions, groundtruth = groundtruth, forest_mask = forest_mask)
}

#' Add metadata to polygons
#'
#' @param polygons SpatVector object containing analysis results
#' @param date Character string containing the date
#' @param method Character string containing the method name
#' @param remove_empty Logical indicating whether to remove empty records
#' @param verbose Logical indicating whether to print progress messages
#' @return Updated SpatVector object
#' @noRd
add_metadata <- function(polygons, date, method, remove_empty = TRUE, verbose = FALSE) {
  polygons$date <- date
  polygons$method <- method

  if (remove_empty) {
    empty_indices <- which(rowSums(as.data.frame(polygons[, c("FP", "FN", "TP")]), na.rm = TRUE) == 0)
    if (length(empty_indices) > 0) {
      ff_cat("removing", length(empty_indices), "empty records", verbose = verbose)
      polygons <- polygons[-empty_indices, ]
    }
  }

  return(polygons)
}

#' Process output and write to CSV
#'
#' @param polygons SpatVector object containing analysis results
#' @param csv_filename Optional path to output CSV file
#' @param append Logical indicating whether to append to existing file
#' @param add_wkt Logical indicating whether to add WKT geometry
#' @param verbose Logical indicating whether to print progress messages
#' @return Processed data frame
#' @noRd
process_and_write_output <- function(polygons, csv_filename = NULL, append = TRUE,
                                     add_wkt = FALSE, verbose = FALSE) {
  if (add_wkt) {
    polygons_dataframe <- as.data.frame(polygons, geom = "wkt")
  } else {
    polygons_dataframe <- as.data.frame(polygons)
  }

  if (!is.null(csv_filename)) {
    if (append && file.exists(csv_filename)) {
      ff_cat("appending to existing dataset", verbose = verbose)
      previous_data <- read.csv(csv_filename)
      previous_data$X <- NULL
      write.csv(rbind(previous_data, polygons_dataframe), csv_filename)
    } else {
      if (!file.exists(csv_filename) && append && verbose) {
        ff_cat("the given file does not exist, while append was set to TRUE",
               color = "yellow", verbose = verbose
        )
      }
      write.csv(polygons_dataframe, csv_filename)
    }
  }

  return(polygons_dataframe)
}

#' Calculate Scores
#'
#' This function calculates scores based on predictions, ground truth, and optional parameters.
#' It processes raster data and compares predictions against ground truth, optionally using
#' a forest mask to restrict the analysis area.
#'
#' @param predictions A SpatRaster object or path to raster file representing the predictions
#' @param groundtruth A SpatRaster object or path to raster file representing the ground truth
#' @param forest_mask Optional SpatRaster object or path to raster file representing the forest mask
#' @param csv_filename Optional path to CSV file for writing results
#' @param country Character string containing ISO3 code for filtering analysis polygons
#' @param append Logical indicating whether to append results to existing CSV file
#' @param analysis_polygons Optional SpatVector object or path to vector file for analysis areas
#' @param remove_empty Logical indicating whether to remove empty records
#' @param date Character string in YYYY-MM-DD format
#' @param tile Character string in format AA{N-S}_BBB{W-E}
#' @param method Character string indicating the analysis method
#' @param add_wkt Logical indicating whether to add WKT geometry to output
#' @param verbose Logical indicating whether to print progress messages
#'
#' @return A SpatVector object containing calculated scores for each polygon
#'
#' @export
ff_analyze <- function(predictions, groundtruth, forest_mask = NULL, csv_filename = NULL,
                       country = NULL, append = TRUE, analysis_polygons = NULL,
                       remove_empty = TRUE, date = NULL, tile = NULL, method = NA,
                       add_wkt = FALSE, verbose = FALSE) {
  # Get date if not provided
  if (is.null(date)) {
    date <- get_date_from_files(predictions, groundtruth)
  }

  # Validate and load input data
  data <- validate_and_load_data(predictions, groundtruth, forest_mask)
  predictions <- data$predictions
  groundtruth <- data$groundtruth
  forest_mask <- data$forest_mask

  ff_cat("rasters loaded", verbose = verbose)

  # Create crosstable raster
  if (!is.null(forest_mask)) {
    ff_cat("using forest mask", verbose = verbose)
    crosstable_raster <- (2 * groundtruth + predictions) * (forest_mask > 0)
  } else {
    crosstable_raster <- 2 * groundtruth + predictions
  }

  # Load or process analysis polygons
  if (is.null(analysis_polygons)) {
    polygons <- terra::vect(get(data("degree_polygons", envir = environment())))
  } else {
    if (inherits(analysis_polygons, "character")) {
      polygons <- terra::vect(analysis_polygons)
    } else {
      polygons <- analysis_polygons
    }
  }

  # Filter by country if specified
  if (!is.null(country) && ("iso3" %in% names(polygons))) {
    polygons <- polygons[which(polygons$iso3 == country)]
  }

  # Crop polygons to raster extent
  polygons <- polygons[terra::ext(crosstable_raster), ]
  if (nrow(polygons) == 0) {
    stop("the country code is incorrect or the loaded polygons do not overlap
         with the area of the predictions or groundtruth")
  }

  # Calculate statistics
  ff_cat("summarizing statistics", verbose = verbose)
  polygons$FP <- terra::extract(crosstable_raster == 1, polygons,
                                fun = "sum",
                                na.rm = TRUE, touches = FALSE
  )[, 2]
  polygons$FN <- terra::extract(crosstable_raster == 2, polygons,
                                fun = "sum",
                                na.rm = TRUE, touches = FALSE
  )[, 2]
  polygons$TP <- terra::extract(crosstable_raster == 3, polygons,
                                fun = "sum",
                                na.rm = TRUE, touches = FALSE
  )[, 2]
  polygons$TN <- terra::extract(crosstable_raster == 0, polygons,
                                fun = "sum",
                                na.rm = TRUE, touches = FALSE
  )[, 2]

  # Calculate and print F0.5 score if verbose
  if (verbose) {
    ff_cat("calculating F0.5 score", verbose = verbose)
    precision <- sum(polygons$TP, na.rm = TRUE) /
      (sum(polygons$TP, na.rm = TRUE) + sum(polygons$FP, na.rm = TRUE))
    recall <- sum(polygons$TP, na.rm = TRUE) /
      (sum(polygons$TP, na.rm = TRUE) + sum(polygons$FN, na.rm = TRUE))
    ff_cat("F0.5 score is:", 1.25 * precision * recall / (0.25 * precision + recall),
           verbose = verbose
    )
  }

  # Add metadata
  ff_cat("adding metadata", verbose = verbose)
  polygons <- add_metadata(polygons, date, method, remove_empty, verbose)

  # Process output and write to file if specified
  process_and_write_output(polygons, csv_filename, append, add_wkt, verbose)

  invisible(polygons)
}
