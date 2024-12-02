#' Data quality control for raster folders
#'
#' This function analyzes all TIF files in a given folder and provides a comprehensive
#' quality assessment of the rasters, including temporal and spatial consistency checks.
#'
#' @param folder_path Character string. The path to the folder containing TIF files.
#' @param return_values Logical. Should the actual values of the rasters be returned.
#'   Default is TRUE.
#'
#' @return A list containing:
#' \itemize{
#'   \item tile: The base name of the folder
#'   \item byfeature: Data frame with quality metrics for each feature
#'   \item all: Data frame with raw values for all files
#'   \item equalextent: Logical indicating if all files have same extent
#'   \item equaldaterange: Logical indicating if date ranges are consistent
#'   \item incorrect_dateformats: Count of files with incorrect date formats
#'   \item minextent: SpatExtent object with minimum overlap extent
#' }
#'
#' @examples
#' \dontrun{
#' result <- ff_dqc("path/to/tif_folder")
#' # Access summary by feature
#' print(result$byfeature)
#' # Check if all files have same extent
#' print(result$equalextent)
#' }
#' @export
ff_dqc <- function(folder_path, return_values = TRUE) {
  # Get and process all TIF files
  tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
  valuelist <- lapply(tif_files, function(x) ff_dqc_file(x, return_values))

  # Combine results into data frame
  allvals <- as.data.frame(as.matrix(do.call(rbind, valuelist)))
  names(allvals) <- names(valuelist[[1]])
  allvals[] <- lapply(allvals, unlist)

  # Extract feature names and dates
  allvals$featurenames <- sapply(basename(tif_files), function(x) substr(x, 21, nchar(x) - 4))
  allvals$dates <- substr(basename(tif_files), 10, 19)
  incorrect_dateformats <- sum(nchar(allvals$dates) != 10)

  # Reorder columns and process features
  allvals <- allvals[, c(ncol(allvals) - 1, ncol(allvals), seq(ncol(allvals) - 2))]
  summary <- suppressWarnings(lapply(
    unique(allvals$featurenames),
    function(x) summary_by_feature(allvals, x)
  ))

  # Create summary table
  summarytable <- as.data.frame(do.call(rbind, summary))
  names(summarytable) <- names(summary[[1]])
  summarytable[] <- lapply(summarytable, unlist)
  summarytable <- summarytable[order(summarytable$type), ]

  # Perform consistency checks
  dyn_summary <- summarytable[which(!is.na(summarytable$min_date)), ]
  datecheck <- check_date_consistency(dyn_summary)
  extentcheck <- check_extent_consistency(allvals)

  return(list(
    "tile" = basename(folder_path),
    "byfeature" = summarytable,
    "all" = allvals,
    "equalextent" = extentcheck,
    "equaldaterange" = datecheck,
    "incorrect_dateformats" = incorrect_dateformats,
    "minextent" = terra::ext(
      max(allvals$xmin), min(allvals$xmax),
      max(allvals$ymin), min(allvals$ymax)
    )
  ))
}

#' Summarize raster features
#'
#' Internal function to generate summary statistics for each feature in the raster dataset.
#'
#' @param dataframe Data frame containing raster information
#' @param feature Character string of feature name to summarize
#' @return List of summary statistics for the feature
#' @noRd
summary_by_feature <- function(dataframe, feature) {
  feature_files <- dataframe[which(dataframe$featurenames == feature), ]
  type <- if (nrow(feature_files) == 1) {
    feature_files <- rbind(feature_files, feature_files)
    "static"
  } else {
    "dynamic"
  }

  # Calculate date ranges
  dates <- as.Date(feature_files$dates)
  date_diffs <- diff(sort(dates))

  return(list(
    "feature" = feature,
    "type" = type,
    "min_date" = if (is.na(dates[1])) NA else as.character(min(dates)),
    "max_date" = if (is.na(dates[1])) NA else as.character(max(dates)),
    "has_gaps" = check_gaps(date_diffs, type),
    "has_doubles" = check_doubles(date_diffs, type),
    "pixel_count" = get_consistent_value(feature_files$npixel),
    "xmin" = get_consistent_value(feature_files$xmin),
    "xmax" = get_consistent_value(feature_files$xmax),
    "ymin" = get_consistent_value(feature_files$ymin),
    "ymax" = get_consistent_value(feature_files$ymax),
    "resolution" = get_consistent_value(feature_files$resolution),
    "crs_name" = get_consistent_value(feature_files$crsname, TRUE),
    "crs_code" = get_consistent_value(feature_files$crscode, TRUE),
    "mean_value" = round(mean(as.numeric(feature_files$mean)), 2),
    "max_value" = round(max(as.numeric(feature_files$max)), 2),
    "has_NA" = get_consistent_value(feature_files$hasNA, TRUE)
  ))
}

#' Check for consistent date ranges
#' @noRd
check_date_consistency <- function(summary_df) {
  all(summary_df$has_gaps == "no") &&
    all(summary_df$has_doubles == "no") &&
    length(unique(summary_df$min_date)) == 1 &&
    length(unique(summary_df$max_date)) == 1
}

#' Check for consistent extents
#' @noRd
check_extent_consistency <- function(vals) {
  extent_vars <- c("npixel", "xmin", "xmax", "ymin", "ymax", "resolution")
  all(sapply(extent_vars, function(var) length(unique(vals[[var]])) == 1))
}

#' Check for gaps in dates
#' @noRd
check_gaps <- function(date_diffs, type) {
  if (type == "static" || length(date_diffs) == 0) {
    return("no")
  }
  if (max(date_diffs) < 32 || min(date_diffs) > 364) "no" else "yes"
}

#' Check for doubles in dates
#' @noRd
check_doubles <- function(date_diffs, type) {
  if (type == "static" || length(date_diffs) == 0) {
    return("no")
  }
  if (min(date_diffs) > 27) "no" else "yes"
}

#' Get consistent value or return "differing"
#' @noRd
get_consistent_value <- function(x, use_unique = FALSE) {
  if (use_unique) {
    if (length(unique(x)) == 1) x[1] else "differing"
  } else {
    if (sd(x) == 0) x[1] else "differing"
  }
}
