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
  file_metrics <- lapply(tif_files, function(x) ff_dqc_file(x, return_values))

  # Combine results into data frame
  all_metrics <- as.data.frame(as.matrix(do.call(rbind, file_metrics)))
  names(all_metrics) <- names(file_metrics[[1]])
  all_metrics[] <- lapply(all_metrics, unlist)

  # Extract feature names and dates
  all_metrics$feature_names <- sapply(
    basename(tif_files),
    function(x) substr(x, 21, nchar(x) - 4)
  )
  all_metrics$dates <- substr(basename(tif_files), 10, 19)
  invalid_date_count <- sum(nchar(all_metrics$dates) != 10)

  # Reorder columns and process features
  all_metrics <- all_metrics[, c(
    ncol(all_metrics) - 1,
    ncol(all_metrics),
    seq(ncol(all_metrics) - 2)
  )]
  feature_summaries <- suppressWarnings(lapply(
    unique(all_metrics$feature_names),
    function(x) summary_by_feature(all_metrics, x)
  ))

  # Create summary table
  summary_table <- as.data.frame(do.call(rbind, feature_summaries))
  names(summary_table) <- names(feature_summaries[[1]])
  summary_table[] <- lapply(summary_table, unlist)
  summary_table <- summary_table[order(summary_table$type), ]

  # Perform consistency checks
  dynamic_features <- summary_table[which(!is.na(summary_table$min_date)), ]
  date_consistency <- check_date_consistency(dynamic_features)
  extent_consistency <- check_extent_consistency(all_metrics)

  return(list(
    "tile" = basename(folder_path),
    "byfeature" = summary_table,
    "all" = all_metrics,
    "equalextent" = extent_consistency,
    "equaldaterange" = date_consistency,
    "incorrect_dateformats" = invalid_date_count,
    "minextent" = terra::ext(
      max(all_metrics$xmin), min(all_metrics$xmax),
      max(all_metrics$ymin), min(all_metrics$ymax)
    )
  ))
}

#' Summarize raster features
#' [Documentation remains the same...]
#' @noRd
summary_by_feature <- function(dataframe, feature) {
  feature_files <- dataframe[which(dataframe$feature_names == feature), ]
  if (length(feature_files) == 0) {
    stop(paste("feature was not found in folder", feature))
  }
  feature_type <- if (nrow(feature_files) == 1) {
    feature_files <- rbind(feature_files, feature_files)
    "static"
  } else {
    "dynamic"
  }

  # Calculate date ranges
  feature_dates <- as.Date(feature_files$dates)
  date_intervals <- diff(sort(feature_dates))

  return(list(
    "feature" = feature,
    "type" = feature_type,
    "min_date" = if (is.na(feature_dates[1])) NA else as.character(min(feature_dates)),
    "max_date" = if (is.na(feature_dates[1])) NA else as.character(max(feature_dates)),
    "has_gaps" = check_gaps(date_intervals, feature_type),
    "has_doubles" = check_doubles(date_intervals, feature_type),
    "pixel_count" = get_consistent_value(feature_files$pixel_count),
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
  extent_vars <- c("pixel_count", "xmin", "xmax", "ymin", "ymax", "resolution")
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
