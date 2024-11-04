#' Select Files Based on Closest Previous Date for Each Feature
#'
#' This function takes a given date and a vector of file names and returns a vector
#' containing the file names corresponding to the closest previous date for each unique feature.
#'
#' @param given_date The reference date in the format "YYYY-MM-DD".
#' @param listed_files A character vector of file names.
#'
#' @return A character vector of selected file names based on the closest previous date for each feature.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' given_date <- "2022-03-15"
#' listed_files <- c(
#'   "mydir/tile_YYYY-MM-DD_feature.tif",
#'   "mydir/tile_YYYY-MM-DD_feature.tif",
#'   "mydir/tile_YYYY-MM-DD_feature.tif",
#'   "mydir/tile_YYYY-MM-DD_feature.tif",
#'   "mydir/tile_YYYY-MM-DD_feature.tif",
#' )
#'
#' result <- select_files_date(given_date, listed_files)
#' print(result)
#' }
#'
#' @seealso \code{\link{grep}}, \code{\link{lubridate::ymd}}
#'
#' @importFrom lubridate ymd
#'
#' @export
select_files_date <- function(given_date, listed_files) {
  matching_indices <- grep(".*_\\d{4}-\\d{2}-\\d{2}_([^_]+).*", listed_files)
  if (length(listed_files) != length(matching_indices)) {
    ff_cat("Listed_files contains files with incorrect name. The correct file name should be in the format: tile_YYYY-MM-DD_feature.tif. Incorect files will be excluded and not processed", color = "yellow")
  }
  matching_files <- listed_files[matching_indices]
  unique_names <- unique(gsub(".*_\\d{4}-\\d{2}-\\d{2}_([^_]+).*", "\\1", matching_files))
  selected_files <- character(0)

  for (feature in sort(unique_names)) {
    matching_files_feature <- grep(feature, matching_files, value = TRUE)
    dates_only <- gsub(".*_(\\d{4}-\\d{2}-\\d{2}).*", "\\1", matching_files_feature)
    date_vector <- lubridate::ymd(dates_only)
    indices <- which(date_vector <= lubridate::ymd(given_date))
    if (length(indices) > 0) {
      closest_previous_date <- date_vector[which.min(abs(date_vector[indices] - lubridate::ymd(given_date)))]
      selected_file <- grep(paste0(closest_previous_date, "_", feature), matching_files_feature, value = TRUE)
      selected_files <- c(selected_files, selected_file)
    }
  }
  return(selected_files)
}
