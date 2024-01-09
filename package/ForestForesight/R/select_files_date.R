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
#'   "mydir/feature1_2022-03-10.txt",
#'   "mydir/feature1_2022-03-12.txt",
#'   "mydir/feature2_2022-03-14.txt",
#'   "mydir/feature2_2022-03-16.txt",
#'   "mydir/feature2_2022-03-18.txt"
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
  matching_indices <- grep(".*/([^_/]+)_\\d{4}-\\d{2}-\\d{2}.*", listed_files)
  matching_files <- listed_files[matching_indices]
  unique_names <- unique(gsub(".*/([^_/]+)_\\d{4}-\\d{2}-\\d{2}.*", "\\1", matching_files))
  selected_files <- character(0)

  for (feature in unique_names) {
    matching_files_feature <- grep(paste0("^.*\\/", feature, "_"), matching_files, value = TRUE)
    dates_only <- gsub(".*_(\\d{4}-\\d{2}-\\d{2}).*", "\\1", matching_files_feature)
    date_vector <- ymd(dates_only)
    closest_previous_date <- date_vector[which.min(abs(date_vector[which(date_vector <= ymd(given_date))] - ymd(given_date)))]
    selected_file <- grep(paste0(feature, "_", closest_previous_date),matching_files_feature, value = TRUE)
    selected_files <- c(selected_files, selected_file)
  }
  return(selected_files)
}
