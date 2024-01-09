#' Find Matching or Previous Date
#'
#' This function takes an input date and a list of dates, and returns either
#' the exact matching date from the list or the closest previous date if there
#' is no exact match.
#'
#' @param input_date A character string representing the input date (e.g., "2021-03-01").
#' @param date_list A character vector representing a list of date strings.
#'
#' @return A character string representing the matching or previous date, or \code{NULL} if no
#'         previous date is found.
#'
#' @examples
#' input_date <- "2022-05-01"
#' date_list <- c("2022-06-01", "2022-01-01", "2022-06-01")
#' find_matching_date(input_date, date_list)
#'
#' @export
find_matching_date <- function(input_date, date_list) {
  input_date <- as.Date(input_date)
  date_list <- as.Date(date_list)

  exact_match <- which(input_date %in% date_list)

  if (length(exact_match) > 0) {
    # Return the exact matching date
    return(as.character(date_list[exact_match]))
  } else {
    # Find the closest previous date
    closest_previous_date <- max(date_list[date_list < input_date], default = NA)

    if (!is.na(closest_previous_date)) {
      return(as.character(closest_previous_date))
    } else {
      # If no previous date is found, return NULL
      return(NULL)
    }
  }
}
