#' Generate a vector with the first of every month between two given dates
#'
#' @param start_date A character string representing the start date in "YYYY-MM-DD" format.
#' @param end_date A character string representing the end date in "YYYY-MM-DD" format.
#' @return A character vector with the first of every month between start_date and end_date.
#' @examples
#' daterange("2022-01-15", "2022-04-15")
#' # Returns: [1] "2022-01-01" "2022-02-01" "2022-03-01" "2022-04-01"
#'
#' @export
daterange <- function(start_date, end_date) {
  # Convert input strings to Date objects using lubridate
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)

  # Generate the sequence of first days of each month
  result <- seq(lubridate::floor_date(start_date, "month"), lubridate::floor_date(end_date, "month"), by = "1 month")

  # Convert the result to character
  result <- as.character(result)

  return(result)
}
