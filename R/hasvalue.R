#' Check if an object has a value and is not NULL or NA
#'
#' This function checks whether an object is not NULL or NA.
#'
#' @param x An R object to be tested.
#'
#' @return TRUE if the object is not NULL or NA, else FALSE. returns TRUE if it is a vector of multiple NA's
#'
#' @examples
#' # Test with a vector containing NA values
#' hasvalue(c(1, 2, NA, 4))
#' # Test with a NULL object
#' hasvalue(NULL)
#'
hasvalue <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  if (class(x) == "logical") {
    if ((length(x) == 1) && is.na(x)[1]) {
      return(FALSE)
    }
  }
  if (class(x) == "character") {
    if (length(x) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
