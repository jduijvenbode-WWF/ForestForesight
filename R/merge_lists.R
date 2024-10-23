#' Merge Two Named Lists with User Preferences Overwriting Defaults
#'
#' This function merges two named lists, allowing elements from the user-specified list
#' to overwrite those in the default list if they have the same names. If there are no
#' overlapping keys, the function simply combines the lists.
#'
#' @param default A named list containing the default values.
#' @param user A named list containing the user-specified values that may overwrite the default values.
#'
#' @return A named list with combined elements from both input lists. User-specified elements
#' overwrite default elements with the same names.
#'
#' @examples
#' default_list <- list(a = 1, b = 2, c = 3)
#' user_list <- list(b = 20, d = 4)
#' merged_list <- merge_lists(default_list, user_list)
#' print(merged_list) # Should print: $a [1], $c [3], $b [20], $d [4]
#'
#' @export
merge_lists <- function(default, user) {
  indices <- -which(names(default) %in% names(user))
  if (length(indices) != 0) {
    c(default[indices], user)
  } else {
    c(default, user)
  }
}
