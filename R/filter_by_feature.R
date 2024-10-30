#' Filter matrix by feature
#'
#' This function filters a matrix based on specified features and conditions.
#'
#' @param fltr_features A character vector specifying the features to filter.
#' @param fltr_condition A character vector specifying the conditions for each feature.
#' @param matrix The input matrix to filter.
#' @param verbose Logical indicating whether to display verbose output (default is TRUE).
#'
#' @return A list containing the filtered matrix and the indices of the filtered rows.
#'
#' @examples
#' filter_by_feature(
#'   fltr_features = c("landpercentage", "forestmask"),
#'   fltr_condition = c(">100", ">0"),
#'   matrix = my_matrix
#' )
#'
#' @export

filter_by_feature <- function(fltr_features, fltr_condition, matrix, verbose = T) {
  sfa_indices <- c()
  if (length(fltr_features) > 0) {
    if (verbose) {
      cat(paste("filtering features\n"))
    }
    for (i in seq(length(fltr_features))) {
      operator <- gsub("[[:alnum:]]", "", fltr_condition[i])
      value <- gsub("[^0-9]", "", fltr_condition[i])
      filtercolumn <- which(colnames(matrix) == fltr_features[i])
      if (verbose) {
        cat(paste("filtering features: before if (length(filtercolumn) == 0)\n"))
      }
      if (length(filtercolumn) == 0) {
        ff_cat("The feature", fltr_features[i], "was not found, skipping filtering for this feature\n", color = "yellow")
      } else {
        if (verbose) {
          cat(paste("filtering feature", fltr_features[i], "on", fltr_condition[i], "\n"))
        }
        
        if (verbose) {
          cat(paste("operator: ", operator,"\n"))
        }
        if (operator == ">") {
          sf_indices <- which(matrix[, filtercolumn] > value)
        }
        if (operator == "<") {
          sf_indices <- which(matrix[, filtercolumn] < value)
        }
        if (operator == "==") {
          sf_indices <- which(matrix[, filtercolumn] == value)
        }
        if (operator == "!=") {
          sf_indices <- which(matrix[, filtercolumn] != value)
        }
        if (operator == ">=") {
          sf_indices <- which(matrix[, filtercolumn] >= value)
        }
        if (operator == "<=") {
          sf_indices <- which(matrix[, filtercolumn] <= value)
        }
      }
      if (verbose) {
        print(paste("length sfa_indices:", length(sfa_indices),"\n", sfa_indices))
      }
      if (length(sfa_indices) == 0) {
        sfa_indices <- c(sfa_indices, sf_indices)
      } else {
        sfa_indices <- intersect(sfa_indices, sf_indices)
      }
    }
    if (verbose) {
      cat(paste("b4 sf_indices <- unique(sfa_indices)\n"))
    }
    sf_indices <- unique(sfa_indices)
  } else {
    sf_indices <- NULL
  }
  if (length(sf_indices) > 0) {
    matrix <- matrix[sf_indices, ]
  }
  return(list("filtered_matrix" = matrix, "filtered_indices" = sf_indices))
}
