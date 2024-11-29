#' Filter matrix by feature
#'
#' This function filters a matrix based on specified features and conditions.
#'
#' @param filter_features A character vector specifying the features to filter.
#' @param filter_conditions A character vector specifying the conditions for each feature.
#' @param matrix The input matrix to filter.
#' @param verbose Logical indicating whether to display verbose output (default is TRUE).
#'
#' @return A list containing the filtered matrix and the indices of the filtered rows.
#'
#' @examples
#' \dontrun{
#' filter_by_feature(
#'   filter_features = c("landpercentage", "forestmask"),
#'   filter_conditions = c(">100", ">0"),
#'   matrix = my_matrix
#' )
#' }
#' @export
filter_by_feature <- function(filter_features, filter_conditions, matrix, verbose = TRUE) {
  merged_spatial_indices <- c()
  if (length(filter_features) > 0) {
    ff_cat("filtering features", verbose = verbose)
    for (i in seq_along(filter_features)) {
      operator <- gsub("[[:alnum:]]", "", filter_conditions[i])
      value <- as.numeric(gsub("[^0-9]", "", filter_conditions[i]))
      filtercolumn <- which(colnames(matrix) == filter_features[i])
      if (length(filtercolumn) == 0) {
        ff_cat("The feature", filter_features[i], "was not found, skipping filtering for this feature", color = "yellow")
        spatial_indices <- seq(dim(matrix)[1])
      } else {
        ff_cat("filtering feature", filter_features[i], "on", filter_conditions[i], verbose = verbose)
        if (operator == ">") {
          spatial_indices <- which(matrix[, filtercolumn] > value)
        }
        if (operator == "<") {
          spatial_indices <- which(matrix[, filtercolumn] < value)
        }
        if (operator == "==") {
          spatial_indices <- which(matrix[, filtercolumn] == value)
        }
        if (operator == "!=") {
          spatial_indices <- which(matrix[, filtercolumn] != value)
        }
        if (operator == ">=") {
          spatial_indices <- which(matrix[, filtercolumn] >= value)
        }
        if (operator == "<=") {
          spatial_indices <- which(matrix[, filtercolumn] <= value)
        }
      }
      if (length(merged_spatial_indices) == 0) {
        merged_spatial_indices <- c(merged_spatial_indices, spatial_indices)
      } else {
        merged_spatial_indices <- intersect(merged_spatial_indices, spatial_indices)
      }
    }
    spatial_indices <- unique(merged_spatial_indices)
  } else {
    spatial_indices <- NULL
  }
  if (length(spatial_indices) > 0) {
    matrix <- matrix[spatial_indices, ]
  }
  return(list("filtered_matrix" = matrix, "filtered_indices" = spatial_indices))
}
