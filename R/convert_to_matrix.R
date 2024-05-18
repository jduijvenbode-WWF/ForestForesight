#' Convert a list of files to a matrix
#'
#' This function converts a list of files into a matrix, where each row represents
#' a unique feature and each column represents a spatial pointer.
#'
#' @param files A character vector of file paths.
#' @param daterange A vector specifying the date range.
#' @return A list containing matrices, where each matrix represents a feature.
#'
#' @export
convert_to_matrix <- function(files, daterange) {
  # Function to select files by feature name
  select_by_featurename <- function(filenames, feature) {
    return(sort(filenames[which(substr(basename(filenames), 21, nchar(basename(filenames)) - 4) == feature)]))
  }

  # Initialize an empty list to store matrices for each feature
  featurelist <- list()

  # Iterate over each date in the date range
  for (i in daterange) {

    # Select files for the current date
    selfiles <- select_files_date(i, files)

    # Extract unique feature names from the selected files
    features <- unique(sapply(selfiles, function(x) substr(basename(x), 21, nchar(basename(x)) - 4)))

    curmat <- t(do.call(cbind, lapply(features, function(x) select_by_featurename(selfiles, x))))

    # Set column names
    colnames(curmat) <- basename(dirname(curmat[1,]))

    # Append the current matrix to the list of feature matrices
    featurelist <- append(featurelist, list(as.data.frame(curmat)))
  }
  return(featurelist)
}
