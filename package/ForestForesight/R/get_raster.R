
#' Retrieve raster files based on specified criteria.
#'
#' This function retrieves raster files from a specified data folder based on the given date, feature, and tile pattern.
#'
#' @param datafolder A character string specifying the path to the data folder where raster files are stored.
#' @param date A Date object representing the date for which raster files are to be retrieved.
#' @param feature A character string specifying the feature of interest to filter raster files.
#' @param tile A character string specifying the pattern to filter specific tiles of raster files.
#' @return A character vector containing the file paths of the selected raster files.
#' @export
#' @examples
#' datafolder <- "/path/to/data"
#' date <- as.Date("2022-01-01")
#' feature <- "initialforestcover"
#' tile <- "00N_070W"
#' get_raster(datafolder, date, feature, tile)
#' # Returns: Vector of file paths to selected raster files.
get_raster <- function(datafolder, date, feature, tile) {
  allfiles <- list.files(datafolder, recursive = TRUE, pattern = tile, full.names = TRUE)
  allfiles <- allfiles[grep("tif$", allfiles)]
  allfiles <- allfiles[grep(feature, allfiles)]
  return(ForestForesight::select_files_date(date, allfiles))
}
