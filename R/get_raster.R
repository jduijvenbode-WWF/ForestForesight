#' Retrieve raster files based on specified criteria.
#'
#' This function retrieves raster files from a specified data folder based on the given date, feature, and tile pattern.
#'
#' @param datafolder A character string specifying the path to the data folder where raster files are stored.This should be in the preprocessed folder of Forest Foresight.
#' @param date A Date object representing the date for which raster files are to be retrieved.
#' @param feature A character string specifying the feature of interest to filter raster files.
#' @param tile A character string specifying the pattern to filter specific tiles of raster files.
#' @param shape A spatial shape that can be used instead of tile to return the files for one or more files.
#' @param return_raster Logical. Whether it should return the (mosaiced) raster instead of just the file names.
#' @param verbose Logical. Whether print statements should be output.
#' @return A character vector containing the file paths of the selected raster files.


#' @export
#' @examples
#' datafolder <- "/path/to/data"
#' date <- as.Date("2022-01-01")
#' feature <- "initialforestcover"
#' tile <- "00N_070W"
#' get_raster(datafolder, date, feature, tile)
#' # Returns: Vector of file paths to selected raster files.
get_raster <- function(datafolder, date, feature, tile = NULL, shape = NULL, return_raster = F, verbose = F) {
  if ((hasvalue(tile) + hasvalue(shape)) != 1) {
    stop("either a tile or a shape should be given")
  }
  if (hasvalue(shape)) {
    gfw_tiles <- terra::vect(get(data("gfw_tiles", envir = environment())))
    tile <- gfw_tiles[terra::project(shape, gfw_tiles), ]$tile_id
    if (verbose) {
      ff_cat("getting data from tiles", paste(tile, collapse = ", "), "\n")
    }
  }
  allfiles <- unlist(sapply(tile, function(x) list.files(datafolder, recursive = TRUE, pattern = x, full.names = TRUE)))
  allfiles <- allfiles[grep("tif$", allfiles)]
  allfiles <- allfiles[grep(paste0("_", feature, "\\."), allfiles)]
  filename <- ForestForesight::select_files_date(date, allfiles)
  if (!hasvalue(filename)) {
    stop("no files were found in this folder with this combination of date, tile and feature")
  }
  if (verbose) {
    ff_cat("found files", paste(filename, collapse = ", "), "\n")
  }
  if (return_raster) {
    if (length(tile) == 1) {
      return(terra::rast(filename))
    } else {
      if (verbose) {
        ff_cat("merging tiles\n")
      }
      return(do.call(terra::merge, unname(sapply(filename, function(x) terra::rast(x)))))
    }
  } else {
    return(filename)
  }
}
