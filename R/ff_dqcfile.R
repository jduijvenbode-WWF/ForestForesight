#' summary data aspects of a raster
#'
#' requires a path to a raster as input and gives some quality parameters of that file
#'
#' @param raster path to raster.
#' @param return_values Should the values of the rasters also be returned.
#'
#' @examples
#' # Example usage:
#' result <- process_tif_files("path/to/tif_file")
#'
#' @export
#'
#'
ff_dqc_file <- function(raster, return_values = T) {
  loadras <- rast(raster)
  return(list(
    "npixel" = ncell(loadras),
    "xmin" = xmin(loadras),
    "xmax" = xmax(loadras),
    "ymin" = ymin(loadras),
    "ymax" = ymax(loadras),
    "resolution" = res(loadras)[1],
    "crsname" = crs(loadras, describe = T)$name,
    "crscode" = crs(loadras, describe = T)$code,
    "mean" = if (return_values) {
      as.numeric(round(global(loadras, "mean", na.rm = T), 2))
    } else {
      NA
    },
    "max" = if (return_values) {
      as.numeric(round(global(loadras, "max", na.rm = T), 2))
    } else {
      NA
    },
    "hasNA" = if (return_values) {
      (length(summary(loadras, warn = F)) == 7)
    } else {
      NA
    }
  ))
}
