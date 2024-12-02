#' summary data aspects of a raster
#'
#' requires a path to a raster as input and gives some quality parameters of that file
#'
#' @param raster path to raster.
#' @param return_values Should the values of the rasters also be returned.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- process_tif_files("path/to/tif_file")
#' }
#' @export
ff_dqc_file <- function(raster, return_values = TRUE) {
  loadras <- terra::rast(raster)
  return(list(
    "pixel_count" = terra::ncell(loadras),
    "xmin" = terra::xmin(loadras),
    "xmax" = terra::xmax(loadras),
    "ymin" = terra::ymin(loadras),
    "ymax" = terra::ymax(loadras),
    "resolution" = terra::res(loadras)[1],
    "crsname" = terra::crs(loadras, describe = TRUE)$name,
    "crscode" = terra::crs(loadras, describe = TRUE)$code,
    "mean" = if (return_values) {
      as.numeric(round(terra::global(loadras, "mean", na.rm = TRUE), 2))
    } else {
      NA
    },
    "max" = if (return_values) {
      as.numeric(round(terra::global(loadras, "max", na.rm = TRUE), 2))
    } else {
      NA
    },
    "hasNA" = if (return_values) {
      (length(summary(loadras, warn = FALSE)) == 7)
    } else {
      NA
    }
  ))
}
