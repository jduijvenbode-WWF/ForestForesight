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
  if (is.character(raster)) {
    if (!file.exists(raster)) {
      stop(paste("file does not exist:", raster))
    }
    loaded_raster <- terra::rast(raster)
  } else {
    if (!inherits(raster, "SpatRaster")) {
      stop("input should be a SpatRaster or a filename of a TIF file")
    }
  }

  return(list(
    "pixel_count" = terra::ncell(loaded_raster),
    "xmin" = terra::xmin(loaded_raster),
    "xmax" = terra::xmax(loaded_raster),
    "ymin" = terra::ymin(loaded_raster),
    "ymax" = terra::ymax(loaded_raster),
    "resolution" = terra::res(loaded_raster)[1],
    "crs_name" = terra::crs(loaded_raster, describe = TRUE)$name,
    "crs_code" = terra::crs(loaded_raster, describe = TRUE)$code,
    "mean" = if (return_values) {
      as.numeric(round(terra::global(loaded_raster, "mean", na.rm = TRUE), 2))
    } else {
      NA
    },
    "max" = if (return_values) {
      as.numeric(round(terra::global(loaded_raster, "max", na.rm = TRUE), 2))
    } else {
      NA
    },
    "has_na" = if (return_values) {
      (length(summary(loaded_raster, warn = FALSE)) == 7)
    } else {
      NA
    }
  ))
}
