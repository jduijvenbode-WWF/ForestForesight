#' Polygonize raster to risk areas
#'
#' This function processes a raster file, applies thresholds, creates polygons,
#' and simplifies them based on various parameters.
#'
#' @param raster Character or SpatRaster. Path to the input raster file or a SpatRaster.
#' @param output_file Character. File path to save shapefiles. Default is NA and means the polygons are returned and not written.
#' @param min_pixel Numeric. Base pixel count to filter on. Default is 5
#' @param threshold Numeric. Threshold to apply to the raster. Default is 0.5
#' @param window_size Numeric. Window size for focal calculation. Default is 7.
#' @param smoothness Numeric. Smoothness parameter for ksmooth method. Default is 2
#'
#' @return A SpatVector object containing the processed and simplified polygons.
#'
#' @import terra
#' @import smoothr
#'
#' @examples
#' \dontrun{
#' ff_polygonize("path/to/raster.tif", "output_polygons.shp",
#'               min_pixel = 5, threshold = 0.5, window_size = 7)
#' }
#'
#' @references
#' Jonas van Duijvenbode (2023)
#' Zillah Calle (2023)
#'
#' @export
ff_polygonize <- function(raster,
                          output_file = NA,
                          min_pixel = 5,
                          threshold = 0.5,
                          window_size = 7,
                          smoothness = 2) {
  # Load raster
  pixel_size = 2e5
  if(class(raster)=="character"){raster <- terra::rast(raster)}
  # Set options and initialize variables
  raster[is.na(raster)] <- 0
  pixel_min <- 5 * pixel_size
  # Apply focal mean and threshold
  br <- terra::focal(raster, w = window_size, fun = "mean")
  br <- br > threshold
  # Create patches
  clumped_raster <- terra::patches(br, directions = 8, zeroAsNA = TRUE)

  # Convert to polygons and filter by size
  pols <- terra::as.polygons(clumped_raster, values = FALSE, aggregate = TRUE, round = FALSE)
  sorted_pols <- pols[order(terra::expanse(pols), decreasing = TRUE)]

  # Take all polygons larger than pixel_min, or at least the 25 largest
  pols <- sorted_pols[1:max(25, sum(terra::expanse(sorted_pols) >= pixel_min))]

  # Select top polygons by size


  # Fill holes and smooth
  suppressWarnings({
    pols <- smoothr::fill_holes(pols, threshold = 5 * pixel_size)
    pols <- smoothr::smooth(pols, method = "ksmooth", smoothness = smoothness)
  })
  # Extract average values from original raster
  pols$risk <- round(terra::extract(raster, pols, fun = "mean", ID = FALSE),2)
  pols$size <- round(terra::expanse(pols)/10000)
  pols$sumrisk <- round(pols$size * pols$risk)
  # Save result
  if(!is.na(output_file)){terra::writeVector(pols, output_file, overwrite = TRUE)}
  return(pols)
}
