#' Polygonize raster to risk areas
#'
#' This function processes a raster file, applies thresholds, creates polygons,
#' and simplifies them based on various parameters.
#'
#' @param raster Character or SpatRaster. Path to the input raster file or a SpatRaster.
#' @param output_file Character. File path to save shapefiles. Default is NA and means the polygons are returned and not written.
#' @param min_pixel Numeric. Base pixel count to filter on. Default is 5
#' @param threshold Numeric or Character. Threshold to apply to the raster. Default is 0.5.
#' Options are:
#' \describe{
#'   \item{"numeric"}{A value between 0 and 1 that reflects the value that you want as a threshold}
#'   \item{"medium"}{Automatically chosen threshold for medium risk}
#'   \item{"high"}{Automatically chosen threshold for medium risk}
#'   \item{"very high"}{Automatically chosen threshold for medium risk}
#' }
#' @param window_size Numeric. Window size for focal calculation. Default is 7.
#' @param smoothness Numeric. Smoothness parameter for ksmooth method. Default is 2
#' @param verbose Logical. Whether the automatically detected threshold should be plotted
#' @param calc_max Logical. If this is set to True a reasonable amount of polygons will be outputted depending on the area size
#' @param contain_polygons Logical. If calc_max is enabled, only polygons will be created that lie within the SpatVector contain_polygons
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
                          smoothness = 2,
                          verbose = F,
                          calc_max = F,
                          contain_polygons = NA) {
  if (calc_max & !hasvalue(contain_polygons)) {
    ff_cat("since no container polygons were given the calc_max option might give weird results, with higher risk areas popping up where no medium risk areas are found.\n",color = "yellow")
  }
  # Load raster
  pixel_size = 2e5
  if (class(raster) == "character") {raster <- terra::rast(raster)}
  # Set options and initialize variables

  if (class(threshold) == "character") {
    if (calc_max) {perc_covered <- as.numeric(terra::global(!is.na(raster),"mean"))}
    raster[raster < 0.5] = NA
  if (terra::global(!is.na(raster), fun = "sum") == 0) {
    ff_cat("no values in this raster above 0.5 were found,
           which is the minimum threshold of predictions FF provides when using auto-thresholding.
           Use a value as threshold if you still want polygons\n",color = "yellow")
    return(NULL)
  }
  }
  pixel_min <- 5 * pixel_size
  # Apply focal mean and threshold
  br <- terra::focal(raster, w = window_size, fun = "mean",na.policy = "omit",na.rm = T)
  if (is.character(threshold)) {
    if (threshold == "medium") {newthreshold <- quantile(as.matrix(br), probs = 0,na.rm = T)}
    if (threshold == "high") {newthreshold <- quantile(as.matrix(br), probs = 0.5,na.rm = T)}
    if (threshold == "very high") {newthreshold <- quantile(as.matrix(br), probs = 0.75,na.rm = T)}
    if (!exists("newthreshold")) {stop("the given character is not one of the possibilities medium, high or very high")}
    if (verbose) {ff_cat("new threshold is",newthreshold,"\n")}
    threshold <- newthreshold
  }
  br <- br > threshold
  # Create patches
  clumped_raster <- terra::patches(br, directions = 8, zeroAsNA = TRUE)

  # Convert to polygons and filter by size
  pols <- terra::as.polygons(clumped_raster, values = FALSE, aggregate = TRUE, round = FALSE)
  suppressWarnings({
    pols <- smoothr::fill_holes(pols, threshold = 5 * pixel_size)
    pols <- smoothr::smooth(pols, method = "ksmooth", smoothness = smoothness)
  })
  pols <- terra::disagg(pols)
  sorted_pols <- pols[order(terra::expanse(pols), decreasing = TRUE)]
  if (calc_max) {
    if (hasvalue(contain_polygons)) {sorted_pols <- sorted_pols[contain_polygons,]}
  # Take all polygons larger than pixel_min, or at least the 25 largest
  if (!exists("perc_covered")) {perc_covered <- as.numeric(terra::global(!is.na(raster),"mean"))}


  sqmras <- as.numeric(terra::expanse(raster)[2])

  ceiling_pols <- ceiling(sqrt(sqmras/1e3)*perc_covered)
  if (verbose) {cat("based on area of raster (hectares:"
                    ,round(sqmras/1e5),
                    ", actual coverage:",round(perc_covered*100),"percent), at maximum",ceiling_pols," polygons are generated\n")}
  pols <- sorted_pols[1:max(1,min(ceiling_pols, sum(terra::expanse(sorted_pols) >= pixel_min)))]
}
  # Select top polygons by size


  # Fill holes and smooth

  if (length(pols) == 0) {ff_cat("Based on the chosen threshold no polygons were generated. Lower the threshold to get polygons for this area\n",color = "yellow")
    return(NULL)
  }
  # Extract average values from original raster
  pols$risk <- round(terra::extract(raster, pols, fun = "mean", ID = FALSE),2)
  pols$size <- round(terra::expanse(pols)/10000)
  pols$sumrisk <- round(pols$size * pols$risk)
  pols$threshold <- threshold
  pols$date = as.character(as.Date(Sys.time()))
  if (verbose) {ff_cat("writing",length(pols),"polygons to",output_file,"\n")}
  # Save result
  if (!is.na(output_file)) {terra::writeVector(x = pols, filename = output_file, overwrite = TRUE)}
  return(pols)
}
