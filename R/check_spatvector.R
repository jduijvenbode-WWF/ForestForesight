#' Check SpatVector Properties
#'
#' This function checks if a given object is a valid SpatVector polygon with
#' appropriate coordinate system and size constraints, and validates its overlap
#' with known country boundaries.
#'
#' @param x Object to check.
#' @param check_size Logical. Whether to check if the area is within reasonable bounds. Default is TRUE.
#'
#' @return The input SpatVector, possibly reprojected to EPSG:4326.
#'
#' @details
#' The function checks:
#' - If input is a SpatVector polygon
#' - If coordinate system is properly defined
#' - If polygons are valid and non-empty
#' - If the area overlaps with known country boundaries
#' - Optionally checks if area is within reasonable bounds (larger than 10,000 ha but smaller than approximately Brazil)
#'
#' @import terra
#'
#' @references
#' Zillah Calle (2023)
#' Jonas van Duijvenbode (2023)
#'
#' @examples
#' \dontrun{
#' shape <- vect("path/to/shapefile.shp")
#' validated_shape <- check_spatvector(shape)
#' }
#'
#' @export
check_spatvector <- function(x, check_size = TRUE) {
  # Check if object is SpatVector
  if (!inherits(x, "SpatVector")) {
    stop(paste0("Input must be a SpatVector object,received ",class(x)))
  }

  # Check if it's a polygon
  if (terra::geomtype(x) != "polygons") {
    stop(paste("Input must be a polygon SpatVector, received",terra::geomtype(x)))
  }

  # Check if empty
  if (terra::nrow(x) == 0) {
    stop("Input SpatVector contains no polygons")
  }

  # Check coordinate system
  if (is.na(terra::crs(x))) {
    if (terra::ext(x)$xmin >= -180 && terra::ext(x)$xmax <= 180) {
      warning("No coordinate system defined but coordinates appear to be in decimal degrees. Assuming WGS84.")
      terra::crs(x) <- "EPSG:4326"
    } else {
      stop("No coordinate system defined and coordinates are not in decimal degrees range")
    }
  } else if (terra::crs(x) != "EPSG:4326" & length(grep("WGS 84",terra::crs(x)))==0) {
    warning("Coordinate system is not EPSG:4326. Converting to WGS84.")
    x <- terra::project(x, "EPSG:4326")
  }

  # Load countries dataset and check overlap
  countries <- terra::aggregate(terra::vect(get(data("countries",envir = environment()))))

  # Calculate overlap
  intersection <- terra::intersect(x, countries)

  if (terra::nrow(intersection) == 0) {
    stop("Input shape does not overlap with any known country boundaries")
  }

  # Calculate overlap percentage
  overlap_area <- terra::expanse(intersection)
  shape_area <- terra::expanse(x)
  overlap_percentage <- (overlap_area / shape_area) * 100

  if (overlap_percentage < 99.9) {  # Using 99.9% to account for potential rounding issues
    warning(sprintf("Input shape only partially overlaps with known country boundaries (%.1f%% overlap)",
                    overlap_percentage))
  }

  # Check area
  area_ha <- shape_area / 10000  # Convert m2 to hectares

  if (area_ha <= 0) {
    stop("Input SpatVector has zero or negative area")
  }

  if (check_size) {
    upper_area_threshold <- 1e9
    lower_area_threshold <- 1e4

    if (area_ha > upper_area_threshold) {
      warning("Input area is very large. This may impact performance and processing time.")
    }

    if (area_ha < 10000) {
      warning("Input area is smaller than 10,000 hectares. This may impact performance for training purposes.")
    }
  }

  return(x)
}
