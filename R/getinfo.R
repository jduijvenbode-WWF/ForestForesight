#' Get Information About a Spatial Shape or Country
#'
#' This function provides information about a given spatial shape or country,
#' including the number of tiles it covers, available features, area, bounding box,
#' overlapping countries, and country groups.
#'
#' @param shape_or_iso Either a SpatVector object representing a spatial shape,
#'   or a 3-digit ISO country code as a character string.
#' @param ff_dir Character string. The directory path to the ForestForesight data.
#'   If NULL, available features will not be determined. Default is NULL.
#' @param verbose Logical. If TRUE, prints the information to the console.
#'   Default is TRUE.
#'
#' @return An invisible list containing:
#'   \item{num_tiles}{Number of tiles covered by the shape}
#'   \item{tile_ids}{Vector of tile IDs covered by the shape}
#'   \item{available_features}{Vector of available features for the covered tiles}
#'   \item{area}{Area of the shape in hectares}
#'   \item{bbox}{Bounding box of the shape (xmin, xmax, ymin, ymax)}
#'   \item{overlapping_countries}{Vector of countries that overlap with the shape}
#'   \item{country_groups}{Vector of country groups the shape belongs to}
#'
#' @details
#' If a 3-digit ISO code is provided, the function uses the 'countries' dataset
#' from the ForestForesight package to get the corresponding country shape.
#' The function then determines which Global Forest Watch (GFW) tiles intersect
#' with the given shape, calculates the area, finds the bounding box, determines
#' overlapping countries, and identifies the country groups. If a ForestForesight
#' directory is provided, it also determines the available features for the
#' intersecting tiles.
#'
#' @examples
#' \dontrun{
#' # Using an ISO code
#' result_iso <- getinfo("BRA", ff_dir = "/path/to/ForestForesight", verbose = TRUE)
#'
#' # Using a SpatVector object
#' my_shape <- vect(your_shapefile_path)
#' result_shape <- getinfo(my_shape, ff_dir = "/path/to/ForestForesight", verbose = TRUE)
#'
#' # Accessing results
#' num_tiles <- result_shape$num_tiles
#' tile_ids <- result_shape$tile_ids
#' overlapping_countries <- result_shape$overlapping_countries
#' country_groups <- result_shape$country_groups
#' }
#'
#' @import terra
#' @importFrom tools file_path_sans_ext
#'
#' @export
getinfo <- function(shape_or_iso, ff_dir = NULL, verbose = TRUE) {
  extract_feature_name <- function(filename) {
    parts <- strsplit(filename, "_")[[1]]
    last_part <- parts[length(parts)]
    feature_name <- tools::file_path_sans_ext(last_part)
    return(feature_name)
  }

  # Load countries data
  countries <- terra::vect(get(data("countries")))

  # Check if input is ISO code or SpatVector
  if (is.character(shape_or_iso) && nchar(shape_or_iso) == 3) {
    shape <- countries[countries$iso3 == shape_or_iso, ]
    if (nrow(shape) == 0) {
      stop("Invalid ISO code: no matching country found")
    }
    country_name <- shape$name
    country_groups <- unique(shape$group)
  } else if (inherits(shape_or_iso, "SpatVector")) {
    shape <- shape_or_iso
    overlapping <- terra::relate(countries, terra::buffer(shape,-1), "intersects")
    overlapping_countries <- countries$name[overlapping]
    country_groups <- unique(countries$group[overlapping])
  } else {
    stop("Input must be either a 3-digit ISO code or a terra SpatVector object")
  }

  # Load gfw_tiles data
  gfw_tiles <- terra::vect(get(data("gfw_tiles")))

  # Find intersecting tiles
  intersecting_tiles <- terra::relate(gfw_tiles, shape, "intersects")
  covered_tiles <- gfw_tiles[intersecting_tiles, ]

  # Get tile IDs
  tile_ids <- covered_tiles$tile_id

  # Check available features
  if (!is.null(ff_dir)) {
    available_features <- unlist(sapply(tile_ids, function(x) list.files(file.path(ff_dir, "preprocessed/input", x), full.names = FALSE)))
    available_features <- sort(unique(sapply(available_features, function(x) extract_feature_name(x))))
  } else {
    available_features <- NULL
  }

  # Calculate area
  area <- terra::expanse(shape) / 10000

  # Get bounding box
  bbox <- terra::ext(shape)

  # Prepare results
  results <- list(
    num_tiles = length(tile_ids),
    tile_ids = tile_ids,
    available_features = available_features,
    area = area,
    bbox = as.vector(bbox),
    overlapping_countries = if (exists("overlapping_countries")) overlapping_countries else country_name,
    country_groups = country_groups
  )

  # Print verbose output if requested
  if (verbose) {
    cat("Shape Information:\n")
    cat("Number of tiles covered:", results$num_tiles, "\n")
    cat("Tile IDs:", paste(results$tile_ids, collapse = ", "), "\n")
    if (!is.null(available_features)) {
      cat("Available features:", paste(results$available_features, collapse = ", "), "\n")
    }
    cat("Area:", format(results$area, scientific = FALSE), "hectares\n")
    cat("Bounding box (xmin, xmax, ymin, ymax):", paste(round(results$bbox,5), collapse = ", "), "\n")
    if (exists("overlapping_countries")) {
      cat("Overlapping countries:", paste(results$overlapping_countries, collapse = ", "), "\n")
    } else {
      cat("Country:", results$overlapping_countries, "\n")
    }
    cat("Country group(s):", paste(results$country_groups, collapse = ", "), "\n")
  }

  # Return results invisibly
  invisible(results)
}
