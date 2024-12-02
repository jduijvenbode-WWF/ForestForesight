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
#' result_iso <- get_info("BRA", ff_dir = "/path/to/ForestForesight", verbose = TRUE)
#'
#' # Using a SpatVector object
#' my_shape <- vect(your_shapefile_path)
#' result_shape <- get_info(my_shape, ff_dir = "/path/to/ForestForesight", verbose = TRUE)
#'
#' # Accessing results
#' num_tiles <- result_shape$num_tiles
#' tile_ids <- result_shape$tile_ids
#' overlapping_countries <- result_shape$overlapping_countries
#' country_groups <- result_shape$country_groups
#' }
#'
#' @return List containing spatial information (invisibly)
#' @export
get_info <- function(shape_or_iso, ff_dir = NULL, verbose = TRUE) {
  shape <- get_spatial_shape(shape_or_iso)
  country_info <- get_country_info(shape)
  tile_info <- get_tile_info(shape, ff_dir)
  spatial_info <- get_spatial_metrics(shape)

  results <- c(tile_info, spatial_info, country_info)

  if (verbose) {
    print_info(results)
  }

  invisible(results)
}

get_spatial_shape <- function(shape_or_iso) {
  countries <- terra::vect(get(data("countries", envir = environment())))

  if (is.character(shape_or_iso) && nchar(shape_or_iso) == 3) {
    shape <- countries[countries$iso3 == shape_or_iso, ]
    if (nrow(shape) == 0) {
      stop("Invalid ISO code: no matching country found")
    }
    return(shape)
  }

  if (inherits(shape_or_iso, "SpatVector")) {
    return(shape_or_iso)
  }

  stop("Input must be either a 3-digit ISO code or a terra SpatVector object")
}

get_country_info <- function(shape) {
  countries <- terra::vect(get(data("countries", envir = environment())))

  if ("iso3" %in% names(shape)) {
    return(list(
      overlapping_countries = shape$name,
      country_groups = unique(shape$group)
    ))
  }

  overlapping <- terra::relate(countries, terra::buffer(shape, -1), "intersects")
  list(
    overlapping_countries = countries$name[overlapping],
    country_groups = unique(countries$group[overlapping])
  )
}

get_tile_info <- function(shape, ff_dir) {
  gfw_tiles <- terra::vect(get(data("gfw_tiles", envir = environment())))
  intersecting_tiles <- terra::relate(gfw_tiles, shape, "intersects")
  covered_tiles <- gfw_tiles[intersecting_tiles, ]
  tile_ids <- covered_tiles$tile_id

  available_features <- if (!is.null(ff_dir)) {
    get_available_features(tile_ids, ff_dir)
  } else {
    NULL
  }

  list(
    num_tiles = length(tile_ids),
    tile_ids = tile_ids,
    available_features = available_features
  )
}

get_available_features <- function(tile_ids, ff_dir) {
  feature_files <- unlist(lapply(tile_ids, function(x) {
    list.files(file.path(ff_dir, "preprocessed/input", x), full.names = FALSE)
  }))

  feature_names <- tools::file_path_sans_ext(basename(feature_files))

  if (length(feature_names) > 0) {
    feature_names <- sort(unique(feature_names))
  } else {
    ff_cat("no features found for this area in given folder",
      color = "yellow", verbose = TRUE
    )
  }

  return(feature_names)
}

get_spatial_metrics <- function(shape) {
  list(
    area = terra::expanse(shape) / 10000,
    bbox = as.vector(terra::ext(shape))
  )
}

print_info <- function(results) {
  cat("Shape Information:\n")
  cat("Number of tiles covered:", results$num_tiles, "\n")
  cat("Tile IDs:", paste(results$tile_ids, collapse = ", "), "\n")

  if (!is.null(results$available_features)) {
    cat("Available features:", paste(results$available_features, collapse = ", "), "\n")
  }

  cat("Area:", format(results$area, scientific = FALSE), "hectares\n")
  cat("Bounding box (xmin, xmax, ymin, ymax):", paste(round(results$bbox, 5), collapse = ", "), "\n")
  cat("Countries:", paste(results$overlapping_countries, collapse = ", "), "\n")
  cat("Country group(s):", paste(results$country_groups, collapse = ", "), "\n")
}
