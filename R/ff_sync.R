#' Sync ForestForesight Data from S3
#'
#' This function synchronizes ForestForesight data from a public S3 bucket to a local folder.
#' It can sync data for a specific tile or an entire country, including input data, ground truth data,
#' and optionally model data.
#'
#' @param ff_folder Character. Local folder to sync data to.
#' @param identifier Character or SpatVector. When a character it should be either a tile ID (e.g., "00N_000E") or a country ISO3 code.
#' @param download_model Logical. Whether to download the corresponding model. Only works when downloading for entire countries. Default is FALSE.
#' @param download_data Logical. Whether to download the preprocessed input data. Default is TRUE.
#' @param download_groundtruth Logical. Whether to download the groundtruth data as well. This should be turned off when you want to use your own data as groundtruth. Default is TRUE.
#' @param download_predictions Logical. Whether to download the prediction data.Only works when downloading for entire countries. Default is FALSE.
#' @param bucket Character. Name of the S3 bucket. Default is "forestforesight-public".
#' @param region Character. AWS region of the bucket. Default is "eu-west-1".
#' @param verbose Logical. Whether the function should be verbose.
#' @param sync_verbose Logical. Whether the syncing should also be verbose. Default
#'
#' @import aws.s3
#' @import terra
#'
#' @return Invisible NULL. The function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' ff_sync("path/to/local/folder", "00N_000E")
#' ff_sync("path/to/local/folder", "BRA", download_model = TRUE)
#' }
#'
#' @export

# source(system.file("R", "load_config.R", package = "ForestForesight"))
config <- ff_load_config()

ff_sync <- function(ff_folder, identifier, download_model = FALSE, download_data = TRUE, download_predictions = FALSE, download_groundtruth = TRUE,
                    bucket = config$AWS_BUCKET, region = config$AWS_BUCKET_REGION, verbose = TRUE, sync_verbose = FALSE) {

  # Create ff_folder if it doesn't exist
  if (!dir.exists(ff_folder)) {
    dir.create(ff_folder, recursive = TRUE)
  }

  country_codes <- ""

  # Determine if identifier is a tile, country code, or SpatVector
  if (class(identifier) == "character" && nchar(identifier) == 7 && grepl("^[0-9]{2}[NS]_[0-9]{3}[EW]$", identifier)) {
    tiles <- identifier
  } else if (inherits(identifier, "SpatVector")) {
    identifier <- terra::buffer(identifier, width = -1)
    # Load necessary data sets
    countries <- terra::vect(get(data("countries")))
    gfw_tiles <- terra::vect(get(data("gfw_tiles")))

    # Intersect the provided SpatVector with countries to get ISO3 codes
    intersected <- terra::intersect(identifier, countries)
    country_codes <- unique(intersected$iso3)

    # Get tiles that intersect with the provided SpatVector
    tiles <- terra::intersect(gfw_tiles, identifier)
    tiles <- tiles$tile_id
  } else {
    # Load necessary data sets
    countries <- terra::vect(get(data("countries")))
    gfw_tiles <- terra::vect(get(data("gfw_tiles")))

    country_codes <- identifier

    # Filter country and get tiles
    country_shape <- countries[countries$iso3 == identifier, ]
    if (nrow(country_shape) == 0) stop("Invalid country code")
    tiles <- terra::intersect(gfw_tiles, country_shape)
    tiles <- tiles$tile_id
  }

  # Sync input and ground truth data for each tile
  if (download_data | download_groundtruth) {
    cat("Downloading input and ground truth data\n")
    for (tile in tiles) {
      # Create input sub-folder
      if (download_data) {
        input_folder <- file.path(ff_folder, "preprocessed", "input", tile)
        dir.create(input_folder, recursive = TRUE, showWarnings = FALSE)
        # Sync input data
        aws.s3::s3sync(input_folder, bucket,
          region = region, direction = "download",
          prefix = paste0("preprocessed/input/", tile), verbose = verbose
        )
      }
      # Create ground truth sub-folder
      if (download_groundtruth) {
        groundtruth_folder <- file.path(ff_folder, "preprocessed", "groundtruth", tile)
        dir.create(groundtruth_folder, recursive = TRUE, showWarnings = FALSE)
        # Sync ground truth data
        aws.s3::s3sync(groundtruth_folder, bucket,
          region = region, direction = "download",
          prefix = paste0("preprocessed/groundtruth/", tile), verbose = verbose
        )
      }
    }
  }

  # Download model if requested
  if (download_model) {
    if (length(country_codes) == 1) { # It's a single country
      group <- countries$group[countries$iso3 == country_codes]
      model_folder <- file.path(ff_folder, "models", group)
      if (verbose) cat("Downloading model to", model_folder, "\n")
      if (!dir.exists(model_folder)) dir.create(model_folder, recursive = TRUE)
      aws.s3::s3sync(model_folder, bucket,
        region = region, direction = "download",
        prefix = paste0("models/", group), verbose = sync_verbose
      )
    } else {
      warning("Model download is only available when specifying a single country code or a SpatVector that intersects with a single country.")
    }
  }

  # Download predictions if requested
  if (download_predictions) {
    if (length(country_codes) == 1) { # It's a single country
      pred_folder <- file.path(ff_folder, "predictions", country_codes)
      if (verbose) cat("Downloading predictions to", pred_folder, "\n")
      if (!dir.exists(pred_folder)) dir.create(pred_folder, recursive = TRUE)
      aws.s3::s3sync(pred_folder, bucket,
        region = region, direction = "download",
        prefix = paste0("predictions/", country_codes), verbose = sync_verbose
      )
    } else {
      warning("Predictions download is only available when specifying a single country code or a SpatVector that intersects with a single country.")
    }
  }

  invisible(NULL)
}
