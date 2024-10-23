#' Download GFW Alerts Data
#'
#' Downloads GFW (Global Forest Watch) Alerts data and saves it to the specified output directory.
#'
#' @param output_dir The directory where the downloaded data will be saved.
#' @param country An optional parameter specifying the ISO3 country code to filter the data. Default is NA.
#' @param separate_folders Logical. If TRUE, data is saved in separate folders based on tile_id. Default is FALSE.
#' @param basename An optional parameter to specify a basename for the downloaded files. Applicable only when separate_folders is TRUE. Default is NA.
#'
#' @return locations of the created output tif-files.
#'
#' @examples
#' \dontrun{
#' # Download GFW Alerts for a specific country and save in separate folders
#' download_gfw_alerts("path/to/output_directory", country = "SUR", separate_folders = TRUE, basename = "my_data")
#' }
#'
#' @import httr
#'
#' @export

download_gfw_alerts <- function(output_dir, country = NA, separate_folders = F, basename = NA) {
  if ((!is.na(basename)) & (!separate_folders)) {
    stop("basename can only be applied if the data is written in separate folders, otherwise they overwrite themselves")
  }
  data(gfw_tiles)
  gfw_tiles <- vect(gfw_tiles)
  if (!is.na(country)) {
    data(countries)
    countries <- vect(countries)
    countries <- countries[gfw_tiles]
    if (sum(countries$iso3 == country) == 0) {
      stop("country code not found. Either it is not covered by the GFW alerts or it is not a valid ISO3 code")
    }
    gfw_tiles <- gfw_tiles[countries[countries$iso3 == country], ]
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  outputfiles <- c()
  for (id in seq(nrow(gfw_tiles))) {
    file <- gfw_tiles$download[id]
    name <- gfw_tiles$tile_id[id]
    filename <- if (!is.na(basename)) {
      paste0(basename, ".tif")
    } else {
      paste0(name, ".tif")
    }
    dataset <- httr::GET(file)
    if (separate_folders) {
      if (!dir.exists(file.path(output_dir, name))) {
        dir.create(file.path(output_dir, name))
      }
      output_dir_new <- file.path(output_dir, name)
    } else {
      output_dir_new <- output_dir
    }
    writeBin(dataset$content, file.path(output_dir_new, filename))
    outputfiles <- c(outputfiles, file.path(output_dir_new, filename))
  }
  return(outputfiles)
}
