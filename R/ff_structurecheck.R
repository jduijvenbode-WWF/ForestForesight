#' Check and Report ForestForesight File Structure
#'
#' This function checks the file structure for ForestForesight data based on a given
#' spatial vector and folder path. It reports on the presence and correctness of
#' various folders and files required for ForestForesight operations.
#'
#' @param shape A SpatVector object representing the area of interest.
#' @param folder_path Character string. Path to the main ForestForesight folder.
#' @param check_date Character string. Date to check for in the format "YYYY-MM-DD".
#'        If NULL, uses the first of the current month.
#'
#' @return Invisible NULL. The function is called for its side effects (printing).
#'
#' @import terra
#' @importFrom lubridate floor_date
#'
#' @export
ff_structurecheck <- function(shape, folder_path, check_date = NULL) {
  # Get info from shape
  info <- getinfo(shape, verbose = F)

  # Set check_date if not provided
  if (is.null(check_date)) {
    check_date <- format(lubridate::floor_date(Sys.Date(), "month"), "%Y-%m-01")
  }

  # Check main folders
  cat("Checking main folder\n")
  main_folders <- c("preprocessed", "models", "predictions")
  for (folder in main_folders) {
    if (!dir.exists(file.path(folder_path, folder))) {
      ff_cat(paste("No", folder, "folder present\n"), color = "red")
    } else {
      ff_cat(paste(folder, "folder present\n"), color = "green")
    }
  }

  # Check preprocessed subfolders
  cat("\nChecking preprocessed folder\n")
  prep_subfolders <- c("input", "groundtruth")
  for (subfolder in prep_subfolders) {
    folder <- file.path(folder_path, "preprocessed", subfolder)
    if (!dir.exists(folder)) {
      ff_cat(paste("No", subfolder, "subfolder in preprocessed\n"), color = "red")
    } else {
      ff_cat(paste(subfolder, "subfolder present in preprocessed\n"), color = "green")
      # Check tile subfolders
      for (tile in info$tile_ids) {
        if (!dir.exists(file.path(folder, tile))) {
          ff_cat(paste("No subfolder for tile", tile, "in", subfolder, "\n"), color = "red")
        } else {
          ff_cat(paste("Subfolder for tile", tile, "present in", subfolder, "\n"), color = "green")
          # Check for correct file naming and date
          files <- list.files(file.path(folder, tile), pattern = "\\.tif$")
          if (length(files) == 0) {
            ff_cat(paste("No tif files in", subfolder, "for tile", tile, "\n"), color = "red")
          } else {
            correct_name_pattern <- paste0("^", tile, "_\\d{4}-\\d{2}-01_[^_]+\\.tif$")
            incorrect_files <- files[!grepl(correct_name_pattern, files)]
            if (length(incorrect_files) > 0) {
              ff_cat(paste("Incorrect file naming in", subfolder, "for tile", tile, ":\n"), color = "red")
              for (file in incorrect_files) {
                ff_cat(paste("  ", basename(file), "\n"), color = "red")
              }
            } else {
              ff_cat(paste("Correct file naming in", subfolder, "for tile", tile, "\n"), color = "green")
            }
            has_check_date <- any(grepl(paste0("^", tile, "_", check_date, "_lastsixmonths.tif$"), files))
            if (subfolder != "groundtruth") {
              if (!has_check_date) {
                ff_cat(paste("No EWS features for check date", check_date, "in", subfolder, "for tile", tile, "\n"), color = "yellow")
              } else {
                ff_cat(paste("EWS features for check date", check_date, "present in", subfolder, "for tile", tile, "\n"), color = "green")
              }
            }
            if (subfolder == "groundtruth") {
              has_groundtruth6m <- any(grepl(paste0("^", tile, "_", check_date, "_groundtruth6m\\.tif$"), files))
              if (!has_groundtruth6m) {
                ff_cat(paste("No groundtruth6m file for check date", check_date, "in groundtruth for tile", tile, "\n"), color = "yellow")
              } else {
                ff_cat(paste("Groundtruth6m file present for check date", check_date, "in groundtruth for tile", tile, "\n"), color = "green")
              }
            }
          }
        }
        cat("\n")
      }
    }
  }

  # Check models folder
  cat("\nChecking models folder\n")
  for (group in info$country_groups) {
    group_folder <- file.path(folder_path, "models", group)
    if (!dir.exists(group_folder)) {
      ff_cat(paste("No subfolder for group", group, "in models\n"), color = "red")
    } else {
      ff_cat(paste("Subfolder for group", group, "present in models\n"), color = "green")
      model_file <- file.path(group_folder, paste0(group, ".model"))
      rda_file <- file.path(group_folder, paste0(group, ".rda"))
      if (!file.exists(model_file)) {
        ff_cat(paste("No .model file for group", group, "\n"), color = "yellow")
      } else {
        ff_cat(paste(".model file present for group", group, "\n"), color = "green")
      }
      if (!file.exists(rda_file)) {
        ff_cat(paste("No .rda file for group", group, "\n"), color = "red")
      } else {
        ff_cat(paste(".rda file present for group", group, "\n"), color = "green")
      }
    }
  }

  # Check predictions folder
  cat("\nChecking predictions folder\n")
  for (cname in info$overlapping_countries) {
    countries <- get(data("countries", envir = environment()))
    iso3 <- countries$iso3[countries$name == cname]
    iso3_folder <- file.path(folder_path, "predictions", iso3)
    if (!dir.exists(iso3_folder)) {
      ff_cat(paste("No subfolder for ISO3 code", iso3, "in predictions\n"), color = "red")
    } else {
      ff_cat(paste("Subfolder for ISO3 code", iso3, "present in predictions\n"), color = "green")
      files <- list.files(iso3_folder, pattern = "\\.tif$")
      if (length(files) == 0) {
        ff_cat(paste("No tif files in predictions for ISO3 code", iso3, "\n"), color = "yellow")
      } else {
        correct_name <- all(grepl(paste0("^", iso3, "_\\d{4}-\\d{2}-01\\.tif$"), files))
        if (!correct_name) {
          ff_cat(paste("Incorrect file naming in predictions for ISO3 code", iso3, "\n"), color = "red")
        } else {
          ff_cat(paste("Correct file naming in predictions for ISO3 code", iso3, "\n"), color = "green")
        }
      }
    }
  }

  invisible(NULL)
}
