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
#' @param error_on_issue Logical. Whether the function should return an error if critical
#' issues are not handled correctly
#' @param silent_on_pass Logical. Whether correct checks should not be output
#'
#' @return Invisible NULL. The function is called for its side effects (printing).
#'
#' @import terra
#' @importFrom lubridate floor_date
#'
#' @export
ff_structurecheck <- function(shape, folder_path, check_date = NULL, error_on_issue = FALSE, silent_on_pass = FALSE) {
  # Get info from shape
  info <- getinfo(shape, verbose = FALSE)

  # Set check_date if not provided
  if (is.null(check_date)) {
    check_date <- format(lubridate::floor_date(Sys.Date(), "month"), "%Y-%m-01")
  }

  # Check main folders
  ff_cat("Checking main folder\n",verbose = silent_on_pass)
  main_folders <- c("preprocessed", "models", "predictions")
  all_correct = T
  for (folder in main_folders) {
    if (!dir.exists(file.path(folder_path, folder))) {
      all_correct = F
      print_result(paste("No", folder, "folder present\n"), color = "red")
    } else {
      print_result(paste(folder, "folder present"), color = "green", verbose = !silent_on_pass)
    }
  }
  if (!all_correct && error_on_issue){
    stop("some main folders are missing.
         Fix issues as printed above and try again")
  }

  # Check preprocessed subfolders
  ff_cat("\nChecking preprocessed folder")
  prep_subfolders <- c("input", "groundtruth")
  for (subfolder in prep_subfolders) {
    folder <- file.path(folder_path, "preprocessed", subfolder)
    if (!dir.exists(folder)) {
      print_result(paste("No", subfolder, "subfolder in preprocessed"), color = "red",error_on_issue = error_on_issue)

    } else {
      print_result(subfolder, "subfolder present in preprocessed", color = "green", verbose = !silent_on_pass)
      # Check tile subfolders
      for (tile in info$tile_ids) {
        if (!dir.exists(file.path(folder, tile))) {
          print_result("No subfolder for tile", tile, "in", subfolder, color = "red",error_on_issue = error_on_issue)
        } else {
          print_result("Subfolder for tile", tile, "present in", subfolder, color = "green", verbose = !silent_on_pass)
          # Check for correct file naming and date
          files <- list.files(file.path(folder, tile), pattern = "\\.tif$")
          if (length(files) == 0) {
            print_result("No tif files in", subfolder, "for tile", tile, color = "red",error_on_issue = error_on_issue)
          } else {
            correct_name_pattern <- paste0("^", tile, "_\\d{4}-\\d{2}-01_[^_]+\\.tif$")
            incorrect_files <- files[!grepl(correct_name_pattern, files)]
            if (length(incorrect_files) > 0) {
              print_result("Incorrect file naming in", subfolder, "for tile", tile, ":", color = "red",error_on_issue = FALSE)
              for (file in incorrect_files) {
                print_result("  ", basename(file), color = "red")
              }
              if (error_on_issue) {stop("please fix issues above and try again")}
            } else {
              print_result(paste("Correct file naming in", subfolder, "for tile", tile, "\n"), color = "green", silent_on_pass = silent_on_pass)
            }
            has_check_date <- any(grepl(paste0("^", tile, "_", check_date, "_lastsixmonths.tif$"), files))
            if (subfolder != "groundtruth") {
              if (!has_check_date) {
                print_result(
                  "No EWS features for check date", check_date, "in", subfolder,
                  "for tile", tile,
                color = "yellow")
              } else {
                print_result(
                  "EWS features for check date", check_date, "present in", subfolder,
                  "for tile", tile, color = "green", silent_on_pass = silent_on_pass)
              }
            }
            if (subfolder == "groundtruth") {
              has_groundtruth6m <- any(grepl(paste0("^", tile, "_", check_date, "_groundtruth6m\\.tif$"), files))
              if (!has_groundtruth6m) {
                print_result(paste(
                  "No groundtruth6m file for check date", check_date,
                  "in groundtruth for tile", tile, "\n"
                ), color = "yellow")
              } else {
                print_result(
                  "Groundtruth6m file present for check date", check_date,
                  "in groundtruth for tile", tile, "\n"
                , color = "green",silent_on_pass = silent_on_pass)
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
      print_result("No subfolder for group", group, "in models", color = "red",error_on_issue = error_on_issue)
    } else {
      print_result("Subfolder for group", group, "present in models", color = "green",silent_on_pass = silent_on_pass)
      model_file <- file.path(group_folder, paste0(group, ".model"))
      rda_file <- file.path(group_folder, paste0(group, ".rda"))
      if (!file.exists(model_file)) {
        print_result(paste("No .model file for group", group, "\n"), color = "yellow")
      } else {
        print_result(paste(".model file present for group", group, "\n"), color = "green",silent_on_pass = silent_on_pass)
      }
      if (!file.exists(rda_file)) {
        if (file.exists(model_file)){
        print_result(paste("No .rda file for group", group, "\n"), color = "red",error_on_issue = error_on_issue)
        }else{
          print_result(paste("No .rda file for group", group, "\n"), color = "yellow")
        }
      } else {
        print_result(paste(".rda file present for group", group, "\n"), color = "green",silent_on_pass = silent_on_pass)
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
      print_result(paste("No subfolder for ISO3 code", iso3, "in predictions\n"), color = "red",error_on_issue = error_on_issue)
    } else {
      print_result(paste("Subfolder for ISO3 code", iso3, "present in predictions\n"), color = "green",silent_on_pass = silent_on_pass)
      files <- list.files(iso3_folder, pattern = "\\.tif$")
      if (length(files) == 0) {
        print_result(paste("No tif files in predictions for ISO3 code", iso3, "\n"), color = "yellow")
      } else {
        correct_name <- all(grepl(paste0("^", iso3, "_\\d{4}-\\d{2}-01\\.tif$"), files))
        if (!correct_name) {
          print_result("Incorrect file naming in predictions for ISO3 code", iso3, color = "red",error_on_issue = error_on_issue)
        } else {
          print_result("Correct file naming in predictions for ISO3 code", iso3, color = "green",silent_on_pass = silent_on_pass)
        }
      }
    }
  }

  invisible(NULL)
}

print_result = function(...,color,silent_on_pass = FALSE, error_on_issue = FALSE){
  statement <- paste(..., sep = " ")
  if(silent_on_pass && color == "green"){ invisible(NULL)}else{
    if(color == "red" && error_on_issue){
      stop(statement)}else{
        ff_cat(statement,color = color)
      }
  }
}
