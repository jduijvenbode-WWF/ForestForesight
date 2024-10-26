# library(ForestForesight)
code_location <- "C:/Kodingan3/ForestForesight"
ff_folder <- "C:/Kodingan3/FFdata"
library("devtools")
load_all(code_location) # with this we are using the live R code, not the build package of ForestForesight
library(sf)

data("countries")
proc_dates <- "2024-09-01"
start_date <- as.Date("2024-09-01")
end_date <- as.Date("2024-09-04")
step_size <- "1 day"
dates <- seq.Date(start_date, end_date, by = step_size)
countrynames <- countries$iso3

for (proc_date in proc_dates) {
  for (x in seq_along(countrynames)) {
    country <- countrynames[x]
    if (country == "PER") {
      cat(paste("processing", country, "\n"))
      setwd("C:/Kodingan3/FFdata/storage/predictions/")
      if (!dir.exists(country)) {
        dir.create(country)
      }
      setwd(country)
      if (!file.exists(paste0(country, "_", proc_date, ".tif"))) {
        cat(paste("processing", country, "for", proc_date, "\n"))
        if (!(country %in% countries$iso3)) { # added this checking
          message(paste("Country", country, "not found in countries$iso3"))
          next
        }
        shape <- terra::vect(countries[countries$iso3 == country, ])
        modelname <- countries$group[countries$iso3 == country]
        modelpath <- file.path(ff_folder, "models", modelname, paste0(modelname, ".model"))
        # if (!file.exists(modelpath)) {
        #  stop(paste(modelpath, "model does not exist"))
        # }
        tryCatch(
          {
            cat("just b4 ff_run")
            b <- ff_run(
              shape = shape,
              prediction_dates = proc_date,
              ff_folder = ff_folder,
              verbose = TRUE,
              # save_path = model_folder
              trained_model = modelpath
            )
            terra::writeRaster(b, paste0(country, "_", proc_date, ".tif"), overwrite = T)
          },
          error = function(e) {
            message("An error occurred: ", e$message)
          }
        )
      } else {
        cat("cannot run prediction: ", paste0(country, "_", proc_date, ".tif"), " already exists!")
      }
    }
  }
}
