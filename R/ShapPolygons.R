library(ForestForesight)
library("SHAPforxgboost")
# load data
data("countries")
countries <- terra::vect(countries)
groups = unique(countries$group)
ff_folder = "D:/ff-dev/results"
date = "2023-08-01"
analysis_polygons = NULL
threshold = 0.5
csvfile = paste0("D:/ff-dev/results/accuracy_analysis/SHAP", gsub("\\.", "", as.character(threshold)) ,".csv")
append = T


for (country in countries$iso3[1:6]) {
  cat("starting on ", country ,"\n")
  prediction_rast= rast(paste0("D:/ff-dev/results/predictions/",country, '/', country, "_",date, ".tif"))>threshold
  plot(prediction_rast)
  group= countries$group[countries$iso3==country]
  modelfilename= paste0("D:/ff-dev/results/models/", group,"/", group,"_small.model")
  model=xgboost::xgb.load(modelfilename)
  model_features <- get(load(gsub("\\.model","\\.rda",modelfilename)))
  attr(model,"feature_names") <- model_features
  if (is.null(analysis_polygons)) {
    data(degree_polygons,envir = environment())
    pols <- terra::vect(degree_polygons)}else{
      if (class(analysis_polygons == "character")) {
        pols <- terra::vect(analysis_polygons)}else{
          pols <- analysis_polygons}}
  pols <- pols[which(pols$iso3 == country)]
  plot(pols, add=T)
  # Initialize an empty list to store data frames
  for (i in 1:length(pols)){
    tryCatch({print(i)
      pol = buffer(pols[i],-100)
      pol_data = ff_prep(shape = pol, start = date, shrink = "extract", addxy = T, datafolder= paste0(ff_folder,"/preprocessed"), inc_features = model_features)
      pred_pol <- mask(prediction_rast, pol)
      coordinates <- terra::xyFromCell(pred_pol, which(values(pred_pol) == 1))
      coordinates_df <- as.data.frame(coordinates)

      # Combine x and y into a single string for easier matching
      pol_data_coords <- paste(pol_data$data_matrix$features[,"x"], pol_data$data_matrix$features[,"y"], sep = ",")
      target_coords <- paste(round(coordinates_df[,1],3), round(coordinates_df[,2],3), sep = ",")

      if (length(target_coords)>0){
        # Select matching rows based on the combined coordinates
        pol_data_sel <- pol_data$data_matrix$features[pol_data_coords %in% target_coords, ][,1:length(model_features)]

        # NOTE NOT ALL PIXELS ARE FOUND!!!
        shap_values = shap.values(xgb_model = model, X_train = pol_data_sel)$mean_shap_score
        shap_values_df <- data.frame(
          feature = names(shap_values),
          shap_value = shap_values,
          coordname = pol$coordname,
          iso3 = pol$iso3,
          name = pol$name,
          tile = pol$tile,
          group= group
        )

        if (!is.null(csvfile)) {
          if ( append & file.exists(csvfile)) {
            pastdata <- read.csv(csvfile)
            pastdata$X <- NULL
            write.csv(rbind(pastdata,shap_values_df),csvfile)}else{
              if (!file.exists(csvfile) & append ) {warning("the given file does not exist, while append was set to TRUE")}
              write.csv(shap_values_df,csvfile)
            }
        }
        plot(pol, add=T, col=rgb(0, 0, 1, 0.5))
      } else {plot(pol, add=T, col=rgb(1, 0, 0, 0.5))}


    }, error = function(e) {
      # Print the error message
      print(paste("An error occurred:", e))
      # Continue the loop or execute other code as needed
    })


  }
}

