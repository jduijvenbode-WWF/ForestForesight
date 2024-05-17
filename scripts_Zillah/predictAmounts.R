## set environment ##

library(ForestForesight)
data("gfw_tiles")
gfw_tiles = vect(gfw_tiles)
data("countries")
countries = vect(countries)
Sys.setenv("xgboost_datafolder" = "D:/ff-dev/results/preprocessed")
groups = unique(countries$group)
dates = daterange("2022-06-01","2023-07-01")
end_date = dates[length(dates)]
exp_name = "pred_amounts"

# Select the model to use for binary classification
bin_model_name = "lastYear_training"

for (group in groups[2]) {
  tryCatch({
    cat(" starting group ",group, "\n")
    countriessel = countries$iso3[which(countries$group == group)]
    alldata = ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",
                      country = countriessel,start = "2021-01-01",end = end_date,
                      fltr_features = c("groundtruth6m","initialforestcover"),fltr_condition = c(">0",">0"),
                      sample_size = 1,verbose = F,shrink = "extract",
                      label_threshold = NA,addxy = F,
                      groundtruth_pattern = "groundtruth6m", validation_sample = 0.2)
    for (dr2 in dates[1]) {
      new_date = round(as.numeric(lubridate::as.period(as.Date(dr2) - as.Date("2019-01-01"),"months"),"months"))
      new_end_date = new_date - 6
      new_start_date = new_end_date - 12
      if (!dir.exists(file.path("D:/ff-dev/predictionsZillah/models/",group))) {dir.create(file.path("D:/ff-dev/predictionsZillah/models/",group))}
      sel_data = alldata$data_matrix$features[,"monthssince2019"] <= new_end_date &
        alldata$data_matrix$features[,"monthssince2019"] >= new_start_date
      val_data = alldata$validation_matrix$features[,"monthssince2019"] <= new_end_date &
        alldata$validation_matrix$features[,"monthssince2019"] >= new_start_date
      cat(paste(sum(sel_data > 0),"pixels for training amounts"))
      train_amounts =  list(features = alldata$data_matrix$features[sel_data,], label = alldata$data_matrix$label[sel_data])
      validation_amounts =  list(features = alldata$validation_matrix$features[val_data,], label = alldata$validation_matrix$label[val_data])

      model_amounts = ff_train(train_amounts,validation_amounts,eta = 0.2,gamma = 0.2,
                           min_child_weight = 3,max_depth = 6,nrounds = 500,
                           subsample = 0.3,verbose = T,
                           modelfilename = file.path("D:/ff-dev/predictionsZillah/models/",group,paste0(group,"_",exp_name,".model")),
                           features = alldata$features,eval_metric = "rmse", objective = "reg:squarederror")

      for (country in countriessel) {
        cat("starting country ",country)
        tiles = gfw_tiles[countries[countries$iso3 == country],]$tile_id
        for (tile in tiles) {
          cat(" starting tile ",tile,"\n")
          predset = ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles = tile,
                            start = dr2,verbose = F,fltr_features = c("groundtruth6m","initialforestcover"),
                            fltr_condition = c(">0",">0"),addxy = F,label_threshold = NA)
          # Predict in deforestation will happen
          #prediction_bin_test = ff_predict(model_bin,test_matrix = predset$data_matrix,indices = predset$testindices,threshold = 0.5,
          #                         templateraster = predset$groundtruthraster,groundtruth = as.numeric(predset$groundtruth > 0),verbose = F, certainty = T)
          # select data where deforestation is predicted

          # Predict the amount of deforestation
          prediction_amounts_test = ff_predict(model_amounts,test_matrix = predset$data_matrix,indices = predset$testindices,threshold = NA,
                                           templateraster = predset$groundtruthraster,groundtruth = predset$groundtruth,verbose = F, certainty = T)
          predicted_raster <- prediction_amounts_test$predicted_raster
          # Set negative values to 0

          predicted_raster[predicted_raster < 0] <- 1


          ## ADJUST USING REMAINING FOREST ! ##
          max_def = 1600 - predset$data_matrix$features[,"totallossalerts"]
          templateraster = predset$groundtruthraster
          templateraster[] = 0
          templateraster[predset$testindices] = max_def
          predicted_raster = min(predicted_raster, templateraster)
          print(paste("Correlation :", cor(predicted_raster[],predset$groundtruthraster[])))

          forestras=get_raster(tile = tile,date = dr2,datafolder = "D:/ff-dev/results/preprocessed/input/",feature="initialforestcover")
          ff_analyze_amounts(predicted_raster,groundtruth = predset$groundtruthraster,
                     csvfile = paste0("D:/ff-dev/predictionsZillah/accuracy_analysis/predictAmounts/", exp_name,".csv")
                     ,tile = tile,date = dr2,return_polygons = F,append = T,country = country,verbose = T,
                     method = exp_name,forestmask = forestras)

          }
      }

    }}, error = function(e) {
      cat("Error occurred:", conditionMessage(e), "\n")})}
