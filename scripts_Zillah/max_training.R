## set environment ##

library(ForestForesight)
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
data("countries")
countries=vect(countries)
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")
# groups=unique(countries$group)
groups = c("Laos","Middle Africa 1", "Colombia","Peru", "Bolivia")
dates =daterange("2023-01-01","2023-05-01")
end_date= format(as.Date(dates[1])-months(7), "%Y-%m-%d")
exp_name= "max_training_fc"
for(group in groups){
  tryCatch({
    cat(" starting group ",group, "\n")
    countriessel=countries$iso3[which(countries$group==group)]
    traindata=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",
                      country=countriessel,start ="2021-01-01",end=end_date,
                      validation_sample= 0.4,
                      fltr_features = "initialforestcover",fltr_condition = ">0",
                      sample_size = 0.2,verbose=F,shrink="extract",
                      label_threshold = 1,addxy=F,
                      groundtruth_pattern = "groundtruth6m")
#    model= ff_train(traindata$data_matrix,eta = 0.2,gamma = 0.2,min_child_weight = 3,max_depth = 6,nrounds = 100,subsample = 0.3,verbose=T,
#                    modelfilename = file.path("D:/ff-dev/predictionsZillah/models/",group,paste0(group,"_",exp_name,".model")),
#                    features=traindata$features)
      for(dr2 in dates){
        new_month= format(as.Date(dr2)-months(6), "%Y-%m-%d")
#        same_month= format(as.Date(dr2)-months(12), "%Y-%m-%d")
        new_train = ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",
                            country=countriessel,start = new_month,
                            fltr_features = "initialforestcover",fltr_condition = ">0",
                            validation_sample= 0.4,
                            sample_size = 0.2,verbose=F,shrink="extract",
                            label_threshold = 1,addxy=F,
                            groundtruth_pattern = "groundtruth6m")
#        same_month_train = ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",
#                            country=countriessel,start = same_month,
#                            fltr_features = "landpercentage",fltr_condition = ">0",
#                            sample_size = 0.1,verbose=F,shrink="extract",
#                            label_threshold = 1,addxy=F,
#                            groundtruth_pattern = "groundtruth6m")
        cat("loaded data ", new_month)
        # combine data trainmatrix
        traindata$data_matrix$features=rbind(traindata$data_matrix$features, new_train$data_matrix$features)
        traindata$data_matrix$label=append(traindata$data_matrix$label, new_train$data_matrix$label)
        # combine data for validation matrix
        traindata$validation_matrix$features=rbind(traindata$validation_matrix$features, new_train$validation_matrix$features)
        traindata$validation_matrix$label=append(traindata$validation_matrix$label, new_train$validation_matrix$label)

        if(!dir.exists(file.path("D:/ff-dev/predictionsZillah/models/",group))){dir.create(file.path("D:/ff-dev/predictionsZillah/models/",group))}
        model=ff_train(traindata$data_matrix,eta = 0.2,gamma = 0.2,
                       min_child_weight = 3,max_depth = 6,nrounds = 100,
                       subsample = 0.3,verbose=T,
                       validation_matrix=traindata$validation_matrix,maximize=T,
                       modelfilename = file.path("D:/ff-dev/predictionsZillah/models/",group,paste0(group,"_",exp_name,".model")),
                       features=traindata$features)
        cat("new model is trained")
#        prediction_same_month=ff_predict(model,test_matrix = same_month_train$data_matrix,
#                                       groundtruth = same_month_train$groundtruth,verbose = T)

#        print("calculate best threshold")
#        th= bestThreshold(prediction_same_month$predictions, same_month_train$data_matrix$label)
#        print(paste("the best threshold for", same_month,"and", group, "is", round(th$bestValue,2), "with an F05 of", round(th$maxOutput,2)))
        for(country in countriessel){
          cat("starting country ",country)
        tiles=gfw_tiles[countries[countries$iso3==country],]$tile_id
        for(tile in tiles){
          cat(" starting tile ",tile,"\n")
          predset=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles=tile,
                          start = dr2,verbose=F,fltr_features = "initialforestcover",
                          fltr_condition = ">0",addxy=F,label_threshold = 1)
#          print("predict with best threshold")
          prediction=ff_predict(model,test_matrix = predset$data_matrix,indices = predset$testindices,threshold = 0.5,
                                templateraster = predset$groundtruthraster,groundtruth = predset$groundtruth,verbose = F)

          #if(!dir.exists(file.path("D:/ff-dev/results/predictions/",country))){dir.create(file.path("D:/ff-dev/results/predictions/",country))}
          print("analyze")
          forestcoverrast=get_raster(datafolder = "D:/ff-dev/results/preprocessed/input/",date=dr2,feature="initialforestcover",tile=tile)
          ff_analyze(prediction$predicted_raster,groundtruth = predset$groundtruthraster,
                     csvfile = paste0("D:/ff-dev/predictionsZillah/accuracy_analysis/", exp_name,".csv")
                     ,tile = tile,date = dr2,return_polygons = F,forestmask = forestcoverrast,append = T,country=country,verbose=T, method = exp_name)
        }
      }

      }}, error = function(e) {
        cat("Error occurred:", conditionMessage(e), "\n")})}
