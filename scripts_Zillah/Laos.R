
###LAOS###

## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")

## set variables ##
files=c("D:/ff-dev/results/preprocessed/20N_100E","D:/ff-dev/results/preprocessed/30N_100E")
tiles = c("20N_100E", "30N_100E")
laos= countries[countries$iso3=="LAO",]$geometry
abr = "LAO"

## Get training data ##
laos_sample = ff_prep(country = abr, start="2022-01-01", end = "2022-12-01",sample_size=0.1, shrink="extract")
print(summary(laos_sample$data_matrix$features))

# plot forest cover and previous deforestation
forest_mask = crop(merge(rast("D:/ff-dev/results/preprocessed/input/20N_100E/20N_100E_2021-01-01_initialforestcover.tif"), rast("D:/ff-dev/results/preprocessed/input/30N_100E/30N_100E_2021-01-01_initialforestcover.tif")), vect(laos),mask=TRUE)
tot_def = crop(merge(rast("D:/ff-dev/results/preprocessed/input/20N_100E/20N_100E_2023-05-01_totallossalerts.tif"), rast("D:/ff-dev/results/preprocessed/input/30N_100E/30N_100E_2023-05-01_totallossalerts.tif")), vect(laos),mask=TRUE)
png("D:/ff-dev/figures/deforestationLaos.png", width = 700, height=700)

plot(forest_mask, col=brewer.pal(5,"Greens"), buffer=T, plg=list(cex=2), pax=list( cex.axis=2))
north(cex=2)
plot(laos, add=TRUE)
tot_def[tot_def <= 0] <- NA
plot(tot_def,col="red", add=TRUE, legend=FALSE)
dev.off()

## data quality check ##
quality_1 = ff_dqc(files[1])
print(quality_1$byfeature)
quality_2 = ff_dqc(files[2])
print(quality_2$byfeature)


### RQ1 ###
## correlation ##
library(corrplot)
laos_combined= cbind(laos_sample$data_matrix$label, laos_sample$data_matrix$features)
colnames(laos_combined)[1]="groundtruth"
M = cor(laos_combined, use= "everything")
testRes = cor.mtest(laos_combined, conf.level = 0.5)

png("D:/ff-dev/figures/corrplotLaos2022.png", width = 950, height=950)
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black',tl.cex=1.2,tl.col = "black", number.cex = 0.9, diag=FALSE)
dev.off()


## plot feature importance ##

# Importance matrix #
png("D:/ff-dev/figures/importance_matrixLaos.png", width = 800, height=800)
importance_matrix <- xgb.importance(model = laos_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix, cex=1.5, left_margin=16)
dev.off()

# SHAP #
library("SHAPforxgboost")
shap_sample= laos_train$data_matrix$features[sample(nrow(laos_train$data_matrix$features), 1000),]
shap_values = shap.values(xgb_model = laos_model, X_train = shap_sample)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score
# To prepare the long-format data:
shap_long <- shap.prep(xgb_model = laos_model, X_train = shap_sample)
# **SHAP summary plot**

png("D:/ff-dev/figures/shapplotLaos.png", width = 800, height=800)
shap.plot.summary(shap_long)+
  ggplot2::theme(text = element_text(size = 18), legend.text = element_text(size = 12))
dev.off()

## Predict ##
laos_test_1 = ff_prep(tiles = tiles[1], start = ffdates[[1]])
laos_test_2 = ff_prep(tiles = tiles[2], start = ffdates[[1]])

results_1=ff_predict(laos_model,laos_test_1$data_matrix,
                   groundtruth=laos_test_1$groundtruth,
                   indices= laos_test_1$testindices,
                   templateraster = laos_test_1$groundtruthraster)
results_2=ff_predict(laos_model,laos_test_2$data_matrix,
                     groundtruth=laos_test_2$groundtruth,
                     indices= laos_test_2$testindices,
                     templateraster = laos_test_2$groundtruthraster)

laos_pred = crop(merge(results_1$predicted_raster, results_2$predicted_raster), vect(laos),mask=TRUE)
laos_gt =crop(merge(laos_test_1$groundtruthraster, laos_test_2$groundtruthraster), vect(laos),mask=TRUE)

png("D:/ff-dev/figures/TRUEvsPRED202301.png", width = 800, height=800)
par(mfrow = c(1,2))
plot(laos_gt, main="True deforestation", legend=FALSE)
plot(laos_pred, main="Predicted deforestation", legend=FALSE)
dev.off()

# Difficult in Laos: change in deforestation patterns!!!
dates= daterange("2021-01-01","2023-05-01")

# Open each GeoTIFF for the specified dates
filenames<- lapply(dates, function(date) {
  raster_path <- paste0("D:/ff-dev/results/preprocessed/20N_100E/20N_100E_", date, "_groundtruth.tif")
})
filenames2<- lapply(dates, function(date) {
  raster_path <- paste0("D:/ff-dev/results/preprocessed/30N_100E/30N_100E_", date, "_groundtruth.tif")
})
# Combine the SpatRasters into a single SpatRaster with different layers
combined_raster <-crop(merge(brick(filenames), brick(filenames2)), vect(laos), mask=TRUE)
animate(combined_raster, pause=0.1, main=dates)


## Analyse ##
## 5 months Iteration  ##
ffdates <- list( "2023-01-01", "2023-02-01", "2023-03-01",
              "2023-04-01", "2023-05-01")


for(tile in tiles){
  for(date in ffdates){
    start_time = Sys.time()
    laos_test =  ff_prep(tiles = tile, start = date, fltr_features = "initialforestcover", fltr_condition = ">0")
    results=ff_predict(laos_model,laos_test$data_matrix,
                       groundtruth=laos_test$groundtruth,
                       indices= laos_test$testindices,
                       templateraster = laos_test$groundtruthraster)
    save_dir= "D:/ff-dev/predictionsZillah/Laos"
    method =  "train_6_forestmask_laosb"
    ff_analyse(results$predicted_raster, laos_test$groundtruthraster,
               csvfile= paste0(save_dir,"/",method,".csv"), tile=tile, method=method, date=date)
    print(paste0("analyzed: ", tile , " on: ", date))
    print(paste0("Duration: ", Sys.time()-start_time))
}
}


## RQ 2 :Areas with no previous deforestation ##
# Smoothed total (5km)? or totallossallerts?
tot_allert = crop(merge(rast("D:/ff-dev/results/preprocessed/20N_100E/20N_100E_2023-05-01_totallossalerts.tif"), rast("D:/ff-dev/results/preprocessed/30N_100E/30N_100E_2023-05-01_totallossalerts.tif")), vect(laos),mask=TRUE)

# check performance within and without mask
# --> use fltr_features on test
ffdates <- daterange("2023-01-01","2023-05-01")
start=T
laos_train = ff_prep(country = abr, start="2022-01-01", end = "2022-06-01", shrink="extract")
print(summary(laos_train$data_matrix$features))
laos_model = ff_train(train_matrix = laos_train$data_matrix,verbose=F,
                      nrounds = 100,eta = 0.2,
                      max_depth = 6,min_child_weight = 3,
                      subsample = 0.3,gamma = 0.2)
for (condition in c(">0","==0",">-1")){
  for(date in ffdates){
    test_data =  ff_prep(country = abr, start = date, shrink="extract", fltr_features = "totallossalerts", fltr_condition = condition)
    for(threshold in c(0.01, seq(0.05,0.95,0.05))){
    predres=ff_predict(laos_model, test_data$data_matrix,
                       groundtruth=test_data$groundtruth, threshold=threshold)
    #shap_sample= test_data$data_matrix$features[sample(nrow(test_data$data_matrix$features), 1000),]
    #shap_values = shap.values(xgb_model = laos_model, X_train = shap_sample)
    #shap <- shap_values$mean_shap_score[order(names(shap_values$mean_shap_score))]
    #iteration=c(date=date,prev_def=condition,F05=predres$F0.5,precision=predres$precision,
    #            recall=predres$recall,numb_predicted=sum(predres$predictions),
    #            pixels_tot=length(test_data$testindices), shap)
    iteration=c(date=date,threshold=threshold, prev_def=condition,F05=predres$F0.5,precision=predres$precision,
                recall=predres$recall,numb_predicted=sum(predres$predictions>threshold),
                pixels_tot=sum(predres$predictions>-1))

    print(iteration)
    if(start){start=F
    results=iteration
    }else{results=rbind(results,iteration)}
    write.csv(results,"D:/ff-dev/experiment_previousdeforestaion_laos_treshold.csv")
    }
  }
}

# list : date, mask , useraccuracy, recall, F05 , num_predictions, and shapvalues!

# If difference : test separate models fltrs_features on train data

## TEST add data to model
ffdates <- daterange("2023-01-01","2023-05-01")
start=T
for(date in ffdates){
  end_date= format(as.Date(date)-months(7), "%Y-%m-%d")
  laos_train = ff_prep(country = abr, start="2021-01-01", end = end_date,
                       sample_size = 0.15, shrink="extract")
  laos_model = ff_train(train_matrix = laos_train$data_matrix,verbose=F,
                        nrounds = 150,eta = 0.4,
                        max_depth = 6,min_child_weight = 5,
                        subsample = 0.6,gamma = 0.05)
  test_data =  ff_prep(country = abr, start = date, sample_size=0.4, shrink="extract")
  predres=ff_predict(laos_model, test_data$data_matrix,
                     groundtruth=test_data$data_matrix$label)
  iteration=c(date=date,F05=predres$F0.5,precision=predres$precision,
              recall=predres$recall, numb_predicted=sum(predres$predictions>0.5))
  print(iteration)
  if(start){start=F
  results=iteration
  }else{results=rbind(results,iteration)}
  write.csv(results,"D:/ff-dev/experiment_Train_new_model_per_month2.csv")
}


## UPDATE MODEL: only new month  ##
ffdates <- daterange("2023-01-01","2023-05-01")
laos_train = ff_prep(country = abr, start="2021-01-01", end ="2021-06-01",
                     sample_size = 0.4, shrink="extract")
laos_model = ff_train(train_matrix = laos_train$data_matrix,verbose=F,
                      nrounds = 150,eta = 0.4,
                      max_depth = 6,min_child_weight = 5,
                      subsample = 0.6,gamma = 0.05)
start=T
for(date in ffdates){
  new_date= format(as.Date(date)-months(7), "%Y-%m-%d")
  new_train = ff_prep(country = abr, start= new_date,
                       sample_size = 0.6, shrink="extract")
  laos_model = ff_train(train_matrix = new_train$data_matrix,verbose=F,
                        nrounds = 150,eta = 0.4,
                        max_depth = 6,min_child_weight = 5,
                        subsample = 0.6,gamma = 0.05, xgb_model= laos_model)
  test_data =  ff_prep(country = abr, start = date, sample_size=0.4, shrink="extract")
  predres=ff_predict(laos_model, test_data$data_matrix,
                     groundtruth=test_data$data_matrix$label)
  iteration=c(date=date,F05=predres$F0.5,precision=predres$precision,
              recall=predres$recall, numb_predicted=sum(predres$predictions>0.5))
  print(iteration)
  if(start){start=F
  results=iteration
  }else{results=rbind(results,iteration)}
  write.csv(results,"D:/ff-dev/experiment_one_month.csv")
}

## Update model : new and old data? moving window? 12 months + reuse model?
# More weight last month + same month last year? for rq 3: More weight areas without def?



# Jan 2021 July 2022 (weighted, of aantal samples per maand) -- voorspel Jan 2023, vanaf daar iteratief tot JUN 2023. Opslaan csv (ff_analyse). Doortrainen Laatste moment JUN 2023.
# Maak script voor LAOS mogelijk voor andere landen. Met forest mask aan.




