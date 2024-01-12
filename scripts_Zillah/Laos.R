
###LAOS###

## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")

_## set variables ##
files=c("D:/ff-dev/results/preprocessed/20N_100E","D:/ff-dev/results/preprocessed/30N_100E")
tiles = c("20N_100E", "30N_100E")
laos= countries[countries$iso3=="LAO",]$geometry

## data quality check ##
quality_1 = ff_dqc(files[1])
print(quality_1$byfeature)
quality_2 = ff_dqc(files[2])
print(quality_2$byfeature)

## data preperation ##
laos_train = ff_prep(country = "LAO", start="2022-01-01", end = "2022-06-01",sample_size=0.2, validation_sample = 0.1, shrink="extract")

laos_test =  ff_prep(country = "LAO", start = "2023-01-01",end= "2023-05-01", sample_size=0.2, shrink="extract")

laos_test_1 = ff_prep(tiles = "20N_100E", start = c(2022,6))
laos_test_2 =  ff_prep(tiles = "20N_100E", start = c(2022,6),end= c(2023,5), sample_size=0.2)

## Run XGBoost ##
laos_model = ff_train(train_matrix = laos_train$data_matrix,validation_matrix=laos_train$validation_matrix, verbose=T)

importance_matrix <- xgb.importance(model = laos_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

## Predict ##
results=ff_predict(laos_model,laos_test_1$data_matrix,
                   groundtruth=laos_test_1$groundtruth,
                   indices= laos_test_1$testindices,
                   templateraster = laos_test_1$groundtruthraster)

## Analyse ##
forest_mask <- rast("D:/ff-dev/results/20N_100E/forestmask2019.tif")>500
save_dir= "D:/ff-dev/predictionsZillah/Laos"
method =  "test_train_12_forestmask_laos"
tile= "20N_100E"
ff_analyse(results$predicted_raster, laos_test_1$groundtruthraster,forestmask = forest_mask,
           csvfile= paste0(save_dir,"/",method,".csv"), tile=tile, method=method,append=F, date="2022-06-01")


## SHAP ##
library("SHAPforxgboost")
shap_sample= laos_train$data_matrix$features[sample(nrow(laos_train$data_matrix$features), 1000),]
shap_values = shap.values(xgb_model = laos_model, X_train = shap_sample)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score
# To prepare the long-format data:
shap_long <- shap.prep(xgb_model = laos_model, X_train = shap_sample)
# **SHAP summary plot**
shap.plot.summary(shap_long)



## 5 months Itteration  ##
ffdates <- list( "2023-01-01", "2023-02-01", "2023-03-01",
              "2023-04-01", "2023-05-01")

start_time = Sys.time()
for(tile in tiles){
  for(date in ffdates){
    laos_test =  ff_prep(tiles = tile, start = date, fltr_features = "initialforestcover", fltr_condition = ">500")
    results=ff_predict(laos_model,laos_test$data_matrix,
                       groundtruth=laos_test$groundtruth,
                       indices= laos_test$testindices,
                       templateraster = laos_test$groundtruthraster)
    save_dir= "D:/ff-dev/predictionsZillah/Laos"
    method =  "train_6_forestmask_laos"
    ff_analyse(results$predicted_raster, laos_test$groundtruthraster,
               csvfile= paste0(save_dir,"/",method,".csv"), tile=tile, method=method, date=date)
    print(paste0("analyzed: ", tile , " on: ", date))
}
}
print(paste0("Duration: ", Sys.time()-start_time))
