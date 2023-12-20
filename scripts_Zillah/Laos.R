
###LAOS###

## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results")

## set variables ##
files=c("D:/ff-dev/results/20N_100E","D:/ff-dev/results/30N_100E")

## data quality check ##
quality_1 = ff_dqc(files[1])
print(quality_1$byfeature)
quality_2 = ff_dqc(files[2])
print(quality_2$byfeature)

## data preperation ##
laos_train = ff_prep(country = "LAO", end = c(2021,12),sample_size=0.01)
laos_test =  ff_prep(country = "LAO", start = c(2022,6),sample_size=0.3, exc_features = c("loss2021"))

## Run XGBoost ##
laos_model = ff_train(train_matrix = laos_train$data_matrix, verbose=T)

## Predict ##
results=ff_predict(laos_model,laos_test$data_matrix,indices=laos_test$testindices,
                   templateraster =laos_test$groundtruthraster, groundtruth=laos_test$groundtruth)

## on 12 months ##
ffdates= list(c(2022,6),c(2022,7),c(2022,8),c(2022,9),c(2022,10),c(2022,11),c(2022,12),c(2023,1),c(2023,2),c(2023,3),c(2023,4),c(2023,5))

for(date in ffdates){
  laos_test =  ff_prep(country = "LAO", start = date, sample_size=0.3, exc_features = c("loss2021"))
  print(paste("prepared:",date ))
  results=ff_predict(laos_model,laos_test$data_matrix,indices=laos_test$testindices,
                     templateraster =laos_test$groundtruthraster, groundtruth=laos_test$groundtruth)
  print(paste("precision=", results$precision, "recall= ", results$recall,"F05=", results$F0.5 ))
  }

