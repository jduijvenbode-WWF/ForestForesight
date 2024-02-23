
evalerrorF05 <- function(preds, dts_matrix) {
  # Check for NAs in preds and labels
  if (any(is.na(preds)) || any(is.na(getinfo(dts_matrix, "label")))) {
    stop("NA values detected in preds or labels.")
  }
  i <- 0.5
  labels <- getinfo(dts_matrix, "label")
  #cat(max(preds))
  a <- table((preds > i) * 2 + (labels > 0))
  if (length(a)==4){
    UA<<-a[4]/(a[3]+a[4])
    PA<<-a[4]/(a[2]+a[4])
    F05<<- 1.25 * UA * PA / (0.25 * UA + PA)
  }else{
    F05 = 0
  }
  return(list(metric = "F05", value = as.numeric(F05)))
}


## Get training data ##
laos_train = ff_prep(country = abr, start="2022-01-01", end = "2022-06-01",sample_size=0.4, shrink="extract")
ffdates <- daterange("2023-01-01","2023-05-01")
start=T
for (evaluation in c(evalerrorF05,"aucpr")){
  maximize=NULL
  naam="aucpr"
  if (is.function(evaluation)){maximize=T
  naam="F05"}
  for(date in ffdates){
    test_data =  ff_prep(country = abr, start = date, validation_sample= 0.4, shrink="extract")
    laos_model = ff_train(train_matrix = laos_train$data_matrix,validation_matrix=test_data$validation_matrix, verbose=T,
                          nrounds = 150,eta = 0.4,
                          max_depth = 6,min_child_weight = 5,
                          subsample = 0.6,eval_metric = evaluation, maximize=maximize, gamma = 0.05)
    predres=ff_predict(laos_model, test_data$data_matrix,
                       groundtruth=test_data$data_matrix$label)
    iteration=c(date=date,eval=naam,F05=predres$F0.5,precision=predres$precision,recall=predres$recall)
    print(iteration)
    if(start){start=F
    results=iteration
    }else{results=rbind(results,iteration)}
    write.csv(results,"D:/ff-dev/experiment_evaluation.csv")
  }
}

## Train and Test dates expirement
# 1) train 2021-01-01 till 2021-12-01
laos_train_1 = ff_prep(country = abr, start="2021-01-01", end = "2021-12-01",sample_size=0.4, shrink="extract")
laos_model1 = ff_train(train_matrix = laos_train_1$data_matrix,
                       nrounds = 150,eta = 0.4,
                       max_depth = 6,min_child_weight = 5,
                       subsample = 0.6, gamma = 0.05)
# 2) Train 2022-01-01 till 2022-06-01
#laos_train !
laos_model2 = ff_train(train_matrix = laos_train$data_matrix,
                       nrounds = 150,eta = 0.4,
                       max_depth = 6,min_child_weight = 5,
                       subsample = 0.6, gamma = 0.05)
# 3) test on all subseq months
ffdates1 <- daterange("2022-01-01","2023-05-01")
ffdates2 <- daterange("2022-07-01","2023-05-01")
start=T
for(date in ffdates1){
  test_data =  ff_prep(country = abr, start = date, validation_sample= 0.4, shrink="extract")
  predres=ff_predict(laos_model1, test_data$data_matrix,
                     groundtruth=test_data$data_matrix$label)
  iteration=c(date=date,model="Train12",F05=predres$F0.5,precision=predres$precision,recall=predres$recall)
  print(iteration)
  if(start){start=F
  results=iteration
  }else{results=rbind(results,iteration)}
  write.csv(results,"D:/ff-dev/experiment_train12_vs_6.csv")
}
for(date in ffdates2){
  test_data =  ff_prep(country = abr, start = date, validation_sample= 0.4, shrink="extract")
  predres=ff_predict(laos_model2, test_data$data_matrix,
                     groundtruth=test_data$data_matrix$label)
  iteration=c(date=date,model="Train6",F05=predres$F0.5,precision=predres$precision,recall=predres$recall)
  print(iteration)
  if(start){start=F
  results=iteration
  }else{results=rbind(results,iteration)}
  write.csv(results,"D:/ff-dev/experiment_train12_vs_6.csv")
}

#test consistency
start=T
tiles = c("20N_100E", "30N_100E")
for(i in seq(1)){
  laos_train = ff_prep(tiles = tiles, start="2022-01-01", end = "2022-06-01",sample_size=0.4)
  laos_model = ff_train(train_matrix = laos_train$data_matrix,
                         nrounds = 150,eta = 0.4,
                         max_depth = 6,min_child_weight = 5,
                         subsample = 0.6, gamma = 0.05)
  ffdates <- daterange("2023-01-01","2023-05-01")
  for(date in ffdates){
    test_data =  ff_prep(country = abr, start = date, validation_sample= 0.4, shrink="extract")
    predres=ff_predict(laos_model, test_data$data_matrix,
                       groundtruth=test_data$data_matrix$label)
    iteration=c(date=date,model="tile",F05=predres$F0.5,precision=predres$precision,recall=predres$recall)
    print(iteration)
    if(start){start=F
    results=iteration
    }else{results=rbind(results,iteration)}
    write.csv(results,"D:/ff-dev/experiment_tilevscountry.csv")
  }
}

