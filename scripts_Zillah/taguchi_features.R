

## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")
abr ="LAO"
feature_exp=(read.csv("D:/ff-dev/taguchi/Taguchi31f2l.csv")==1)
start=T
laos_train = ff_prep(country = abr, start="2022-01-01", end = "2022-06-01",sample_size=0.3, shrink="extract")
colnames(feature_exp)=laos_train$features

for(i in 1:nrow(feature_exp)){
  print(paste("Start experiment", i))
  features_sel= feature_exp[i,]
  train_matrix=list(features= laos_train$data_matrix$features[,features_sel],
                    label=laos_train$data_matrix$label)
  ## train model ##
  model_laos_taguchi = ff_train(train_matrix = train_matrix,
                        verbose=F,
                        nrounds = 100,eta = 0.2,
                        max_depth = 6,min_child_weight = 3,
                        subsample = 0.3,gamma = 0.2)
  print("Training finished")

  ## 5 months Itteration  ##
  ffdates <- daterange("2023-01-01","2023-05-01")
  for(date in ffdates){
    test_data =  ff_prep(country = abr, start = date, sample_size=0.3, shrink="extract", fltr_features = "initialforestcover", fltr_condition = ">0")
    test_matrix=list(features= test_data$data_matrix$features[,features_sel],
                      label=test_data$data_matrix$label)
    predres=ff_predict(model_laos_taguchi,test_matrix,
                       groundtruth=test_data$groundtruth)
    iteration=c(date=date,features_sel,F05=predres$F0.5,precision=predres$precision,recall=predres$recall)
    print(iteration)
    if(start){start=F
    results=iteration
    }else{results=rbind(results,iteration)}
    print("writing results to csv")
    write.csv(results,"D:/ff-dev/experiment_features_laos.csv")
  }

}

## analyse results ##

results2= read.csv("D:ff-dev/experiment_features_laos.csv")
df=as.data.frame(results2)
result_list <- list()

for (i in colnames(df)[2:30]) {
  result <- aggregate(df$X.3, by = list(df[[i]]), FUN = mean)
  result_list[[i]] <- result
}
result_df <- do.call(rbind, result_list)
print(result_df)



