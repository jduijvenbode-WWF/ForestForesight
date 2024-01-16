
## tile10S070W train 12 predict 12 ##

## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")
tiles=c("10N_080W","10S_070W","00N_010E","10N_110E")
params=read.csv("D:/ff-dev/experiment_20240115.csv")
start=T
for(i in 1:nrow(params)){
  for(tile in tiles){

    train_data = ff_prep(tiles = tile, end = "2021-12-01", sample_size = params$datasample[i],relativedate = (params$relative_date[i]==1))


    ## train model ##
    model_data = ff_train(train_matrix = train_data$data_matrix,validation_matrix=train_data$validation_matrix,
                          verbose=T,
                          nrounds = params$nrounds[i],eta = params$eta[i],
                          max_depth = params$tree_depth[i],min_child_weight = params$min_child_weight[i],
                          subsample = params$subsample[i],gamma = params$gamma[i])

    ## 12 months Itteration  ##
    ffdates <- daterange("2022-06-01","2023-05-01")
    for(date in ffdates){
      start_time = Sys.time()
      test_data =  ff_prep(tiles = tile, start = date)
      predres=ff_predict(model_data,test_data$data_matrix,
                         groundtruth=test_data$groundtruth,
                         templateraster = test_data$groundtruthraster)
      iteration=c(tile,date,params[i,],predres$F0.5,predres$precision,predres$recall)
      print(iteration)
      if(start){start=F
      results=iteration
      }else{results=rbind(results,iteration)}
      write.csv(results,"D:/ff-dev/experiment_20240116_results.csv")
    }
  }
}
