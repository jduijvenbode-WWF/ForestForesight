

## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")
#tiles=c("10N_080W","10S_070W","00N_010E","10N_110E")
abr ="LAO"
params=read.csv("D:/ff-dev/Taguchi/TaguchiLaosFinal.csv")
start=T
#for(tile in tiles){
for(i in 1:nrow(params)){
  if (params$forest_mask[i]==1)
  {train_data = ff_prep(country=abr,start="2022-01-01", end = "2022-06-01",
                        sample_size = params$datasample[i],
                        fltr_features = "initialforestcover", fltr_condition = ">0", shrink="extract")
  }else{train_data = ff_prep(country=abr,start="2022-01-01", end = "2022-06-01",
                             sample_size = params$datasample[i], shrink="extract")
  }
  print(paste("Training data is loaded for expiriment", i))

  ## train model ##
  model_data = ff_train(train_matrix = train_data$data_matrix,validation_matrix=train_data$validation_matrix,
                        verbose=F,
                        nrounds = params$nrounds[i],eta = params$eta[i],
                        max_depth = params$tree_depth[i],min_child_weight = params$min_child_weight[i],
                        subsample = params$subsample[i],gamma = params$gamma[i])
  print("Training finished")

  ## 12 months Itteration  ##
  ffdates <- daterange("2023-01-01","2023-05-01")
  for(date in ffdates){
    start_time = Sys.time()
    if (params$forest_mask[i]==1){test_data =  ff_prep(country = abr, start = date,fltr_features = "initialforestcover", fltr_condition = ">0", shrink="extract")
    } else{test_data =  ff_prep(country = abr, start = date, shrink="extract")}

    predres=ff_predict(model_data,test_data$data_matrix,
                       groundtruth=test_data$groundtruth)
    iteration=c(date= date,params[i,],F05= predres$F0.5,precision=predres$precision,recall=predres$recall)
    print(iteration)
    if(start){start=F
    results=iteration
    }else{results=rbind(results,iteration)}

    print("writing results to csv")
    write.csv(results,"D:/ff-dev/experiment_laos_param_final.csv")

  }

}
#  }

## analyse results ##

results2= read.csv("D:ff-dev/experiment_laos_param_final.csv")
df2=as.data.frame(results2)

#for (tile in tiles){
  result_list <- list()
#  df= df2[df2$X.1==tile,]
  for (i in colnames(df2)[4:11]) {
    result <- aggregate(df2$F05, by = list(df2[[i]]), FUN = mean)
    select= result$Group.1[which.max(result$x)]
    result_list[[i]] <- select
  }
  result_df <- do.call(rbind, result_list)
  print(result_df)
#}


