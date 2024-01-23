

## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")
tiles=c("10N_080W","10S_070W","00N_010E","10N_110E")
params=read.csv("D:/ff-dev/TaguchiLandscape.csv")
start=T
for(tile in tiles){
  for(i in 1:nrow(params)){
    if (params$forest_mask[i]==1)
    {train_data = ff_prep(tile=tile, end = "2021-12-01", sample_size = params$datasample[i],fltr_features = "initialforestcover", fltr_condition = ">0")
    }else{train_data = ff_prep(tile=tile, end = "2021-12-01", sample_size = params$datasample[i])
    }
    print(paste("Training data is loaded for expiriment", i, "and tile", tile))

    ## train model ##
    model_data = ff_train(train_matrix = train_data$data_matrix,validation_matrix=train_data$validation_matrix,
                          verbose=F,
                          nrounds = params$nrounds[i],eta = params$eta[i],
                          max_depth = params$tree_depth[i],min_child_weight = params$min_child_weight[i],
                          subsample = params$subsample[i],gamma = params$gamma[i])
    print("Training finished")

    ## 12 months Itteration  ##
    ffdates <- daterange("2022-06-01","2023-05-01")
    for(date in ffdates){
      start_time = Sys.time()
      if (params$forest_mask[i]==1){test_data =  ff_prep(tile=tile, start = date,fltr_features = "initialforestcover", fltr_condition = ">0")
      } else{test_data =  ff_prep(tile=tile, start = date)}

      predres=ff_predict(model_data,test_data$data_matrix,
                         groundtruth=test_data$groundtruth)
      iteration=c(tile,date,params[i,],predres$F0.5,predres$precision,predres$recall)
      print(iteration)
      if(start){start=F
      results=iteration
      }else{results=rbind(results,iteration)}
      print("writing results to csv")
      write.csv(results,"D:/ff-dev/experiment_landscape_newparameters.csv")
    }

  }
  }



## analyse results ##

results2= read.csv("D:ff-dev/experiment_20240116_results.csv")
df2=as.data.frame(results2)

tiles= c("00N_010E", "10N_080W","10N_110E", "10S_070W")

for (tile in tiles){
  result_list <- list()
  df= df2[df2$X.1==tile,]
  for (i in colnames(df)[3:12]) {
    result <- aggregate(df$X.3, by = list(df[[i]]), FUN = mean)
    result_list[[i]] <- result
  }
  result_df <- do.call(rbind, result_list)
  print(result_df)
}

# Combine the results into a single data frame


