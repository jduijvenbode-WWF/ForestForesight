
## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")

## choice country ##
abr = "LAO"


# Jan 2021 July 2022 (weighted, of aantal samples per maand) -- voorspel Jan 2023, vanaf daar iteratief tot JUN 2023. Opslaan csv (ff_analyse). Doortrainen Laatste moment JUN 2023.
# Maak script voor LAOS mogelijk voor andere landen. Met forest mask aan.

## Train an initial model on Jan 2021 July 2022

train_data = ff_prep(country = abr, start="2021-01-01", end ="2022-07-01",
                     sample_size = 0.4, shrink="extract",
                     fltr_features = "initialforestcover", fltr_condition = ">0")

iterative_model = ff_train(train_matrix = laos_train$data_matrix,verbose=F,
                      nrounds = 150,eta = 0.4,
                      max_depth = 6,min_child_weight = 5,
                      subsample = 0.6,gamma = 0.05)

## Analyse vanaf January 2023
start=T
ffdates <- daterange("2023-01-01","2023-05-01")
for(date in ffdates){
  new_date= format(as.Date(date)-months(7), "%Y-%m-%d")
  new_train = ff_prep(country = abr, start= new_date,
                      sample_size = 0.6, shrink="extract")
  iterative_model = ff_train(train_matrix = new_train$data_matrix,verbose=F,
                        nrounds = 150,eta = 0.4,
                        max_depth = 6,min_child_weight = 5,
                        subsample = 0.6,gamma = 0.05, xgb_model= iterative_model)
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
