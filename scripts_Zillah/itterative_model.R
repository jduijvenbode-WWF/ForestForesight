
## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")
input_dir = "D:/ff-dev/results/preprocessed"
save_dir= "D:/ff-dev/predictionsZillah/Countries"

data(gfw_tiles)
tilesvect=vect(gfw_tiles)
data(countries)
borders=vect(countries)

## choice country ##
abr_countries= c("COL","PER","BOL","LAO")


for (abr in abr_countries){
  ## Get tiles ## (Needed for ff_analysis)
  selected_country=borders[which(borders$iso3==abr)]
  tiles= tilesvect[selected_country]$tile_id

  # Jan 2021 July 2022 (weighted, of aantal samples per maand) -- voorspel Jan 2023, vanaf daar iteratief tot JUN 2023. Opslaan csv (ff_analyse). Doortrainen Laatste moment JUN 2023.
  # Maak script voor LAOS mogelijk voor andere landen. Met forest mask aan.

  ## Train an initial model on Jan 2021 July 2022 ##
  # Using the forest mask
  # With higher weights on the more recent months
  ffdates_train= daterange("2021-01-01","2022-06-01")
  weight_values= seq(0,1, length.out=length(ffdates_train)+1)[-1] # might be an idea to divide these values in order to get a smaller dataset
  i=1
  for (date in ffdates_train){
    train_data = ff_prep(country = abr, start=date,
                         sample_size = weight_values[i], shrink="extract",
                         fltr_features = "initialforestcover", fltr_condition = ">0")
    if (i==1){
      train_matrix = train_data$data_matrix
    }else{
      features= rbind(train_matrix$features, train_data$data_matrix$features)
      labels = c(train_matrix$label, train_data$data_matrix$label)
      train_matrix = list(features= features, label=labels)
    }
    i=i+1
  }

  # part_size = as.integer(length(train_data$data_matrix$features)/ 19)
  # weights = rep(weight_values, each = part_size)

  iterative_model = ff_train(train_matrix = train_matrix,verbose=F,
                             nrounds = 150,eta = 0.4,
                             max_depth = 6,min_child_weight = 5,
                             subsample = 0.6,gamma = 0.05)

  ## Analyse from January 2023 till May 2023

  ffdates_analyse <- daterange("2023-01-01","2023-05-01")
  for(date in ffdates_analyse){
    new_date= format(as.Date(date)-months(6), "%Y-%m-%d")
    new_train = ff_prep(country = abr, start= new_date,
                        sample_size = 0.6, shrink="extract",
                        fltr_features = "initialforestcover", fltr_condition = ">0")
    iterative_model = ff_train(train_matrix = new_train$data_matrix,verbose=F,
                               nrounds = 150,eta = 0.4,
                               max_depth = 6,min_child_weight = 5,
                               subsample = 0.6,gamma = 0.05, xgb_model= iterative_model)
    for(tile in tiles){
      test_data =  ff_prep(tiles = tile, start = date)
      results=ff_predict(iterative_model,test_data$data_matrix,
                         groundtruth=test_data$data_matrix$label,
                         indices= test_data$testindices,
                         templateraster = test_data$groundtruthraster)
      method =  "itterative_training"
      extent_elev = ext(rast(paste0(input_dir,"/", tile,"/", tile,"_2021-01-01_elevation.tif")))
      forest_mask = rast(paste0(input_dir,"/", tile,"/", tile,"_2021-01-01_initialforestcover.tif"))>0
      forest_mask= crop(forest_mask, extent_elev)

      ff_analyze(results$predicted_raster, test_data$groundtruthraster,forestmask=forest_mask,
                 csvfile= paste0(save_dir,"/",method,".csv"), tile=tile, method=method, date=date)
      print(paste0("analyzed: ",abr," tile : " , tile , ", on: ", date, ", F05:", round(results$F0.5,3),
                   ", precision: ", round(results$precision,3), ", recall: ", round(results$recall,3)))
    }

  }

}
