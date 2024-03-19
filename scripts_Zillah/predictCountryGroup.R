## set environment ##

library(ForestForesight)
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
data("countries")
colombia = countries[countries$iso3=="COL",]$geometry
gabon = countries[countries$iso3=="GAB",]$geometry

group = "Middle Africa 1"
countriessel=countries$iso3[which(countries$group==group)]
traindata_gab=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",
                        country=countriessel,start ="2021-01-01",end="2021-12-01",
                        fltr_features = "landpercentage",fltr_condition = ">0",
                        sample_size = 0.1,verbose=T,shrink="extract",
                        label_threshold = 1,addxy=F,
                        groundtruth_pattern = "groundtruth6m")

model_MiddleAfrica1_noXY=ff_train(traindata$data_matrix,eta = 0.2,gamma = 0.2,min_child_weight = 3,
                        max_depth = 6,nrounds = 100,subsample = 0.3,verbose=F,
                        ,modelfilename = file.path("D:/ff-dev/predictionsZillah/models",group,paste0(group,"_12months.model"))
                        ,features=traindata$features)
countries= vect(countries)
tiles=gfw_tiles[countries[countries$iso3=="GAB"],]$tile_id

all_predictions <- list()
for(tile in tiles){
            cat(" starting tile ",tile)
            predset=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles=tile,start = "2023-08-01",
                            verbose=F,fltr_features = "landpercentage",fltr_condition = ">0",
                            addxy=F,label_threshold = 1 )
            prediction=ff_predict(model_MiddleAfrica1_noXY,test_matrix = predset$data_matrix,indices = predset$testindices,
                                  templateraster = predset$groundtruthraster,groundtruth = predset$groundtruth,verbose = T,
                                  certainty = T)
            all_predictions[[tile]] = prediction$predicted_raster
}

raster_list <- unname(all_predictions)
MiddleAfrica1_rast <- do.call(merge, raster_list)
gab_rast= crop(MiddleAfrica1_rast, vect(gabon), mask=T)
#col_rast= crop(merge(all_predictions$"00N_070W",all_predictions$"00N_080W",all_predictions$"10N_070W",all_predictions$"10N_080W",all_predictions$"20N_080W",all_predictions$"20N_090W"),vect(colombia), mask=T)
writeRaster(gab_rast, "D:/ff-dev/predictionsZillah/Gabon_noXY.tiff", overwrite=T)
plot(gab_rast)
plot(gabon, add=T)
