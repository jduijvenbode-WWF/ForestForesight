library(ForestForesight)
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
data("countries")
countries=vect(countries)
groups=unique(gfw_tiles[countries[countries$iso3=="COL",]]$group)
othergroups=unique(gfw_tiles[countries[countries$iso3=="COL",]]$group)
groups=c(groups,unique(gfw_tiles$group)[-which(unique(gfw_tiles$group) %in% groups)])
for(group in groups[16:length(groups)]){
  tiles=gfw_tiles[which(gfw_tiles$group==group),]$tile_id
  traindata=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles=rev(tiles),start = "2022-01-01",end="2023-01-01",sample_size = 0.3,verbose=T)
  model=ff_train(traindata$data_matrix,eta = 0.2,gamma = 0.2,min_child_weight = 3,max_depth = 6,nrounds = 100,subsample = 0.3,verbose=T)
  if(!dir.exists(file.path("D:/ff-dev/results/models/",group))){dir.create(file.path("D:/ff-dev/results/models/",group))}
  saveRDS(model,file.path("D:/ff-dev/results/models/",group,paste0(group,"_model.rds")))
  dater=daterange("2024-01-01","2024-01-01")
  for(tile in tiles){
    for(dr in dater){
      predset=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles=tile,start = dr,verbose=T)
      prediction=ff_predict(model,test_matrix = predset$data_matrix,indices = predset$testindices,templateraster = predset$groundtruthraster,groundtruth = predset$groundtruth,threshold = 0.8,verbose = T)
      if(!dir.exists(file.path("D:/ff-dev/results/predictions/",tile))){dir.create(file.path("D:/ff-dev/results/predictions/",tile))}
      res=ff_visualize(prediction$predicted_raster,return_polygons = T,outputfile=(file.path("D:/ff-dev/results/predictions/",tile,paste0(tile,"_",dr,"_predictions.json"))),t_cutoff = 0.6)
    }

  }

}

