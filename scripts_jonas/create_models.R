library(ForestForesight)
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
data("countries")
countries=vect(countries)
groups=unique(gfw_tiles[countries[countries$iso3=="COL",]]$group)
othergroups=unique(gfw_tiles[countries[countries$iso3=="COL",]]$group)
groups=c(groups,unique(gfw_tiles$group)[-which(unique(gfw_tiles$group) %in% groups)])
for(group in groups[26:length(groups)]){
  tiles=gfw_tiles[which(gfw_tiles$group==group),]$tile_id
  traindata=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles=tiles,start = "2021-01-01",end="2022-01-01",sample_size = 0.3,verbose=T)
  model=ff_train(traindata$data_matrix,eta = 0.2,gamma = 0.2,min_child_weight = 3,max_depth = 6,nrounds = 120,subsample = 0.3,verbose=T)
  if(!dir.exists(file.path("D:/ff-dev/results/models/",group))){dir.create(file.path("D:/ff-dev/results/models/",group))}
  saveRDS(model,file.path("D:/ff-dev/results/models/",group,paste0(group,"_model.rds")))
  dater=c(daterange("2022-06-01","2023-06-01"),"2024-01-01")
  for(tile in tiles){
    for(dr in dater){
      predset=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles=tile,start = dr,verbose=T)
      if(dr=="2024-01-01"){
      cat(paste("predict\n"))
      res=ff_predict(model = model,test_matrix = predset$data_matrix,templateraster = predset$groundtruthraster,certainty=T)
      cat(paste("visualize\n"))
      if(!dir.exists(file.path("D:/ff-dev/results/hotzones",tile))){dir.create(file.path("D:/ff-dev/results/hotzones",tile))}
      if(global(res$predicted_raster>50,"sum")>0){
      vis=ff_visualize(res$predicted_raster,return_polygons = T,t_cutoff = 50,outputfile_pols = file.path("D:/ff-dev/results/hotzones",tile,paste0(tile,"_hotzones.json")),outputfile_centers = file.path("D:/ff-dev/results/hotzones",tile,paste0(tile,"_hotzones_centers.json")))}
      cat(paste("contextualize\n"))
      #con_vis=ff_contextualize(contextfolder = "D:/ff-dev/results/contextualization/",hotzones=list(vis),return_vector = T,outputfile = file.path("D:/ff-dev/results/hotzones",tile,paste0(tile,"_hotzones.json")))
      }else{
      prediction=ff_predict(model,test_matrix = predset$data_matrix,indices = predset$testindices,templateraster = predset$groundtruthraster,groundtruth = predset$groundtruth,verbose = T)
      if(!dir.exists(file.path("D:/ff-dev/results/predictions/",tile))){dir.create(file.path("D:/ff-dev/results/predictions/",tile))}

      ff_analyze(prediction$predicted_raster,groundtruth = predset$groundtruthraster,csvfile = "D:/ff-dev/results/accuracy_analysis/2024-02-06_current_accuracy.csv",tile = tile,date = dr,return_polygons = F,append = T)
      }
    }

  }

}

