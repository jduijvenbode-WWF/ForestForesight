library(ForestForesight)
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
data("countries")
countries=vect(countries)
groups=unique(countries$group)
#groups=c(groups[c(41,42,43,45,25,6,10)],groups[-c(41,42,43,45,25,6,10)])
groups=groups[c(41,42,43,45,25,6,10)]
for(group in groups[5]){
  cat("starting group ",group)
  countriessel=countries$iso3[which(countries$group==group)]
  traindata=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",country=countriessel,start = "2021-01-01",end="2022-01-01",fltr_features = "landpercentage",fltr_condition = ">0",sample_size = 0.5,verbose=T,shrink="extract")
  if(!dir.exists(file.path("D:/ff-dev/results/models/",group))){dir.create(file.path("D:/ff-dev/results/models/",group))}
  model=ff_train(traindata$data_matrix,eta = 0.2,gamma = 0.2,min_child_weight = 3,max_depth = 6,nrounds = 120,subsample = 0.3,verbose=T,modelfilename = file.path("D:/ff-dev/results/models/",group,paste0(group,"_model.model")),features=traindata$features)


  dater=c(daterange("2022-06-01","2023-06-01"),"2024-01-01")
  for(country in countriessel){
    if(country!="URY"){
      cat("starting country ",country)
      for(dr in dater){
        cat("starting date ",dr)
        rm(gfw_tiles)
        data("gfw_tiles")
        gfw_tiles=vect(gfw_tiles)
        rm(countries)
        data("countries")
        countries=vect(countries)
        tiles=gfw_tiles[countries[countries$iso3==country],]$tile_id
        for(tile in tiles){
          tryCatch({
          cat("starting tile ",tile)
          if(!tile=="30S_060W"){
            predset=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles=tile,start = dr,verbose=T,,fltr_features = "landpercentage",fltr_condition = ">0")
            if(dr=="2024-01-01"){
              cat(paste("predict\n"))
              #res=ff_predict(model = model,test_matrix = predset$data_matrix,templateraster = predset$groundtruthraster,certainty=T)
              cat(paste("visualize\n"))
              #if(!dir.exists(file.path("D:/ff-dev/results/hotzones",country))){dir.create(file.path("D:/ff-dev/results/hotzones",country))}
              #if(global(res$predicted_raster>50,"sum")>0){
              #vis=ff_visualize(res$predicted_raster,return_polygons = T,t_cutoff = 50,outputfile_pols = file.path("D:/ff-dev/results/hotzones",country,paste0(country,"_hotzones.json")),outputfile_centers = file.path("D:/ff-dev/results/hotzones",country,paste0(country,"_hotzones_centers.json")))
              #}
              cat(paste("contextualize\n"))
              #con_vis=ff_contextualize(contextfolder = "D:/ff-dev/results/contextualization/",hotzones=list(vis),return_vector = T,outputfile = file.path("D:/ff-dev/results/hotzones",tile,paste0(tile,"_hotzones.json")))
            }else{
              print("predict")
              prediction=ff_predict(model,test_matrix = predset$data_matrix,indices = predset$testindices,
                                    templateraster = predset$groundtruthraster,groundtruth = predset$groundtruth,verbose = T)
              #if(!dir.exists(file.path("D:/ff-dev/results/predictions/",country))){dir.create(file.path("D:/ff-dev/results/predictions/",country))}
              print("analyze")
              ff_analyze(prediction$predicted_raster,groundtruth = predset$groundtruthraster,csvfile = "D:/ff-dev/results/accuracy_analysis/2024-03-05_current_accuracy.csv",tile = tile,date = dr,return_polygons = F,append = T,country=country,verbose=T)
            }
          }
          }, error = function(e) {
            cat("Error occurred:", conditionMessage(e), "\n")
          })
        }
      }
    }
  }
}

