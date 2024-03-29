library(ForestForesight)
data(gfw_tiles)
gfw_tiles=vect(gfw_tiles)
groups=unique(gfw_tiles$group)
for(group in groups){
  tiles=gfw_tiles$tile_id[which(gfw_tiles$group==group)]
  model=readRDS(file.path("D:/ff-dev/results/models/",group,paste0(group,"_model.rds")))
  data_prep=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles=tiles,start = "2021-06-01",end="2022-06-01")
  for(tile in tiles){
    cat(paste(tile,"\n"))
    datr=daterange("2022-12-01","2024-01-01")
    datr=datr[c(seq(7),length(datr))]
    for(dr in datr){
      cat(paste(dr,"\n"))
      cat(paste("load data\n"))
      dat=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles=tile,start = dr)
      if(dr=="2024-01-01"){
        cat(paste("predict\n"))
      res=ff_predict(model = model,test_matrix = dat$data_matrix,templateraster = dat$groundtruthraster,certainty=T)
      cat(paste("visualize\n"))
      vis=ff_visualize(res$predicted_raster,return_polygons = T,t_cutoff = 50,outputfile_pols = file.path("D:/ff-dev/results/hotzones",tile,paste0(tile,"_hotzones.json")),outputfile_centers = outputfile_pols = file.path("D:/ff-dev/results/hotzones",tile,paste0(tile,"_hotzones_centers.json")))
      cat(paste("contextualize\n"))
      #con_vis=ff_contextualize(contextfolder = "D:/ff-dev/results/contextualization/",hotzones=list(vis),return_vector = T,outputfile = file.path("D:/ff-dev/results/hotzones",tile,paste0(tile,"_hotzones.json")),centers=T)
      }else{
        cat(paste("predict\n"))
        res=ff_predict(model = model,test_matrix = dat$data_matrix,templateraster = dat$groundtruthraster)
        cat(paste("analyze\n"))
        ana=ff_analyze(res$predicted_raster,groundtruth = dat$groundtruthraster,csvfile = "D:/ff-dev/results/accuracy_analysis/2024-02-06_accuracy.csv",tile = tile,date = dr,return_polygons = F,append = T)
      }
    }
  }
}

