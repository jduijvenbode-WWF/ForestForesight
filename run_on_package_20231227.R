library(ForestForesight)
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
plot(gfw_tiles)
poly=click(n=4)
t2=vect(poly[c(1,2,3,4,1),],type="polygons")
crs(t2)=crs(gfw_tiles)
tiles=gfw_tiles[t2]$tile_id
setwd("D:/ff-dev/predictions20231128/")
tiles=tiles[-which(tiles %in% list.dirs(full.names = F))]
for(tile in tiles[12:length(tiles)]){
  window=ff_dqc(file.path("D:/ff-dev/results/",tile))$minextent
  if(!dir.exists(tile)){dir.create(tile)}
  prepped_data=ForestForesight::ff_prep("D:/ff-dev/results/",tiles = tile,start=c(2021,1),end=c(2021,12),shrink = "extract",window = window)
  if(!(sum(prepped_data$data_matrix$label)==0)){
  model=ff_train(train_matrix = prepped_data$data_matrix)
  saveRDS(model,file.path(tile,"predictor.rds"))
  for(j in c(2022,2023)){
    for(i in seq(12)){
      print(tile);print(j);print(i)
      newfilename=file.path(tile,paste0("predictions_",as.character(ymd(paste(j,i,1,collapse=" "))),".tif"))
      if(!file.exists(newfilename)){
        if((j==2022&i>6)|(j==2023&i<6)){
          future=ForestForesight::ff_prep("D:/ff-dev/results/",tiles = tile,start=c(j,i),shrink = "none",window = window)
          results=ff_predict(model,future$data_matrix,groundtruth = future$groundtruth,templateraster = future$groundtruthraster,indices = future$testindices)

          writeRaster(results$predicted_raster,newfilename)
        }
      }
      }
    }
  }
}
ff_dqc(file.path("D:/ff-dev/results",tile))
setwd("D:/ff-dev/results")
edges=list.files(pattern="edge",recursive=T)
for(file in edges){file.rename(file,file.path("../edges",gsub("/","MU",file)))}
