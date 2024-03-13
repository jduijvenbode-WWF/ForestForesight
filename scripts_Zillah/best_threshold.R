## Find best threshold over time ##
F05 <- function(gt, pred) {
  # Calculate true positives, false positives, and false negatives
  tp <- global(gt == 1 & pred == 1,"sum", na.rm=T)
  fp <- global(gt == 0 & pred == 1,"sum", na.rm=T)
  fn <- global(gt == 1 & pred == 0,"sum", na.rm=T)

  # Calculate precision and recall
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)

  # Calculate F0.5 score (beta = 0.5 for higher weight on precision)
  beta <- 0.5
  f_score <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

  return(f_score)
}
## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")

library(ForestForesight)

data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
groups = c("Lao People's Democratic Republic","Middle Africa 1", "Colombia","Peru", "Bolivia")
setwd("D:/ff-dev/results/predictions/")
files_all=list.files(recursive=T,pattern="predictions.tif",full.names = T)
alldat=data.frame()
for (group in groups[1:2]){
  data("countries")
  group_geom = countries[countries$group==group,]$geometry
  plot(group_geom)
  countries=vect(countries)
  tids=gfw_tiles[countries[which(countries$group == group),],]$tile_id
  tilinds=unique(as.numeric(unlist(sapply(tids,function(x) grep(x,files_all)))))
  #files=c(files[tilinds],files[-tilinds])
  files=c(files_all[tilinds])
  gtfiles=file.path("D:/ff-dev/results/preprocessed/groundtruth/",gsub("predictions","groundtruth6mbin", files))
  #dates= daterange(substr(basename(files[1]),10,19),substr(basename(files[length(files)]),10,19))
  dates= daterange(substr(basename(files[1]),10,19),"2023-06-01")
  for(date in dates){
    id= unique(as.numeric(unlist(sapply(date,function(x) grep(x,files)))))
    pred_list <- lapply(id, function(i) {
      crop(rast(files[i]), vect(group_geom), mask = TRUE)
    })

    gt_list <- lapply(id, function(i) {
      crop(rast(gtfiles[i]), vect(group_geom), mask = TRUE)
    })

    # Merge the list of cropped rasters into a single raster
    pred <- do.call(merge, pred_list)
    gt <- do.call(merge, gt_list)
    plot(pred)
    thresholds= seq(0.1,0.8,0.02)
    pred[is.na(pred)]=0
    gt[is.na(gt)]=0
    F05values= F05(gt,as.numeric(pred>thresholds*100))
    bestf05= max(F05values)
    increasef05= bestf05-F05values$sum[21]
    threshold <- thresholds[which.max(F05values$sum)]
    print(paste(group, date, "Best treshold:", threshold, ", F0.5 score: ", round(bestf05,2),
                ", F05 increase:", round(increasef05,2)))
    # ff_analyze(predictions = pred>threshold,groundtruth = gt,return_polygons = F,tile=basename(dirname(files[i])),date=substr(basename(files[i]),10,19),csvfile = "D:/ff-dev/newres6.csv",append=T)
    alldat=rbind(alldat,c(group, date,threshold,round(bestf05,4),round(increasef05,4)))
  }

}
