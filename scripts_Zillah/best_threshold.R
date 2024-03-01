## Find best treshold over time ##
F05 <- function(gt, pred) {
  # Calculate true positives, false positives, and false negatives
  tp <- global(gt == 1 & pred == 1,"sum")
  fp <- global(gt == 0 & pred == 1,"sum")
  fn <- global(gt == 1 & pred == 0,"sum")

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
data("countries")
countries=vect(countries)
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
tids=gfw_tiles[countries[which(countries$iso3 %in% c("LAO")),],]$tile_id
setwd("D:/ff-dev/results/predictions/")
files=list.files(recursive=T,pattern="predictions.tif",full.names = T)
tilinds=unique(as.numeric(unlist(sapply(tids,function(x) grep(x,files)))))
#files=c(files[tilinds],files[-tilinds])
files=c(files[tilinds])
gtfiles=file.path("D:/ff-dev/results/preprocessed/groundtruth/",gsub("predictions","groundtruth6mbin", files))
alldat=data.frame()
for(i in seq(length(files))[12:length(files)]){
  tryCatch({
    if(all(file.exists(c(files[i],gtfiles[i])))){
      pred=rast(files[i])
      gt=rast(gtfiles[i],win=ext(pred))
      thresholds= seq(0.1,0.7,0.02)
      F05values= F05(gt,as.numeric(pred>thresholds*100))
      bestf05= max(F05values)
      increasef05= bestf05-F05values$sum[21]
      threshold <- thresholds[which.max(F05values$sum)]
      print(paste(basename(dirname(files[i])),substr(basename(files[i]),10,19), "Best treshold:", threshold, ", F0.5 score: ", round(bestf05,2),
            " F05 increase:", round(increasef05,2)))
      # ff_analyze(predictions = pred>threshold,groundtruth = gt,return_polygons = F,tile=basename(dirname(files[i])),date=substr(basename(files[i]),10,19),csvfile = "D:/ff-dev/newres6.csv",append=T)
      alldat=rbind(alldat,c(basename(dirname(files[i])),substr(basename(files[i]),10,19),threshold,round(bestf05,4),round(increasef05,4)))
    }}, error = function(e) {
      # Print the error message
      cat("Error occurred for iteration", i, ": ", conditionMessage(e), "\n")
      # Continue to the next iteration
    })

}
