# F05 <- function(gt, pred) {
#   # Calculate true positives, false positives, and false negatives
#   tp <- global(gt == 1 & pred == 1,"sum")
#   fp <- global(gt == 0 & pred == 1,"sum")
#   fn <- global(gt == 1 & pred == 0,"sum")
# 
#   # Calculate precision and recall
#   precision <- tp / (tp + fp)
#   recall <- tp / (tp + fn)
# 
#   # Calculate F0.5 score (beta = 0.5 for higher weight on precision)
#   beta <- 0.5
#   f_score <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)
# 
#   return(f_score)
# }
library(ForestForesight)
data("countries")
countries=vect(countries)
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
tids=gfw_tiles[countries[which(countries$iso3 %in% c("IDN","GAB","BOL","PER","COL","LAO")),],]$tile_id
setwd("D:/ff-dev/results/predictions/")
files=list.files(recursive=T,pattern="predictions.tif",full.names = T)
tilinds=unique(as.numeric(unlist(sapply(tids,function(x) grep(x,files)))))
files=c(files[tilinds],files[-tilinds])
m6files=file.path("D:/ff-dev/results/preprocessed/input/",gsub("predictions","lastsixmonths", files))
gtfiles=file.path("D:/ff-dev/results/preprocessed/groundtruth/",gsub("predictions","groundtruth6mbin", files))
alldat=data.frame()
for(i in seq(length(files))[105:length(files)]){
  tryCatch({
  if(all(file.exists(c(files[i],m6files[i],gtfiles[i])))){
    pred=rast(files[i])
    #if(global(is.na(pred),"sum")>0){pred[is.na(pred[])]=0}
    
    gt=rast(gtfiles[i],win=ext(pred))
    #if(global(is.na(gt),"sum")>0){gt[is.na(gt[])]=0}
    gt=gt>0
    m6=rast(m6files[i],win=ext(pred))
    #if(global(is.na(m6),"sum")>0){m6[is.na(m6[])]=0}
    m6=m6>0
    # # Scenario 1: Classification using threshold 0.5
    # pred_thresholded <- ifel(pred > 0.5, 1, 0)
    # f05_scenario1 <- F05(gt, pred_thresholded)
    
    # Scenario 2: Classification to match the number of positive pixels in m6
    num_pos_pixels_m6 <- global(m6 > 0,"sum",na.rm=T)
    
    threshold <- quantile(as.matrix(pred), 1-num_pos_pixels_m6 / ncell(pred), names = FALSE)
    print(threshold)
    # pred_matched <- ifel(pred > threshold, 1, 0)
    # f05_scenario2 <- F05(gt, pred_matched)
    # 
    # # Output F0.5 scores for each scenario
    print(basename(files[i]))
    print(i)
    # print(paste("F0.5 score for Scenario 1:", f05_scenario1))
    # print(paste("F0.5 score for Scenario 2:", f05_scenario2))
    ff_analyze(predictions = pred>threshold,groundtruth = gt,return_polygons = F,tile=basename(dirname(files[i])),date=substr(basename(files[i]),10,19),csvfile = "D:/ff-dev/newres4.csv",append=T)
    #alldat=rbind(alldat,c(files[i],threshold,as.numeric(f05_scenario1),as.numeric(f05_scenario2)))
  }}, error = function(e) {
    # Print the error message
    cat("Error occurred for iteration", i, ": ", conditionMessage(e), "\n")
    # Continue to the next iteration
    next})
  
}
