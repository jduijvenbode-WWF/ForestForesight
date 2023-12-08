library(terra)
library(sf)
library(xgboost)

### CHANGE DATES AND TILES ON KING KONG!! ##
colombia=c("10N_080W","10N_070W","20N_080W","00N_080W","00N_070W")
laos= c("30N_100E","20N_100E")
tiles= laos

if(Sys.info()[4]=="LAPTOP-DMVN4G1N"){
  source("C:/data/xgboost_test/helpers/functions.R")
  inputdir="C:/data/colombia_tiles/input/"
  outputdir="C:/data/colombia_tiles/results20231128/"
} else if (Sys.info()[4]=="DESKTOP-3DNFBGC"){
  source("C:/Users/admin/Documents/GitHub/ForestForesight/functions.R")
  inputdir="D:/ff-dev/results"
  outputdir="D:/ff-dev/predictionsZillah"
} else{
  source("/Users/temp/Documents/GitHub/ForestForesight/functions.R")
  inputdir= "/Users/temp/Documents/FF/input"
  outputdir= "/Users/temp/Documents/FF/predictions"
}
if(!dir.exists(outputdir)){dir.create(outputdir)}


borders=st_read(file.path(inputdir,"borders.geojson"))
borders=vect(borders)
borders=borders[-which(is.na(borders$iso3))]
borders$status=NULL
borders$color_code=NULL
borders$iso_3166_1_alpha_2_codes=NULL
borders$geo_point_2d=NULL
borders$french_short=NULL
pols=as.polygons(rast(nrows=60, ncols=360, nlyrs=1, xmin=-180, xmax=180,ymin=-30, ymax=30,vals=rep(0,60*360)),dissolve=F,na.rm=F,values=F)
pols$coordname=paste0(round(crds(centroids(pols))[,1]-0.5),"_",round(crds(centroids(pols))[,2]-0.5))
pols2=intersect(pols,borders)

ffdates= paste(sort(rep(c(2021, 2022, 2023), each = 12)),sprintf("%02d", seq(12)),"01",sep = "-")
ffdates=ffdates[1:29]
ffdates_backup=ffdates
datfram=data.frame()
# Loop over each tile
for (tile in tiles) {
  output_csv=file.path(outputdir,tile,"results.csv")
  files <- list.files(file.path(inputdir, tile), pattern = "tif", full.names = TRUE)
  static_files = files[-grep("01\\.", files)]
  # Set the output directory path
  writedir = file.path(outputdir, tile)
  if (!dir.exists(writedir)) {
    dir.create(writedir)
  }
  # Exclude losses based on the minimum year in ffdates
  if (min(as.numeric(substr(ffdates, 1, 4))) < 2021) {
    static_files = static_files[-grep("loss2020", static_files)]
  }
  if (min(as.numeric(substr(ffdates, 1, 4))) < 2022) {
    static_files = static_files[-grep("loss2021", static_files)]
  }
  if (min(as.numeric(substr(ffdates, 1, 4))) < 2023) {
    static_files = static_files[-grep("loss2022", static_files)]
  }
  
  start = TRUE
  # Loop over each date in ffdates
  for (i in ffdates) {
    # Get dynamic_files for the current date
    dynamic_files = files[grep(i, files)]
    # Create a raster stack
    if(Sys.info()[4]=="Temps-MacBook-Pro.local"){
      rasstack = rast(c(dynamic_files, static_files), win = ext(rast(static_files[1])-4.8))
      } else {rasstack = rast(c(dynamic_files, static_files), win = ext(rast(static_files[1])))}
    # Extract data from the raster stack
    dts = as.matrix(rasstack)
    # Get coordinates from the raster stack
    coords = xyFromCell(rasstack, seq(ncol(rasstack) * nrow(rasstack)))
    # Extract file date from the first dynamic file
    filedate = substr(dynamic_files[1], tail(gregexpr("_", dynamic_files[1])[[1]], 1) + 1, nchar(dynamic_files[1]) - 4)
    # Combine coordinates and data
    dts = cbind(coords, dts)
    # Add columns for yearday_relative and date
    dts = cbind(dts, rep(abs(round(as.numeric(as.Date(i)) %% 365.25) - 183), nrow(dts)))
    dts = cbind(dts, rep(as.numeric(as.Date(i)), nrow(dts)))
    colnames(dts) = c("x", "y", gsub(".tif", "", c(gsub(paste0("_", filedate), "", basename(dynamic_files)), basename(static_files))),
                      "yearday_relative", "date")
    if (i == ffdates[1]) {
      fulldts = dts
      start = FALSE
    } else {
      fulldts = rbind(fulldts, dts)
    }
  }
  
  # Replace NA values with 0
  fulldts[is.na(fulldts)] = 0
  
  # Add additional columns to fulldts
  fulldts = cbind(fulldts, fulldts[, "6months"] - fulldts[, "3months"])
  fulldts = cbind(fulldts, fulldts[, "pop2025"] - fulldts[, "pop2020"])
  fulldts = fulldts[, -which(colnames(fulldts) %in% c("6months", "pop2025", "pop2030"))]
  
  # Rename columns
  colnames(fulldts) = c(colnames(fulldts)[1:(ncol(fulldts) - 2)], "3-6months", "popdiff")
  

  # Train and test only within forest 
  mask_forest = which(fulldts[,which(colnames(fulldts)=="forestmask2019")]>0)
  fulldts= fulldts[mask_forest,]

  #sample train data 
  trainsamples=which(fulldts[,which(colnames(fulldts)=="date")]==as.numeric(as.Date(ffdates[1:24])))# first 2 jaar
  dts=fulldts[trainsamples,]
  groundtruth_index=which(colnames(dts)=="groundtruth")
  label=dts[,"groundtruth"]
  dts=dts[,-groundtruth_index]
  label[label>1]=1
  dts=dts[,-which(colnames(dts)=="date")]
  dts=dts[,-which(colnames(dts)=="yearday_relative")]
  
  eta=0.1
  depth=5
  subsample=0.9
  nrounds=200
  bst <- xgboost(data = dts, label = label,
                 max_depth = depth, eta = eta, subsample=subsample,  nrounds = nrounds, early_stopping_rounds = 10,
                 objective = "binary:logistic",eval_metric="aucpr",verbosity = 1)
  saveRDS(object = bst,file.path(writedir,paste0("predictor.rds")))
  print("model saved")

  for (datenum in seq(25, length(ffdates_backup))) {
    print(ffdates[datenum])
    # Define the path for the predicted raster
    pred_raster = file.path(writedir, paste0("predictions_", ffdates[datenum], ".tif"))
    # Check if the predicted raster file doesn't exist
    if (!file.exists(pred_raster)){
      testsamples=which(fulldts[,which(colnames(fulldts)=="date")]==as.numeric(as.Date(ffdates[datenum])))
      testdts=fulldts[testsamples,][,-which(colnames(fulldts)=="date")]
      test_label=testdts[,"groundtruth"]
      testdts=testdts[,-groundtruth_index]
      testdts=testdts[,-which(colnames(testdts)=="yearday_relative")]
    
      pred <- predict(bst, testdts)
      
      tileF05 = getF05(pred, test_label)
      print(paste("F05:", tileF05 ))
      
      pred_ini = numeric(dim(rasstack)[1]*dim(rasstack)[2])
      pred_ini[mask_forest[mask_forest<(dim(rasstack)[1]*dim(rasstack)[2])]]=pred
      
      predictions=rast(t(matrix(pred_ini>0.5,nrow=ncol(rasstack))),crs=crs(rasstack))
      print("predictions transformed")
      ext(predictions)=ext(rasstack)
      print("extent transferred")
      writeRaster(predictions,pred_raster,overwrite=T)
      writeRaster(rast(t(matrix(pred_ini,nrow=ncol(rasstack))),crs=crs(rasstack)),gsub("predictions_","predictions_unclassified",pred_raster),overwrite=T)
      groundtruth=rast(file.path(inputdir,tile,paste0("groundtruth_",ffdates[datenum],".tif")))
      print("groundtruth created")
      groundtruth[is.na(groundtruth)]=0
      eval=predictions*2+groundtruth
      FN=extract(eval==1,pols2,fun="sum",na.rm=T,touches=F)[,2]
      FP=extract(eval==2,pols2,fun="sum",na.rm=T,touches=F)[,2]
      TP=extract(eval==3,pols2,fun="sum",na.rm=T,touches=F)[,2]
      TN=extract(eval==0,pols2,fun="sum",na.rm=T,touches=F)[,2]
      print("values extracted")
      precision=TP/(TP+FP)
      recall=TP/(TP+FN)
      accuracy=(TP+TN)/(TP+TN+FN+FP)
      F1score=(2*precision*recall)/(precision+recall)
      F05score=(1.25*precision*recall)/(0.25*precision+recall)
      print("metrics calculated")
      resdat=data.frame(coordname=pols2$coordname,TP=TP,FN=FN,FP=FP,TN=TN,precision=precision,recall=recall,accuracy=accuracy,F1score=F1score,F05score=F05score,date=as.Date(max(ffdates)),tile=tile,country=pols2$iso3)
      resdat=resdat[which(!is.nan(TN)),]
      datfram=rbind(datfram,resdat)
      write.csv(datfram,output_csv)
      print("data written")
    }
      
    }
  }



