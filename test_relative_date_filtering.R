library(terra)
library(sf)
library(xgboost)
tiles=rev(c("10N_080W","10N_070W","20N_080W","00N_080W","00N_070W"))

if(Sys.info()[4]=="LAPTOP-DMVN4G1N"){
  months=3
  source("C:/data/xgboost_test/helpers/functions.R")
  inputdir="C:/data/colombia_tiles/input/"
  outputdir="C:/data/colombia_tiles/results20231128/"
} else if (Sys.info()[4]=="DESKTOP-3DNFBGC"){
  output_csv=file.path(outputdir,"results.csv")
  months=2
  source("C:/Users/admin/Documents/GitHub/ForestForesight/functions.R")
  inputdir="D:/ff-dev/results"
  outputdir="D:/ff-dev/predictions20231128"
} else{
  source("/Users/temp/Documents/GitHub/ForestForesight/functions.R")
  files=list.files("/Users/temp/Documents/FF/10N_080W", pattern ="tif",full.names = T)
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

ffdates=paste(sort(rep(c(2021,2022,2023),12)),seq(12),"01",sep="-")
ffdates=ffdates[1:29]
ffdates_backup=ffdates
datfram=data.frame()
for(tile in tiles){
  files=list.files(file.path(inputdir,tile), pattern ="tif",full.names = T)
  init_static_files= files[-grep("01\\.",files)]
  for(datenum in seq(7,length(ffdates_backup))){
    static_files=init_static_files
    ffdates=ffdates_backup[c(max(1,(datenum-(5+months))):(datenum-6),datenum)]
    print(max(ffdates))
    writedir=file.path(outputdir,tile)
    if(!dir.exists(writedir)){dir.create(writedir)}
    pred_raster=file.path(writedir,paste0("predictions_",max(ffdates),".tif"))
    if(!file.exists(pred_raster)){
      if(min(as.numeric(substr(ffdates,1,4)))<2021){static_files=static_files[-grep("loss2020",static_files)]}
      if(min(as.numeric(substr(ffdates,1,4)))<2022){static_files=static_files[-grep("loss2021",static_files)]}
      if(min(as.numeric(substr(ffdates,1,4)))<2023){static_files=static_files[-grep("loss2022",static_files)]}
      start=T
      for(i in ffdates){
        dynamic_files = files[grep(i,files)]
        rasstack = rast(c(dynamic_files, static_files),win=ext(rast(static_files[1])))
        dts=as.matrix(rasstack)
        coords=xyFromCell(rasstack,seq(ncol(rasstack)*nrow(rasstack)))
        filedate=substr(dynamic_files[1],tail(gregexpr("_",dynamic_files[1])[[1]],1)+1,nchar(dynamic_files[1])-4)
        dts=cbind(coords,dts)
        
        dts=cbind(dts,rep(abs(round(as.numeric(as.Date(i))%%365.25)-183),nrow(dts)))
        dts=cbind(dts,rep(as.numeric(as.Date(i)),nrow(dts)))
        colnames(dts)=c("x","y",gsub(".tif","",c(gsub(paste0("_",filedate),"",basename(dynamic_files)), basename(static_files))),"yearday_relative","date")
        if(i==ffdates[1]){
          fulldts=dts;start=F}else{fulldts=rbind(fulldts,dts)}
      }
      fulldts[is.na(fulldts)]=0
      fulldts=cbind(fulldts,fulldts[,"6months"]-fulldts[,"3months"])
      fulldts=cbind(fulldts,fulldts[,"pop2025"]-fulldts[,"pop2020"])
      fulldts=fulldts[,-which(colnames(fulldts) %in% c("6months","pop2025","pop2030"))]
      colnames(fulldts)=c(colnames(fulldts)[1:(ncol(fulldts)-2)],"3-6months","popdiff")
      dts=fulldts
      
      
      testsamples=which(dts[,"date"]==max(dts[,"date"]))
      trainsamples=which(dts[,"date"]!=max(dts[,"date"]))
      testdts=dts[testsamples,]
      dts=dts[trainsamples,]
      #next part doesn't work because it totally changes the balance between positives and negatives and gives very bad results in the end
      # filterindex=which(colnames(dts)=="groundtruth")
      # priority_index=which(colnames(dts)=="smtotaldeforestation")
      # deforestation_count=sum(dts[,filterindex]>0)
      # deforestedindices=which(dts[,filterindex]>0)
      # forestindices=which((dts[,filterindex]==0)&dts[,priority_index]>0)
      # nonforestindices=which((dts[,filterindex]==0)&dts[,priority_index]==0)
      # 
      # keep_indices=c(deforestedindices,sample(forestindices,max(length(forestindices),deforestation_count)))
      # if(length(forestindices)<deforestation_count){keep_indices=c(keep_indices,sample(nonforestindices,deforestation_count-length(nonforestindices)))}
      # #dts=dts[keep_indices,]
      
      groundtruth_index=which(colnames(dts)=="groundtruth")
      label=dts[,"groundtruth"]
      dts=dts[,-groundtruth_index]
      test_label=testdts[,"groundtruth"]
      testdts=testdts[,-groundtruth_index]
      
      #sample test data and exclude test data from training data
      #filter too many true negatives
      label[label>1]=1
      dts=dts[,-which(colnames(dts)=="date")]
      testdts=testdts[,-which(colnames(testdts)=="date")]
      dts=dts[,-which(colnames(dts)=="yearday_relative")]
      testdts=testdts[,-which(colnames(testdts)=="yearday_relative")]
      #boost and predict
      #dts_matrix= xgb.DMatrix(dts[,-which(colnames(dts)=="latestdeforestation")], label=label)
      #test_matrix= xgb.DMatrix(testdts[,-which(colnames(testdts)=="latestdeforestation")], label=test_label)
      dts_matrix= xgb.DMatrix(dts, label=label)
      test_matrix= xgb.DMatrix(testdts, label=test_label)
      watchlist = list(train = dts_matrix, eval = test_matrix)
      eta=0.1
      depth=5
      subsample=0.9
      nrounds=200
      bst <- xgb.train(data = dts_matrix,
                       max_depth = depth, eta = eta, subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 10,
                       objective = "binary:logistic", feval= evalerrorF05 , maximize= TRUE, verbose = 1, watchlist= watchlist)
      
      pred <- predict(bst, testdts)
      
      predictions=rast(t(matrix(pred>0.5,nrow=ncol(rasstack))),crs=crs(rasstack))
      print("predictions transformed")
      ext(predictions)=ext(rasstack)
      print("extent transferred")
      writeRaster(predictions,pred_raster,overwrite=T)
      writeRaster(rast(t(matrix(pred,nrow=ncol(rasstack))),crs=crs(rasstack)),gsub("predictions_","predictions_unclassified",pred_raster),overwrite=T)
      saveRDS(object = bst,file.path(writedir,paste0("predictor_",max(ffdates),".rds")))
      print("model saved")
      groundtruth=rast(file.path(inputdir,tile,paste0("groundtruth_",max(ffdates),".tif")))
      print("groundtruth created")
      groundtruth[is.na(groundtruth)]=0
      eval=predictions*2+groundtruth
      FP=extract(eval==1,pols2,fun="sum",na.rm=T,touches=F)[,2]
      FN=extract(eval==2,pols2,fun="sum",na.rm=T,touches=F)[,2]
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



