library(terra)
library(xgboost)
source("C:/data/git/ForestForesight/functions.R")
#source("/Users/temp/Documents/GitHub/ForestForesight/functions.R")
#files=list.files("/Users/temp/Documents/FF/10N_080W", pattern ="tif",full.names = T)

files=list.files("C:/data/colombia_tiles/input/10N_080W", pattern ="tif",full.names = T)
static_files= files[-grep("01\\.",files)]
ffdates=paste(sort(rep(c(2021,2022,2023),12)),seq(12),"01",sep="-")
ffdates=ffdates[1:29]
ffdates_backup=ffdates
for(datenum in seq(12,29)){
  ffdates=ffdates_backup[c((datenum-6),datenum)]
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
  filterindex=which(colnames(dts)=="groundtruth")
  priority_index=which(colnames(dts)=="smtotaldeforestation")
  deforestation_count=sum(dts[,filterindex]>0)
  forestindices=which((dts[,filterindex]==0)&dts[,priority_index]>0)
  nonforestindices=which((dts[,filterindex]==0)&dts[,priority_index]==0)
  
  keep_indices=sample(forestindices,max(length(forestindices),deforestation_count))
  if(length(forestindices)<deforestation_count){keep_indices=c(keep_indices,sample(nonforestindices,deforestation_count-length(nonforestindices)))}
  #dts=dts[keep_indices,]

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
  startF05=0
  for(i in seq(0.45,0.55,0.01)){
    a=table((pred > i)*2+(test_label>0))
    UA=a[4]/(a[3]+a[4])
    PA=a[4]/(a[2]+a[4])
    F05=round(1.25*UA*PA/(0.25*UA+PA),2)
    if(!is.na(F05)){
      if(F05>startF05){
        threshold=i
        startF05=F05
        sUA=round(UA,2)
        sPA=round(PA,2)
      }
    }
  }
  cat(paste("date:",datenum,"threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*sUA,", PA:",100*sPA,"F05:",startF05,"\n"))
  cat(paste("date:",datenum,"threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*sUA,", PA:",100*sPA,"F05:",startF05,"\n"),file="C:/data/results.txt",append=T)
}

preds=pred
testset=testdts
testset=testset[which(preds>0.5),]
res=res(rasstack)[1]

