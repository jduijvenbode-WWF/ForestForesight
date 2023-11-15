
# open packages 
library(terra)
library(xgboost)

#source("/Users/temp/Documents/GitHub/ForestForesight/functions.R")
#files=list.files("/Users/temp/Documents/FF/10N_080W", pattern ="tif",full.names = T)
source("C:/data/xgboost_test/helpers/functions.R")
files=list.files("C:/Users/jonas/Downloads/10N_080W", pattern ="tif",full.names = T)
static_files= files[-grep("01.",files)]
ffdates=paste(sort(rep(c(2021,2022,2023),12)),seq(12),"01",sep="-")
ffdates=ffdates[1:29][c(15,25)]

start=T
for(j in seq(0,10,2.5)){
  for(i in ffdates){
    
    dynamic_files = files[grep(i,files)]
    rasstack = rast(c(dynamic_files, static_files))
    dts=as.matrix(rasstack)
    coords=xyFromCell(rasstack,seq(ncol(rasstack)*nrow(rasstack)))
    filedate=substr(dynamic_files[1],tail(gregexpr("_",dynamic_files[1])[[1]],1)+1,nchar(dynamic_files[1])-4)
    dts=cbind(coords,dts)
    
    dts=cbind(dts,rep(abs(round(as.numeric(as.Date(i))%%365.25)-183),nrow(dts)))
    dts=cbind(dts,rep(as.numeric(as.Date(i)),nrow(dts)))
    colnames(dts)=c("x","y",gsub(".tif","",c(gsub(paste0("_",filedate),"",basename(dynamic_files)), basename(static_files))),"yearday_relative","date")
    # dts=dts[-which(dts[,"smtotaldeforestation"]==0),]
    # dts=dts[sample(seq(nrow(dts)),round(nrow(dts)/3)),]
    dts=dts[which((dts[,'x']<j)&(dts[,'x']>(j-2.5))),]
    if(start){
      fulldts=dts;start=F}else{fulldts=rbind(fulldts,dts)}
  }
  unidates=sort(unique(dts[,ncol(dts)]))
  fulldts[is.na(fulldts)]=0
  dts=fulldts
  groundtruth_index=which(colnames(dts)=="groundtruth")
  label=dts[,"groundtruth"]
  dts=dts[,-groundtruth_index]
  dts_backup=dts
  label_backup=label
  #sample test data and exclude test data from training data
  testsamples=which(dts[,"date"]==max(dts[,"date"]))
  trainsamples=which(dts[,"date"]!=max(dts[,"date"]))
  testdts=dts[testsamples,]
  dts=dts[trainsamples,]
  test_label=label[testsamples]
  label=label[trainsamples]
  
  #filter too many true negatives
  label[label>1]=1
  dts=dts[,-which(colnames(dts)=="date")]
  testdts=testdts[,-which(colnames(testdts)=="date")]
  #boost and predict
  dts_matrix= xgb.DMatrix(dts, label=label)
  test_matrix= xgb.DMatrix(testdts, label=test_label)
  watchlist = list(train = dts_matrix, eval = test_matrix)
  eta=0.1
  depth=5
  subsample=0.9
  nrounds=200
  bst <- xgboost(data = dts_matrix,
                 max_depth = depth, eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
                 objective = "binary:logistic",feval=evalerrorF05,maximize= TRUE,verbose = T,watchlist=watchlist)
  
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
