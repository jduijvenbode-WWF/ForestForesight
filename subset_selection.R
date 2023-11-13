
# open packages 
library(terra)
library(xgboost)
if(Sys.info()[4]=="LAPTOP-DMVN4G1N"){
  source("C:/data/xgboost_test/helpers/functions.R")
  files=list.files("C:/Users/jonas/Downloads/10N_080W", pattern ="tif",full.names = T)
}else{
  source("/Users/temp/Documents/GitHub/ForestForesight/functions.R")
  files=list.files("/Users/temp/Documents/FF/10N_080W", pattern ="tif",full.names = T)
}


static_files= files[-grep("01.",files)]
data = c("2022-1-01","2022-4-01","2022-7-01","2022-10-01","2022-11-01","2023-1-01","2023-2-01","2023-4-01")

start=T
for(i in data){
  dynamic_files = files[grep(i,files)]
  rasstack = rast(c(dynamic_files, static_files),extent=ext(rast(dynamic_files[1]))-3) # win argument resulted in error 
  rasstack = crop(rasstack,ext(rast(rasstack))-3) # used crop instead 
  dts=as.matrix(rasstack)
  coords=xyFromCell(rasstack,seq(ncol(rasstack)*nrow(rasstack)))
  filedate=substr(dynamic_files[1],tail(gregexpr("_",dynamic_files[1])[[1]],1)+1,nchar(dynamic_files[1])-4)
  dts=cbind(coords,dts)
  dts=cbind(dts,rep(abs(round(as.numeric(as.Date(i))%%365.25)-183),nrow(dts)))
  dts=cbind(dts,rep(as.numeric(as.Date(i)),nrow(dts)))
  colnames(dts)=c("x","y",gsub(".tif","",c(gsub(paste0("_",filedate),"",basename(dynamic_files)), basename(static_files))),"yearday_relative","date")
  #dts=spatSample(rasstack,size=ncol(rasstack)*nrow(rasstack),xy=T,method="regular", values=FALSE)
  if(start){
    fulldts=dts;start=F}else{fulldts=rbind(fulldts,dts)}
}


######method 2: other date A: xgboost###########
for(datenum in seq(2)){
  dts=fulldts
  dts=dts[,-which(colnames(dts)=="yearday_relative")]
  dts[is.na(dts)]=0
  groundtruth_index=which(colnames(dts)=="groundtruth")
  label=dts[,groundtruth_index]
  dts=dts[,-groundtruth_index]
  dts_backup=dts
  label_backup=label
  #sample test data and exclude test data from training data
  testsamples=which(dts[,which(colnames(dts)=="date")]==as.numeric(as.Date(data[datenum])))
  testdts=dts[testsamples,]
  dts=dts[-testsamples,]
  test_label=label[testsamples]
  label=label[-testsamples]
  
  #filter too many true negatives
  filterindex=which(colnames(dts)=="smtotaldeforestation")
  deforestation_count=sum(dts[,filterindex]>0)
  nonforestindices=which(dts[,filterindex]==0)
  remove_indices=sample(nonforestindices,length(nonforestindices)-deforestation_count)
  dts=dts[-remove_indices,]
  label=label[-remove_indices]
  label[label>1]=1
  dts=dts[,-which(colnames(dts)=="date")]
  testdts=testdts[,-which(colnames(testdts)=="date")]
  #boost and predict
  eta=0.4
  depth  = 5
  subsample=0.6
  nrounds = 200
  F05_max= 0 
  for(i in seq(dim(dts)[2])){
    dts_2 = dts[,-i]
    bst <- xgboost(data = dts_2, label = label, max_depth = depth, 
                 eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
                 objective = "binary:logistic",eval_metric="aucpr",verbose = F)
    pred <- predict(bst, testdts)
    a=table((pred > 0.55)*2+(test_label>0))
    UA=a[4]/(a[3]+a[4])
    PA=a[4]/(a[2]+a[4])
    F05=1.25*UA*PA/(0.25*UA+PA)
    if(F05>F05_max){
      del = i
      F05_max=F05
    }
    cat(paste("removed",colnames(dts)[2],"threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*sUA,", PA:",100*sPA,"F05:",F05=,"\n"))
        #        cat(paste("date:",datenum,"threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*sUA,", PA:",100*sPA,"F05:",startF05,"\n"),file="C:/data/results.txt",append=T)
  }
  cat(paste("Attribute to remove : ",colnames(dts)[del] ))
}