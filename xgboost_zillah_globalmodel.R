
# open packages 
library(terra)
library(xgboost)

#source("/Users/temp/Documents/GitHub/ForestForesight/functions.R")
#files=list.files("/Users/temp/Documents/FF/10N_080W", pattern ="tif",full.names = T)
source("C:/data/xgboost_test/helpers/functions.R")
files=list.files("C:/Users/jonas/Downloads/10N_080W", pattern ="tif",full.names = T)
static_files= files[-grep("01.",files)]
data = c("2022-1-01","2023-4-01")

start=T
for(i in data){
  dynamic_files = files[grep(i,files)]
  rasstack = rast(c(dynamic_files, static_files))
  dts=as.matrix(rasstack)
  coords=xyFromCell(rasstack,seq(ncol(rasstack)*nrow(rasstack)))
  dts=cbind(coords,dts)
  dts=cbind(dts,rep(abs(round(as.numeric(as.Date(i))%%365.25)-183),nrow(dts)))
  dts=cbind(dts,rep(as.numeric(as.Date(i)),nrow(dts)))
  print(unique(dts[,17]))
  colnames(dts)=c("x","y",gsub(".tif","",c(gsub(paste0("_",date),"",basename(dynamic_files)), basename(static_files))),"yearday_relative","date")
  #dts=spatSample(rasstack,size=ncol(rasstack)*nrow(rasstack),xy=T,method="regular", values=FALSE)
  if(start){
    fulldts=dts;start=F}else{fulldts=rbind(fulldts,dts)}
  }


######method 1: random sample###########

dts=fulldts
dts[is.na(dts)]=0
groundtruth_index=which(colnames(dts)=="groundtruth")
label=dts[,groundtruth_index]
dts=dts[,-groundtruth_index]
dts_backup=dts
label_backup=label
#sample test data and exclude test data from training data
testsamples=sample(seq(nrow(dts)),round(nrow(dts)/4))
testdts=dts[testsamples,]
testdts=testdts
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

#boost and predict
eta=0.1
subsample=0.9
nrounds=500
depth=9

bst <- xgboost(data = dts, label = label,
               max_depth = depth, eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
               objective = "binary:logistic",eval_metric="aucpr",verbose = T)

pred <- predict(bst, testdts)

startF05=0
for(i in quantile(pred,seq(0.4,1,0.01))){
  a=table((pred > i)*2+(test_label>0))
  UA=round(a[4]/(a[3]+a[4]),2)
  PA=round(a[4]/(a[2]+a[4]),2)
  F05=round(1.25*UA*PA/(0.25*UA+PA),2)
  if(!is.na(F05)){
    if(F05>startF05){
      threshold=i
      startF05=F05
      sUA=UA
      sPA=PA
    }}

}
cat(paste("threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*sUA,", PA:",100*sPA,"F05:",startF05,"\n"))



######method 2: other date###########
dts=fulldts
dts[is.na(dts)]=0
groundtruth_index=which(colnames(dts)=="groundtruth")
label=dts[,groundtruth_index]
dts=dts[,-groundtruth_index]
dts_backup=dts
label_backup=label
#sample test data and exclude test data from training data
testsamples=which(dts[,which(colnames(dts)=="date")]==19448)
testdts=dts[testsamples,]
testdts=testdts
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
#boost and predict
eta=0.1
subsample=0.9
nrounds=500
depth=9

bst <- xgboost(data = dts, label = label,
               max_depth = depth, eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
               objective = "binary:logistic",eval_metric="aucpr",verbose = F)

pred <- predict(bst, testdts)

startF05=0
for(i in quantile(pred,seq(0.4,1,0.01))){
  a=table((pred > i)*2+(test_label>0))
  UA=round(a[4]/(a[3]+a[4]),2)
  PA=round(a[4]/(a[2]+a[4]),2)
  F05=round(1.25*UA*PA/(0.25*UA+PA),2)
  if(!is.na(F05)){
    if(F05>startF05){
      threshold=i
      startF05=F05
      sUA=UA
      sPA=PA
    }}
  
}
cat(paste("threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*sUA,", PA:",100*sPA,"F05:",startF05,"\n"))


#####test part#####
files=list.files(pattern="stack")
files=files[grep("2022",files)]
start=T
for(file in files){
  rasstack=rast(file)
  dts=spatSample(rasstack,size=ncol(rasstack)*nrow(rasstack),xy=T,method="regular")
  names(dts)=c("x","y",gsub(".tif","",alllayers))
  if(start){fulldts=dts}else{fulldts=rbind(fulldts,dts)}
}

testdts=fulldts
#write.table(dts,"helperspowerbicsv.csv",dec=",",sep=";",row.names=F)
testdts[is.na(testdts)]=0
test_label_s=testdts$groundtruth
testdts$groundtruth=NULL
testdts$latestdeforestation=NULL
#sample test data and exclude test data from training data
testdts=as.matrix(testdts)
test_label_s=as.matrix(test_label_s)
test_label_s[test_label_s>1]=1

pred <- predict(bst, testdts)

startF05=0
for(i in quantile(pred,seq(0.9,1,0.001))){
  a=table((pred > i)*2+(test_label_s>0))
  UA=round(a[4]/(a[3]+a[4]),2)
  PA=round(a[4]/(a[2]+a[4]),2)
  F05=round(1.25*UA*PA/(0.25*UA+PA),2)
  if(!is.na(F05)){
    if(F05>startF05){
      threshold=i
      startF05=F05
      sUA=UA
      sPA=PA
    }}

}
cat(paste("threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*sUA,", PA:",100*sPA,"F05:",startF05,"\n"))


############train set############




pred_train <- predict(bst, dts)



for(i in quantile(pred,seq(0.9,1,0.001))){
  a=table((pred_train > i)*2+(label_s>0))
  UA=round(a[4]/(a[3]+a[4]),2)
  PA=round(a[4]/(a[2]+a[4]),2)
  F05=round(1.25*UA*PA/(0.25*UA+PA),2)
  cat(paste("quantile:",round(i,2), "UA:",100*UA,", PA:",100*PA,"F05:",F05,"\n"))
}
