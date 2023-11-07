
# open packages 
library(terra)
library(xgboost)

source("/Users/temp/Documents/GitHub/ForestForesight/functions.R")
files=list.files("/Users/temp/Documents/FF/10N_080W", pattern ="tif",full.names = T)

static_files= files[-grep("01.",files)]
data = c("2023-2-01","2023-3-01")

start=T
for(i in data){
  dynamic_files = files[grep(i,files)]
  rasstack = rast(c(dynamic_files, static_files))
  dts=as.matrix(rasstack)
  #dts=spatSample(rasstack,size=ncol(rasstack)*nrow(rasstack),xy=T,method="regular", values=FALSE)
  if(start){fulldts=dts}else{fulldts=rbind(fulldts,dts)}
}


######method 1: random sample###########

dts=fulldts
write.table(dts,"helpers/powerbicsv.csv",dec=",",sep=";",row.names=F)
dts[is.na(dts)]=0
label_s=dts$groundtruth
dts$groundtruth=NULL
#sample test data and exclude test data from training data
testsamples=sample(seq(nrow(dts)),round(nrow(dts)/4))
testdts=dts[testsamples,]
testdts=as.matrix(testdts)
dts=dts[-testsamples,]
test_label_s=label_s[testsamples]
label_s=label_s[-testsamples]
dts=as.matrix(dts)
deforestation_count=sum(rowSums(dts[,3:8])>0)
nonforestindices=which(rowSums(dts[,3:8])==0)
remove_indices=sample(nonforestindices,length(nonforestindices)-deforestation_count)
dts=dts[-remove_indices,]
label_s=label_s[-remove_indices]
label_s=as.matrix(label_s)
label_s[label_s>1]=1

#boost and predict
eta=0.1
subsample=0.9
nrounds=500
depth=9

bst <- xgboost(data = dts, label = label_s,
               max_depth = depth, eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
               objective = "binary:logistic",eval_metric="aucpr",verbose = T)

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



######method 2: other date###########
files=list.files(pattern="stack")
files=files[-grep("2022",files)]
start=T
for(file in files){
  rasstack=rast(file)
  dts=spatSample(rasstack,size=ncol(rasstack)*nrow(rasstack),xy=T,method="regular")
  names(dts)=c("x","y",gsub(".tif","",alllayers))
  if(start){fulldts=dts}else{fulldts=rbind(fulldts,dts)}
}

dts=fulldts
#write.table(dts,"helperspowerbicsv.csv",dec=",",sep=";",row.names=F)
dts[is.na(dts)]=0
label_s=dts$groundtruth
dts$groundtruth=NULL
dts$latestdeforestation=NULL
#sample test data and exclude test data from training data
dts=as.matrix(dts)
label_s=as.matrix(label_s)
label_s[label_s>1]=1
#boost and predict
eta=0.1
subsample=0.9
nrounds=1000
depth=9

bst <- xgboost(data = dts, label = label_s,
               max_depth = depth, eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
               objective = "binary:logistic",eval_metric="aucpr",verbose = F)

pred_train <- predict(bst, dts)
falsepositives=which((2*(pred_train>0.5)-label_s)==1)
deforestation_count=sum(rowSums(dts[,3:8])>0)
nonforestindices=which(rowSums(dts[,3:8])==0)
remove_indices=sample(nonforestindices,length(nonforestindices)-deforestation_count+length(falsepositives))
dts=dts[-remove_indices,]
label_s=label_s[-remove_indices]

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
