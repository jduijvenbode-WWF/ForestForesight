
# open packages 
library(terra)
library(xgboost)

#source("/Users/temp/Documents/GitHub/ForestForesight/functions.R")
#files=list.files("/Users/temp/Documents/FF/10N_080W", pattern ="tif",full.names = T)
source("C:/data/xgboost_test/helpers/functions.R")
files=list.files("C:/Users/jonas/Downloads/10N_080W", pattern ="tif",full.names = T)
static_files= files[-grep("01.",files)]
data = c("2022-1-01","2022-4-01","2022-7-01","2022-10-01","2022-11-01","2023-1-01","2023-2-01","2023-4-01")

start=T
for(i in data){
  dynamic_files = files[grep(i,files)]
  rasstack = rast(c(dynamic_files, static_files))
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
testdts=testdts[,-which(colnames(testdts)=="date")]
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
for(datenum in seq(8)){
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
  eta=0.1
  
  for(depth in c(5)){
    for(subsample in c(0.6)){
      for(nrounds in c(200)){
        bst <- xgboost(data = dts, label = label,
                       max_depth = depth, eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
                       objective = "binary:logistic",eval_metric="aucpr",verbose = F)
        
        pred <- predict(bst, testdts)
        
        startF05=0
        for(i in seq(0.25,0.65,0.01)){
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
    }
  }
}



######method 3: different area###########

dts=fulldts
dts[is.na(dts)]=0
groundtruth_index=which(colnames(dts)=="groundtruth")
label=dts[,groundtruth_index]
dts=dts[,-groundtruth_index]
dts_backup=dts
label_backup=label
#sample test data and exclude test data from training data
testsamples=which(dts[,2]>=7)
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
eta=0.1
subsample=0.7
nrounds=100
depth=5

bst <- xgboost(data = dts, label = label,
               max_depth = depth, eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
               objective = "binary:logistic",eval_metric="aucpr",verbose = T)

pred <- predict(bst, testdts)

startF05=0
for(i in seq(0.25,0.65,0.01)){
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
cat(paste("threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*sUA,", PA:",100*sPA,"F05:",startF05,"\n"))

