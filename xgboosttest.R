library(terra)
library(xgboost)
setwd("C:/data/xgboost_test/")
source("helpers/functions.R")
#####split extent in parts######
file="C:/data/accuracy_analysis/origs/Integrated_alerts_COL_4.tif"
files=list.files("C:/data/accuracy_analysis/origs/")
exts=splitintoparts(file,4)
ffdates=paste(sort(rep(c(2021,2022,2023),12)),seq(12),"01",sep="-")
ffdates=ffdates[1:26]
ffdate="blabla"
for(extnum in seq(length(exts))){
  if(!file.exists(paste0("stack",ffdate,"_",extnum,".tif"))){
    print(extnum)
    ##########create SRTM polygons###########
    # files=list.files(path="C:/users/jonas/Downloads/",full.names=T,pattern="SRTM")
    # 
    # for(i in seq(length(files))){
    #   pol=as.polygons(ext(rast(files[i])))
    #   pol$name=files[i]
    #   if(i==1){
    #     allpols=pol}else{allpols=rbind(allpols,pol)
    #   }}
    # writeVector(allpols,"srtmindices.json",overwrite=T)
    #######elevation derivatives######
    ras=rast(file,win=exts[[extnum]])
    polis=vect("helpers/srtmindices.json")
    srtmpols=crop(polis,ext(ras))
    if(length(srtmpols$name)>1){srtm=merge(sprc(srtmpols$name))}else{srtm=rast(srtmpols$name)}
    srtm=project(srtm,ras,method="bilinear",filename="elevation_rough.tif",overwrite=T)
    terrain(srtm,filename="slope_rough.tif",overwrite=T)
    terrain(srtm,v="roughness",filename="roughness_rough.tif",overwrite=T)
    aggregate(rast("slope_rough.tif"),40,fun="mean",na.rm=T,filename="slope.tif",overwrite=T)
    aggregate(rast("roughness_rough.tif"),40,fun="mean",na.rm=T,filename="roughness.tif",overwrite=T)
    aggregate(rast("elevation_rough.tif"),40,fun="mean",na.rm=T,filename="elevation.tif",overwrite=T)
    ####forest mask#####
    forestmask("C:/users/jonas/Downloads/SouthAmerica_2001_primary.tif",ras)
    for(ffdate in ffdates){
      diffdate=as.numeric(as.Date(ffdate)-as.Date("2015-01-01"))
      #########labels#######
      labels(ras=ras,diffdate=diffdate)
      ######average confidence level##########
      mean_confidence(ras=ras,diffdate=diffdate)
      #######total 6 months#########
      lastsixmonths(ras=ras,diffdate=diffdate)
      #######total 12-6 months#########
      twelvetosixmonths(ras=ras,diffdate=diffdate)
      #####total deforestation#########
      totaldeforestation(ras=ras,diffdate=diffdate)
      patchsize(ras,matrixcreator(21))
      ######smoothed deforestation######
      smoothed_deforestation(inputfile="totaldeforestation.tif",window_matrix = matrixcreator(11))
      ######smoothed last six months#####
      smoothed_6months(inputfile="6months.tif",window_matrix = matrixcreator(11))
      #####edge filter######
      edgedetection_nomask(ras,diffdate)
      edgedetection_withmask(ras,diffdate)
      
      
      #create training, validation and test data
      
      alllayers=c("12-6months.tif","6months.tif","confidence.tif","deforestation_smoothed.tif",
                  "deforestation_smoothed6months.tif","totaldeforestation.tif","edgedensity_withmask.tif","edgedensity.tif",
                  "forestmask.tif","slope.tif","roughness.tif","elevation.tif","latestdeforestation.tif","groundtruth.tif")
      rasstack=rast(alllayers)
      writeRaster(rasstack,paste0("stack",ffdate,"_",extnum,".tif"),overwrite=T)
    }
  }
}
######method 1: random sample###########
files=list.files(pattern="stack")
start=T
for(file in files){
  rasstack=rast(file)
  dts=spatSample(rasstack,size=ncol(rasstack)*nrow(rasstack),xy=T,method="regular")
  names(dts)=c("x","y",gsub(".tif","",alllayers))
  if(start){fulldts=dts}else{fulldts=rbind(fulldts,dts)}
}

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
               objective = "binary:logistic",eval_metric="aucpr",verbose = F)

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

