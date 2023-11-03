library(terra)
library(xgboost)
setwd("C:/data/xgboost_test/validation")
source("../helpers/functions.R")
#####split extent in parts######
splitintoparts=function(r,n){
  extlist=list()
  ext=ext(rast(r))
  for(i in ext[1]+((ext[2]-ext[1])/sqrt(n))*(seq(sqrt(n))-1)){
    for(j in ext[3]+((ext[4]-ext[3])/sqrt(n))*(seq(sqrt(n))-1)){
      extlist=append(extlist,ext(c(i,i+((ext[2]-ext[1])/sqrt(n)),j,j+((ext[4]-ext[3])/sqrt(n)))))
    }
  }
  return(extlist)
}
# Create an empty matrix filled with 0s
matrix_size <- 11
center <- (matrix_size + 1) / 2
window_matrix <- matrix(0, nrow = matrix_size, ncol = matrix_size)

# Define the radius for the circular window
radius <- (matrix_size - 1) / 2
# Fill in the circular window
for (i in 1:matrix_size) {
  for (j in 1:matrix_size) {
    distance_to_center <- sqrt((i - center)^2 + (j - center)^2)
    if (distance_to_center <= radius) {
      window_matrix[i, j] <- 1/max(distance_to_center,1)
    }
  }
}


file="C:/data/accuracy_analysis/origs/Integrated_alerts_BOL_3.tif"
exts=splitintoparts(file,9)
ffdates=c("2022-04-01")
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
    polis=vect("../helpers/srtmindices.json")
    srtmpols=crop(polis,ext(ras))
    if(length(srtmpols$name)>1){srtm=merge(sprc(srtmpols$name))}else{srtm=rast(srtmpols$name)}
    srtm=project(srtm,ras,filename="elevation.tif",overwrite=T)
    terrain(srtm,filename="slope.tif",overwrite=T)
    terrain(srtm,v="roughness",filename="roughness.tif",overwrite=T)
    aggregate(rast("slope.tif"),40,fun="mean",na.rm=T,filename="slope_res.tif",overwrite=T)
    aggregate(rast("roughness.tif"),40,fun="mean",na.rm=T,filename="roughness_res.tif",overwrite=T)
    aggregate(rast("elevation.tif"),40,fun="mean",na.rm=T,filename="elevation_res.tif",overwrite=T)
    ####forest mask#####
    forestmask("C:/users/jonas/Downloads/Forest_BOL_2001.tif",ras)
    
    
    for(ffdate in ffdates){
      diffdate=as.Date(ffdate)-as.Date("2015-01-01")
      #########labels#######
      labels(ras=ras,diffdate=diffdate)
      ######average confidence level##########
      mean_confidence(ras=ras,diffdate=diffdate)
      #######total 6 months#########
      lastsixmonths(ras=ras,diffdate=diffdate)
      #######total 12-6 months#########
      twelvetosixmonths(ras=ras,diffdate=diffdate)
      #####total deforestation#########
      pastdef=totaldeforestation(ras=ras,diffdate=diffdate)
      
      ######smoothed deforestation######
      smoothed_deforestation(inputfile="totaldeforestation.tif")
      ######smoothed last six months#####
      smoothed_6months(inputfile="6months.tif")
      #####edge filter######
      edgedetection(ras,diffdate)
      
      
      #create training, validation and test data
      
      alllayers=c("12-6months.tif","6months.tif","confidence.tif","deforestation_smoothed.tif",
                  "deforestation_smoothed6months.tif","totaldeforestation.tif","edgedensity.tif",
                  "forestmask.tif","slope_res.tif","roughness_res.tif","elevation_res.tif","groundtruth.tif")
      rasstack=rast(alllayers)
      writeRaster(rasstack,paste0("stack",ffdate,"_",extnum,".tif"),overwrite=T)
    }
  }
}

files=list.files(pattern="stack")
start=T
for(file in files){
  rasstack=rast(file)
  dts=spatSample(rasstack,size=1e6,xy=T,method="regular")
  names(dts)=c("x","y",gsub(".tif","",alllayers))
  if(start){fulldts=dts}else{fulldts=rbind(fulldts,dts)}
}
dts=fulldts
#write.table(dts,"helpers/powerbicsv.csv",dec=",",sep=";",row.names=F)
dts[is.na(dts)]=0
label_s=dts$groundtruth
dts$groundtruth=NULL
#sample test data and exclude test data from training data
dts=as.matrix(dts)
label_s=as.matrix(label_s)
label_s[label_s>1]=1
#boost and predict

pred <- predict(bst, dts)

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







pred_train <- predict(bst, dts)



for(i in quantile(pred,seq(0.9,1,0.001))){
  a=table((pred_train > i)*2+(label_s>0))
  UA=round(a[4]/(a[3]+a[4]),2)
  PA=round(a[4]/(a[2]+a[4]),2)
  F05=round(1.25*UA*PA/(0.25*UA+PA),2)
  cat(paste("quantile:",round(i,2), "UA:",100*UA,", PA:",100*PA,"F05:",F05,"\n"))
}

