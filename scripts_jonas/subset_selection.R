
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


###### Subsset selection substraction ###########
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
  eta=0.3
  depth  = 4
  subsample=0.6
  nrounds = 100
  dts_2 = dts
  testdts_2= testdts
  while(dim(dts_2)[2]>1){
    F05_max= 0 
  for(i in seq(dim(dts_2)[2])){
    dts_temp = dts_2[,-i]
    testdts_temp = testdts_2[,-i] 
    bst <- xgboost(data = dts_temp, label = label, max_depth = depth, 
                 eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
                 objective = "binary:logistic",eval_metric="aucpr",verbose = F)
    pred <- predict(bst, testdts_temp)
    a=table((pred > 0.55)*2+(test_label>0))
    UA=a[4]/(a[3]+a[4])
    PA=a[4]/(a[2]+a[4])
    F05=1.25*UA*PA/(0.25*UA+PA)
    if(F05>F05_max){
      del = i
      F05_max=F05
    }
    cat(paste("removed",colnames(dts_2)[i],"UA:",round(UA,2),", PA:",round(PA,2),"F05:",round(F05,2),"\n"))
    cat(paste("removed",colnames(dts_2)[i],"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",round(UA,2),", PA:",round(PA,2),"F05:",round(F05,2),"\n"),file="/Users/temp/Documents/GitHub/ForestForesight/subset_results.txt",append=T)
  }
  cat(paste("Attribute to remove : ",colnames(dts_2)[del],"F05 max:", F05_max ,"\n" ))
  dts_2=dts_2[,-del]
  testdts_2 = testdts_2[,-del] 
  
  }
}


###### Subset selection addition###########

dts_left = dts
dts_new = matrix(nrow=dim(dts)[1], ncol=0)
dts_test_left = testdts
dts_test_new = matrix(nrow=dim(testdts)[1], ncol=0)
while(dim(dts_left)[2]>0){
  F05_max= 0 
  for(i in seq(dim(dts_left)[2])){
    dts_temp = cbind(dts_new, dts_left[,i])
    testdts_temp = cbind(dts_test_new, dts_test_left[,i])
    bst <- xgboost(data = dts_temp, label = label, max_depth = depth, 
                   eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
                   objective = "binary:logistic",eval_metric="aucpr",verbose = F)
    pred <- predict(bst, testdts_temp)
    a=table((pred > 0.55)*2+(test_label>0))
    UA=a[4]/(a[3]+a[4])
    PA=a[4]/(a[2]+a[4])
    F05=1.25*UA*PA/(0.25*UA+PA)
    if (is.na(F05)){F05=0}
    if(F05>F05_max){
      add = i
      F05_max=F05
    }
    cat(paste("added",colnames(dts_left)[i],"UA:",round(UA,2),", PA:",round(PA,2),"F05:",round(F05,2),"\n"))
    #cat(paste("added",colnames(dts_2)[i],"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",round(UA,2),", PA:",round(PA,2),"F05:",round(F05,2),"\n"),file="/Users/temp/Documents/GitHub/ForestForesight/subset_results.txt",append=T)
  }
  cat(paste("Attribute to add : ",colnames(dts_left)[add],"F05 max:", F05_max ,"\n" ))
  dts_new=cbind(dts_new, dts_left[,add])
  dts_left =dts_left[,-add] 
  dts_test_new = cbind(dts_test_new, dts_test_left[,add])
  dts_test_left = dts_test_left[,-add]
}


###### Function subset selection addition###########

selectsubset = function(dts,label,testdts,test_label, eta=0.3, depth = 4
                        , subsample=0.6, nrounds = 100, treshold=0.55){
  dts_left = dts
  dts_new = matrix(nrow=dim(dts)[1], ncol=0)
  dts_test_left = testdts
  dts_test_new = matrix(nrow=dim(testdts)[1], ncol=0)
  F05_best=0
  li=list()
  while(!is.null(dim(dts_left))){
    F05_max= 0 
    for(i in seq(dim(dts_left)[2])){
      dts_temp = cbind(dts_new, dts_left[,i])
      testdts_temp = cbind(dts_test_new, dts_test_left[,i])
      bst <- xgboost(data = dts_temp, label = label, max_depth = depth, 
                     eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
                     objective = "binary:logistic",eval_metric="aucpr",verbose = F)
      pred <- predict(bst, testdts_temp)
      a=table((pred > treshold)*2+(test_label>0))
      UA=a[4]/(a[3]+a[4])
      PA=a[4]/(a[2]+a[4])
      F05=1.25*UA*PA/(0.25*UA+PA)
      if (is.na(F05)){F05=0}
      if(F05>F05_max){
        add = i
        F05_max=F05
      }
      cat(paste("added",colnames(dts_left)[i],"UA:",round(UA,2),", PA:",round(PA,2),"F05:",round(F05,4),"\n"))
    }
    cat(paste("Attribute to add : ",colnames(dts_left)[add],"F05 max:", round(F05_max,4) ,"\n" ))
    li=append(li, colnames(dts_left)[add])
    dts_new=cbind(dts_new, dts_left[,add])
    dts_left =dts_left[,-add] 
    dts_test_new = cbind(dts_test_new, dts_test_left[,add])
    dts_test_left = dts_test_left[,-add]
    if(F05_max>F05_best){
      F05_best=F05_max
    }
  }
  return(list(best_subset= li, F05 = F05_best))
}

