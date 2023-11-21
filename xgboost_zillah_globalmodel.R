
# open packages 
library(terra)
library(xgboost)
if(Sys.info()[4]=="LAPTOP-DMVN4G1N"){
  source("C:/data/xgboost_test/helpers/functions.R")
  files=list.files("C:/Users/jonas/Downloads/10N_080W", pattern ="tif",full.names = T)
} else if (Sys.info()[4]=="DESKTOP-3DNFBGC"){
  source("C:/Users/admin/Documents/GitHub/ForestForesight/functions.R")
  files=list.files("D:/ff-dev/results/10N_080W", pattern ="tif",full.names = T)
} else{
  source("/Users/temp/Documents/GitHub/ForestForesight/functions.R")
  files=list.files("/Users/temp/Documents/FF/10N_080W", pattern ="tif",full.names = T)
}


static_files= files[-grep("01\\.",files)]
data = c("2022-1-01","2022-2-01","2022-3-01","2022-4-01","2022-5-01","2022-6-01")

start=T
for(i in data){
  dynamic_files = files[grep(i,files)]
  rasstack = rast(c(dynamic_files, static_files)) # win argument resulted in error 
  #rasstack = crop(rasstack,ext(rast(rasstack))-3) # used crop instead 
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



######method 2: other date A: xgboost###########
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
#        cat(paste("date:",datenum,"threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*sUA,", PA:",100*sPA,"F05:",startF05,"\n"),file="C:/data/results.txt",append=T)
      }
    }
  }
}



######method 2: other date B: xgb.train###########

evalerrorF05 <- function(preds, dts_matrix) {
  # Check for NAs in preds and labels
  if (any(is.na(preds)) || any(is.na(getinfo(dts_matrix, "label")))) {
    stop("NA values detected in preds or labels.")
  }
  i <- 0.55
  labels <- getinfo(dts_matrix, "label")
  #cat(max(preds))
  a <- table((preds > i) * 2 + (labels > 0))
  if (length(a)==4){
    UA<<-a[4]/(a[3]+a[4])
    PA<<-a[4]/(a[2]+a[4])
    F05<<- 1.25 * UA * PA / (0.25 * UA + PA)  
  }else{
    F05 = 0
  }
  return(list(metric = "error F05", value = as.numeric(F05)))
}


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
  
  #convert to xgb.Matrix
  dts_matrix= xgb.DMatrix(dts, label=label)
  test_matrix= xgb.DMatrix(testdts, label=test_label)
  watchlist = list(train = dts_matrix, eval = test_matrix)
  
  
  #boost and predict
  eta=0.4
  
  for(depth in c(5)){
    for(subsample in c(0.6)){
      for(nrounds in c(200)){
        bst <- xgb.train(data = dts_matrix,
                       max_depth = depth, eta = eta, subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 10,
                       objective = "binary:logistic", feval= evalerrorF05 , maximize= TRUE, verbose = 1, watchlist= watchlist)
        
        pred <- predict(bst, testdts)
        a=table((pred > 0.5)*2+(test_label>0))
        UA=a[4]/(a[3]+a[4])
        PA=a[4]/(a[2]+a[4])
        F05=round(1.25*UA*PA/(0.25*UA+PA),2)
        
        cat(paste("date:",datenum,"threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*UA,", PA:",100*PA,"F05:",F05,"\n"))
        #        cat(paste("date:",datenum,"threshold:",threshold,"eta:",eta,"subsample:",subsample,"nrounds:",nrounds,"depth:",depth,"UA:",100*sUA,", PA:",100*sPA,"F05:",startF05,"\n"),file="C:/data/results.txt",append=T)
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


 
### Test : train once, test on subsequent data ##

dts=fulldts
dts=dts[,-which(colnames(dts)=="yearday_relative")]
dts[is.na(dts)]=0
groundtruth_index=which(colnames(dts)=="groundtruth")
label=dts[,groundtruth_index]
dts=dts[,-groundtruth_index]


#sample train data 
trainsamples=which(dts[,which(colnames(dts)=="date")]==as.numeric(as.Date(data[1])))
traindts=dts[trainsamples,]
train_label=label[trainsamples]

#filter too many true negatives
filterindex=which(colnames(traindts)=="smtotaldeforestation")
deforestation_count=sum(traindts[,filterindex]>0)
nonforestindices=which(traindts[,filterindex]==0)
remove_indices=sample(nonforestindices,length(nonforestindices)-deforestation_count)
traindts=traindts[-remove_indices,]
train_label=train_label[-remove_indices]
train_label[train_label>1]=1
traindts=traindts[,-which(colnames(dts)=="date")]

# train the model on first date (2022-1-01)
eta=0.1
subsample=0.7
nrounds=100
depth=5

bst <- xgboost(data = traindts, label = train_label,
               max_depth = depth, eta = eta,subsample=subsample,  nrounds = nrounds,early_stopping_rounds = 3,
               objective = "binary:logistic",eval_metric="aucpr",verbose = F)
pred <- predict(bst, traindts)

# train F05 
F05 <- function(preds, label){
  a=table((pred > 0.5)*2+(label>0))
  UA=a[4]/(a[3]+a[4])
  PA=a[4]/(a[2]+a[4])
  F05=round(1.25*UA*PA/(0.25*UA+PA),4)
  return(F05)
}

cat("Train F05 is:", F05(pred,train_label))

# test the model on the subsequent 6 months

for(datenum in seq(2,length(data))){
  testsamples=which(dts[,which(colnames(dts)=="date")]==as.numeric(as.Date(data[datenum])))
  testdts=dts[testsamples,][,-which(colnames(dts)=="date")]
  test_label=label[testsamples]
  test_pred <- predict(bst, testdts)
  cat("For",data[datenum]," the test F05 is:", F05(test_pred,test_label),"\n")
}


