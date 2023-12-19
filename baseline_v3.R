setwd("C:/data/colombia_tiles/")
files=list.files(path="results",recursive=T,pattern="predictions_2")
write.csv(files,"donefiles.csv")
pols=vect("../global_1x1degree_polygons.json")
calculate_scores=function(predfile,groundtruthfile,pols,adddate=T,fmaskfile=NULL){
  pred=rast(predfile)
  groundtruth=rast(groundtruthfile,win=ext(pred))
  groundtruth[is.na(groundtruth)]=0
  date=substr(groundtruthfile,nchar(groundtruthfile)-13,nchar(groundtruthfile)-4)
  tile=basename(dirname(predfile))
  if(!is.null(fmaskfile)){
    cat("using forest mask")
    fmask=rast(fmaskfile)
    cross=2*groundtruth+pred*fmask
  }else{cross=2*groundtruth+pred}
  
  pols$FP=extract(cross==1,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$FN=extract(cross==2,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$TP=extract(cross==3,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$TN=extract(cross==0,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$date=date
  pols$tile=tile
  return(pols)
}
for(file in files){
  print(file)
  predfile=file.path("results",file)
  fmaskfile=file.path("input",dirname(file),"forestmask2019.tif")
  groundtruthfile=file.path("input",gsub("predictions","groundtruth",file))
  calcpols=calculate_scores(predfile,groundtruthfile,pols)
  if(file==files[1]){allpols=calcpols}else{allpols=rbind(allpols,calcpols)}
}
ap=as.data.frame(allpols)
ap=ap[-which(rowSums(ap[,c("FP","FN","TP")],na.rm=T)==0),]
ap2=read.csv("C:/data/results20231118.csv")
ap2$X=NULL
ap3=rbind(ap2,ap)
write.csv(ap3,"C:/data/results20231118.csv")
# calculate precision, recall, F1 and F0.5. These are not used in powerbi because the score also depends on the size of the polygon
# so the scores below should be calculated on the highest order of scale you are presenting in the data 
# example: a polygon of 10x10 meters should not have the same impact on total F05 as a polygon of 10000x10000 meters with many more FN, FP, TP and TN


setwd("C:/data/colombia_tiles/input/")
setwd("D:/ff-dev/results")
files=list.files(recursive=T,pattern="01.tif")
files=files[grep("groundtruth",files)]
for(file in files){
  if(nchar(strsplit(file,"_")[[1]][3])==13){
    newname=paste0(substr(file,1,nchar(file)-8),"0",substr(file,nchar(file)-7,nchar(file)))
    file.rename(file,newname)}
}
