setwd("D:/ff-dev/results/preprocessed")
outputfile="../accuracy_analysis/baseline_20240501.csv"
library(ForestForesight)
files=list.files(recursive=T,pattern="groundtruth6m")

data("degree_polygons")
pols=vect(degree_polygons)
calculate_scores=function(predfile,groundtruthfile,pols,adddate=T,fmaskfile=NULL){
  pred=rast(predfile)
  groundtruth=rast(groundtruthfile,win=ext(pred))
  groundtruth[is.na(groundtruth)]=0
  date=substr(groundtruthfile,nchar(groundtruthfile)-13,nchar(groundtruthfile)-4)
  tile=basename(dirname(predfile))
  if(!is.null(fmaskfile)){
    cat("using forest mask")
    fmask=rast(fmaskfile)>0
    cross=(2*groundtruth+pred)*fmask
  }else{cross=2*groundtruth+pred}

  pols$FP=extract(cross==1,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$FN=extract(cross==2,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$TP=extract(cross==3,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$TN=extract(cross==0,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$date=date
  pols$tile=tile
  pols=pols[-which(rowSums(as.data.frame(pols)[,c("FP","FN","TP")],na.rm=T)==0),]
  return(pols)
}
for(x in 1:length(files)){
  file=files[x]
  cat(paste0(file," (",x," out of",length(files),")\n"))
  blfile=gsub("groundtruth","input",gsub("groundtruth6m","lastsixmonths",file))
  fmaskfile=file.path("input",basename(dirname(file)),paste0(basename(dirname(file)),"_2021-01-01_initialforestcover.tif"))

  groundtruthfile=gsub("predictions","groundtruth",file)
  calcpols=calculate_scores(predfile = blfile,groundtruthfile = groundtruthfile,pols = pols,fmaskfile = fmaskfile)
  if(x==1){allpols=calcpols}else{allpols=rbind(allpols,calcpols)}
}
ap=as.data.frame(allpols)

write.csv(ap,outputfile)
# calculate precision, recall, F1 and F0.5. These are not used in powerbi because the score also depends on the size of the polygon
# so the scores below should be calculated on the highest order of scale you are presenting in the data
# example: a polygon of 10x10 meters should not have the same impact on total F05 as a polygon of 10000x10000 meters with many more FN, FP, TP and TN
