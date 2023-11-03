library(terra)
library(sf)
setwd("C:/data/accuracy_analysis/")
borders=st_read("borders.geojson")
borders=vect(borders)
borders=borders[-which(is.na(borders$iso3))]
borders=rbind(borders,aggregate(disagg(borders[borders$iso3 %in% c("IDN","MYS")])[c(133,145)]))
borders$iso3[length(borders)]="BOR"
files=list.files(path="origs",full.names = T,pattern="tif")
ffdates=paste(sort(rep(c(2021,2022,2023),12)),seq(12),"01",sep="-")
ffdates=ffdates[1:26]
datfram=data.frame()
datfram=read.csv("baseline_res2.csv")
datfram$X=NULL
methods=c("6months","everything","sameseason")
for(resolution in c(400,250)){
for(method in methods){
  for(file in files){
    ras_start=rast(file)
    country=substr(file,25,27)
    border=borders[borders$iso3==country]
    extb=ext(crop(border,ext(ras_start)))
    extb<-c(floor(extb[1]),ceiling(extb[2]),floor(extb[3]),ceiling(extb[4]))
    ras_start=rast(file,win=extb)
    for(ffdate in ffdates){
      if(sum((datfram$filename==file)*(datfram$date==ffdate)*(datfram$method==method)*datfram$resolution==resolution)==0){
        diffdate=as.Date(ffdate)-as.Date("2015-01-01")
        if(method=="6months"){
        m=c(20000+diffdate-180,19999+diffdate,1,
            20000+diffdate,20000+diffdate+180,2,
            30000+diffdate-180,29999+diffdate,1,
            30000+diffdate,30000+diffdate+180,2,
            40000+diffdate-180,39999+diffdate,1,
            40000+diffdate,40000+diffdate+180,2)
        }
        if(method=="everything"){
          m=c(20000,19999+diffdate,1,
              20000+diffdate,20000+diffdate+180,2,
              30000,29999+diffdate,1,
              30000+diffdate,30000+diffdate+180,2,
              40000,39999+diffdate,1,
              40000+diffdate,40000+diffdate+180,2)
        }
        if(method=="sameseason"){
        m=c(20000+diffdate-365,19999+diffdate-180,1,
            20000+diffdate,20000+diffdate+180,2,
            30000+diffdate-365,29999+diffdate-182,1,
            30000+diffdate,30000+diffdate+180,2,
            40000+diffdate-365,39999+diffdate-182,1,
            40000+diffdate,40000+diffdate+180,2)
      }
        rclmat <- matrix(m, ncol=3, byrow=TRUE)
        ras=terra::aggregate(terra::classify(ras_start,rcl=rclmat,others=NA,filename="C:/data/temp_ras2.tif",overwrite=T),fun="mean",resolution/10,na.rm=T)
        pols=as.polygons(aggregate(ras,100000/resolution),dissolve=F,na.rm=F,values=F)
        pols=crop(pols,border)
        FP=extract(ras==1,pols,fun="sum",na.rm=T)[,2]
        FN=extract(ras==2,pols,fun="sum",na.rm=T)[,2]
        all=extract(ras>0,pols,fun="sum",na.rm=T)[,2]
        TN=extract(ras==0,pols,fun="sum",na.rm=T)[,2]
        TP=all-FP-FN
        precision=TP/(TP+FP)
        recall=TP/(TP+FN)
        accuracy=(TP+TN)/(TP+TN+FN+FP)
        F1score=(2*precision*recall)/(precision+recall)
        resdat=data.frame(WKT=geom(pols,wkt=T),filename=file,TP=all-FP-FN,FN=FN,FP=FP,TN=TN,precision=precision,recall=recall,accuracy=accuracy,F1score=F1score,date=ffdate,country=country,method=method,resolution=resolution)
        datfram=rbind(datfram,resdat)
        write.csv(datfram,"baseline_res2.csv")
        print(file)
        print(ffdate)
        print(method)
        print(resolution)
      }}}
}
}
