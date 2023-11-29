#load packages
library(terra)
library(sf)


setwd("C:/data/accuracy_analysis/")

#load borders (global map of all countries) and remove all unnecessary attributes and polygons without an ISO3 country code
borders=st_read("borders.geojson")
borders=vect(borders)
borders=borders[-which(is.na(borders$iso3))]
borders$status=NULL
borders$color_code=NULL
borders$iso_3166_1_alpha_2_codes=NULL
borders$geo_point_2d=NULL
borders$french_short=NULL

#list all the files of the global preprocessed tiles
files=list.files(path="C:/data/colombia_tiles/input",recursive=T,full.names = T,pattern="tif$")

#filter the files for six months in the past
sixmonths=files[grep("\\/6months",files)]

#create the polygon WKT file to visualise spatially in powerbi for all polygons between 30N and 30S
pols=as.polygons(rast(nrows=60, ncols=360, nlyrs=1, xmin=-180, xmax=180,ymin=-30, ymax=30, crs="epsg:4326",vals=rep(0,60*360)),dissolve=F,na.rm=F,values=F)
pols$coordname=paste0(round(crds(centroids(pols))[,1]-0.5),"_",round(crds(centroids(pols))[,2]-0.5))
pols2=intersect(pols,borders)
#write.csv(data.frame(WKT=geom(pols2,wkt=T),coordname=pols2$coordname,continent=pols2$continent,region=pols2$region,iso3=pols2$iso3,country=pols2$name),"C:/data/global_baseline.csv")

#read the existing result csv that is used in powerbi
datfram=read.csv("C:/data/baseline_results.csv")
#remove automatically generated column of previous write
datfram$X=NULL
#loop over all rasters of the past six months
for(i in 1:length(sixmonths)){
  # get date of file
  filedate=as.Date(substr(sixmonths[i],tail(gregexpr("_",sixmonths[i])[[1]],1)+1,nchar(sixmonths[i])-4))
  # do not run when already processed
  if(!(paste0(basename(dirname(sixmonths[i])),filedate)) %in% paste0(datfram$tile,datfram$date)){
    print(i)
    print(filedate)
    #replace all NA with 0 and everything that is left set to 1
    smras=rast(sixmonths[i])
    smras[is.na(smras[])] <- 0
    smras=smras>0
    
    #identify the groundtruth raster
    groundtruth=gsub("6months","groundtruth",sixmonths[i])
    if(file.exists(groundtruth)){
      #load the groundtruth raster, replace all NA with zero and make sure that there are only 1 and 0 values in there
      gtras=rast(groundtruth)
      gtras[is.na(gtras[])] <- 0
      gtras=gtras>0
      #creates a new raster where:
      #0 means no groundtruth, no past deforestation (TN) 
      #1 means only groundtruth (FN), 2 means only 
      #2 means only past deforestation, no groundtruth (FP)
      #3 means both groundtruth and past deforestation (TP)
      ras=smras*2+gtras
      #extract per value per polygon, do not take pixels that only touch the polygons, just take the pixels that overlap with the polygons
      FP=extract(ras==1,pols2,fun="sum",na.rm=T,touches=F)[,2]
      FN=extract(ras==2,pols2,fun="sum",na.rm=T,touches=F)[,2]
      TP=extract(ras==3,pols2,fun="sum",na.rm=T,touches=F)[,2]
      TN=extract(ras==0,pols2,fun="sum",na.rm=T,touches=F)[,2]
      # calculate precision, recall, F1 and F0.5. These are not used in powerbi because the score also depends on the size of the polygon
      # so the scores below should be calculated on the highest order of scale you are presenting in the data 
      # example: a polygon of 10x10 meters should not have the same impact on total F05 as a polygon of 10000x10000 meters with many more FN, FP, TP and TN
      precision=TP/(TP+FP)
      recall=TP/(TP+FN)
      accuracy=(TP+TN)/(TP+TN+FN+FP)
      F1score=(2*precision*recall)/(precision+recall)
      F05score=(1.25*precision*recall)/(0.25*precision+recall)
      #add all parameters together, remove the ones that have no values in it 
      #because there are polygons for the entire world that extract and only the ones that overlap with the raster should be in there
      resdat=data.frame(coordname=pols2$coordname,TP=TP,FN=FN,FP=FP,TN=TN,precision=precision,recall=recall,accuracy=accuracy,F1score=F1score,F05score=F05score,date=as.character(filedate),tile=basename(dirname(groundtruth)),country=pols2$iso3)
      resdat=resdat[which(!is.nan(TN)),]
      #bind with the total dataset and write
      datfram=rbind(datfram,resdat)
      write.csv(datfram,"C:/data/baseline_results.csv")
    }
  }
}
write.csv(datfram,"C:/data/baseline_results.csv")
