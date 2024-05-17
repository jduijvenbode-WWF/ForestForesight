setwd("C:/users/jonas/Downloads/")
library(ForestForesight)
temp=rast("tas_Amon_EC-Earth3_s2020.nc")
#cpp hieronder moet ptr voor rainfall zijn
times=as.character(as.Date(as_datetime(temp@cpp$time,origin="1970-01-01"))+1-months(1))
utimes=unique(times)
tempras=sapply(list.dirs(path = "C:/data/storage/preprocessed/input/",full.names = T)[2:104],function(x) list.files(x,full.names=T,pattern="elev")[1])
x=1
tras=tempras[1]
alldates=ForestForesight::daterange("2021-01-01","2027-07-01")
for(x in seq(79)){
  print(x)
  seltimes=utimes[x:(x+5)]
  cdate=alldates[x]
  indices=which(times%in% seltimes)
  medtemp=10*app(temp[[indices]],fun="median")
  for(ras in tempras){
    newfilename=gsub("elevation","temperature",ras)
    newfilename=gsub("2021-01-01",cdate,newfilename)
    rast=project(medtemp,rast(ras),method="cubic")
    print(newfilename)
    writeRaster(round(rast),newfilename,datatype="INT2U",gdal="COMPRESS=DEFLATE",overwrite=T)
  }
}
