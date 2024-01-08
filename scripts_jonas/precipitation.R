setwd("C:/users/jonas/Downloads/")
library(ForestForesight)
prec=c(rast("pr_Amon_MPI-ESM1-2-LR_s2020.nc"),rast("pr_Amon_MPI-ESM1-2-LR_s2021.nc"))
times=as.character(as.Date(as_datetime(prec@ptr$time,origin="1970-01-01"))+1-months(1))
utimes=unique(times)
tempras=sapply(list.dirs(path = "C:/data/colombia_tiles/input/",full.names = T)[2:104],function(x) list.files(x,full.names=T)[1])
for(x in 1:(length(utimes)-5)){
  seltimes=utimes[x:(x+5)]
  indices=which(times%in% seltimes)
  medprec=1e6*app(prec[[indices]],fun="median",filename="C:/temp/tempras3.tif",overwrite=T)
  for(tras in tempras){
    newfilename=file.path("C:/data/precipitation/",paste0(basename(dirname(tras)),"_",as.character(utimes[x]),"_precipitation.tif"))
    if(!file.exists(newfilename)){                    
    resras=project(medprec,rast(tras),method="cubic",
            filename="C:/temp/tempras4.tif",overwrite=T)
    writeRaster(round(resras),newfilename,datatype="INT2U",gdal="COMPRESS=DEFLATE",overwrite=T)
    }
  }
}

setwd("C:/data/precipitation/")
files=list.files()
for(file in files){
  file.rename(file,file.path("C:/data/colombia_tiles/input/",substr(file,1,8),file))
}
