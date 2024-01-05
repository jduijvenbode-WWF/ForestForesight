setwd("C:/users/jonas/Downloads/")
library(ForestForesight)
prec=c(rast("pr_Amon_MPI-ESM1-2-LR_s2020.nc"),rast("pr_Amon_MPI-ESM1-2-LR_s2021.nc"))
times=as.character(as.Date(as_datetime(prec@ptr$time,origin="1970-01-01"))+1-months(1))
utimes=unique(times)
tempras=sapply(list.dirs(path = "C:/data/colombia_tiles/input/",full.names = T)[2:104],function(x) list.files(x,full.names=T)[1])
for(time in utimes){
  print(time)
  indices=which(times==time)
  medprec=1e6*app(prec[[indices]],fun="median")
  for(tras in tempras){
    project(medprec,rast(tras),method="cubic",
            filename=file.path("C:/data/precipitation/",paste0(basename(dirname(tras)),"_",as.character(time),"_precipitation.tif")))
  }
}
