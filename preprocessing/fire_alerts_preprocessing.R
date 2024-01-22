library(ForestForesight)
fires1=vect("D:/ff-dev/fires/fire_archive_SV-C2_415940.shp")
setwd("D:/ff-dev/fires/")
times=daterange("2020-07-01","2024-01-01")
data(gfw_tiles)
gfw_tiles=vect(gfw_tiles)
for(i in 1:nrow(gfw_tiles)){
  print(i)
  tempras=rast(list.files(full.names=T,pattern="elevation",path=file.path("D:/ff-dev/results/preprocessed",gfw_tiles$tile_id[i]))[1])
  tilefires=fires1[gfw_tiles[i,],]
  if(!dir.exists(gfw_tiles$tile_id[i])){dir.create(gfw_tiles$tile_id[i])}
  for(x in 7:length(times)){
    selfires=tilefires[which((ymd(tilefires$ACQ_DATE)>=ymd(times[x-6]))&(ymd(tilefires$ACQ_DATE)<ymd(times[x])))]
    selfires$counter=1
    rasterize(selfires,tempras,"counter",fun="sum",background=0,
              filename=file.path(gfw_tiles$tile_id[i],paste0(gfw_tiles$tile_id[i],"_",times[x],"_sensor1.tif")))
  }
}
rm(fires1)
fires2=vect("D:/ff-dev/fires/fire_nrt_J1V-C2_415939.shp")
times=daterange("2020-07-01","2024-01-01")
data(gfw_tiles)
gfw_tiles=vect(gfw_tiles)
for(i in 1:nrow(gfw_tiles)){
  print(i)
  tempras=rast(list.files(full.names=T,pattern="elevation",path=file.path("D:/ff-dev/results/preprocessed",gfw_tiles$tile_id[i]))[1])
  tilefires=fires2[gfw_tiles[i,],]
  if(!dir.exists(gfw_tiles$tile_id[i])){dir.create(gfw_tiles$tile_id[i])}
  for(x in 7:length(times)){
    selfires=tilefires[which((ymd(tilefires$ACQ_DATE)>=ymd(times[x-6]))&(ymd(tilefires$ACQ_DATE)<ymd(times[x])))]
    selfires$counter=1
    rasterize(selfires,tempras,"counter",fun="sum",background=0,
              filename=file.path(gfw_tiles$tile_id[i],paste0(gfw_tiles$tile_id[i],"_",times[x],"_sensor2.tif")))
  }
}
rm(fires2)
fires3=vect("D:/ff-dev/fires/fire_nrt_SV-C2_415940.shp")
times=daterange("2020-07-01","2024-01-01")
data(gfw_tiles)
gfw_tiles=vect(gfw_tiles)
for(i in 1:nrow(gfw_tiles)){
  print(i)
  tempras=rast(list.files(full.names=T,pattern="elevation",path=file.path("D:/ff-dev/results/preprocessed",gfw_tiles$tile_id[i]))[1])
  tilefires=fires3[gfw_tiles[i,],]
  if(!dir.exists(gfw_tiles$tile_id[i])){dir.create(gfw_tiles$tile_id[i])}
  for(x in 7:length(times)){
    selfires=tilefires[which((ymd(tilefires$ACQ_DATE)>=ymd(times[x-6]))&(ymd(tilefires$ACQ_DATE)<ymd(times[x])))]
    selfires$counter=1
    rasterize(selfires,tempras,"counter",fun="sum",background=0,
              filename=file.path(gfw_tiles$tile_id[i],paste0(gfw_tiles$tile_id[i],"_",times[x],"_sensor3.tif")))
  }
}
files=list.files(recursive=T,pattern="sensor")
a=matrix(files,nrow=3)
b=as.data.frame(t(a))
for(x in 1:nrow(b)){
  app(rast(as.character(b[x,])),"sum",na.rm=T,filename=file.path("D:/ff-dev/results/preprocessed/",dirname(b[x,1]),gsub("sensor1","firealerts",basename(b[x,1]))))
}
