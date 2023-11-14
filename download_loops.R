library(httr)
files=read.csv("D:/ff-dev/forestloss.txt",header=F)$V1
setwd("D:/ff-dev/forestloss_2000_2022/")
for(file in files){
  a=gregexpr("lossyear_",file)[[1]]
  filename=substr(file,a[1]+9,nchar(file))
  b=httr::GET(file)
  writeBin(b$content,filename)
}

library(httr)
files=read.csv("D:/ff-dev/gain.txt",header=F)$V1
dir.create("D:/ff-dev/forestgain_2000_2012")
setwd("D:/ff-dev/forestgain_2000_2012")
for(file in files){
  a=gregexpr("gain_",file)[[1]]
  filename=substr(file,a[1]+5,nchar(file))
  b=httr::GET(file)
  writeBin(b$content,filename)
}

library(terra)
IA=vect("../integratedalerts.geojson")
setwd("../alerts/")
for(id in seq(nrow(IA))){
  file=IA$download[id]
  name=IA$tile_id[id]
  b=httr::GET(file)
  writeBin(b$content,paste0(name,".tif"))
}
