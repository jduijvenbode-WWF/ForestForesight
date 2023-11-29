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
files=read.csv("D:/ff-dev/treecover2000.txt",header=F)$V1
indices=unique(unlist(sapply(list.dirs(path="D:/ff-dev/results",full.names = F)[2:104],function(x) grep(x,files))))
files=files[indices]
setwd("D:/ff-dev/forestmasks_tiled/")
for(file in files){
  a=gregexpr("treecover2000_",file)[[1]]
  filename=substr(file,a[1]+14,nchar(file))
  if(!file.exists(filename)){
  b=httr::GET(file)
  writeBin(b$content,filename)
  }
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
