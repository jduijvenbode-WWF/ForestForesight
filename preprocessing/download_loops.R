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

library(httr)

files=read.csv("D:/ff-dev/landuse/2020.txt",header=F)$V1

setwd("D:/ff-dev/landuse/")
for(file in files){
  a=gregexpr("GLCLU2000",file)[[1]]
  filename=substr(file,a[1]+5,nchar(file))
  filename=gsub("\\.tif","_landuse\\.tif",basename(filename))
  b=httr::GET(file)
  writeBin(b$content,filename)
}

library(ForestForesight)
data("gfw_tiles")
IA=vect(gfw_tiles)
setwd("D:/ff-dev/alerts/")
apikey="8892b7c2-7350-46ef-9b0e-5f3aacf92c0e"
for(id in seq(nrow(IA))){
  file=IA$download[id]
  file=paste0(substr(file,1,gregexpr("key=",file)[[1]][1]+3),apikey)
  name=IA$tile_id[id]
  b=httr::GET(file)
  writeBin(b$content,paste0(name,".tif"))
  cat(paste(Sys.time(),name,"\n"))
}

for(i in gfw_tiles$tile_id){
  file=paste0("https://glad.umd.edu/users/Potapov/GLCLUC2020/Forest_height_2020/2020_",i,".tif")
  b=httr::GET(file)
  writeBin(b$content,paste0(i,".tif"))
}
files=list.files()
for(file in files){
  print(file)
  tempras=list.files(file.path("D:/ff-dev/results/preprocessed/",substr(file,1,8)),full.names = T,pattern="elevation")[1]
  project(rast(file),rast(tempras),method="average",
          filename=file.path(dirname(tempras),paste0(substr(basename(tempras),1,8),"_2020-01-01_forestheight.tif")))
}
