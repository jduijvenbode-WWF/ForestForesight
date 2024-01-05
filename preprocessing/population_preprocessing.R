library(ForestForesight)
setwd("D:/ff-dev/")
source("C:/Users/EagleView/Documents/GitHub/ForestForesight/functions.R")
data("gfw_tiles")
ialerts=vect(gfw_tiles)
setwd("population/")
files=list.files(pattern="tif$")
elevations=list.files("D:/ff-dev/results",full.names = T,recursive = T,pattern="elevation.tif")
increase=rast(files[3])-rast(files[1])
current=rast(files[2])
for(el in elevations){
  print(el)

  filename=gsub("elevation","popcurrent",el)
  filename2=gsub("elevation","popincrease",el)
  template=rast(el)
  extent=ext(template)+0.01
  #pop2020
  #pop2025
  if(!file.exists(filename)){
  popcur=crop(current,extent)
  popcur=project(popcur,template,method="max")
  focal(popcur,fun="sum",w=matrixcreator(25),filename=filename)
  }
  #pop2030
  if(!file.exists(filename2)){
  popinc=crop(increase,extent)
  popinc=project(popinc,template,method="max")
  focal(popinc,fun="sum",w=matrixcreator(25),filename=filename2)

  }
}
