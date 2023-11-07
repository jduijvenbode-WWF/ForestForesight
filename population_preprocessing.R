library(terra)
setwd("D:/ff-dev/")
source("C:/Users/EagleView/Documents/GitHub/ForestForesight/functions.R")
ialerts=vect("integratedalerts.geojson")
setwd("population/")
files=list.files(pattern="tif$")
elevations=list.files("D:/ff-dev/results",full.names = T,recursive = T,pattern="elevation.tif")
start=T
for(el in elevations){
  print(el)
  filename=gsub("elevation","pop2020",el)
  filename2=gsub("elevation","pop2025",el)
  filename3=gsub("elevation","pop2030",el)
  if(!file.exists(filename)){
  template=rast(el)
  #pop2020
  pop2020=rast(files[1],win=ext(template)+1)
  pop2020=project(pop2020,template,method="max")
  focal(pop2020,fun="sum",w=matrixcreator(25),filename=filename)
  #pop2025
  pop2025=rast(files[2],win=ext(template)+1)
  pop2025=project(pop2025,template,method="max")
  focal(pop2025,fun="sum",w=matrixcreator(25),filename=filename2)
  #pop2030
  pop2030=rast(files[3],win=ext(template)+1)
  pop2030=project(pop2030,template,method="max")
  focal(pop2030,fun="sum",w=matrixcreator(25),filename=filename3)
  
  }
}
