library(ForestForesight)
crast=rast("D:/downloads/Agri_potential_mosaic_10.tif")
tempras=list.files(file.path("D:/ff-dev/results/preprocessed/input/"),pattern="elev",full.names = T,recursive=T)
for(ras in tempras){
  print(ras)
  newname=gsub("elevation","croplandcapacity100p",ras)

  writeRaster(round(255*project(crast,rast(ras))),newname,datatype="INT1U",gdal=c("COMPRESS=LZW"),overwrite=T)
}

crast=rast("D:/downloads/Agri_potential_mosaic_30.tif")

for(ras in tempras){
  print(ras)
  newname=gsub("elevation","croplandcapacityover50p",ras)

  writeRaster(round(255*project(crast,rast(ras),method="near")),newname,datatype="INT1U",gdal=c("COMPRESS=LZW"),overwrite=T)
}

crast=rast("D:/downloads/Agri_potential_mosaic_40.tif")

for(ras in tempras){
  print(ras)
  newname=gsub("elevation","croplandcapacitybelow50p",ras)

  writeRaster(round(255*project(crast,rast(ras))),newname,datatype="INT1U",gdal=c("COMPRESS=LZW"),overwrite=T)
}

