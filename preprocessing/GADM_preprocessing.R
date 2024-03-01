setwd("C:/data/storage/contextualization/")
GADM=vect("GADM.gpkg")
GADM$ID=seq(nrow(GADM))
tempras=list.files("C:/data/storage/preprocessed/input/",recursive=T,full.names = T,pattern="elev")
for(i in tempras){
  print(i)
  tras=rast(i)
  newname=file.path("C:/data/storage/contextualization/GADM",gsub("elevation","GADM",basename(i)))
  focGADM=crop(GADM,ext(tras))
  if(nrow(focGADM)>0){rasterize(focGADM,tras,field="ID",fun="max",filename=newname,overwrite=T)}else{writeRaster(tras*0,newname,overwrite=T)}

}
head(GADM)
