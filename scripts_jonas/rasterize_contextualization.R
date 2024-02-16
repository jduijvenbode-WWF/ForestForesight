for(i in tempras[64:length(tempras)]){
  rasje=rast(i)
  newa=crop(a,ext(rasje))
  t1=rasterize(newa,rasje,field="ID",fun="min")
  writeRaster(t1,file.path("C:/data/storage/contextualization/GADM/",gsub("elevation","GADM",basename(i))),datatype="INT2U",gdal="COMPRESS=DEFLATE")
}
writeVector(a,"C:/data/storage/contextualization/GADM.gpkg",overwrite=T)

a=vect("C:/data/storage/contextualization/ECOBIOME.gpkg")
a$ID=seq(length(a))
for(i in tempras){
  rasje=rast(i)
  newa=crop(a,ext(rasje))
  if(length(newa)==0){t1[]=NA}else{t1=rasterize(newa,rasje,field="ID",fun="min")}
  writeRaster(t1,file.path("C:/data/storage/contextualization/ECOBIOME/",gsub("elevation","GADM",basename(i))),datatype="INT2U",gdal="COMPRESS=DEFLATE")
}
writeVector(a,"C:/data/storage/contextualization/ECOBIOME.gpkg",overwrite=T)
