peatlands=rast("C:/data/opusinsights_data/FAO_wetlands_geotiff.tif")
tempras=sapply(list.dirs(path = "C:/data/preprocessed/",full.names = T)[2:104],function(x) list.files(x,full.names=T,pattern="elev")[1])
for(ras in tempras){
  a=project(peatlands,rast(ras),method="near")>0
  writeRaster(a,gsub("2021-01-01_elevation","2004-01-01_wetlands",ras))
}


peatlands=rast("C:/Users/jonas/Downloads/TROP-SUBTROP_PeatDepthV2_2016_CIFOR.tif")
tempras=sapply(list.dirs(path = "C:/data/preprocessed/",full.names = T)[2:104],function(x) list.files(x,full.names=T,pattern="elev")[1])
for(ras in tempras){
  a=project(peatlands,rast(ras),method="average")
  a[a==15]=0
  writeRaster(a,gsub("2021-01-01_elevation","2016-01-01_peatlands",ras))
}
