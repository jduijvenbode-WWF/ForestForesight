

library(terra)
library(gdalUtilities)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"
dir= "D:/ff-dev/results/preprocessed"
tiles=list.files(file.path(inputdir), pattern ="_")
date= "2020-01-01"

gdal_translate(paste0(sg_url,'cec/cec_0-5cm_mean.vrt'),
               paste0(dir,"/tempCEC/cec_0_5cm.tif"))
cec05_b=  rast(paste0(dir,"/tempCEC/cec_0_5cm.vrt"))
cec05= rast( paste0(dir,"/tempCEC/cec_0_5cm.tif"))


for (tile in tiles){
  elev_rast= rast(paste0(dir,"/", tile,'/', tile, "_2021-01-01_elevation.tif" ))
  cec05_tile = project(cec05_b, elev_ras)
  cec05_tile = resample(cec05_b, elev_rast, method="near")


}


t1=rast("C:/data/cec/cec_0-5cm_mean.vrt",win=ext(c(-10,-8,-3,-1)))
for(rasje in tempras){
  rasjeras=rast(rasje)
  cecras=project(rast("C:/data/cec/testtiff2.tif"),rasjeras,method="mode")
  cecras[is.na(cecras)]=0
  writeRaster(cecras,gsub("elevation","catexcap",rasje),datatype="INT2U",gdal="COMPRESS=DEFLATE")
}
