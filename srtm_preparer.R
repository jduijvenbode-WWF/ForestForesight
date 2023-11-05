srtmbb=vect("../srtm.shp")
alerts=vect("../integratedalerts.geojson")
srtms=srtmbb[alerts]
setwd("../srtms")

SRTM<-function(lon, lat) {
  stopifnot(lon >= -180 & lon <= 180)
  stopifnot(lat >= -60 & lat <= 60)
  rs <- raster::raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
  rowTile <- rowFromY(rs, lat)
  colTile <- colFromX(rs, lon)
  if (rowTile < 10) { rowTile <- paste('0', rowTile, sep='') }
  if (colTile < 10) { colTile <- paste('0', colTile, sep='') }
  
  f <- paste('srtm_', colTile, '_', rowTile, sep="")
  theurl <- paste("https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/", f, ".zip", sep="")
  if(!file.exists(paste0(file,'.zip'))){
    try(utils::download.file(url=theurl, destfile=paste0(f,'.zip'), method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE),silent=T)
    }
}
for(img in seq(length(alerts))){
  for(x in c(0,5)){
    for(y in c(0,5)){
      print(img)
      SRTM(ext(alerts[img])[1]+x,ext(alerts[img])[3]+y)
    }
  }
}

files=list.files(pattern="zip")
for(file in files){
  unzip(file)
  if(file.exists(gsub("zip","tif",file))){
  a=rast(gsub("zip","tif",file))
  writeRaster(a,file.path("elevation",gsub(".zip","_elevation.tif",file)))
  terra::terrain(a,v="slope",filename=file.path("slope",gsub(".zip","_slope.tif",file)))
  terra::terrain(a,v="roughness",filename=file.path("roughness",gsub(".zip","_roughness.tif",file)))
  }
}
