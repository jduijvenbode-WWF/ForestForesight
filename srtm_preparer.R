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
  print(theurl)
  if(!file.exists(paste0("elevation/",f,'_elevation.tif'))){
    try(utils::download.file(url=theurl, destfile=paste0(f,'.zip'), method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE),silent=T)
  }
}
for(img in seq(length(alerts))){
  for(x in c(0,5)){
    for(y in c(5,10)){
      print(img)
      SRTM(ext(alerts[img])[1]+x,ext(alerts[img])[3]+y)
    }
  }
}

files=list.files(pattern="zip")
for(file in files[45:length(files)]){
  unzip(file)
  if(file.exists(gsub("zip","tif",file))){
    a=rast(gsub("zip","tif",file))
    writeRaster(a,file.path("elevation",gsub(".zip","_elevation.tif",file)),overwrite=T)
    terra::terrain(a,v="slope",filename=file.path("slope",gsub(".zip","_slope.tif",file)),overwrite=T)
    terra::terrain(a,v="roughness",filename=file.path("roughness",gsub(".zip","_roughness.tif",file)),overwrite=T)
  }
}
setwd("elevation")
srtms=list.files()
srtm=srtms[1]
start=T
for(srtm in srtms){
  pol=as.polygons(ext(rast(srtm)))
  if(start){
    start=F
    pols=pol}else{pols=rbind(pols,pol)}
}

plot(alerts[1],add=T)
alert=alerts[1]
for(ind in seq(1,length(alerts))){
  alert=alerts[ind]
  elevationras=rast("D:/ff-dev/results/",alert$tile_id,"/elevation.tif")
  if(nrow(elevationras)==1250){
  selpol=pols[buffer(alert,-2)]
  if(length(selpol)>1){
    srtmras=merge(sprc(selpol$names))
    rnras=merge(sprc(paste0("../roughness/",gsub("elevation","roughness",selpol$names))))
  }else{srtmras=rast(selpol$names)
        rnras=rast(paste0("../roughness/",gsub("elevation","roughness",selpol$names)))
  }
  if(!dir.exists(file.path("D:/ff-dev/results/",alert$tile_id))){dir.create(paste0("D:/ff-dev/results/",alert$tile_id))}
  baseras=aggregate(rast(paste0("../../alerts/",alert$tile_id,".tif")),40,"max")
  srtmras_proj=project(srtmras,baseras,method="average",align=T,filename=paste0("D:/ff-dev/results/",alert$tile_id,"/elevation.tif"),overwrite=T)
  rnras_proj=project(rnras,baseras,method="average",align=T,filename=paste0("D:/ff-dev/results/",alert$tile_id,"/slope.tif"),overwrite=T)
}
