files=list.files(path="C:/users/jonas/downloads/nightlights202305",pattern="h5$",full.names=T)
curdate="2023-5-01"
library(terra)
areas=vect("C:/data/accuracy_analysis/integratedalerts.geojson")
for(file in files){
  a=system(paste("gdalinfo",file),intern=T)
  if(length(grep("GRingPointLatitude=",a))==1){
    lat=strsplit(trimws(gsub("GRingPointLatitude=","",a[grep("GRingPointLatitude=",a)]))," ")[[1]]
    lon=strsplit(trimws(gsub("GRingPointLongitude=","",a[grep("GRingPointLongitude=",a)]))," ")[[1]]
    foldername=paste0(sprintf("%02d",abs(as.numeric(lat[2]))),if(as.numeric(lat[2])<0){"S"}else{"N"},"_",
                      sprintf("%03d",abs(as.numeric(lon[1]))),if(as.numeric(lon[1])<0){"W"}else{"E"})
    cat(paste(foldername,"\n"))
    if(foldername %in% areas$tile_id){
      if(!file.exists(paste0('C:/data/nightlights_refurbished/',foldername,"/nightlights_",curdate,".tif"))){
        if(!dir.exists(file.path('C:/data/nightlights_refurbished/',foldername))){dir.create(file.path('C:/data/nightlights_refurbished/',foldername))}
        system(paste("gdal_translate -a_srs EPSG:4326 -a_nodata 65535 -a_ullr",lon[1],lat[2],lon[3],lat[1],
                     paste0('-of GTiff HDF5:"',file,'"://HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data_Fields/NearNadir_Composite_Snow_Free ', 
                            'C:/data/nightlights_refurbished/',foldername,"/nightlights_",curdate,".tif")),intern=T)
      }
    }
  }}