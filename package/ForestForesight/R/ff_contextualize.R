#' contextualize hotzones or centroids
#'
#' This function gives context for searchability in e.g. ArcGIS online or powerBI
#'
#' @param hotzones A character vector or raster object representing the predictions. Can be empty if country and date and datafolder are not empty
#' @param hotzonesfolder A path to the folder containing the tiles with predictions.
#' @param contextfolder A path to the folder that contains all the files that should be added as context features to the predictions
#' @param NA_label When the hotzone does not overlap with the context polygons, this is the value that should be used instead.
#' @param country A character vector of the iso3 code of the country that you want to process
#' @param date character. should be in format (YYYY-MM-DD). Optional if either groundtruth or predictions is a character to the tiffile.
#' @param outputfile character. The filename of the hotzone outputs
#' @param return_vector Logical. If TRUE, the SpatVector that was used as input is returned.
#' @param overwrite. Whether existing polygons should be overwritten. default is TRUE
#' @param WDPA Logical. Whether the WDPA classification should be added. Default is TRUE
#' @param GADM Logical. Whether the GADM classification should be added. Default is TRUE
#' @param ECOBIOME Logical. Whether the ECOBIOME classifications should be added. Default is TRUE
#' @param centers Logical. Whether the centers should also be written. Default is FALSE
#'
#' @return A vector dataset containing the input data with all columns of the files in the context folder added
#'
#' @export

ff_contextualize=function(hotzones=NA,hotzonesfolder=NA,contextfolder,NA_label="Unknown",country=NA,date=NA,outputfile=NA,centers=F,return_vector=F,overwrite=T,WDPA=TRUE,GADM=TRUE,ECOBIOME=TRUE){
  if(is.na(hotzones[1])&any(is.na(country),is.na(date),is.na(hotzonesfolder))){stop("either a raster for predictions should be given or the datafolder that contains the prediction tiles along with tile and date")}
  if(all(is.na(outputfile),!return_vector)){stop("return_polygons is set to False and outputfile is not given so this function does nothing")}
  if(is.null(contextfolder)){stop("contextfolder needs to be provided")}
  if(is.na(hotzones[1])){
    data("countries")
    countries=vect(countries)
    proc_country=countries[which(countries$iso3==country),]
    data("gfw_tiles")
    gfw_tiles=vect(gfw_tiles)
    tiles=gfw_tiles[proc_country,]$tile_id
    files=list.files(path=hotzonesfolder,recursive=T,full.names = T)
    files=files[unique(c(grep("shp",files),grep("gpkg",files),grep("geojson",files),grep("json",files)))]
    tilefileinds=unique(unlist(sapply(tiles,function(x) grep(x,files))))
    datefileinds=grep(date,files)
    if(any(length(tilefileinds)==0,length(datefileinds)==0)){stop("no files with the extension geojson, json, shp or gpkg found based on your criteria")}
    hotzones=files[intersect(tilefileinds,datefileinds)]
  }else{
    if(length(hotzones)==0){stop("no files found")}
    if(!class(hotzones[[1]]) %in% c("character","SpatVector")){stop("predictions is not a vector or path to a raster")}
  }
  if(GADM){GADMp=vect(file.path(contextfolder,"GADM.gpkg"))}
  if(WDPA){WDPAp=vect(file.path(contextfolder,"WDPA.gpkg"))}
  if(ECOBIOME){ECOBIOMEp=vect(file.path(contextfolder,"ECOBIOME.gpkg"))}
  for(i in 1:length(hotzones)){
    if(class(hotzones[[i]])=="character"){hz=vect(hotzones[[i]])}else{hz=hotzones[[i]]}
    hz$TID=seq(length(hz))
    c_hz=centroids(hz)
    if(GADM){
      GADMvals=extract(GADMp,c_hz)
      GADMvals[is.na(GADMvals)]=NA_label
      hz=merge(hz,GADMvals,by.x="TID",by.y="id.y")
    }
    if(WDPA){
      WDPAp$wdpa="yes"
      WDPAvals=extract(WDPAp,c_hz)
      WDPAvals[is.na(WDPAvals)]="no"
      hz=merge(hz,WDPAvals,by.x="TID",by.y="id.y")
    }
    if(ECOBIOME){
      ECOBIOMEvals=extract(ECOBIOMEp,c_hz)
      ECOBIOMEvals[is.na(ECOBIOMEvals)]=NA_label
      hz=merge(hz,ECOBIOMEvals,by.x="TID",by.y="id.y")
    }
    hz$TID=NULL
    if(!exists("allhz")){allhz=hz}else{allhz=rbind(allhz,hz)}
  }
  if(exists("proc_country")){allhz=allhz[proc_country]}
  if(!is.na(outputfile)){
    writeVector(allhz,outputfile,overwrite=overwrite)
    if(centers){
      ext=strsplit(basename(outputfile),"\\.")[[1]][2]
      centerfile=gsub(paste0(".",ext),paste0("_centers.",ext),outputfile)
      writeVector(centroids(allhz),centerfile,overwrite=overwrite)}
    }
  if(return_vector){return(allhz)}
}

