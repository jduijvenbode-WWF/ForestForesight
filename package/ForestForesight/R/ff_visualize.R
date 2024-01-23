#' visualize hotzones
#'
#' This function creates the hotzones of 400x400 meters with accuracy built in
#'
#' @param predictions A character vector or raster object representing the predictions.can be empty if country and date and predfolder are not empty
#' @param predfolder A path to the folder containing the tiles with predictions.
#' @param country A character vector of the iso3 code of the country that you want to process
#' @param date character. should be in format (YYYY-MM-DD). Optional if either groundtruth or predictions is a character to the tiffile.
#' @param outputfile character. The filename of the hotzone outputs
#' @param t_cutoff The cutoff value below which no polygons should be returned. normally set at 0.5
#' @param return_polygons Logical. If TRUE, the hotzone polygons.
#' @param overwrite. Whether existing polygons should be overwritten. default is TRUE
#'
#' @return A vector dataset containing calculated scores for each polygon.
#'
#' @export

ff_visualize=function(predictions=NA,predfolder=NA,country=NA,date=NA,outputfile,t_cutoff=0.5,return_polygons=F,overwrite=T){
  if(is.na(predictions)&any(is.na(country),is.na(date),is.na(datafolder))){stop("either a raster for predictions should be given or the datafolder that contains the prediction tiles along with tile and date")}
  if(is.na(predictions)){
    data("countries")
    countries=vect(countries)
    proc_country=countries[which(countries$iso3==country),]
    data("gfw_tiles")
    gfw_tiles=vect(gfw_tiles)
    tiles=gfw_tiles[proc_country,]$tile_id
    files=list.files(path=predfolder,recursive=T,full.names = T)
    predictions=files[intersect(unique(sapply(tiles,function(x) grep(x,files))),grep(date,files))]
  }else{
    if(!class(predictions) %in% c("character","SpatRaster")){stop("predictions is not a raster or path to a raster")}
  }
  for(prediction in predictions){
    if(class(prediction=="character")){pred=rast(predictions)}else{pred=prediction}
    pred[pred<t_cutoff]=NA
    pols=as.polygons(pred,dissolve=F,values=T,na.rm=T)
    names(pols)="certainty"
    if(exists(proc_country)){pols=pols[proc_country]}
    if(!exists("allpols")){allpols=pols}else{allpols=rbind(allpols,pols)}
  }

  writeVector(allpols,outputfile,overwrite=overwrite)
  if(return_polygons){return(pols)}
}

