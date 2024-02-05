#' visualize hotzones
#'
#' This function creates the hotzones of 400x400 meters with accuracy built in
#'
#' @param predictions A character vector or raster object representing the predictions. Can be empty if country and date and datafolder are not empty
#' @param predfolder A path to the folder containing the tiles with predictions.
#' @param country A character vector of the iso3 code of the country that you want to process
#' @param date character. should be in format (YYYY-MM-DD). Optional if predictions is a character to the tiffile.
#' @param outputfile_pols character. The filename of the hotzone outputs
#' @param outputfile_centers character. The filename of the hotzone centroids outputs
#' @param t_cutoff The cutoff value below which no polygons should be returned. Default is 0.5
#' @param return_polygons Logical. If TRUE, returns the hotzone polygons as SpatVector, otherwise nothing is returned
#' @param overwrite. Whether existing polygons should be overwritten. default is TRUE
#'
#' @return A vector dataset containing calculated scores for each polygon.
#'
#' @export

ff_visualize=function(predictions=NA,predfolder=NA,country=NA,date=NA,outputfile_pols=NA,outputfile_centers=NA,t_cutoff=0.5,return_polygons=F,overwrite=T){
  if(is.na(predictions[1])&any(is.na(country),is.na(date),is.na(predfolder))){stop("either a raster for predictions should be given or the datafolder that contains the prediction tiles along with tile and date")}
  if(all(is.na(outputfile_pols),is.na(outputfile_centers),!return_polygons)){stop("return_polygons is set to False and outputfile is not given so this function does nothing")}
  if(is.na(predictions[1])){
    data("countries")
    countries=vect(countries)
    proc_country=countries[which(countries$iso3==country),]
    data("gfw_tiles")
    gfw_tiles=vect(gfw_tiles)
    tiles=gfw_tiles[proc_country,]$tile_id
    files=list.files(path=predfolder,recursive=T,full.names = T,pattern="tif$")
    tilefileinds=unique(unlist(sapply(tiles,function(x) grep(x,files))))
    datefileinds=grep(date,files)
    if(any(length(tilefileinds)==0,length(datefileinds)==0)){stop("no files found based on your criteria")}
    predictions=files[intersect(tilefileinds,datefileinds)]
  }else{
    if(!class(predictions) %in% c("character","SpatRaster")){stop("predictions is not a raster or path to a raster")}
  }

  for(i in 1:length(predictions)){
    if(class(predictions[[i]])=="character"){pred=rast(predictions[i])}else{pred=predictions[[i]]}
    pred[pred<t_cutoff]=NA
    pols=as.polygons(pred,dissolve=F,values=T,na.rm=T)
    names(pols)="certainty"
    if(exists("proc_country")){pols=pols[proc_country]}
    if(!exists("allpols")){allpols=pols}else{allpols=rbind(allpols,pols)}
  }

  if(!is.na(outputfile_pols)){writeVector(allpols,outputfile_pols,overwrite=overwrite)}
  if(!is.na(outputfile_centers)){writeVector(centroids(allpols),outputfile_centers,overwrite=overwrite)}
  if(return_polygons){return(allpols)}
}

