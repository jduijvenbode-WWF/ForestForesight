#' summary data aspects of a raster
#'
#' requires a path to a raster as input and gives some quality parameters of that file
#'
#' @param raster path to raster.
#'
#' @examples
#' # Example usage:
#' result <- process_tif_files("path/to/tif_file")
#'
#' @export
#'
#'
 ff_dqc_file=function(raster){
loadras=rast(raster)
return(list("npixel"=ncell(loadras),"xmin"=xmin(loadras),"xmax"=xmax(loadras),"ymin"=ymin(loadras),"ymax"=ymax(loadras),"resolution"=res(loadras)[1],
            "crsname"=crs(loadras, describe=T)$name,"crscode"=crs(loadras, describe=T)$code,"mean"=as.numeric(round(global(loadras,"mean",na.rm=T),2)),"max"=as.numeric(round(global(loadras,"max",na.rm=T),2)),"hasNA"=(length(summary(loadras,warn=F))==7)))
}
