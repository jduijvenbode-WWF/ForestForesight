#' Calculate Scores
#'
#' This function calculates scores based on predictions, ground truth, and optional parameters.
#'
#' @param predictions A character vector or raster object representing the predictions.
#' @param groundtruth A character vector or raster object representing the ground truth.
#' @param forestmask An optional character vector or raster object representing the forest mask.
#' @param csvfile An optional CSV file to which the results will be written.
#' @param append Logical. If TRUE, results will be appended to the existing CSV file.
#' @param analysis_polygons Optional vector or character vector representing analysis polygons.
#' @param return_polygons Logical. If TRUE, the polygons with calculated scores will be returned.
#' @param remove_empty Logical. If TRUE, empty rows (with all scores being zero) will be removed from the output.
#'
#' @return A vector dataset containing calculated scores for each polygon.
#'
#' @examples
#' \dontrun{
#' calculate_scores(predictions, groundtruth, forestmask, csvfile, append, analysis_polygons, return_polygons, remove_empty)
#' }
#'
#' @export

ff_analyse=function(predictions,groundtruth,forestmask=NULL,csvfile=NULL,append=T,analysis_polygons=NULL,return_polygons=T,remove_empty=T){
  if(class(predictions)=="character"){predictions=rast(predictions)}
  if(class(groundtruth)=="character"){groundtruth=rast(groundtruth,win=ext(predictions))}
  groundtruth[is.na(groundtruth)]=0
  date=substr(groundtruth,nchar(groundtruth)-13,nchar(groundtruth)-4)
  tile=basename(dirname(predictions))
  if(!is.null(forestmask)){
    cat("using forest mask/n")
    if(class(forestmask)=="character"){forestmask=rast(forestmask)}
    cross=2*groundtruth+predictions*forestmask
  }else{cross=2*groundtruth+predictions}
  if(is.null(analysis_polygons)){
    data("degree_polygons")
    pols=vect(degree_polygons)}else{
      if(class(analysis_polygons=="character")){
        pols=vect(analysis_polygons)}else{
          pols=analysis_polygons}}
  pols$FP=extract(cross==1,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$FN=extract(cross==2,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$TP=extract(cross==3,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$TN=extract(cross==0,pols,fun="sum",na.rm=T,touches=F)[,2]
  pols$date=date
  pols$tile=tile
  if(remove_empty){pols=pols[-which(rowSums(as.data.frame(pols[,c("FP","FN","TP")]),na.rm=T)==0),]}
  if(!is.null(csvfile)){
    if(append){pastdata=read.csv(csvfile)
    pastdata$X=NULL
    write.csv(rbind(pastdata,as.data.frame(pols)),csvfile)}else{
      write.csv(as.data.frame(pols),csvfile)
    }
  }
  if(return_polygons){return(pols)}
}
