#' Predict with a trained XGBoost model
#'
#' This function predicts on a trained xgboost model and a test dataset. it returns the precision, recall and F0.5
#'
#' @param model The trained XGboost model
#' @param test_matrix The xgbd test matrix
#' @param threshold Vector with chosen threshold(s). Default 0.5, which has shown to be the best in almost all scenarios
#' @param groundtruth A vector of the same length as the test matrix to verify against
#' @param indices. a vector of the indices of the template raster that need to be filled in.
#' @param templateraster. a spatraster that can serve as the template to fill in the predictions
#' @param verbose. whether the output should be verbose
#' @param certainty. if True the certainty in percentage of the prediction will be returned, otherwise just true or false
#'
#' @return list with precision, recall and F0.5
#'
#' @import xgboost
#' @export
#'
#' @references
#' Jonas van Duijvenbode (2023)
#'
#' @keywords XGBoost data preparation
#' @rdname ff_predict
#' @name ff_predict


ff_predict <- function(model, test_matrix, threshold=0.5,groundtruth=NA,indices=NA,templateraster=NA,verbose=F,certainty=F){
  # Get the features
  if(class(model)=="character"){
    if(file.exists(model)){
      xgb.load(model)
      if(file.exists(gsub("\\.model","\\.rda",model))){
        features=load(gsub("\\.model","\\.rda",model))
        attr(model,"feature_names")=features
      }
    }
  }
  model_features <- model$feature_names
  if(!is.null(model_features)){
    test_features <- colnames(test_matrix$features)
    # Check for features in the test matrix not present in the model
    extra_features <- setdiff(test_features, model_features)
    # If there are extra features, remove them from the test matrix
    if (length(extra_features) > 0) {
      warning(paste("Removing extra features from the test matrix:", paste(extra_features, collapse = ", ")))
      test_matrix$features <- test_matrix$features[, setdiff(test_features, extra_features), drop = FALSE]
    }
  }
  # Convert the matrix to a DMatrix object
  if(!is.na(test_matrix$label[1])){test_matrix = xgb.DMatrix(test_matrix$features, label=test_matrix$label)}else{test_matrix = xgb.DMatrix(test_matrix$features)}
  if(verbose){cat("calculating predictions\n")}
  predictions=predict(model,test_matrix)
  if(certainty){predictions=as.integer(predictions*100)}
  if(!is.na(groundtruth[1])){
    if(class(groundtruth)=="SpatRaster"){groundtruth=as.numeric(as.matrix(groundtruth))}
    if(verbose){cat("calculationg scores\n")}
    precision=c()
    recall=c()
    F05=c()
    for(thresh in threshold){
      res=table(2*(predictions>thresh)+groundtruth)
      prec=as.numeric(res[4]/(res[4]+res[3]))
      rec=as.numeric(res[4]/(res[4]+res[2]))
      precision=c(precision,prec)
      recall=c(recall,rec)
      F05=c(F05,1.25*prec*rec/(0.25*prec+rec))
    }
  }else{precision=NA;recall=NA;F05=NA}
  if(class(templateraster)=="SpatRaster"){
    templateraster[]=0
    if(length(indices)>1){
      if(verbose){cat("filling raster\n")}
      if(!certainty){templateraster[indices]=predictions>threshold}else{templateraster[indices]=predictions}
    }else{
      if(ncell(templateraster)==length(predictions)){
        if(verbose){cat("filling raster\n")}
        if(!certainty){templateraster[]=predictions>threshold}else{templateraster[]=predictions}
      }else{templateraster<-NA}
    }
  }else{templateraster<-NA}
  if(verbose){cat(paste("F0.5:",F05,"\n"))}
  return(list(threshold=threshold,"precision"=precision,"recall"=recall,"F0.5"=F05,"predicted_raster"=templateraster,"predictions"=predictions))
}
