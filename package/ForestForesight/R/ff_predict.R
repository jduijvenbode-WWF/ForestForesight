#' Predict with a trained XGBoost model
#'
#' This function predicts on a trained xgboost model and a test dataset. it returns the precision, recall and F0.5
#'
#' @param model The trained XGboost model
#' @param test_matrix The xgbd test matrix
#' @param threshold Vector with chosen threshold(s). Default 0.5, which has shown to be the best in almost all scenarios
#' @param groundtruth A vector of the same length as the test matrix to verify against
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


ff_predict <- function(model, test_matrix, threshold=0.5,groundtruth,indices=NA,templateraster=NA){
  # Get the features
  model_features <- model$feature_names
  test_features <- colnames(test_matrix$features)
  # Check for features in the test matrix not present in the model
  extra_features <- setdiff(test_features, model_features)
  # If there are extra features, remove them from the test matrix
  if (length(extra_features) > 0) {
    warning(paste("Removing extra features from the test matrix:", paste(extra_features, collapse = ", ")))
    test_matrix$features <- test_matrix$features[, setdiff(test_features, extra_features), drop = FALSE]
     }
  # Convert the matrix to a DMatrix object
  test_matrix = xgb.DMatrix(test_matrix$features, label=test_matrix$label)

  predictions=predict(model,test_matrix)
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
  if(!is.na(indices[1])&!is.na(templateraster[1])){
    templateraster[]=0
    templateraster[indices]=predictions>threshold
  }
  return(list(threshold=threshold,"precision"=precision,"recall"=recall,"F0.5"=F05,"predicted_raster"=templateraster))
}
