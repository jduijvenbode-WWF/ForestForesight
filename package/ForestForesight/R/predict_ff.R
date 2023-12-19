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
#' @rdname predict_ff
#' @name predict_ff


predict_ff <- function(model, test_matrix, threshold=0.5,groundtruth){
  predictions=predict(a,test_matrix)
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
  return(list(threshold=threshold,"precision"=precision,"recall"=recall,"F0.5"=F05))
}
