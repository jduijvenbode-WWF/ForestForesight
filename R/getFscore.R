#' Calculate F-score
#'
#' This function calculates the F-score given the ground truth, threshold and predicted values.
#'
#' @param gt The ground truth vector.
#' @param pred The predicted vector.
#' @param threshold The threshold for prediction.
#' @param beta The weight of precision in the F-score calculation.
#' @param pr Logical. whether it should also return the precision and recall
#' @return F-score value.
#' @export
getFscore <- function(gt, pred, threshold = 0.5, beta = 0.5, pr = F) {
  gt[is.na(gt)] = 0
  pred[is.na(pred)] = 0
  # Convert predictions to binary
  pred <- as.numeric(pred >= threshold) + gt*2

  # Calculate true positives, false positives, and false negatives
  tp <- sum(pred == 3)
  fp <- sum(pred == 1)
  fn <- sum(pred == 2)

  # Calculate precision and recall
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)

  # Calculate F-score
  f_score <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)
  if(!pr){return(f_score)}
  return(list("F05"=f_score,"precision"=precision,"recall"=recall))
}
