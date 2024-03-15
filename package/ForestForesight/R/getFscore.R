#' Calculate F-score
#'
#' This function calculates the F-score given the ground truth, threshold and predicted values.
#'
#' @param gt The ground truth vector.
#' @param pred The predicted vector.
#' @param threshold The threshold for prediction.
#' @param beta The weight of precision in the F-score calculation.
#' @return F-score value.
#' @export
getFscore <- function(gt, pred, threshold = 0.5, beta = 0.5) {

  # Convert predictions to binary
  pred <- as.numeric(pred > threshold)

  # Calculate true positives, false positives, and false negatives
  tp <- sum(gt == 1 & pred == 1)
  fp <- sum(gt == 0 & pred == 1)
  fn <- sum(gt == 1 & pred == 0)

  # Calculate precision and recall
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)

  # Calculate F-score
  f_score <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

  return(f_score)
}
