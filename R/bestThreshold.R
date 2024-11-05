#' Find Best Threshold
#'
#' This function finds the best threshold for a given prediction function by maximizing the evaluation function.
#'
#' @param prediction A vector of predictions (numeric).
#' @param groundtruth A vector of ground truth values (binary).
#' @param optimize_function The evaluation function to optimize. Default is getFscore.
#' @param a Initial guess for the lower bound of threshold search.
#' @param b Initial guess for the upper bound of threshold search.
#' @param tol Tolerance for convergence.
#' @param maxiter Maximum number of iterations.
#' @param beta The weight of precision in the F-score calculation.
#' @return A list containing the best threshold and the corresponding F-score.
#' @examples
#' best_threshold(c(0.2, 0.6, 0.7), c(0, 1, 1))
#' @export
best_threshold <- function(prediction, groundtruth, optimize_function = getFscore,
                           a = 0.45, b = 0.55, tol = 0.001, maxiter = 100, beta = 0.5) {
  # Golden ratio
  phi <- (1 + sqrt(5)) / 2

  # Calculate step sizes
  inv_phi <- 1 / phi

  # Initialize variables
  x1 <- b - inv_phi * (b - a)
  x2 <- a + inv_phi * (b - a)
  f1 <- optimize_function(groundtruth, prediction, x1, beta)
  f2 <- optimize_function(groundtruth, prediction, x2, beta)

  # Iteration loop
  for (i in 1:maxiter) {
    if (f1 > f2) {
      b <- x2
      x2 <- x1
      f2 <- f1
      x1 <- b - inv_phi * (b - a)
      f1 <- optimize_function(groundtruth, prediction, x1, beta)
    } else {
      a <- x1
      x1 <- x2
      f1 <- f2
      x2 <- a + inv_phi * (b - a)
      f2 <- optimize_function(groundtruth, prediction, x2, beta)
    }

    # Check for convergence
    if (abs(b - a) < tol) break
  }

  # Return the best threshold and the corresponding F-score
  return(list(bestThreshold = (a + b) / 2, maxFscore = optimize_function(groundtruth, prediction, ((a + b) / 2), beta)))
}
