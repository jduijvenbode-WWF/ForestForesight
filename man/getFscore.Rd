% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFscore.R
\name{getFscore}
\alias{getFscore}
\title{Calculate F-score}
\usage{
getFscore(gt, pred, threshold = 0.5, beta = 0.5, pr = F)
}
\arguments{
\item{gt}{The ground truth vector.}

\item{pred}{The predicted vector.}

\item{threshold}{The threshold for prediction.}

\item{beta}{The weight of precision in the F-score calculation.}

\item{pr}{Logical. whether it should also return the precision and recall}
}
\value{
F-score value.
}
\description{
This function calculates the F-score given the ground truth, threshold and predicted values.
}
