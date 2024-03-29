% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ff_prep.R
\name{ff_prep}
\alias{ff_prep}
\title{Prepare data for XGboost training, validating and predicting.}
\usage{
ff_prep(
  datafolder = NA,
  country = NA,
  shape = NA,
  tiles = NULL,
  groundtruth_pattern = "groundtruth6m",
  start = "2021-01-01",
  end = NA,
  inc_features = NA,
  exc_features = NA,
  fltr_features = NULL,
  fltr_condition = NULL,
  sample_size = 1,
  validation_sample = 0,
  adddate = T,
  sampleraster = T,
  verbose = F,
  shrink = "none",
  window = NA,
  label_threshold = NA,
  addxy = F
)
}
\arguments{
\item{datafolder}{Path to the data folder. Default is the system variable ff_datafolder. should contain the degrees folders}

\item{country}{Country or countries for which the data is prepared. Is optional when either shape or tiles is given. Should be the ISO3 code.}

\item{shape}{SpatVector for which the data is prepared. Is optional when either country or tiles is given.}

\item{tiles}{Vector of tiles in the syntax of e.g. 10N_080W.optional when either shape or country is given.}

\item{groundtruth_pattern}{Pattern to identify ground truth files. Default is groundtruth6mbin (groundtruth of future six months in binary format).}

\item{start}{Start date for training data in the format "YYYY-MM-DD". Default is "2021-01-01".}

\item{end}{End date for training data in the format "YYYY-MM-DD". Default is NA to only process the start month.}

\item{inc_features}{Vector of included features. States which features to include in the data preparation.}

\item{exc_features}{Vector of excluded features. States which features to exclude in the data preparation.}

\item{fltr_features}{vector of features for filtering data. Default is empty. EXAMPLE: 'initialforestcover'. needs to be combined with fltr_condition of the same length}

\item{fltr_condition}{Vector of filtering conditions. Default is empty. EXAMPLE:'>0'. Should consist of operator and value and needs to be combined with fltr_features of same length vector}

\item{sample_size}{Fraction size of the random sample. Should be bigger than 0 and smaller or equal to 1. Default is 1}

\item{validation_sample}{float between 0 and 1 that indicates how much of the training dataset should be used for validation. Default is 0. Advised is to not set it above 0.3}

\item{shrink}{Option to shrink the input area if a country was selected. Use none to keep all the data within the tile. Use crop to crop the extent, crop-deg to crop to the nearest outer degree and use extract to keep only the values that overlap with the country}

\item{window}{Set the extent on which to process. normally NA to not use this option and derive it from the data}

\item{relativedate}{Boolean indicating whether the date is relative. Default is \code{TRUE}.}
}
\value{
A prepared dataset for machine learning.
}
\description{
This function prepares data for the ForestForesight training and predicting algorithm based on specified parameters.
}
\references{
Jonas van Duijvenbode (2023)
Zillah Calle (2023)
}
\keyword{XGBoost}
\keyword{data}
\keyword{preparation}
