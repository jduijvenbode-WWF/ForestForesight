% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_raster.R
\name{get_raster}
\alias{get_raster}
\title{Retrieve raster files based on specified criteria.}
\usage{
get_raster(datafolder, date, feature, tile)
}
\arguments{
\item{datafolder}{A character string specifying the path to the data folder where raster files are stored.}

\item{date}{A Date object representing the date for which raster files are to be retrieved.}

\item{feature}{A character string specifying the feature of interest to filter raster files.}

\item{tile}{A character string specifying the pattern to filter specific tiles of raster files.}
}
\value{
A character vector containing the file paths of the selected raster files.
}
\description{
This function retrieves raster files from a specified data folder based on the given date, feature, and tile pattern.
}
\examples{
datafolder <- "/path/to/data"
date <- as.Date("2022-01-01")
feature <- "initialforestcover"
tile <- "00N_070W"
get_raster(datafolder, date, feature, tile)
# Returns: Vector of file paths to selected raster files.
}
