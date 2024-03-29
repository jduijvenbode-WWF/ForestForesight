% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pythoncheck.R
\name{ff_pythoncheck}
\alias{ff_pythoncheck}
\title{Check Python and Package Dependencies}
\usage{
ff_pythoncheck(packages = c("numpy", "scipy", "rasterio", "argparse"))
}
\arguments{
\item{packages}{A character vector specifying the Python packages to check.}
}
\value{
A list containing the Python version, package versions, and a logical value indicating whether all packages were found.
}
\description{
This function checks the versions of specified Python packages and the Python version.
}
\examples{
\dontrun{
  result <- package_dependencies(c("numpy", "scipy", "rasterio", "argparse"))
  print(result)
}

}
