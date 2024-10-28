#' Check Python and Package Dependencies
#'
#' This function checks the versions of specified Python packages and the Python version.
#'
#' @param packages A character vector specifying the Python packages to check.
#' @return A list containing the Python version, package versions, and a logical value indicating whether all packages were found.
#'
#' @examples
#' \dontrun{
#' result <- package_dependencies(c("numpy", "scipy", "rasterio", "argparse"))
#' print(result)
#' }
#'
#' @export
ff_pythoncheck <- function(packages = c("numpy", "scipy", "rasterio", "argparse")) {
  version <- system("python -V", intern = T)
  packageresult <- c()
  for (package in packages) {
    res <- suppressWarnings(system(paste0('python -c "import ', package, ";print(", package, '.__version__)"'), intern = T))
    if (length(grep("ModuleNotFoundError", res)) > 0) {
      res <- "not found"
    }
    packageresult <- c(packageresult, res)
  }
  return(list("python-version" = version, "package-versions" = data.frame("packages" = packages, "versions" = packageresult), "passed" = !any(packageresult == "not found")))
}
