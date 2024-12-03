.onLoad <- function(libname, pkgname) {
  print(".onLoad is called")
  config_load() # Call the config_load function when the package is loaded
}
