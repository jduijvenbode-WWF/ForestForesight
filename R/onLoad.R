.onLoad <- function(libname, pkgname) {
  print(".onLoad is called")
  # remove Warning: replacing previous import 'smoothr::densify' by 'terra::densify'
  conflicted::conflicts_prefer(terra::densify) 
  config_load() # Call the config_load function when the package is loaded
}
