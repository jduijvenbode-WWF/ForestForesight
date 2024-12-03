.onLoad <- function(libname, pkgname) {
  print(".onLoad is called")
  # remove Warning: replacing previous import 'smoothr::densify' by 'terra::densify'
  if (requireNamespace("conflicted", quietly = TRUE)) {
    conflicted::conflicts_prefer(terra::densify)
    suppressWarnings({
      conflicted::conflict_scout()  # This can help identify other potential conflicts
    })
  } else {
    message("Package 'conflicted' is not installed. Preferred conflict for 'terra::densify' not set.")
  }
  config_load() # Call the config_load function when the package is loaded
}
