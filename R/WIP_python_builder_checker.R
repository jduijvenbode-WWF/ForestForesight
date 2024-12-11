.onLoad <- function(libname, pkgname) {
  required_packages <- c("numpy", "pandas", "mlflow","torch")

  # Set custom virtualenv location in your AppData folder
  if (.Platform$OS.type == "windows") {
    venv_root <- file.path(Sys.getenv("APPDATA"), "forestforesight", "pythonenv")
  } else {
    venv_root <- file.path(Sys.getenv("HOME"), ".forestforesight", "pythonenv")
  }

  # Ensure directory exists
  dir.create(venv_root, recursive = TRUE, showWarnings = FALSE)

  # Tell reticulate to use this location
  Sys.setenv(RETICULATE_VIRTUALENV_ROOT = venv_root)

  venv_name <- paste0(pkgname, "-env")

  if (!reticulate::virtualenv_exists(venv_name)) {
    reticulate::virtualenv_create(venv_name)
    reticulate::virtualenv_install(venv_name, packages = required_packages)
  }

  reticulate::use_virtualenv(venv_name, required = TRUE)
}

check_python_env <- function(pkgname = "forestforesight") {
  required_packages <- c("numpy", "pandas", "mlflow", "torch")

  # Set environment path
  if (.Platform$OS.type == "windows") {
    venv_root <- file.path(Sys.getenv("APPDATA"), "forestforesight", "pythonenv")
  } else {
    venv_root <- file.path(Sys.getenv("HOME"), ".forestforesight", "pythonenv")
  }

  # Check if environment exists
  venv_name <- paste0(pkgname, "-env")
  if (!reticulate::virtualenv_exists(venv_name)) {
    message("Python environment '", venv_name, "' not found")
    return(FALSE)
  }

  # Try to use the environment
  tryCatch({
    reticulate::use_virtualenv(file.path(venv_root, venv_name), required = TRUE)

    # Use pip list to get installed packages
    py_path <- reticulate::py_config()$python
    cmd <- sprintf('"%s" -m pip list', py_path)
    installed_raw <- system(cmd, intern = TRUE)

    # Skip the header rows and parse package names
    installed_pkgs <- installed_raw[-1:-2] # Skip "Package" and "---" headers
    installed_pkgs <- sapply(strsplit(installed_pkgs, "\\s+"), `[`, 1)

    # Check each required package
    missing_pkgs <- required_packages[!tolower(required_packages) %in% tolower(installed_pkgs)]

    if (length(missing_pkgs) > 0) {
      message("Missing packages: ", paste(missing_pkgs, collapse = ", "))
      return(FALSE)
    }

    message("All required packages are installed!")
    return(TRUE)

  }, error = function(e) {
    message("Error checking Python environment: ", e$message)
    return(FALSE)
  })
}
