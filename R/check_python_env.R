check_python_env <- function(pkgname = "forestforesight") {
  required_packages <- c("numpy", "pandas", "mlflow", "torch")

  # Set environment path
  if (.Platform$OS.type == "windows") {
    venv_root <- file.path(Sys.getenv("APPDATA"), "forestforesight", "pythonenv")
  } else {
    venv_root <- file.path(Sys.getenv("HOME"), ".forestforesight", "pythonenv")
  }

  # Create venv directory if it doesn't exist
  dir.create(venv_root, recursive = TRUE, showWarnings = FALSE)

  # Check if environment exists
  venv_name <- paste0(pkgname, "-env")
  env_missing <- !reticulate::virtualenv_exists(venv_name)

  if (env_missing) {
    message("Python environment '", venv_name, "' not found.")
    response <- readline(prompt = "Would you like to create it? (y/n): ")

    if (tolower(response) == "y") {
      message("\nCreating Python environment...")
      tryCatch({
        reticulate::virtualenv_create(venv_name)
        message("Installing required packages...")
        reticulate::virtualenv_install(venv_name, packages = required_packages)
      }, error = function(e) {
        message("Error creating environment: ", e$message)
        return(FALSE)
      })
    } else {
      message("Environment creation cancelled.")
      return(FALSE)
    }
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
      message("\nMissing packages detected: ", paste(missing_pkgs, collapse = ", "))
      response <- readline(prompt = "Would you like to install them? (y/n): ")

      if (tolower(response) == "y") {
        message("Installing missing packages...")
        reticulate::virtualenv_install(venv_name, packages = missing_pkgs)
        message("Packages installed successfully!")
        return(TRUE)
      } else {
        message("Package installation cancelled.")
        return(FALSE)
      }
    }

    message("All required packages are installed!")
    return(TRUE)

  }, error = function(e) {
    message("Error checking Python environment: ", e$message)
    return(FALSE)
  })
}
