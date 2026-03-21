.jsf_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Set up project-specific virtualenv
  venv_path <- file.path(getwd(), ".venv")
  
  # Auto-create virtualenv if it doesn't exist
  if (!dir.exists(venv_path)) {
    message("Creating project-specific virtualenv at: ", venv_path)
    tryCatch(
      reticulate::virtualenv_create(venv_path),
      error = function(e) {
        warning("Failed to create virtualenv: ", e$message, "\n",
                "Falling back to default Python environment.")
      }
    )
  }
  
  # Use the project virtualenv
  if (dir.exists(venv_path)) {
    reticulate::use_virtualenv(venv_path, required = FALSE)
  }
  
  .jsf_env$jsf <- NULL
  .jsf_env$random <- NULL
}

#' Ensure the Python jsf module is available. Lazily imports and caches the module.
#' @return 
#' @keywords
ensure_jsf <- function() {
  if (is.null(.jsf_env$jsf)) {
    if (!reticulate::py_module_available("jsf")) {
      stop(
        "Python module 'jsf' is not installed.\n",
        "Run jsfR::install_jsf() to install it.",
        call. = FALSE
      )
    }

    .jsf_env$jsf    <- reticulate::import("jsf",    delay_load = TRUE)
    .jsf_env$random <- reticulate::import("random", delay_load = TRUE)
  }

  invisible(.jsf_env$jsf)
}