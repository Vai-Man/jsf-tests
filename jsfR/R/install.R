#' Install the Python jsf package
#' 
#' @param method Installation method for reticulate::py_install().
#' @param envname Target environment name (virtualenv or conda).
#' @param jsf_path Optional local path to the jsf package.
#'
#' @export
#' @importFrom reticulate py_install
install_jsf <- function(method = "auto", envname = NULL, jsf_path = NULL) {
  pkg <- if (!is.null(jsf_path)) {
    normalizePath(jsf_path, winslash = "/")
  } else {
    "git+https://github.com/DGermano8/jsf.git"
  }

  reticulate::py_install(
    packages = pkg,
    envname  = envname,
    method   = method,
    pip      = TRUE
  )

  message("jsf Python package installed successfully.")
}

#' Check if the jsf Python module is available
#'
#' @return TRUE if jsf is available, FALSE otherwise.
#'
#' @export
#' @importFrom reticulate py_module_available
jsf_available <- function() {
  reticulate::py_module_available("jsf")
}