.jsf_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
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