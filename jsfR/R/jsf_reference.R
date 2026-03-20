#' Generate JSF reference data for a birth-death process
#'
#' @param lambda Birth rate.
#' @param mu     Death rate.
#' @param x0     Initial population.
#' @param t_max  Maximum time.
#' @param n_reps Number of replicate simulations.
#' @param dt     Time step for JSF.
#' @param threshold Switching threshold.
#' @param seed   Python RNG seed.
#' @param cache_file Optional path to cache the result as RDS.
#'
#' @return A data.frame with columns `time`, `population`, `rep`.
#' @export
generate_jsf_reference <- function(lambda, mu, x0, t_max,
                                    n_reps    = 100L,
                                    dt        = 0.01,
                                    threshold = 10L,
                                    seed      = 42L,
                                    cache_file = NULL) {
  if (!is.null(cache_file) && file.exists(cache_file)) {
    message("Loading cached JSF reference from: ", cache_file)
    return(readRDS(cache_file))
  }

  use_python <- requireNamespace("reticulate", quietly = TRUE) &&
    reticulate::py_module_available("jsf")

  if (use_python) {
    message("Generating JSF reference data via Python jsf...")
    jsf_mod    <- reticulate::import("jsf")
    random_mod <- reticulate::import("random")

    rates_fn <- function(x, t) {
      list(lambda * x[[1]], mu * x[[1]])
    }

    nu_reactants <- list(list(1L), list(1L))
    nu_products  <- list(list(2L), list(0L))
    nu <- list(list(1L), list(-1L))

    stoich <- list(
      nu         = nu,
      DoDisc     = list(1L),
      nuReactant = nu_reactants,
      nuProduct  = nu_products
    )

    opts <- list(
      EnforceDo          = list(0L),
      dt                 = dt,
      SwitchingThreshold = list(as.integer(threshold))
    )

    dfs <- lapply(seq_len(n_reps), function(i) {
      random_mod$seed(as.integer(seed + i))
      sim <- jsf_mod$jsf(
        list(as.integer(x0)), rates_fn, stoich, t_max,
        config = opts, method = "operator-splitting"
      )
      data.frame(
        time       = unlist(sim[[2]]),
        population = unlist(sim[[1]][[1]]),
        rep        = i
      )
    })

    result <- do.call(rbind, dfs)

  } else {
    message("Python jsf not available. Using mean-field mock data.")
    grid <- seq(0, t_max, by = dt)
    dfs <- lapply(seq_len(n_reps), function(i) {
      set.seed(seed + i)
      mf <- x0 * exp((lambda - mu) * grid)
      noisy <- pmax(0, mf + rnorm(length(grid), 0, sqrt(pmax(mf, 1))))
      data.frame(time = grid, population = noisy, rep = i)
    })
    result <- do.call(rbind, dfs)
  }

  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(result, cache_file)
    message("Cached JSF reference to: ", cache_file)
  }

  result
}
