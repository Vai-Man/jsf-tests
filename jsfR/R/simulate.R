#' Run a Jump-Switch-Flow simulation
#' 
#' @param x0 Initial compartment values.
#' @param rates Function `function(x, t)` returning reaction rates.
#' @param stoich List containing stoichiometry components (`nu`, `DoDisc`, `nuReactant`, `nuProduct`).
#' @param t_max Maximum simulation time.
#' @param method Simulation method: "operator-splitting" or "exact".
#' @param dt Time step for integration.
#' @param switching_threshold Thresholds for switching regimes.
#' @param enforce_do Lock compartments in initial regime (0/1).
#' @param seed Optional RNG seed.
#'
#' @return List with `compartments` and `times`.
#'
#' @export
#' @importFrom reticulate import
simulate_jsf <- function(x0,
                         rates,
                         stoich,
                         t_max,
                         method              = "operator-splitting",
                         dt                  = 0.01,
                         switching_threshold = rep(10L, length(x0)),
                         enforce_do          = rep(0L,  length(x0)),
                         seed                = NULL) {
  stopifnot(
    is.numeric(x0),       length(x0) >= 1,
    is.function(rates),
    is.list(stoich),
    is.numeric(t_max),    t_max > 0,
    is.character(method),
    method %in% c("operator-splitting", "exact"),
    is.numeric(dt),       dt > 0,
    length(switching_threshold) == length(x0),
    length(enforce_do)          == length(x0)
  )

  ensure_jsf()

  if (!is.null(seed)) {
    .jsf_env$random$seed(as.integer(seed))
  }

  py_x0 <- as.list(as.integer(x0))

  py_rates <- function(x, t) {
    as.list(rates(x, t))
  }

  py_stoich <- list(
    nu         = lapply(stoich$nu,         function(r) as.list(as.integer(r))),
    DoDisc     = as.list(as.integer(stoich$DoDisc)),
    nuReactant = lapply(stoich$nuReactant, function(r) as.list(as.integer(r))),
    nuProduct  = lapply(stoich$nuProduct,  function(r) as.list(as.integer(r)))
  )

  py_opts <- list(
    EnforceDo          = as.list(as.integer(enforce_do)),
    dt                 = dt,
    SwitchingThreshold = as.list(as.integer(switching_threshold))
  )

  result <- .jsf_env$jsf$jsf(
    py_x0, py_rates, py_stoich, t_max,
    config = py_opts, method = method
  )

  compartments <- lapply(result[[1]], function(c) unlist(c))
  times        <- unlist(result[[2]])

  list(compartments = compartments, times = times)
}


#' Simulate a birth-death process using JSF
#'
#' Convenience wrapper over [simulate_jsf()].
#'
#' @param lambda Birth rate.
#' @param mu Death rate.
#' @param x0 Initial population.
#' @param t_max Maximum simulation time.
#' @param method Simulation method.
#' @param dt Time step.
#' @param threshold Switching threshold.
#' @param seed Optional RNG seed.
#'
#' @return Same as [simulate_jsf()].
#'
#' @export
simulate_birth_death_jsf <- function(lambda, mu, x0,
                                      t_max,
                                      method    = "operator-splitting",
                                      dt        = 0.01,
                                      threshold = 10L,
                                      seed      = NULL) {
  stopifnot(
    is.numeric(lambda), lambda >= 0,
    is.numeric(mu),     mu >= 0,
    is.numeric(x0),     x0 >= 0,
    is.numeric(t_max),  t_max > 0
  )

  rates_fn <- function(x, t) {
    c(lambda * x[[1]], mu * x[[1]])
  }

  stoich <- list(
    nu         = list(c(1L), c(-1L)),
    DoDisc     = c(1L),
    nuReactant = list(c(1L), c(1L)),
    nuProduct  = list(c(2L), c(0L))
  )

  simulate_jsf(
    x0                  = as.integer(x0),
    rates               = rates_fn,
    stoich              = stoich,
    t_max               = t_max,
    method              = method,
    dt                  = dt,
    switching_threshold = as.integer(threshold),
    enforce_do          = 0L,
    seed                = seed
  )
}