#' S7 class system for jsfR simulation results
#'
#' Defines the `JSFSimResult` S7 class and its `print()` / `plot()` methods,
#' plus a `jsf_run()` convenience constructor that returns one.
#'
#' @keywords internal

#' S7 class representing a JSF simulation result
#'
#' @description
#' A structured representation of a single Jump-Switch-Flow simulation run.
#' Returned by [jsf_run()]. Supports `print()` for a tidy summary and
#' `plot()` for a ggplot2 time-series visualisation.
#'
#' @slot compartments Named list of numeric vectors, one per compartment.
#' @slot times        Numeric vector of simulation time points.
#' @slot names        Character vector of compartment label names.
#' @slot t_max        Numeric maximum simulation time.
#' @slot call_info    Named character vector recording key simulation parameters.
#'
#' @export
JSFSimResult <- S7::new_class(
  "JSFSimResult",
  properties = list(
    compartments = S7::new_property(class = S7::class_list),
    times        = S7::new_property(class = S7::class_numeric),
    names        = S7::new_property(class = S7::class_character),
    t_max        = S7::new_property(class = S7::class_numeric),
    call_info    = S7::new_property(class = S7::class_character)
  )
)

#' Run a JSF simulation and return a `JSFSimResult` S7 object
#'
#' A convenience wrapper around [simulate_jsf()] that packages the output
#' into a typed [JSFSimResult] object with print and plot methods.
#'
#' @param x0                  Initial compartment values (integer vector).
#' @param rates               Rate function `function(x, t)`.
#' @param stoich              Stoichiometry list (`nu`, `DoDisc`, `nuReactant`, `nuProduct`).
#' @param t_max               Maximum simulation time.
#' @param method              Simulation method (`"operator-splitting"` or `"exact"`).
#' @param dt                  ODE time step (default 0.01).
#' @param switching_threshold Integer vector of switching thresholds, one per compartment.
#' @param enforce_do          Integer vector (0/1), lock compartment to initial regime.
#' @param seed                Optional RNG seed (passed to Python `random.seed()`).
#' @param compartment_names   Optional character vector of compartment labels.
#'
#' @return A [JSFSimResult] S7 object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rates  <- function(x, t) c(1.0 * x[[1]], 0.5 * x[[1]])
#' stoich <- list(
#'   nu         = list(c(1L), c(-1L)),
#'   DoDisc     = c(1L),
#'   nuReactant = list(c(1L), c(1L)),
#'   nuProduct  = list(c(2L), c(0L))
#' )
#' result <- jsf_run(
#'   x0 = 10L, rates = rates, stoich = stoich,
#'   t_max = 5.0, compartment_names = "Population"
#' )
#' print(result)
#' plot(result)
#' }
jsf_run <- function(x0,
                    rates,
                    stoich,
                    t_max,
                    method              = "operator-splitting",
                    dt                  = 0.01,
                    switching_threshold = rep(10L, length(x0)),
                    enforce_do          = rep(0L,  length(x0)),
                    seed                = NULL,
                    compartment_names   = NULL) {

  if (is.null(compartment_names))
    compartment_names <- paste0("Compartment_", seq_along(x0))

  stopifnot(length(compartment_names) == length(x0))

  raw <- simulate_jsf(
    x0                  = x0,
    rates               = rates,
    stoich              = stoich,
    t_max               = t_max,
    method              = method,
    dt                  = dt,
    switching_threshold = switching_threshold,
    enforce_do          = enforce_do,
    seed                = seed
  )

  named_compartments        <- raw$compartments
  names(named_compartments) <- compartment_names

  info <- c(
    method    = method,
    dt        = as.character(dt),
    threshold = paste(switching_threshold, collapse = ", "),
    seed      = if (is.null(seed)) "none" else as.character(seed)
  )

  JSFSimResult(
    compartments = named_compartments,
    times        = raw$times,
    names        = compartment_names,
    t_max        = t_max,
    call_info    = info
  )
}

#' @exportS3Method print JSFSimResult
print.JSFSimResult <- function(x, ...) {
  cli_line <- function(...) cat(sprintf(...), "\n", sep = "")

  cli_line("  Method    : %s", x@call_info[["method"]])
  cli_line("  dt        : %s", x@call_info[["dt"]])
  cli_line("  Threshold : %s", x@call_info[["threshold"]])
  cli_line("  Seed      : %s", x@call_info[["seed"]])
  cli_line("  Time range: [%.3f, %.3f]  (%d steps)",
           min(x@times), max(x@times), length(x@times))

  for (nm in x@names) {
    vals <- x@compartments[[nm]]
    cat(sprintf("  %-20s  init=%6.1f  final=%6.1f  min=%6.1f  max=%6.1f\n",
                nm, vals[1], vals[length(vals)], min(vals), max(vals)))
  }
  invisible(x)
}

#' @exportS3Method plot JSFSimResult
plot.JSFSimResult <- function(x, ...) {
  n <- length(x@names)

  df <- do.call(rbind, lapply(seq_len(n), function(i) {
    data.frame(
      time        = x@times,
      population  = x@compartments[[i]],
      compartment = x@names[i],
      stringsAsFactors = FALSE
    )
  }))

  ggplot2::ggplot(df, ggplot2::aes(
    x = time, y = population, colour = compartment
  )) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(
      title    = "JSF Simulation",
      subtitle = sprintf("t_max = %.1f  |  method: %s", x@t_max, x@call_info[["method"]]),
      x        = "Time",
      y        = "Population",
      colour   = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "top")
}
