#' Simulate a single birth-death trajectory via Gillespie SSA
#' 
#' @param lambda Birth rate (>= 0).
#' @param mu     Death rate (>= 0).
#' @param x0     Initial population (integer >= 0).
#' @param t_max  Maximum simulation time.
#'
#' @return A data.frame with columns `time` and `population`.
#'
#' @details
#' Edge cases handled:
#' * If `x0 == 0`, the population remains at 0.
#' * If `lambda == 0`, only deaths can occur.
#' * If `mu == 0`, only births can occur.
#' * The simulation stops when `time >= t_max` or `population == 0`.
#'
#' @export
simulate_birth_death <- function(lambda, mu, x0, t_max) {
stopifnot(
    is.numeric(lambda), length(lambda) == 1, lambda >= 0,
    is.numeric(mu),     length(mu) == 1,     mu >= 0,
    is.numeric(x0),     length(x0) == 1,     x0 >= 0, x0 == floor(x0),
    is.numeric(t_max),  length(t_max) == 1,  t_max > 0
  )

  capacity <- max(1024L, as.integer(10 * (lambda + mu) * x0 * t_max))
  times <- numeric(capacity)
  pops  <- numeric(capacity)
  times[1] <- 0.0
  pops[1]  <- x0
  n <- 1L

  t_now <- 0.0
  X     <- as.integer(x0)

  while (t_now < t_max && X > 0L) {
    total_rate <- (lambda + mu) * X

    if (total_rate <= 0) break

    dt <- rexp(1, rate = total_rate)
    t_now <- t_now + dt

    if (t_now > t_max) break

    if (runif(1) < (lambda * X) / total_rate) {
      X <- X + 1L
    } else {
      X <- X - 1L
    }

    n <- n + 1L
    if (n > capacity) {
      capacity <- capacity * 2L
      length(times) <- capacity
      length(pops)  <- capacity
    }
    times[n] <- t_now
    pops[n]  <- X
  }

  data.frame(
    time       = times[seq_len(n)],
    population = pops[seq_len(n)]
  )
}

#' Run multiple birth-death SSA trajectories
#'
#' @param lambda Birth rate.
#' @param mu     Death rate.
#' @param x0     Initial population.
#' @param t_max  Maximum time.
#' @param n_reps Number of replicates.
#' @param seed   Optional RNG seed for reproducibility.
#'
#' @return A data.frame with columns `time`, `population`, `rep`.
#'
#' @export
simulate_birth_death_ensemble <- function(lambda, mu, x0, t_max,
                                          n_reps = 100L,
                                          seed   = NULL) {
  if (!is.null(seed)) set.seed(seed)

  dfs <- lapply(seq_len(n_reps), function(i) {
    df     <- simulate_birth_death(lambda, mu, x0, t_max)
    df$rep <- i
    df
  })

  do.call(rbind, dfs)
}

#' Interpolate trajectories onto a regular time grid
#'
#' Uses step-function interpolation (last observation carried forward).
#'
#' @param df       Data.frame from [simulate_birth_death_ensemble()].
#' @param grid_dt  Grid spacing (default 0.05).
#'
#' @return A data.frame with columns `time`, `rep`, `population`.
#'
#' @export
interpolate_ensemble <- function(df, grid_dt = 0.05) {
  t_max  <- max(df$time)
  grid   <- seq(0, t_max, by = grid_dt)
  reps   <- sort(unique(df$rep))

  out <- data.frame(
    time       = rep(grid, times = length(reps)),
    rep        = rep(reps, each  = length(grid)),
    population = NA_real_
  )

  for (r in reps) {
    sub  <- df[df$rep == r, ]
    vals <- stats::approx(sub$time, sub$population,
                          xout = grid, method = "constant",
                          rule = 2)$y
    out$population[out$rep == r] <- vals
  }

  out
}
