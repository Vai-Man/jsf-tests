#' Plotting functions for JSF simulation results
#'
#' Functions to visualize trajectories, error heatmaps, and error comparisons
#' from JSF and SSA simulations.
#'
#' @keywords internal

library(ggplot2)

#' Custom theme for all plots
#' @keywords internal
theme_jsf <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(colour = "grey40"),
      legend.position  = "top",
      panel.grid.minor = element_blank()
    )
}

# Plot 1: SSA vs JSF trajectories + means
#' Plot SSA and JSF trajectories with their ensemble means
#'
#' @param ssa_raw  Raw SSA data.frame (time, population, rep).
#' @param jsf_raw  Raw JSF data.frame (time, population, rep).
#' @param comp     Comparison data.frame from [compute_comparison()].
#' @param lambda   Birth rate (for labelling).
#' @param mu       Death rate (for labelling).
#' @param x0       Initial population (for labelling).
#'
#' @return A ggplot object.
#' @export
plot_trajectories <- function(ssa_raw, jsf_raw, comp,
                              lambda, mu, x0) {
  ggplot() +
    geom_step(
      data = ssa_raw,
      aes(x = time, y = population, group = rep),
      colour = "#abd9e9", alpha = 0.15, linewidth = 0.25
    ) +
    geom_step(
      data = jsf_raw,
      aes(x = time, y = population, group = rep),
      colour = "#fdae61", alpha = 0.15, linewidth = 0.25
    ) +
    geom_line(
      data = comp,
      aes(x = time, y = mean_ssa, colour = "Gillespie SSA mean"),
      linewidth = 1.1
    ) +
    geom_line(
      data = comp,
      aes(x = time, y = mean_jsf, colour = "JSF mean"),
      linewidth = 1.1
    ) +
    stat_function(
      fun = function(t) x0 * exp((lambda - mu) * t),
      aes(colour = "Mean-field  E[X(t)]"),
      linewidth = 0.9, linetype = "dashed"
    ) +
    scale_colour_manual(
      values = c(
        "Gillespie SSA mean" = "#2c7bb6",
        "JSF mean"           = "#d7191c",
        "Mean-field  E[X(t)]" = "black"
      )
    ) +
    labs(
      title    = "Birth-Death Process: SSA vs JSF Trajectories",
      subtitle = bquote(lambda == .(lambda) ~ ", " ~
                          mu == .(mu) ~ ", " ~
                          X[0] == .(x0)),
      x = "Time", y = "Population", colour = NULL
    ) +
    theme_jsf()
}


# Plot 2: Absolute error over time
#' Plot absolute error between SSA and JSF means over time
#'
#' @param comp Comparison data.frame.
#' @return A ggplot object.
#' @export
plot_error_vs_time <- function(comp) {
  ggplot(comp, aes(x = time, y = abs_error)) +
    geom_area(fill = "#fc8d59", alpha = 0.35) +
    geom_line(colour = "#d73027", linewidth = 0.7) +
    labs(
      title = "Absolute Error: |mean(JSF) − mean(SSA)| over Time",
      x     = "Time",
      y     = "Absolute error"
    ) +
    theme_jsf()
}


# Plot 3: Error heat-map over parameter space
#' Sweep (λ, μ) and plot RMSE heat-map
#'
#' @param lambda_seq Numeric vector of λ values.
#' @param mu_seq     Numeric vector of μ values.
#' @param x0         Initial population.
#' @param t_max      Simulation time.
#' @param n_reps     Replicates per parameter pair.
#' @param grid_dt    Grid spacing for interpolation.
#' @param seed       Base seed.
#'
#' @return A ggplot object (tile plot).
#' @export
plot_error_heatmap <- function(lambda_seq = seq(0.2, 2.0, by = 0.3),
                               mu_seq     = seq(0.2, 2.0, by = 0.3),
                               x0         = 10L,
                               t_max      = 4.0,
                               n_reps     = 30L,
                               grid_dt    = 0.1,
                               seed       = 100L) {
  params <- expand.grid(lambda = lambda_seq, mu = mu_seq)
  params$rmse <- NA_real_

  for (i in seq_len(nrow(params))) {
    lam <- params$lambda[i]
    m   <- params$mu[i]

    set.seed(seed)
    ssa_raw  <- simulate_birth_death_ensemble(lam, m, x0, t_max,
                                              n_reps = n_reps,
                                              seed   = seed)
    jsf_raw  <- generate_jsf_reference(lam, m, x0, t_max,
                                        n_reps = n_reps,
                                        seed   = seed)

    ssa_grid <- interpolate_ensemble(ssa_raw, grid_dt = grid_dt)
    jsf_grid <- interpolate_ensemble(jsf_raw, grid_dt = grid_dt)
    comp     <- compute_comparison(ssa_grid, jsf_grid)

    params$rmse[i] <- sqrt(mean(comp$mean_diff^2, na.rm = TRUE))
  }

  ggplot(params, aes(x = lambda, y = mu, fill = rmse)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    scale_fill_viridis_c(option = "inferno", direction = -1,
                         name = "RMSE") +
    labs(
      title = "RMSE(SSA vs JSF) across Birth/Death Rate Space",
      x     = expression(lambda ~ "(birth rate)"),
      y     = expression(mu ~ "(death rate)")
    ) +
    coord_equal() +
    theme_jsf()
}
