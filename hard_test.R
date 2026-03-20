# ======================================================================
# hard_test.R
# Script to run the Gillespie SSA vs JSF Comparison Test
# ======================================================================
# Note: This heavily relies on the stochastic capabilities and error
# metrics now natively embedded in the `jsfR` package.
# ======================================================================

# Load the jsfR package
library(jsfR)
library(ggplot2)

LAMBDA  <- 1.0       # birth rate
MU      <- 0.5       # death rate
X0      <- 5L        # initial population
T_MAX   <- 6.0       # simulation horizon
N_REPS  <- 100L      # number of replicate trajectories
GRID_DT <- 0.05      # interpolation grid spacing
SEED    <- 2026L      # master seed

OUT_DIR <- "output"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("\n[1/5] Running Gillespie SSA ensemble natively in R...\n")
set.seed(SEED)
ssa_raw <- simulate_birth_death_ensemble(
  lambda = LAMBDA, mu = MU, x0 = X0,
  t_max  = T_MAX,   n_reps = N_REPS, seed = SEED
)
cat(sprintf("  → %d trajectories, %d total events\n",
            N_REPS, nrow(ssa_raw)))

cat("\n[2/5] Obtaining JSF reference data (via Python/reticulate)...\n")
jsf_raw <- generate_jsf_reference(
  lambda     = LAMBDA, mu = MU, x0 = X0,
  t_max      = T_MAX,
  n_reps     = N_REPS,
  dt         = 0.01,
  threshold  = 10L,
  seed       = SEED,
  cache_file = file.path(OUT_DIR, "jsf_reference.rds")
)
cat(sprintf("  → %d trajectories, %d total data points\n",
            N_REPS, nrow(jsf_raw)))

cat("\n[3/5] Interpolating onto regular grid...\n")
ssa_grid <- interpolate_ensemble(ssa_raw, grid_dt = GRID_DT)
jsf_grid <- interpolate_ensemble(jsf_raw, grid_dt = GRID_DT)

cat("\n[4/5] Computing comparison metrics...\n")
comp <- compute_comparison(ssa_grid, jsf_grid)
print_comparison_summary(comp)

write.csv(comp, file.path(OUT_DIR, "comparison_metrics.csv"),
          row.names = FALSE)

cat("\n[5/5] Generating plots...\n")

p1 <- plot_trajectories(ssa_raw, jsf_raw, comp,
                        lambda = LAMBDA, mu = MU, x0 = X0)
ggsave(file.path(OUT_DIR, "01_trajectories.png"), p1,
       width = 10, height = 6, dpi = 300)
cat("  → Saved 01_trajectories.png\n")

p2 <- plot_error_vs_time(comp)
ggsave(file.path(OUT_DIR, "02_error_vs_time.png"), p2,
       width = 9, height = 5, dpi = 300)
cat("  → Saved 02_error_vs_time.png\n")

cat("  → Computing heat-map (this may take a minute)...\n")
p3 <- plot_error_heatmap(
  lambda_seq = seq(0.4, 2.0, by = 0.4),
  mu_seq     = seq(0.2, 1.8, by = 0.4),
  x0         = X0,
  t_max      = 4.0,
  n_reps     = 30L,
  grid_dt    = 0.1,
  seed       = SEED
)
ggsave(file.path(OUT_DIR, "03_error_heatmap.png"), p3,
       width = 8, height = 7, dpi = 300)
cat("  → Saved 03_error_heatmap.png\n")

cat("\n✓ All done. Output in:", normalizePath(OUT_DIR), "\n")
