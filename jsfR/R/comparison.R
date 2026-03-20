#' Compute comparison metrics between SSA and JSF ensembles
#' 
#' @param ssa_grid Data.frame from [interpolate_ensemble()] (SSA).
#' @param jsf_grid Data.frame from [interpolate_ensemble()] (JSF).
#'
#' @return A data.frame with columns:
#'   `time`, `mean_ssa`, `mean_jsf`, `mean_diff`,
#'   `abs_error`, `rel_error`.
#'
#' @export
compute_comparison <- function(ssa_grid, jsf_grid) {
  ssa_agg <- stats::aggregate(population ~ time, data = ssa_grid,
                              FUN = mean, na.rm = TRUE)
  jsf_agg <- stats::aggregate(population ~ time, data = jsf_grid,
                              FUN = mean, na.rm = TRUE)

  merged <- merge(ssa_agg, jsf_agg, by = "time",
                  suffixes = c("_ssa", "_jsf"))

  merged$mean_diff  <- merged$population_jsf - merged$population_ssa
  merged$abs_error  <- abs(merged$mean_diff)
  merged$rel_error  <- ifelse(
    merged$population_ssa > 0,
    merged$abs_error / merged$population_ssa,
    NA_real_
  )

  names(merged)[names(merged) == "population_ssa"] <- "mean_ssa"
  names(merged)[names(merged) == "population_jsf"] <- "mean_jsf"

  merged
}
#' Print a summary of comparison metrics
#'
#' @param comp Data.frame from [compute_comparison()].
#' @export
print_comparison_summary <- function(comp) {
  cat("SSA vs JSF Comparison Summary:\n")
  cat(sprintf("  Time range         : [%.2f, %.2f]\n",
              min(comp$time), max(comp$time)))
  cat(sprintf("  Mean absolute error: %.4f\n",
              mean(comp$abs_error, na.rm = TRUE)))
  cat(sprintf("  Max  absolute error: %.4f\n",
              max(comp$abs_error, na.rm = TRUE)))
  cat(sprintf("  Mean relative error: %.4f\n",
              mean(comp$rel_error, na.rm = TRUE)))
  cat(sprintf("  RMSE               : %.4f\n",
              sqrt(mean(comp$mean_diff^2, na.rm = TRUE))))
}
