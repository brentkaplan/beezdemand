# Shared zben expenditure-curve ground-truth helper for the GH #19
# domain-expansion tests (test-pmax-omax-engine.R, test-fit_demand_tmb.R,
# test-boot-demand.R).
#
# testthat auto-sources `helper-*.R` files before running any test, so this
# avoids the antipattern of test files sourcing each other / duplicating the
# same numerical-robustness logic three times.

# The natural-scale expenditure function for zben: back-transforms the
# LL4-scale exponential decay via ll4_inv() (see .tmb_predict_equation()'s
# "zben" branch and src/MixedDemand.h eqn_type == 3 for the underlying
# formula this mirrors).
.zben_expenditure_natural <- function(q0, alpha) {
  q0_log10 <- max(log10(q0), 1e-3)
  rate <- (alpha / q0_log10) * q0
  function(p) p * ll4_inv(q0_log10 * exp(-rate * p))
}

# Independent (no package Pmax/Omax helper involved) ground-truth finder for
# the zben expenditure curve. A single stats::optimize() call over one wide
# fixed interval is NOT safe for this curve shape: golden-section search can
# converge to a spurious point -- verified directly: an interval that is
# too wide can return a LOWER local optimum than a narrower,
# correctly-bracketing interval, or collapse to the right edge with an
# objective of ~0. This instead scans a log-spaced grid to locate the
# peak's neighborhood (robust: evaluates the function directly rather than
# trusting an optimizer's search heuristic over a huge bracket), then
# refines with optimize() on a narrow bracket around the grid maximum
# (well-conditioned for this curve).
.zben_truth <- function(q0, alpha, upper = 1e6, n_grid = 3000) {
  E <- .zben_expenditure_natural(q0, alpha)
  grid <- exp(seq(log(1e-3), log(upper), length.out = n_grid))
  vals <- vapply(grid, E, numeric(1))
  vals[!is.finite(vals)] <- -Inf
  best_idx <- which.max(vals)
  lo <- if (best_idx > 1) grid[best_idx - 1] else grid[best_idx] / 2
  hi <- if (best_idx < length(grid)) grid[best_idx + 1] else grid[best_idx] * 2
  stats::optimize(E, interval = c(lo, hi), maximum = TRUE,
                  tol = .Machine$double.eps^0.5)
}
