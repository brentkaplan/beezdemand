# Shared weak-fit fixture for the hessian_pd inference-gate tests
# (TICKET-063: test-tmb-broom-contracts.R, test-tmb-parametric-draws.R,
# test_emms_comparisons.R, test-boot-demand.R, test-hurdle_methods.R).
#
# testthat auto-sources `helper-*.R` files before running any test, so this
# avoids test files sourcing each other. Deterministic weak-fit recipe from
# TICKET-046 / TICKET-063: 2 subjects x 4 prices, degenerate consumption
# curve fit with 2 random effects -- produces a non-PD Hessian on most
# platforms. Whether the pathology materializes depends on BLAS/compiler, so
# callers must still guard classed-warning assertions with
# `skip_if(!isFALSE(fit$hessian_pd), ...)`.

.weak_pd_tmb_fit <- function() {
  d <- expand.grid(id = factor(1:2), x = c(0.1, 1, 5, 20))
  d$y <- c(10, 9, 0.5, 0.1, 10.2, 8.8, 0.6, 0.05)
  suppressWarnings(fit_demand_tmb(
    d,
    equation = "exponential",
    random_effects = c("q0", "alpha"),
    verbose = 0
  ))
}
