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

# Codex 2C review fold (RECOMMENDED 5, TICKET-063): `testthat::capture_warnings()`
# discards condition class (it returns `get_messages()` of the captured
# stack, character only), so text-based `grepl("not positive definite",
# ...)` assertions can't distinguish the `beezdemand_hessian_not_pd_warning`
# class from any other warning that happens to contain that phrase. This
# collects the actual condition objects so tests can assert on class.
.capture_warning_conditions <- function(expr) {
  conds <- list()
  withCallingHandlers(
    expr,
    warning = function(w) {
      conds[[length(conds) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  conds
}

.n_hessian_pd_warnings <- function(conds) {
  sum(vapply(conds, inherits, logical(1), "beezdemand_hessian_not_pd_warning"))
}
