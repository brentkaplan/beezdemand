# Shared weak-fit fixture for the NLME convergence-gate tests (TICKET-064).
#
# testthat auto-sources `helper-*.R` files before running any test. Recipe
# from TICKET-064: fit a small, well-behaved NLME model, then inject the
# documented nlme apVar-failure sentinel (a character string in place of the
# numeric approximate-variance matrix) -- exactly what nlme stores when that
# Hessian fails to invert. This is deterministic (no platform dependence)
# because the injected state, not organic non-convergence, drives the test.

.weak_conv_nlme_fit <- function() {
  set.seed(11)
  d <- expand.grid(id = factor(1:6), x = c(0.1, 0.5, 1, 2.5, 5, 10, 20))
  q0i <- 10 * exp(stats::rnorm(6, 0, 0.4))
  ali <- 0.01 * exp(stats::rnorm(6, 0, 0.6))
  d$y <- pmax(0, q0i[d$id] * exp(-ali[d$id] * q0i[d$id] * d$x) + stats::rnorm(nrow(d), 0, 2.5))
  d$y_ll4 <- ll4(d$y, lambda = 4)
  fit <- fit_demand_mixed(
    d, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )
  fit$model$apVar <- "Non-positive definite approximate variance-covariance"
  fit
}
