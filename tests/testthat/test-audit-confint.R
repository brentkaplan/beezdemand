# tests/testthat/test-audit-confint.R
#
# Pre-CRAN audit: non-circular verification that every Wald confint() method
# implements the textbook interval
#     estimate +/- qnorm((1 + level) / 2) * SE
# on the internal estimation scale, back-transforms the natural scale by exp(),
# and maps `level` to the two-sided normal quantile. The external invariant is
# the Wald formula; the point/SE inputs are pulled from an independent accessor
# (the fit's own results/coefficient/SE slots), not from confint() itself.

.z_at <- function(level) stats::qnorm((1 + level) / 2)

test_that("confint.beezdemand_fixed equals estimate +/- z*SE (per subject)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]
  fit <- suppressMessages(fit_demand_fixed(apt_small, equation = "hs", k = 2))

  z <- .z_at(0.95)
  ci <- confint(fit, level = 0.95)
  q0 <- ci[ci$term == "Q0", ]

  res <- fit$results
  ord <- match(q0$id, as.character(res$id))
  est <- res$Q0d[ord]
  se <- res$Q0se[ord]
  keep <- is.finite(est) & is.finite(se)
  expect_gt(sum(keep), 0)

  expect_equal(q0$estimate[keep], est[keep], tolerance = 1e-10)
  expect_equal(q0$conf.low[keep], (est - z * se)[keep], tolerance = 1e-10)
  expect_equal(q0$conf.high[keep], (est + z * se)[keep], tolerance = 1e-10)
  # CI is symmetric about the point estimate
  expect_equal(((q0$conf.low + q0$conf.high) / 2)[keep], est[keep],
               tolerance = 1e-10)
})

test_that("confint level maps to the two-sided qnorm((1+level)/2)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]
  fit <- suppressMessages(fit_demand_fixed(apt_small, equation = "hs", k = 2))

  q95 <- (function(d) d[d$term == "Q0", ])(confint(fit, level = 0.95))
  q90 <- (function(d) d[d$term == "Q0", ])(confint(fit, level = 0.90))
  w95 <- q95$conf.high - q95$conf.low
  w90 <- q90$conf.high - q90$conf.low
  keep <- is.finite(w95) & w95 > 0
  expect_gt(sum(keep), 0)
  # Width ratio is exactly the z ratio, independent of the (unknown) SE values.
  expect_equal(
    (w90 / w95)[keep],
    rep(.z_at(0.90) / .z_at(0.95), sum(keep)),
    tolerance = 1e-8
  )
})

test_that("confint.beezdemand_hurdle is Wald on internal scale, exp() on natural", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:6], ]
  fit <- fit_demand_hurdle(
    apt_small, y_var = "y", x_var = "x", id_var = "id", verbose = 0
  )

  z <- .z_at(0.95)
  co <- unname(fit$model$coefficients)
  se <- unname(fit$model$se)
  nms <- names(fit$model$coefficients)

  ci <- confint(fit, level = 0.95) # internal default; rows in coef order
  expect_equal(ci$estimate, co, tolerance = 1e-10)
  expect_equal(ci$conf.low, co - z * se, tolerance = 1e-10)
  expect_equal(ci$conf.high, co + z * se, tolerance = 1e-10)

  # Natural scale exponentiates exactly the log-scale consumption parameters.
  logp <- nms %in% c("log_q0", "log_alpha", "log_k")
  ci_nat <- confint(fit, level = 0.95, report_space = "natural")
  exp_est <- co
  exp_est[logp] <- exp(co[logp])
  exp_lo <- co - z * se
  exp_lo[logp] <- exp(exp_lo[logp])
  exp_hi <- co + z * se
  exp_hi[logp] <- exp(exp_hi[logp])
  expect_equal(ci_nat$estimate, exp_est, tolerance = 1e-8)
  expect_equal(ci_nat$conf.low, exp_lo, tolerance = 1e-8)
  expect_equal(ci_nat$conf.high, exp_hi, tolerance = 1e-8)
})

test_that("confint.beezdemand_tmb default is Wald = estimate +/- z*SE", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:6], ]
  fit <- fit_demand_tmb(
    apt_small, y_var = "y", x_var = "x", id_var = "id",
    equation = "simplified", random_effects = "q0",
    multi_start = FALSE, verbose = 0
  )

  z <- .z_at(0.95)
  co <- unname(fit$model$coefficients)
  se <- unname(fit$model$se)
  nms <- names(fit$model$coefficients)

  ci <- confint(fit, level = 0.95)
  # Default method is "wald": identical to the explicit request.
  expect_equal(ci, confint(fit, level = 0.95, method = "wald"))
  expect_equal(ci$estimate, co, tolerance = 1e-10)
  expect_equal(ci$conf.low, co - z * se, tolerance = 1e-10)
  expect_equal(ci$conf.high, co + z * se, tolerance = 1e-10)

  # Natural scale exponentiates beta_q0 / beta_alpha (no k in simplified).
  logp <- nms %in% c("beta_q0", "beta_alpha", "log_k")
  ci_nat <- confint(fit, level = 0.95, report_space = "natural")
  exp_est <- co
  exp_est[logp] <- exp(co[logp])
  expect_equal(ci_nat$estimate, exp_est, tolerance = 1e-8)
})
