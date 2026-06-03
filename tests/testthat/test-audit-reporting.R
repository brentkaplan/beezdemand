# v0.3.0 audit: verify the reported test statistic / p-value formulas directly
# (complements test-report-space-test-invariance.R, which checks invariance
# across report_space but not the underlying z-test arithmetic).

test_that("TMB summary p-value is the two-sided z-test 2*pnorm(-|stat|)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, equation = "exponential", random_effects = "q0",
    multi_start = FALSE, verbose = 0
  )
  s <- summary(fit, report_space = "internal")$coefficients
  fixed <- s[s$component == "fixed", ]
  expect_true(nrow(fixed) > 0)
  # TMB uses the Laplace/asymptotic normal approximation -> z, not t.
  expect_equal(
    fixed$statistic, fixed$estimate / fixed$std.error,
    tolerance = 1e-6
  )
  expect_equal(
    fixed$p.value, 2 * pnorm(-abs(fixed$statistic)),
    tolerance = 1e-9
  )
})
