# Regression tests for TICKET-006: statistical-correctness fixes for the
# NLME summary parameter-transformation path and a labelling fix on the
# hurdle summary coefficient matrix.
#
# Bug 1 (NLME), superseded by the v0.3.0 release audit: `summary`/`tidy` no
# longer recompute the Wald test after the delta-method back-transform. Under the
# broom/emmeans convention `statistic`/`p.value` are kept on the estimation scale
# (nlme's native containment-t test) for every `report_space`, while only
# `estimate`/`std.error` are back-transformed. So the natural-scale test simply
# equals nlme's native DF-aware t-test (transformation-invariant). The cross-tier
# contract lives in test-report-space-test-invariance.R.
#
# Bug 2 (hurdle): `summary.beezdemand_hurdle()` labelled the test-statistic
# column "t value" while the p-values were correctly computed via `pnorm()`
# (z-test, the right choice for TMB Laplace-approximation models). The label
# now reads "z value", consistent with glmmTMB convention.

# --- Bug 1: NLME t-test preservation -----------------------------------------

test_that("NLME summary report_space='natural' keeps the native nlme (DF-aware t) test", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  data("apt", package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_mixed(apt, y_var = "y_ll4", x_var = "x", id_var = "id",
                          equation_form = "zben")

  s_natural <- summary(fit, report_space = "natural")

  # Sanity: p-values are valid
  expect_true(all(is.numeric(s_natural$coefficients$p.value)))
  expect_true(all(s_natural$coefficients$p.value >= 0))
  expect_true(all(s_natural$coefficients$p.value <= 1))

  # Broom convention: the back-transform rescales estimate/SE but the Wald test is
  # kept on the estimation scale, so statistic/p.value equal nlme's native
  # containment-t (DF-aware) values -- NOT a recomputed natural-scale test.
  tt <- summary(fit$model)$tTable
  expect_equal(unname(s_natural$coefficients$statistic),
               unname(tt[, "t-value"]), tolerance = 1e-9)
  expect_equal(unname(s_natural$coefficients$p.value),
               unname(tt[, "p-value"]), tolerance = 1e-9)
})

test_that("NLME natural-scale p-values are >= z-test p-values (heavier t tails)", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  data("apt", package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_mixed(apt, y_var = "y_ll4", x_var = "x", id_var = "id",
                          equation_form = "zben")

  s_natural <- summary(fit, report_space = "natural")
  z_pvals <- 2 * stats::pnorm(-abs(s_natural$coefficients$statistic))

  # With finite df, pt() has heavier tails than pnorm(), so two-sided p-values
  # from pt() are always >= those from pnorm(). Non-strict comparison handles
  # very large df where they converge.
  expect_true(
    all(s_natural$coefficients$p.value + 1e-12 >= z_pvals),
    info = "t-test p-values should be >= z-test p-values."
  )
})

test_that("NLME default summary (report_space matches internal) preserves nlme native p-values", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  data("apt", package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_mixed(apt, y_var = "y_ll4", x_var = "x", id_var = "id",
                          equation_form = "zben")

  internal_space <- fit$param_space %||% fit$param_info$param_space %||% "log10"
  s_internal <- summary(fit, report_space = internal_space)

  native_pvals <- summary(fit$model)$tTable[, "p-value"]
  expect_equal(unname(s_internal$coefficients$p.value),
               unname(native_pvals),
               tolerance = 1e-12)
})

# --- Bug 2: hurdle "z value" column rename -----------------------------------

test_that("hurdle summary coef_matrix uses 'z value' column (not 't value')", {
  skip_on_cran()

  data("apt", package = "beezdemand")
  fit_h <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                             verbose = 0)

  s <- summary(fit_h)

  expect_true("z value" %in% colnames(s$coefficients_matrix))
  expect_false("t value" %in% colnames(s$coefficients_matrix))
})

test_that("hurdle summary p-values still use pnorm() (z-test is correct for TMB)", {
  skip_on_cran()

  data("apt", package = "beezdemand")
  fit_h <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                             verbose = 0)

  s <- summary(fit_h)

  # statistic/p.value are kept on the estimation scale (broom convention), so the
  # hurdle p.value still comes from pnorm() of the retained z-statistic regardless
  # of report_space (estimate/SE are back-transformed; the test is not).
  expected_pvals <- 2 * stats::pnorm(-abs(s$coefficients$statistic))
  expect_equal(s$coefficients$p.value, expected_pvals, tolerance = 1e-12)
})

test_that("TMB summary p-values still use pnorm() (regression: do not switch to pt)", {
  skip_on_cran()

  data("apt", package = "beezdemand")
  fit_tmb <- fit_demand_tmb(apt, equation = "exponential",
                            random_effects = "q0",
                            multi_start = FALSE, verbose = 0)

  s <- summary(fit_tmb)

  z_pvals <- 2 * stats::pnorm(-abs(s$coefficients$statistic))
  expect_equal(s$coefficients$p.value, z_pvals, tolerance = 1e-12)
})
