# Regression tests for TICKET-006: statistical-correctness fixes for the
# NLME summary parameter-transformation path and a labelling fix on the
# hurdle summary coefficient matrix.
#
# Bug 1 (NLME): when the user requests `report_space != internal_space`,
# `summary.beezdemand_nlme()` previously discarded nlme's containment-based
# degrees of freedom and recomputed p-values via `pnorm()`, producing
# anti-conservative (z-style) results for small N. Fix preserves DF and uses
# `pt()` after the delta-method transformation.
#
# Bug 2 (hurdle): `summary.beezdemand_hurdle()` labelled the test-statistic
# column "t value" while the p-values were correctly computed via `pnorm()`
# (z-test, the right choice for TMB Laplace-approximation models). The label
# now reads "z value", consistent with glmmTMB convention.

# --- Bug 1: NLME t-test preservation -----------------------------------------

test_that("NLME summary with report_space='natural' uses pt() with nlme DF, not pnorm()", {
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

  # Bug-1 guard: p-values must NOT match what pnorm() would produce on the
  # same statistics. If they match exactly, the bug has returned.
  z_pvals <- 2 * stats::pnorm(-abs(s_natural$coefficients$statistic))
  expect_false(
    isTRUE(all.equal(s_natural$coefficients$p.value, z_pvals, tolerance = 1e-12)),
    info = "Natural-scale NLME p-values must use pt(), not pnorm()."
  )
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

  # The tibble's p.value is computed AFTER beezdemand_transform_coef_table()
  # rescales estimates/SEs (log → natural for log_q0/log_alpha/log_k). The
  # invariant we care about for TICKET-006 is that the *post-transform*
  # p-values still come from pnorm() — verifying that with the post-transform
  # statistic.
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
