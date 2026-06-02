# Contract: under the broom/emmeans convention, summary() and tidy() report the
# Wald `statistic` and `p.value` on the ESTIMATION (log/log10) scale regardless of
# `report_space`; only `estimate`/`std.error` are back-transformed. The test is
# therefore transformation-invariant. This replaces the prior natural-scale
# recompute (statistic = estimate/std.error on the back-transformed scale), which
# was degenerate -- statistic = 1/(c*SE), independent of the point estimate, and
# for factor effects tested an impossible null (ratio = 0 rather than ratio = 1).
#
# These assertions FAIL on the pre-fix code (natural-scale statistic differs from
# the log10/internal-scale statistic) and PASS after the recompute is removed.

# --- TMB tier ----------------------------------------------------------------

test_that("TMB summary: statistic/p.value invariant across report_space; estimate back-transformed", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", random_effects = "q0",
                        multi_start = FALSE, verbose = 0)

  s_nat <- summary(fit, report_space = "natural")$coefficients
  s_log <- summary(fit, report_space = "log10")$coefficients
  s_int <- summary(fit, report_space = "internal")$coefficients

  # The test statistic and p-value do not depend on the reporting scale.
  expect_equal(s_nat$statistic, s_log$statistic, tolerance = 1e-9)
  expect_equal(s_nat$p.value,   s_log$p.value,   tolerance = 1e-9)
  expect_equal(s_nat$statistic, s_int$statistic, tolerance = 1e-9)
  expect_equal(s_nat$p.value,   s_int$p.value,   tolerance = 1e-9)

  # But the estimate IS back-transformed: core fixed rows differ natural vs log10.
  core <- s_nat$component == "fixed"
  expect_true(any(core))
  expect_false(isTRUE(all.equal(s_nat$estimate[core], s_log$estimate[core])))
})

test_that("TMB tidy: statistic/p.value invariant across report_space", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", random_effects = "q0",
                        multi_start = FALSE, verbose = 0)

  t_nat <- tidy(fit, effects = "fixed", report_space = "natural")
  t_log <- tidy(fit, effects = "fixed", report_space = "log10")

  expect_equal(t_nat$statistic, t_log$statistic, tolerance = 1e-9)
  expect_equal(t_nat$p.value,   t_log$p.value,   tolerance = 1e-9)
  expect_false(isTRUE(all.equal(t_nat$estimate, t_log$estimate)))
})

# --- NLME tier ---------------------------------------------------------------

test_that("NLME summary: statistic/p.value invariant across report_space and equal to native tTable", {
  skip_on_cran()
  skip_if_not_installed("nlme")
  data("apt", package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_mixed(apt, y_var = "y_ll4", x_var = "x", id_var = "id",
                          equation_form = "zben")

  s_nat <- summary(fit, report_space = "natural")$coefficients
  s_log <- summary(fit, report_space = "log10")$coefficients

  expect_equal(s_nat$statistic, s_log$statistic, tolerance = 1e-9)
  expect_equal(s_nat$p.value,   s_log$p.value,   tolerance = 1e-9)

  # Estimation-scale (native nlme) test is preserved on the natural scale.
  tt <- summary(fit$model)$tTable
  expect_equal(unname(s_nat$statistic), unname(tt[, "t-value"]), tolerance = 1e-9)
  expect_equal(unname(s_nat$p.value),   unname(tt[, "p-value"]), tolerance = 1e-9)

  # Estimate is back-transformed (log10 vs natural differ).
  expect_false(isTRUE(all.equal(s_nat$estimate, s_log$estimate)))
})

test_that("NLME tidy: statistic/p.value invariant across report_space and equal to native tTable", {
  skip_on_cran()
  skip_if_not_installed("nlme")
  data("apt", package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_mixed(apt, y_var = "y_ll4", x_var = "x", id_var = "id",
                          equation_form = "zben")

  t_nat <- tidy(fit, effects = "fixed", report_space = "natural")
  t_log <- tidy(fit, effects = "fixed", report_space = "log10")

  expect_equal(t_nat$statistic, t_log$statistic, tolerance = 1e-9)
  expect_equal(t_nat$p.value,   t_log$p.value,   tolerance = 1e-9)

  tt <- summary(fit$model)$tTable
  expect_equal(unname(t_nat$statistic), unname(tt[, "t-value"]), tolerance = 1e-9)
  expect_false(isTRUE(all.equal(t_nat$estimate, t_log$estimate)))
})

# --- Hurdle tier -------------------------------------------------------------

test_that("hurdle summary: statistic/p.value invariant across report_space", {
  skip_on_cran()
  data("apt", package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                           verbose = 0)

  s_nat <- summary(fit, report_space = "natural")$coefficients
  s_log <- summary(fit, report_space = "log10")$coefficients

  expect_equal(s_nat$statistic, s_log$statistic, tolerance = 1e-9)
  expect_equal(s_nat$p.value,   s_log$p.value,   tolerance = 1e-9)
  expect_false(isTRUE(all.equal(s_nat$estimate, s_log$estimate)))
})

test_that("hurdle tidy: statistic/p.value invariant across report_space", {
  skip_on_cran()
  data("apt", package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                           verbose = 0)

  t_nat <- tidy(fit, report_space = "natural")
  t_log <- tidy(fit, report_space = "log10")

  expect_equal(t_nat$statistic, t_log$statistic, tolerance = 1e-9)
  expect_equal(t_nat$p.value,   t_log$p.value,   tolerance = 1e-9)
  expect_false(isTRUE(all.equal(t_nat$estimate, t_log$estimate)))
})
