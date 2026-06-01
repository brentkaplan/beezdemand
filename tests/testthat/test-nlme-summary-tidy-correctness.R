# Regression tests for the v0.3.0 release audit (Codex findings C1 + C2).
#
# C1 (summary<->tidy p-value consistency): TICKET-006 established that, when the
# user requests `report_space != internal_space`, the NLME *summary* path
# preserves nlme's containment-based degrees of freedom and recomputes p-values
# via `pt()` after the delta-method transform. `tidy.beezdemand_nlme()` was left
# recomputing via `pnorm()` (mixed-methods.R), so summary() and tidy() on the
# SAME fit + report_space disagree (anti-conservative tidy p-values for small N).
# Fix: tidy() reuses the same DF-aware `pt()` recompute as summary().
#
# C2 (summary convergence contract): TICKET-020 made `glance$converged` the
# operational gate (apVar PD AND no terminal error) via `.check_nlme_convergence`.
# `summary.beezdemand_nlme()` hard-coded `converged = TRUE`, contradicting glance
# on the very same fit. Fix: summary() routes through the same helper.
#
# Base fit memoized at file level (mirrors test-nlme-convergence-heuristic.R /
# test-fit_demand_tmb.R). Tests bind a local `fit` and mutate copies; R
# copy-on-modify keeps the cached object pristine.

.nstc_cache <- new.env(parent = emptyenv())

.nstc_fit <- function() {
  if (is.null(.nstc_cache$fit)) {
    data(apt, package = "beezdemand")
    apt$y_ll4 <- ll4(apt$y)
    # zben REQUIRES the LL4-transformed y_var; small N -> finite containment DF.
    .nstc_cache$fit <- fit_demand_mixed(
      data = apt, y_var = "y_ll4", x_var = "x", id_var = "id",
      equation_form = "zben"
    )
  }
  .nstc_cache$fit
}

# --- C1: tidy must match summary's DF-aware test ------------------------------

test_that("tidy.beezdemand_nlme natural-scale p-values use pt() with nlme DF, not pnorm()", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  fit <- .nstc_fit()
  td <- tidy(fit, report_space = "natural", effects = "fixed")

  expect_true(all(is.finite(td$p.value)))
  expect_true(all(td$p.value >= 0 & td$p.value <= 1))

  # Bug guard: tidy p-values must NOT equal what pnorm() would give on the same
  # statistics. If they match exactly, the pnorm() bug has returned.
  z_pvals <- 2 * stats::pnorm(-abs(td$statistic))
  expect_false(
    isTRUE(all.equal(td$p.value, z_pvals, tolerance = 1e-12)),
    info = "Natural-scale NLME tidy p-values must use pt(), not pnorm()."
  )

  # t-tails are heavier than normal -> two-sided pt p-values >= pnorm p-values.
  expect_true(all(td$p.value + 1e-12 >= z_pvals))
})

test_that("summary() and tidy() agree on fixed-effect statistic/p.value for the same NLME fit", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  fit <- .nstc_fit()
  s <- summary(fit, report_space = "natural")
  td <- tidy(fit, report_space = "natural", effects = "fixed")

  s_fixed <- s$coefficients[s$coefficients$component == "fixed", , drop = FALSE]
  # Align by term.
  s_fixed <- s_fixed[order(s_fixed$term), ]
  td <- td[order(td$term), ]

  expect_identical(s_fixed$term, td$term)
  expect_equal(s_fixed$statistic, td$statistic, tolerance = 1e-9)
  expect_equal(s_fixed$p.value, td$p.value, tolerance = 1e-9)
})

test_that("tidy() default (report_space == internal) preserves nlme native p-values", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  fit <- .nstc_fit()
  internal_space <- fit$param_space %||% fit$param_info$param_space %||% "log10"
  td <- tidy(fit, report_space = internal_space, effects = "fixed")

  native_pvals <- summary(fit$model)$tTable[, "p-value"]
  expect_equal(unname(td$p.value), unname(native_pvals), tolerance = 1e-12)
})

# --- C2: summary convergence must match glance --------------------------------

test_that("summary$converged matches glance$converged on a usable (PD apVar) fit", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  fit <- .nstc_fit()
  skip_if_not(is.matrix(fit$model$apVar) && all(is.finite(fit$model$apVar)))

  expect_true(summary(fit)$converged)
  expect_identical(summary(fit)$converged, glance(fit)$converged)
})

test_that("summary$converged flips to FALSE when apVar is non-positive-definite (matches glance)", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  fit <- .nstc_fit()
  # nlme stores a character sentinel (not a matrix) when apVar inversion fails.
  fit$model$apVar <- "Non-positive definite approximate variance-covariance"

  expect_false(summary(fit)$converged)
  expect_identical(summary(fit)$converged, glance(fit)$converged)
})
