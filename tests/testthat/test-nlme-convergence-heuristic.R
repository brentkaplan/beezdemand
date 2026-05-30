# Tests for TICKET-020: decouple NLME final-fit assessment from iteration-level
# convergence warnings.
#
# `glance(fit_nlme)$converged` previously flipped to FALSE whenever nlme emitted
# a PNLS/LME *iteration-level* warning matching a convergence-failure regex
# ("false convergence", "singular", "did not converge", ...), even when the
# *final* fit was usable: a positive-definite `apVar` and no terminal error.
# The fix reports the operational gate (apVar PD AND no error) as
# `converged`/`final_fit_ok`, and the regex hits separately as the diagnostic
# `fit_warned`.
#
# nlme's real PNLS iteration warnings are non-deterministic (data / platform /
# optimizer-tolerance dependent — precisely why the bug is subtle and why
# downstream code cannot gate on the old `converged`). So the regression test
# *synthesizes* the warned condition by assigning `fit$fit_warnings` after a
# clean, PD-apVar fit. That exercises the exact semantic deterministically.
#
# The base fit (`apt`, simplified, ~160 rows / 10 subjects, PD apVar, zero real
# warnings) is memoized at file level (new.env cache, mirroring
# test-fit_demand_tmb.R / test-anova-tmb.R). Tests bind it to a local `fit` and
# mutate copies; R copy-on-modify keeps the cached object pristine.

.nch_cache <- new.env(parent = emptyenv())

.nch_fit <- function() {
  if (is.null(.nch_cache$fit)) {
    data(apt, package = "beezdemand")
    .nch_cache$fit <- fit_demand_mixed(
      data = apt, y_var = "y", x_var = "x", id_var = "id",
      equation_form = "simplified"
    )
  }
  .nch_cache$fit
}

test_that("glance$converged stays TRUE on PD apVar despite iteration warnings (TICKET-020)", {
  skip_on_cran()
  fit <- .nch_fit()
  skip_if_not(is.matrix(fit$model$apVar) && all(is.finite(fit$model$apVar)))

  # Synthesize the iteration-level warnings nlme emits during PNLS-LME
  # alternation (see file header — real emission is non-deterministic).
  fit$fit_warnings <- c("false convergence (code = 4)",
                        "Singular precision matrix in level -1, block 1")

  g <- glance(fit)
  expect_true(g$converged)
  expect_true(g$final_fit_ok)
  expect_true(g$fit_warned)
})

test_that("glance.beezdemand_nlme exposes final_fit_ok/fit_warned; converged aliases final_fit_ok", {
  skip_on_cran()
  fit <- .nch_fit()
  # Pin the no-warning state for this contract test: real PNLS warning emission
  # is platform/tolerance dependent (see file header), so don't rely on the
  # cached fit happening to be warning-free.
  fit$fit_warnings <- character(0)

  g <- glance(fit)
  expect_true(all(c("converged", "final_fit_ok", "fit_warned") %in% names(g)))
  expect_type(g$final_fit_ok, "logical")
  expect_type(g$fit_warned, "logical")
  expect_identical(g$converged, g$final_fit_ok)
  # No convergence-pattern warnings -> fit_warned is FALSE.
  expect_false(g$fit_warned)
})

test_that("glance$converged stays FALSE when apVar is non-positive-definite", {
  skip_on_cran()
  fit <- .nch_fit()
  # nlme stores a character sentinel (not a matrix) when apVar inversion fails.
  fit$model$apVar <- "Non-positive definite approximate variance-covariance"

  g <- glance(fit)
  expect_false(g$converged)
  expect_false(g$final_fit_ok)
})

test_that(".check_nlme_convergence decouples final_fit_ok from fit_warned and keeps a diagnostic message", {
  skip_on_cran()
  fit <- .nch_fit()
  skip_if_not(is.matrix(fit$model$apVar) && all(is.finite(fit$model$apVar)))
  fit$fit_warnings <- c("nlm() did not converge (code = 4)")

  res <- beezdemand:::.check_nlme_convergence(fit)
  expect_true(res$final_fit_ok)
  expect_true(res$fit_warned)
  expect_true(res$converged)              # alias for final_fit_ok
  expect_false(is.null(res$message))      # iteration warning surfaced as diagnostic
  expect_match(res$message, "did not converge")
})

# Edge cases on synthetic objects (no fitting): the new final_fit_ok gate must
# reject a fit whose apVar was never computed, and a fit with a terminal error
# even when apVar happens to be a finite matrix.

test_that("final_fit_ok is FALSE when apVar is NULL (apVar not computed)", {
  fake <- structure(list(
    model = list(apVar = NULL),
    fit_warnings = character(0)
  ), class = "beezdemand_nlme")

  res <- beezdemand:::.check_nlme_convergence(fake)
  expect_false(res$final_fit_ok)
  expect_false(res$converged)
  expect_false(res$fit_warned)
})

test_that("a terminal error_message forces final_fit_ok FALSE despite a finite apVar", {
  fake <- structure(list(
    model = list(apVar = matrix(1)),   # finite/PD matrix
    error_message = "optimizer failed",
    fit_warnings = character(0)
  ), class = "beezdemand_nlme")

  res <- beezdemand:::.check_nlme_convergence(fake)
  expect_false(res$final_fit_ok)
  expect_false(res$converged)
  expect_match(res$message, "optimizer failed")
})
