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


# --- TICKET-064 (F11): inference surfaces honour .check_nlme_convergence() -

test_that("get_demand_param_emms.beezdemand_nlme warns once on a non-converged fit", {
  skip_on_cran()
  skip_if_not_installed("emmeans")
  fit <- .weak_conv_nlme_fit()
  expect_false(glance(fit)$converged)
  warns <- testthat::capture_warnings(get_demand_param_emms(fit, param = "Q0"))
  gate_warns <- grepl("convergence gate", warns, ignore.case = TRUE)
  expect_identical(sum(gate_warns), 1L)
})

test_that("get_demand_comparisons.beezdemand_nlme warns once on a non-converged fit", {
  skip_on_cran()
  skip_if_not_installed("emmeans")
  fit <- .weak_conv_nlme_fit()
  warns <- testthat::capture_warnings(
    get_demand_comparisons(fit, param = "Q0")
  )
  gate_warns <- grepl("convergence gate", warns, ignore.case = TRUE)
  expect_identical(sum(gate_warns), 1L)
})

test_that("confint.beezdemand_nlme warns once on a non-converged fit", {
  skip_on_cran()
  fit <- .weak_conv_nlme_fit()
  warns <- testthat::capture_warnings(ci <- confint(fit))
  gate_warns <- grepl("convergence gate", warns, ignore.case = TRUE)
  expect_identical(sum(gate_warns), 1L)
  expect_true(nrow(ci) > 0)
})

test_that("get_subject_pars.beezdemand_nlme warns once on a non-converged fit", {
  skip_on_cran()
  fit <- .weak_conv_nlme_fit()
  warns <- testthat::capture_warnings(sp <- get_subject_pars(fit))
  gate_warns <- grepl("convergence gate", warns, ignore.case = TRUE)
  expect_identical(sum(gate_warns), 1L)
})

test_that("tidy.beezdemand_nlme warns once on a non-converged fit", {
  skip_on_cran()
  fit <- .weak_conv_nlme_fit()
  warns <- testthat::capture_warnings(td <- tidy(fit))
  gate_warns <- grepl("convergence gate", warns, ignore.case = TRUE)
  expect_identical(sum(gate_warns), 1L)
})

test_that("get_individual_coefficients warns once on a non-converged fit", {
  skip_on_cran()
  fit <- .weak_conv_nlme_fit()
  warns <- testthat::capture_warnings(ic <- get_individual_coefficients(fit))
  gate_warns <- grepl("convergence gate", warns, ignore.case = TRUE)
  expect_identical(sum(gate_warns), 1L)
})

test_that("calc_group_metrics.beezdemand_nlme warns exactly once (not once per Q0/alpha call)", {
  skip_on_cran()
  skip_if_not_installed("emmeans")
  fit <- .weak_conv_nlme_fit()
  warns <- testthat::capture_warnings(cm <- calc_group_metrics(fit))
  gate_warns <- grepl("convergence gate", warns, ignore.case = TRUE)
  expect_identical(sum(gate_warns), 1L)
})

test_that("anova.beezdemand_nlme warns once per non-converged model in the comparison", {
  skip_on_cran()
  # Same dataset as .weak_conv_nlme_fit() (nlme::anova.lme() requires equal
  # N across compared fits); fit1 is a healthy Q0-only-RE baseline, fit2 is
  # the injected-apVar-failure fixture on the identical data.
  set.seed(11)
  d <- expand.grid(id = factor(1:6), x = c(0.1, 0.5, 1, 2.5, 5, 10, 20))
  q0i <- 10 * exp(rnorm(6, 0, 0.4))
  ali <- 0.01 * exp(rnorm(6, 0, 0.6))
  d$y <- pmax(0, q0i[d$id] * exp(-ali[d$id] * q0i[d$id] * d$x) + rnorm(nrow(d), 0, 2.5))
  d$y_ll4 <- ll4(d$y, lambda = 4)
  fit1 <- fit_demand_mixed(d, y_var = "y_ll4", x_var = "x", id_var = "id",
                           equation_form = "zben", random_effects = Q0 ~ 1)
  fit2 <- .weak_conv_nlme_fit()
  warns <- testthat::capture_warnings(a <- anova(fit1, fit2))
  gate_warns <- grepl("convergence gate", warns, ignore.case = TRUE)
  expect_identical(sum(gate_warns), 1L)
})

test_that("NLME inference surfaces: healthy fit raises no convergence-gate warning", {
  skip_on_cran()
  skip_if_not_installed("emmeans")
  set.seed(3)
  d <- expand.grid(id = factor(1:20), x = c(0.1, 0.5, 1, 2.5, 5, 10, 20))
  q0i <- 10 * exp(rnorm(20, 0, 0.3)); ali <- 0.01 * exp(rnorm(20, 0, 0.4))
  d$y <- pmax(0, q0i[d$id] * exp(-ali[d$id] * q0i[d$id] * d$x) + rnorm(nrow(d), 0, 1.5))
  fit <- fit_demand_mixed(d, y_var = "y", x_var = "x", id_var = "id",
                          equation_form = "simplified")
  expect_true(glance(fit)$converged)
  expect_no_warning(get_demand_param_emms(fit, param = "Q0"))
  expect_no_warning(confint(fit))
  expect_no_warning(get_subject_pars(fit))
  expect_no_warning(tidy(fit))
  expect_no_warning(get_individual_coefficients(fit))
  expect_no_warning(calc_group_metrics(fit))
})
