# Tests for TICKET-028: formula(), model.matrix(), update() methods for
# beezdemand_tmb (+ formula and model.matrix for beezdemand_hurdle).
#
# Fixture notes (same rationale as TICKET-026 tests):
# * The factor-expansion tests use `gender` (3-level) from `apt_full` rather
#   than the ticket's draft `id_group` column, which does not exist on this
#   branch's apt_full.
# * Hurdle tests use `simulate_hurdle_data()` to match the convention in
#   test-hurdle_methods.R (fit_demand_hurdle has no defaults for
#   y_var/x_var/id_var).

test_that("S3 methods are registered for formula / model.matrix / update", {
  # Structural / dispatch check that does not require a fit and is therefore
  # safe to run on CRAN. Guards against accidental deregistration if a future
  # roxygen edit drops `@export` from one of these methods.
  expect_false(is.null(getS3method("formula",      "beezdemand_tmb",    optional = TRUE)))
  expect_false(is.null(getS3method("model.matrix", "beezdemand_tmb",    optional = TRUE)))
  expect_false(is.null(getS3method("update",       "beezdemand_tmb",    optional = TRUE)))
  expect_false(is.null(getS3method("formula",      "beezdemand_hurdle", optional = TRUE)))
  expect_false(is.null(getS3method("model.matrix", "beezdemand_hurdle", optional = TRUE)))
})

test_that("formula.beezdemand_tmb returns named list with Q0, alpha, random", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  f <- formula(fit)
  expect_named(f, c("Q0", "alpha", "random"))
  expect_s3_class(f$Q0,    "formula")
  expect_s3_class(f$alpha, "formula")
  # Regression: rhs_q0 / rhs_alpha already include "~" — guard against
  # nested `~~term` shapes (`paste("~", "~ term")` style bug).
  q0_dep    <- deparse(f$Q0)
  alpha_dep <- deparse(f$alpha)
  expect_false(grepl("~~", q0_dep, fixed = TRUE),
               info = "formula(fit)$Q0 should not be a nested formula (`~~...`).")
  expect_false(grepl("~~", alpha_dep, fixed = TRUE),
               info = "formula(fit)$alpha should not be a nested formula (`~~...`).")
  # The factor name "gender" should appear in both one-sided formulas:
  expect_match(q0_dep,    "gender")
  expect_match(alpha_dep, "gender")
  # And they should round-trip through stats::terms() without surprise:
  expect_silent(stats::terms(f$Q0))
  expect_silent(stats::terms(f$alpha))
})

test_that("fit_demand_tmb stores call exactly once on the fit object", {
  # Regression: a draft of TICKET-028 added a second `call = cl` entry
  # at the bottom of the return structure, duplicating the one already
  # present near the top (~R/tmb-demand.R:1760). The duplicate slipped
  # past type-only assertions because both entries hold the same value.
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  expect_equal(sum(names(fit) == "call"), 1L)
  expect_false(as.logical(anyDuplicated(names(fit))))
})

test_that("model.matrix.beezdemand_tmb returns named list of four matrices by default", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  mm <- model.matrix(fit)
  expect_type(mm, "list")
  expect_named(mm, c("X_q0", "X_alpha", "Z_q0", "Z_alpha"))
  expect_true(is.matrix(mm$X_q0))
  expect_equal(nrow(mm$X_q0), nobs(fit))
})

test_that("model.matrix reuses stored formula_details$X_q0 / X_alpha (zero-copy)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  expect_identical(model.matrix(fit, what = "X_q0"),    fit$formula_details$X_q0)
  expect_identical(model.matrix(fit, what = "X_alpha"), fit$formula_details$X_alpha)
})

test_that("asymmetric factors via collapse_levels produce independent X / formula slots", {
  # fit_demand_tmb() exposes only the symmetric `factors` arg publicly; the
  # asymmetric `factors_q0` / `factors_alpha` internal slots are populated
  # via `collapse_levels`. Collapse `gender` from 3 levels to 2 for Q0 only;
  # alpha keeps the original 3-level gender.
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt_full, equation = "exponential",
    factors = "gender",
    collapse_levels = list(
      # Format: list(factor = list(new_level = c(old_levels), ...))
      Q0 = list(gender = list(
        "MF"   = c("Male", "Female"),
        "WRNS" = c("Would rather not say")
      ))
      # alpha key omitted on purpose — alpha sees the original 3-level gender
    ),
    verbose = 0
  )
  # Confirm the internal asymmetry is wired up correctly:
  expect_false(identical(fit$param_info$factors_q0,
                         fit$param_info$factors_alpha))

  f  <- formula(fit)
  mm <- model.matrix(fit)
  # The reconstructed formulas reference different factor columns:
  expect_false(identical(deparse(f$Q0), deparse(f$alpha)))
  # X_q0 has fewer columns than X_alpha (collapsed → 2 levels vs 3 levels):
  expect_lt(ncol(mm$X_q0), ncol(mm$X_alpha))
})

test_that("model.matrix(what='Z_alpha') on 1-RE fit returns NULL with a message", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential",
                        random_effects = Q0 ~ 1, verbose = 0)
  expect_message(
    Z <- model.matrix(fit, what = "Z_alpha"),
    "alpha"
  )
  expect_null(Z)
})

test_that("model.matrix(what='X') errors with named valid alternatives", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  expect_error(model.matrix(fit, what = "X"),
               "X_q0|X_alpha")
})

test_that("update.beezdemand_tmb refits with replaced arguments", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit_full <- fit_demand_tmb(apt_full, equation = "exponential",
                             factors = "gender", verbose = 0)
  fit_null <- update(fit_full, factors = NULL)
  expect_s3_class(fit_null, "beezdemand_tmb")
  # Removing the factor should reduce the fixed-effect count:
  expect_lt(length(coef(fit_null, type = "internal")),
            length(coef(fit_full, type = "internal")))
})

test_that("update.beezdemand_tmb evaluate=FALSE returns the call", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  call <- update(fit, equation = "simplified", evaluate = FALSE)
  # `call` is a base type ("language"), not an S3 object — use is.call().
  expect_true(is.call(call))
  expect_equal(call$equation, "simplified")
})

test_that("formula round-trip produces equivalent fit (random spec preserved)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit  <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  f    <- formula(fit)
  fit2 <- fit_demand_tmb(apt, equation = "exponential",
                         random_effects = f$random, verbose = 0)
  expect_equal(as.numeric(logLik(fit)), as.numeric(logLik(fit2)),
               tolerance = 1e-6)
})

test_that("formula.beezdemand_hurdle returns named list with binary, consumption, random", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )
  f <- formula(fit)
  expect_named(f, c("binary", "consumption", "random"))
  expect_s3_class(f$binary,       "formula")
  expect_s3_class(f$consumption,  "formula")
})

test_that("model.matrix.beezdemand_hurdle returns named list of component matrices", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )
  mm <- model.matrix(fit)
  expect_type(mm, "list")
  # Required component matrices:
  expect_true(all(c("X_binary", "X_consumption") %in% names(mm)))
  expect_true(is.matrix(mm$X_binary))
  expect_true(is.matrix(mm$X_consumption))
  expect_equal(nrow(mm$X_consumption), nobs(fit))
})
