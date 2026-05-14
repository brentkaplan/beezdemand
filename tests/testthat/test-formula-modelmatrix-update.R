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
