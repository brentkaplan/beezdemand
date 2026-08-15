# Tests for TICKET-026: vcov(), fitted(), residuals() methods for
# beezdemand_tmb and beezdemand_hurdle.
#
# Fixture notes
# -------------
# * TMB tests use the bundled `apt` dataset (small; fast fit). The
#   factor-expansion exercise uses `gender` from `apt_full` (a column that
#   actually exists; the ticket's draft referenced `id_group`, which does
#   not exist in this branch's `apt_full`).
# * Hurdle tests use `simulate_hurdle_data(n_subjects = 30, seed = 123)` to
#   match the convention in test-hurdle_methods.R (fit_demand_hurdle has no
#   defaults for y_var/x_var/id_var, and the simulated dataset is much
#   cheaper to fit than apt_full).

test_that("S3 methods are registered for vcov / fitted / residuals", {
  # Structural / dispatch check that does not require a fit and is therefore
  # safe to run on CRAN. Guards against accidental deregistration if a future
  # roxygen edit drops `@export` from one of these methods.
  expect_false(is.null(getS3method("vcov",      "beezdemand_tmb",    optional = TRUE)))
  expect_false(is.null(getS3method("fitted",    "beezdemand_tmb",    optional = TRUE)))
  expect_false(is.null(getS3method("residuals", "beezdemand_tmb",    optional = TRUE)))
  expect_false(is.null(getS3method("vcov",      "beezdemand_hurdle", optional = TRUE)))
  expect_false(is.null(getS3method("fitted",    "beezdemand_hurdle", optional = TRUE)))
  expect_false(is.null(getS3method("residuals", "beezdemand_hurdle", optional = TRUE)))
})

test_that("vcov.beezdemand_tmb returns symmetric PSD matrix with names", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  V <- vcov(fit)
  expect_true(is.matrix(V))
  expect_true(isSymmetric(V, tol = 1e-8))
  expect_true(all(eigen(V, only.values = TRUE)$values >= -1e-8))
  expect_false(is.null(rownames(V)))
  expect_equal(rownames(V), colnames(V))
})

test_that("fitted.beezdemand_tmb default scale matches augment()$.fitted", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  y_hat <- fitted(fit)                    # scale="model", level="subject" defaults
  aug   <- broom::augment(fit)
  expect_equal(length(y_hat), nobs(fit))
  expect_equal(y_hat, aug$.fitted, tolerance = 1e-10)
})

test_that("residuals.beezdemand_tmb default scale matches augment()$.resid", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  r   <- residuals(fit)                   # type="response", scale="model" defaults
  aug <- broom::augment(fit)
  expect_equal(length(r), nobs(fit))
  expect_equal(r, aug$.resid, tolerance = 1e-10)
})

test_that("residuals + fitted reconstruct y on chosen scale", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  # Model scale:
  y_model <- fitted(fit, scale = "model") + residuals(fit, type = "response", scale = "model")
  expect_equal(length(y_model), nobs(fit))
  expect_true(all(is.finite(y_model) | is.na(y_model)))
  # Natural scale (opt-in):
  y_nat <- fitted(fit, scale = "natural") + residuals(fit, type = "response", scale = "natural")
  expect_equal(length(y_nat), nobs(fit))
})

test_that("residuals(type='pearson', scale='natural') falls back with a message", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  expect_message(
    r <- residuals(fit, type = "pearson", scale = "natural"),
    "natural"
  )
  expect_equal(length(r), nobs(fit))
})

test_that("residuals(scale='natural') for zben back-transforms y before differencing (#18)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_tmb(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", verbose = 0
  )

  r_nat <- residuals(fit, scale = "natural")
  fitted_nat <- fitted(fit, scale = "natural")
  y_natural <- ll4_inv(fit$data$y_ll4)

  expect_equal(r_nat, y_natural - fitted_nat, tolerance = 1e-10)

  # Regression guard: the pre-fix bug subtracted natural-scale fitted values
  # from the raw (still LL4-transformed) y_var, i.e. LL4(y) - fitted_natural.
  # Confirm the current output is NOT that scale-mixed quantity.
  wrong_resid <- fit$data$y_ll4 - fitted_nat
  expect_false(isTRUE(all.equal(r_nat, wrong_resid, tolerance = 1e-6)))

  # Mean natural-scale residual bias at price 0 should be small (the
  # scale-mixed bug produced a bias of roughly -6.5 drinks there).
  at_zero <- fit$data$x == 0
  expect_true(any(at_zero))
  expect_lt(abs(mean(r_nat[at_zero])), 1)
})

test_that("residuals(scale='model') for zben is unaffected by the natural-scale fix (#18)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_tmb(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", verbose = 0
  )

  r_model <- residuals(fit, scale = "model")
  fitted_model <- fitted(fit, scale = "model")
  expect_equal(r_model, fit$data$y_ll4 - fitted_model, tolerance = 1e-10)
})

test_that("vcov.beezdemand_hurdle has component-prefixed dim names", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )
  V <- vcov(fit)
  expect_true(any(grepl("zero_probability", rownames(V))))
  expect_true(any(grepl("consumption",      rownames(V))))
  expect_true(isSymmetric(V, tol = 1e-8))
})

test_that("fitted.beezdemand_hurdle marginal=TRUE matches predict(type='demand')$.fitted", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )
  y_marg <- fitted(fit, marginal = TRUE)
  # predict.beezdemand_hurdle() returns a tibble; the marginal expected
  # consumption is in $.fitted (which == $expected_consumption when
  # type = "demand"). fitted() unwraps the column into a numeric vector
  # and strips any subject-derived names — match that here via unname().
  y_pred <- predict(fit, newdata = fit$data, type = "demand")
  expect_type(y_marg, "double")
  expect_null(dim(y_marg))
  expect_equal(y_marg, unname(y_pred$.fitted), tolerance = 1e-10)
  expect_equal(y_marg, unname(y_pred$expected_consumption), tolerance = 1e-10)
})

test_that("fitted.beezdemand_hurdle marginal=TRUE differs from FALSE", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )
  y_marg <- fitted(fit, marginal = TRUE)
  y_cond <- fitted(fit, marginal = FALSE)
  # Marginal is shrunk toward zero by the participation probability:
  expect_true(all(y_marg <= y_cond + 1e-10))
})

test_that("vcov errors helpfully on unconverged fit", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  fit$sdr$cov.fixed <- NULL
  expect_error(vcov(fit), "converge|cov\\.fixed")
})

test_that("car::deltaMethod with explicit numeric-vector form is callable", {
  # Smoke test using the recommended downstream pattern. We do NOT pass `fit`
  # directly because car::deltaMethod.default() calls coef(object); we pass
  # the explicit internal parameter vector so the call is robust regardless
  # of coef()'s default (and to any future coef() default change).
  skip_on_cran()
  skip_if_not_installed("TMB")
  skip_if_not_installed("car")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  beta <- coef(fit, type = "internal")
  # Pick any expression involving names actually present in beta:
  expr <- paste0(names(beta)[1], " * 1")
  expect_no_error(
    car::deltaMethod(beta, expr, vcov. = vcov(fit))
  )
})
