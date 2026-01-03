# tests/testthat/test-cross-price.R
# Test suite for cross-price NLS models per EQUATIONS_CONTRACT.md

# ==============================================================================
# Helper functions for simulating cross-price demand data
# ==============================================================================

#' Simulate cross-price demand data for exponentiated equation
#' Per EQUATIONS_CONTRACT.md:
#' y = Q_alone * 10^(I * exp(-beta * x))
simulate_cp_exponentiated <- function(
    n = 50,
    qalone = 10,
    I = 1.5,
    beta = 0.05,
    sigma = 0.5,
    seed = 42
) {
  set.seed(seed)
  x <- runif(n, min = 1, max = 50)
  # True model: y = qalone * 10^(I * exp(-beta * x))
  y_true <- qalone * 10^(I * exp(-beta * x))
  # Add multiplicative noise
  y <- y_true * exp(rnorm(n, 0, sigma))
  data.frame(x = x, y = y)
}

#' Simulate cross-price demand data for exponential equation
#' Per EQUATIONS_CONTRACT.md:
#' log10(y) = log10(Q_alone) + I * exp(-beta * x)
simulate_cp_exponential <- function(
    n = 50,
    qalone = 10,
    I = 1.5,
    beta = 0.05,
    sigma = 0.2,
    seed = 42
) {
  set.seed(seed)
  x <- runif(n, min = 1, max = 50)
  # True model on log10 scale: log10(y) = log10(qalone) + I * exp(-beta * x)
  log10_y_true <- log10(qalone) + I * exp(-beta * x)
  # Add noise on log10 scale
  log10_y <- log10_y_true + rnorm(n, 0, sigma)
  y <- 10^log10_y
  data.frame(x = x, y = y)
}

#' Simulate cross-price demand data for additive equation
#' Per EQUATIONS_CONTRACT.md:
#' y = Q_alone + I * exp(-beta * x)
simulate_cp_additive <- function(
    n = 50,
    qalone = 5,
    I = 10,
    beta = 0.05,
    sigma = 1,
    seed = 42
) {
  set.seed(seed)
  x <- runif(n, min = 1, max = 50)
  # True model: y = qalone + I * exp(-beta * x)
  y_true <- qalone + I * exp(-beta * x)
  # Add additive noise
  y <- y_true + rnorm(n, 0, sigma)
  y <- pmax(y, 0.1)  # Ensure positive
  data.frame(x = x, y = y)
}


# ==============================================================================
# Test: Basic fitting functionality
# ==============================================================================

test_that("fit_cp_nls fits exponentiated equation", {
  skip_if_not_installed("nls.multstart")

  data <- simulate_cp_exponentiated(n = 30, qalone = 10, I = 1.5, beta = 0.05, seed = 123)

  fit <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE, iter = 50)

  expect_s3_class(fit, "cp_model_nls")
  expect_equal(fit$equation, "exponentiated")
  expect_false(is.null(fit$model))

  # Check coefficient names match new parameterization
  coefs <- coef(fit$model)
  expect_true("log10_qalone" %in% names(coefs))
  expect_true("I" %in% names(coefs))
  expect_true("log10_beta" %in% names(coefs))
})

test_that("fit_cp_nls fits exponential equation", {
  skip_if_not_installed("nls.multstart")

  data <- simulate_cp_exponential(n = 30, qalone = 10, I = 1.5, beta = 0.05, seed = 123)

  fit <- fit_cp_nls(data, equation = "exponential", return_all = TRUE, iter = 50)

  expect_s3_class(fit, "cp_model_nls")
  expect_equal(fit$equation, "exponential")
  expect_false(is.null(fit$model))

  # Check coefficient names match new parameterization
  coefs <- coef(fit$model)
  expect_true("log10_qalone" %in% names(coefs))
  expect_true("I" %in% names(coefs))
  expect_true("log10_beta" %in% names(coefs))
})

test_that("fit_cp_nls fits additive equation", {
  skip_if_not_installed("nls.multstart")

  data <- simulate_cp_additive(n = 30, qalone = 5, I = 10, beta = 0.05, seed = 123)

  fit <- fit_cp_nls(data, equation = "additive", return_all = TRUE, iter = 50)

  expect_s3_class(fit, "cp_model_nls")
  expect_equal(fit$equation, "additive")
  expect_false(is.null(fit$model))

  # Check coefficient names match new parameterization
  coefs <- coef(fit$model)
  expect_true("log10_qalone" %in% names(coefs))
  expect_true("I" %in% names(coefs))
  expect_true("log10_beta" %in% names(coefs))
})


# ==============================================================================
# Test: Zero handling for exponential equation
# ==============================================================================

test_that("fit_cp_nls removes zeros with warning for exponential equation", {
  skip_if_not_installed("nls.multstart")

  # Create data with some zeros
  data <- simulate_cp_exponential(n = 30, qalone = 10, I = 1.5, beta = 0.05, seed = 123)
  data$y[c(1, 5, 10)] <- 0  # Add some zeros

  # Should warn about removing zeros

  expect_warning(
    fit <- fit_cp_nls(data, equation = "exponential", return_all = TRUE, iter = 50),
    "Removing 3 observation"
  )

  # Model should still fit on remaining data
  expect_s3_class(fit, "cp_model_nls")
  expect_equal(nrow(fit$data), 27)  # 30 - 3 zeros
})

test_that("fit_cp_nls errors with insufficient data after zero removal", {
  # Create data that's mostly zeros
  data <- data.frame(x = 1:10, y = c(rep(0, 8), 1, 2))

  expect_error(
    fit_cp_nls(data, equation = "exponential", return_all = TRUE),
    "Insufficient data"
  )
})

test_that("fit_cp_nls does not remove zeros for exponentiated equation", {
  skip_if_not_installed("nls.multstart")

  # Create data with some zeros
  data <- simulate_cp_exponentiated(n = 30, qalone = 10, I = 1.5, beta = 0.05, seed = 123)
  data$y[c(1, 5, 10)] <- 0.1  # Small values, no zeros (exponentiated naturally handles near-zero)

  # Should not warn
  expect_no_warning(
    fit <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE, iter = 50)
  )

  expect_s3_class(fit, "cp_model_nls")
})


# ==============================================================================
# Test: Parameter recovery
# ==============================================================================

test_that("fit_cp_nls recovers parameters for exponentiated equation", {
  skip_if_not_installed("nls.multstart")

  # True parameters
  true_qalone <- 10
  true_I <- 1.5
  true_beta <- 0.05

  # Simulate with low noise for parameter recovery
  data <- simulate_cp_exponentiated(
    n = 100,
    qalone = true_qalone,
    I = true_I,
    beta = true_beta,
    sigma = 0.1,  # Low noise
    seed = 42
  )

  fit <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE, iter = 100)

  coefs <- coef(fit$model)
  est_qalone <- 10^coefs["log10_qalone"]
  est_I <- coefs["I"]
  est_beta <- 10^coefs["log10_beta"]

  # Check parameters are within reasonable bounds (20% tolerance)
  expect_true(abs(est_qalone - true_qalone) / true_qalone < 0.2)
  expect_true(abs(est_I - true_I) / abs(true_I) < 0.2)
  expect_true(abs(est_beta - true_beta) / true_beta < 0.2)
})

test_that("fit_cp_nls recovers parameters for exponential equation", {
  skip_if_not_installed("nls.multstart")

  # True parameters
  true_qalone <- 10
  true_I <- 1.5
  true_beta <- 0.05

  # Simulate with low noise for parameter recovery
  data <- simulate_cp_exponential(
    n = 100,
    qalone = true_qalone,
    I = true_I,
    beta = true_beta,
    sigma = 0.1,  # Low noise
    seed = 42
  )

  fit <- fit_cp_nls(data, equation = "exponential", return_all = TRUE, iter = 100)

  coefs <- coef(fit$model)
  est_qalone <- 10^coefs["log10_qalone"]
  est_I <- coefs["I"]
  est_beta <- 10^coefs["log10_beta"]

  # Check parameters are within reasonable bounds (20% tolerance)
  expect_true(abs(est_qalone - true_qalone) / true_qalone < 0.2)
  expect_true(abs(est_I - true_I) / abs(true_I) < 0.2)
  expect_true(abs(est_beta - true_beta) / true_beta < 0.2)
})


# ==============================================================================
# Test: Equation relationship (exponentiated is 10^ of exponential)
# ==============================================================================

test_that("exponentiated and exponential equations are mathematically related", {
  skip_if_not_installed("nls.multstart")

  # Use same underlying parameters
  qalone <- 10
  I <- 1.5
  beta <- 0.05

  # Generate test x values
  x_test <- c(1, 5, 10, 20, 30)

  # Exponential equation: log10(y) = log10(qalone) + I * exp(-beta * x)
  log10_y_exp <- log10(qalone) + I * exp(-beta * x_test)

  # Exponentiated equation: y = qalone * 10^(I * exp(-beta * x))
  y_expo <- qalone * 10^(I * exp(-beta * x_test))

  # The exponentiated should be 10^(exponential)
  expect_equal(y_expo, 10^log10_y_exp, tolerance = 1e-10)
})


# ==============================================================================
# Test: Summary and predict methods
# ==============================================================================

test_that("summary.cp_model_nls works with log10 parameterization", {
  skip_if_not_installed("nls.multstart")

  data <- simulate_cp_exponentiated(n = 30, qalone = 10, I = 1.5, beta = 0.05, seed = 123)
  fit <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE, iter = 50)

  summ <- summary(fit)

  expect_s3_class(summ, "summary.cp_model_nls")
  expect_true(!is.null(summ$derived_metrics))
  expect_true("qalone" %in% names(summ$derived_metrics))
  expect_true("beta" %in% names(summ$derived_metrics))
  expect_true("log10_qalone" %in% names(summ$derived_metrics))
  expect_true("log10_beta" %in% names(summ$derived_metrics))
  expect_true("qalone_se" %in% names(summ$derived_metrics))
  expect_true("beta_se" %in% names(summ$derived_metrics))
  expect_true("log10_qalone_se" %in% names(summ$derived_metrics))
  expect_true("log10_beta_se" %in% names(summ$derived_metrics))

  # Check that natural-scale values are back-transformed correctly
  expect_equal(
    summ$derived_metrics$qalone,
    10^summ$derived_metrics$log10_qalone,
    tolerance = 1e-10
  )
  expect_equal(
    summ$derived_metrics$beta,
    10^summ$derived_metrics$log10_beta,
    tolerance = 1e-10
  )

  # Delta-method SE for back-transforms (natural scale)
  expect_equal(
    summ$derived_metrics$qalone_se,
    log(10) * summ$derived_metrics$qalone * summ$derived_metrics$log10_qalone_se,
    tolerance = 1e-10
  )
  expect_equal(
    summ$derived_metrics$beta_se,
    log(10) * summ$derived_metrics$beta * summ$derived_metrics$log10_beta_se,
    tolerance = 1e-10
  )
})

test_that("predict.cp_model_nls works with log10 parameterization", {
  skip_if_not_installed("nls.multstart")

  data <- simulate_cp_exponentiated(n = 30, qalone = 10, I = 1.5, beta = 0.05, seed = 123)
  fit <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE, iter = 50)

  newdata <- data.frame(x = c(1, 5, 10, 20))
  preds <- predict(fit, newdata = newdata)

  expect_true("x" %in% names(preds))
  expect_true("y_pred" %in% names(preds))
  expect_equal(nrow(preds), 4)
  expect_true(all(preds$y_pred > 0))  # Predictions should be positive
})

test_that("predict.cp_model_nls returns log10 and natural scale for exponential", {
  skip_if_not_installed("nls.multstart")

  data <- simulate_cp_exponential(n = 30, qalone = 10, I = 1.5, beta = 0.05, seed = 123)
  fit <- fit_cp_nls(data, equation = "exponential", return_all = TRUE, iter = 50)

  newdata <- data.frame(x = c(1, 5, 10, 20))
  preds <- predict(fit, newdata = newdata)

  expect_true("y_pred" %in% names(preds))
  expect_true("y_pred_log10" %in% names(preds))

  # y_pred is returned on the natural y scale; y_pred_log10 is the internal scale
  expect_equal(preds$y_pred, 10^preds$y_pred_log10, tolerance = 1e-10)
})


# ==============================================================================
# Test: Formula consistency with EQUATIONS_CONTRACT.md
# ==============================================================================

test_that("equation_text in summary matches EQUATIONS_CONTRACT.md", {
  skip_if_not_installed("nls.multstart")

  data <- simulate_cp_exponentiated(n = 30, qalone = 10, I = 1.5, beta = 0.05, seed = 123)

  # Exponentiated
  fit_expo <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE, iter = 50)
  summ_expo <- summary(fit_expo)
  expect_true(grepl("10\\^log10_qalone", summ_expo$equation_text))
  expect_true(grepl("10\\^\\(I", summ_expo$equation_text))
  expect_true(grepl("10\\^log10_beta", summ_expo$equation_text))

  # Exponential
  data_exp <- simulate_cp_exponential(n = 30, qalone = 10, I = 1.5, beta = 0.05, seed = 123)
  fit_exp <- fit_cp_nls(data_exp, equation = "exponential", return_all = TRUE, iter = 50)
  summ_exp <- summary(fit_exp)
  expect_true(grepl("log10\\(y\\)", summ_exp$equation_text))
  expect_true(grepl("log10_qalone", summ_exp$equation_text))

  # Additive
  data_add <- simulate_cp_additive(n = 30, qalone = 5, I = 10, beta = 0.05, seed = 123)
  fit_add <- fit_cp_nls(data_add, equation = "additive", return_all = TRUE, iter = 50)
  summ_add <- summary(fit_add)
  expect_true(grepl("10\\^log10_qalone", summ_add$equation_text))
})


# ==============================================================================
# Test: Edge cases
# ==============================================================================

test_that("fit_cp_nls handles single price point gracefully", {
  data <- data.frame(x = rep(5, 10), y = runif(10, 1, 10))

  # Should either fail gracefully or fit with warning
  result <- tryCatch(
    fit_cp_nls(data, equation = "exponentiated", return_all = TRUE, iter = 20),
    error = function(e) e
  )

  # Either error or failed fit is acceptable for degenerate data

  expect_true(inherits(result, "error") || inherits(result, "cp_model_nls"))
})

test_that("fit_cp_nls handles negative I (substitutes)", {
  skip_if_not_installed("nls.multstart")

  # Simulate data with negative I (substitute relationship)
  set.seed(42)
  n <- 50
  x <- runif(n, min = 1, max = 50)
  qalone <- 20
  I <- -0.5  # Negative: alternative price increase decreases target consumption
  beta <- 0.05

  y_true <- qalone * 10^(I * exp(-beta * x))
  y <- y_true * exp(rnorm(n, 0, 0.1))
  data <- data.frame(x = x, y = y)

  fit <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE, iter = 50)

  # I should be estimated as negative
  coefs <- coef(fit$model)
  expect_true(coefs["I"] < 0)
})
