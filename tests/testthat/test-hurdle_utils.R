# Tests for utility functions

test_that("calc_omax_pmax returns correct values for valid parameters", {
  # With k > e, a strict interior maximum exists
  result <- calc_omax_pmax(Q0 = 10, k = 3, alpha = 0.5)

  expect_true(is.list(result))
  expect_true(all(c("Pmax", "Omax", "Qmax") %in% names(result)))
  expect_true(is.numeric(result$Pmax))
  expect_true(is.numeric(result$Omax))
  expect_true(result$Pmax > 0)
  expect_true(result$Omax > 0)
})

test_that("calc_omax_pmax handles k < e with numerical fallback", {
  # With k < e, no closed-form Lambert W solution exists,

  # but numerical optimization can still find the maximum.
  # Per EQUATIONS_CONTRACT.md: implementation should fall back to numerical methods.

  # Expect a warning about k <= e and numerical optimization
  expect_warning(
    result <- calc_omax_pmax(Q0 = 10, k = 2, alpha = 0.5),
    "k.*<= e.*numerical optimization"
  )

  # Numerical fallback should still return valid values
  expect_true(is.numeric(result$Pmax))
  expect_true(is.numeric(result$Omax))
  expect_true(!is.na(result$Pmax))
  expect_true(!is.na(result$Omax))
  expect_true(result$Pmax > 0)
  expect_true(result$Omax > 0)

  # Should have a note explaining the fallback
  expect_true(!is.null(result$note))
})

test_that("calc_omax_pmax treats k = e as no strict interior maximum", {
  # At exactly k = e the two stationary points of expenditure merge into a
  # tangent inflection, so no strict interior maximum exists. The boundary
  # must take the bounded-range fallback (warning + note), not the analytic
  # root path, matching the engine's strict threshold (NEWS 0.3.0).
  expect_warning(
    result <- calc_omax_pmax(Q0 = 10, k = exp(1), alpha = 0.5),
    "no strict interior maximum"
  )

  expect_true(!is.null(result$note))
  expect_match(result$note, "bounded-range")
  expect_true(is.numeric(result$Pmax))
})

test_that("calc_omax_pmax handles edge cases", {
  # NA alpha
  result1 <- calc_omax_pmax(Q0 = 10, k = 3, alpha = NA)
  expect_true(is.na(result1$Pmax))

  # Zero alpha
  result2 <- calc_omax_pmax(Q0 = 10, k = 3, alpha = 0)
  expect_true(is.na(result2$Pmax))

  # NA Q0
  result3 <- calc_omax_pmax(Q0 = NA, k = 3, alpha = 0.5)
  expect_true(is.na(result3$Pmax))
})

test_that("calc_omax_pmax_vec works for multiple subjects", {
  Q0_vec <- c(10, 15, 20)
  k_vec <- c(3, 3, 3)
  alpha_vec <- c(0.5, 0.3, 0.1)

  result <- calc_omax_pmax_vec(Q0_vec, k_vec, alpha_vec)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true(all(c("Pmax", "Omax", "Qmax") %in% names(result)))
})

test_that("calc_group_metrics works on fitted model", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  metrics <- calc_group_metrics(fit)

  expect_true(is.list(metrics))
  expect_true(all(c("Pmax", "Omax", "Qmax") %in% names(metrics)))
})

test_that("get_subject_pars returns data frame", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  pars <- get_subject_pars(fit)

  expect_true(is.data.frame(pars))
  expect_equal(nrow(pars), 30)
  expect_true(all(c("Q0", "alpha", "Pmax", "Omax") %in% names(pars)))
})

test_that("compare_hurdle_models performs LRT correctly", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  # Simulate data with 3 RE
  sim_data <- simulate_hurdle_data(
    n_subjects = 50,
    n_random_effects = 3,
    seed = 789
  )

  # Fit both models
  fit3 <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )
  fit2 <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Compare
  result <- expect_output(
    compare_hurdle_models(fit3, fit2),
    "Likelihood Ratio Test"
  )

  expect_true(is.list(result))
  expect_true("lr_stat" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("model_comparison" %in% names(result))
  expect_true(result$lr_stat >= 0) # LRT stat should be non-negative
  expect_true(result$df > 0)
})

test_that("compare_hurdle_models errors for non-hurdle objects", {
  expect_error(
    compare_hurdle_models(list(), list()),
    "must be beezdemand_hurdle objects"
  )
})

test_that("zhao_exponential Pmax values are finite and positive (2-RE)", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 42)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    part2 = "zhao_exponential",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  pars <- get_subject_pars(fit)
  expect_true(all(is.finite(pars$Pmax)), info = "All 2-RE Pmax values should be finite")
  expect_true(all(pars$Pmax > 0), info = "All 2-RE Pmax values should be positive")
  expect_true(all(is.finite(pars$Omax)), info = "All 2-RE Omax values should be finite")
  expect_true(all(pars$Omax > 0), info = "All 2-RE Omax values should be positive")
})

test_that("zhao_exponential Pmax values are finite and positive (3-RE)", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(
    n_subjects = 50,
    n_random_effects = 3,
    seed = 789
  )
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    part2 = "zhao_exponential",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )

  pars <- get_subject_pars(fit)
  expect_true(all(is.finite(pars$Pmax)), info = "All 3-RE Pmax values should be finite")
  expect_true(all(pars$Pmax > 0), info = "All 3-RE Pmax values should be positive")
  expect_true(all(is.finite(pars$Omax)), info = "All 3-RE Omax values should be finite")
  expect_true(all(pars$Omax > 0), info = "All 3-RE Omax values should be positive")
})

# TICKET-061: .hurdle_chol_or_fallback() must warn (classed) when it falls
# back to an uncorrelated diagonal covariance, and stay silent when Sigma is
# genuinely positive definite.

test_that(".hurdle_chol_or_fallback warns once when Sigma is not PD", {
  # Jointly-impossible pairwise correlations (audit mechanism repro,
  # TICKET-061): eigenvalues 1.9, 1.9, -0.8 -> chol() errors.
  Sigma <- matrix(c(1, .9, .9, .9, 1, -.9, .9, -.9, 1), nrow = 3)
  sigma_diag <- c(1, 1, 1)

  expect_warning(
    L <- beezdemand:::.hurdle_chol_or_fallback(Sigma, sigma_diag),
    class = "beezdemand_hurdle_chol_fallback_warning"
  )
  # Fallback is the Cholesky factor of the diagonal fallback: L'L == identity.
  expect_equal(unname(t(L) %*% L), diag(sigma_diag), tolerance = 1e-10)
})

test_that(".hurdle_chol_or_fallback is silent for a positive-definite Sigma", {
  Sigma <- matrix(c(1, .3, .3, 1), nrow = 2)
  sigma_diag <- c(1, 1)

  expect_no_warning(L <- beezdemand:::.hurdle_chol_or_fallback(Sigma, sigma_diag))
  expect_equal(unname(t(L) %*% L), Sigma, tolerance = 1e-10)
})
