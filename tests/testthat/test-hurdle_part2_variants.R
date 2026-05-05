# Tests for hurdle Part II variants added in WS4.4 / WS4.5.
# `simulate_hurdle_part2_data()` is defined in helper-hurdle-part2.R and
# auto-sourced by testthat before tests run.

test_that("fit_demand_hurdle supports HS-standardized Part II (part2 = 'exponential')", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_part2_data(
    n_subjects = 40,
    prices = seq(0, 5, by = 0.5),
    part2 = "exponential",
    seed = 123
  )

  fit <- suppressWarnings(fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    part2 = "exponential",
    verbose = 0
  ))

  expect_s3_class(fit, "beezdemand_hurdle")
  expect_true(isTRUE(fit$converged))
  expect_equal(fit$param_info$part2, "exponential")

  # Derived metrics compute without error
  group_metrics <- calc_group_metrics(fit)
  expect_true(is.finite(group_metrics$Pmax))
  expect_true(is.finite(group_metrics$Omax))

  # Sanity: setting Q0 = 1 reduces to the unstandardized Zhao mean (same alpha,k)
  coefs <- fit$model$coefficients
  alpha_hat <- exp(unname(coefs[["log_alpha"]]))
  k_hat <- exp(unname(coefs[["log_k"]]))
  log_q0_hat <- unname(coefs[["log_q0"]])
  p_vec <- c(0, 1, 2)

  mu_stdq0_q0is1 <- log_q0_hat + k_hat * (exp(-alpha_hat * 1 * p_vec) - 1)
  mu_zhao <- log_q0_hat + k_hat * (exp(-alpha_hat * p_vec) - 1)
  expect_equal(as.numeric(mu_stdq0_q0is1), as.numeric(mu_zhao))

  # Higher-Q0 subjects drop faster at the same alpha under HS-standardization
  subject_pars <- fit$subject_pars
  id_hi <- subject_pars$id[[which.max(subject_pars$Q0)]]
  id_lo <- subject_pars$id[[which.min(subject_pars$Q0)]]

  preds <- predict(fit, type = "demand", prices = c(0, 1))
  mu_hi <- preds$predicted_log_consumption[preds$id == id_hi]
  mu_lo <- preds$predicted_log_consumption[preds$id == id_lo]

  expect_equal(length(mu_hi), 2)
  expect_equal(length(mu_lo), 2)
  expect_lt(diff(mu_hi), diff(mu_lo))
})

test_that("fit_demand_hurdle supports SND Part II (part2 = 'simplified_exponential')", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_part2_data(
    n_subjects = 40,
    prices = seq(0, 5, by = 0.5),
    part2 = "simplified_exponential",
    seed = 456
  )

  fit <- suppressWarnings(fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    part2 = "simplified_exponential",
    verbose = 0
  ))

  expect_s3_class(fit, "beezdemand_hurdle")
  expect_true(isTRUE(fit$converged))
  expect_equal(fit$param_info$part2, "simplified_exponential")
  expect_false("log_k" %in% names(fit$model$coefficients))

  # Part II mean is log-linear in price
  preds <- predict(fit, type = "demand", prices = c(0, 1, 2, 3))
  one_id <- preds$id[[1]]
  mu <- preds$predicted_log_consumption[preds$id == one_id]
  expect_equal(length(mu), 4)
  expect_equal(diff(mu), rep(diff(mu)[[1]], 3), tolerance = 1e-6)

  # SND Pmax branch is used and returns finite values when alpha,Q0>0
  s <- summary(fit)
  expect_equal(s$pmax_method_info$method_model, "analytic_snd")
  expect_true(is.finite(s$group_metrics$Pmax))
  expect_true(is.finite(s$group_metrics$Omax))
})

# ---------------------------------------------------------------------------
# TICKET-011 Phase 4 test gap close: 3RE x exponential and 3RE x
# simplified_exponential were "supported on paper" pre-Phase-4 — the
# template files HurdleDemand3RE_StdQ0.h and HurdleDemand3RE_SND.h
# existed and were dispatched by R/hurdle-demand.R, but had zero
# dedicated isolation tests. These tests establish a regression
# baseline before the Phase 4 consolidation rewrites the templates.
# ---------------------------------------------------------------------------

test_that("fit_demand_hurdle supports 3RE x HS-standardized Part II (part2 = 'exponential')", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_part2_data(
    n_subjects = 60,
    prices = seq(0, 5, by = 0.5),
    part2 = "exponential",
    seed = 124
  )

  fit <- suppressWarnings(fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    part2 = "exponential",
    verbose = 0
  ))

  expect_s3_class(fit, "beezdemand_hurdle")
  expect_true(isTRUE(fit$converged))
  expect_equal(fit$param_info$part2, "exponential")
  expect_equal(fit$param_info$n_random_effects, 3L)

  # 3RE: per-subject alpha varies via c_i.
  expect_true("c_i" %in% names(fit$subject_pars))
  expect_true(stats::var(fit$subject_pars$c_i) > 0)

  # Group metrics finite.
  group_metrics <- calc_group_metrics(fit)
  expect_true(is.finite(group_metrics$Pmax))
  expect_true(is.finite(group_metrics$Omax))

  # log_k present in coef table for HS-standardized part II.
  expect_true("log_k" %in% names(fit$model$coefficients))
})

test_that("fit_demand_hurdle supports 3RE x SND Part II (part2 = 'simplified_exponential')", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_part2_data(
    n_subjects = 60,
    prices = seq(0, 5, by = 0.5),
    part2 = "simplified_exponential",
    seed = 457
  )

  fit <- suppressWarnings(fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    part2 = "simplified_exponential",
    verbose = 0
  ))

  expect_s3_class(fit, "beezdemand_hurdle")
  expect_true(isTRUE(fit$converged))
  expect_equal(fit$param_info$part2, "simplified_exponential")
  expect_equal(fit$param_info$n_random_effects, 3L)
  expect_false("log_k" %in% names(fit$model$coefficients))

  # 3RE: per-subject alpha varies via c_i.
  expect_true("c_i" %in% names(fit$subject_pars))
  expect_true(stats::var(fit$subject_pars$c_i) > 0)

  # SND Pmax/Omax finite.
  s <- summary(fit)
  expect_equal(s$pmax_method_info$method_model, "analytic_snd")
  expect_true(is.finite(s$group_metrics$Pmax))
  expect_true(is.finite(s$group_metrics$Omax))
})

