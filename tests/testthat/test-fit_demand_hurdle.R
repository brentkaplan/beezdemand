# Tests for fit_demand_hurdle() core function

test_that("fit_demand_hurdle fits 2-RE model on simulated data", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  # Simulate small dataset
  sim_data <- simulate_hurdle_data(
    n_subjects = 30,
    n_random_effects = 2,
    seed = 123
  )

  # Fit model
  fit <- suppressMessages(fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  ))

  expect_s3_class(fit, "beezdemand_hurdle")
  expect_true(fit$converged)
  expect_equal(fit$param_info$n_random_effects, 2)
  expect_equal(fit$param_info$n_subjects, 30)
})

test_that("fit_demand_hurdle recovers known simulation parameters (2-RE)", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(
    n_subjects = 60,
    n_random_effects = 2,
    stop_at_zero = FALSE,
    seed = 123,
    beta0 = -2,
    beta1 = 1,
    log_q0 = log(10),
    k = 2,
    alpha = 0.5,
    sigma_a = 0.2,
    sigma_b = 0.2,
    sigma_e = 0.1,
    rho_ab = 0
  )
  true_params <- attr(sim_data, "true_params")

  fit <- suppressMessages(suppressWarnings(fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0,
    tmb_control = list(max_iter = 150, eval_max = 500, trace = 0)
  )))
  expect_s3_class(fit, "beezdemand_hurdle")
  expect_true(fit$converged)

  s <- suppressMessages(suppressWarnings(summary(fit, report_space = "natural")))
  coefs <- s$coefficients
  coefs <- coefs[coefs$term %in% c("beta0", "beta1", "Q0", "k", "alpha"), ]

  expect_equal(coefs$estimate[coefs$term == "beta0"], true_params$beta0, tolerance = 0.4)
  expect_equal(coefs$estimate[coefs$term == "beta1"], true_params$beta1, tolerance = 0.4)
  expect_equal(coefs$estimate[coefs$term == "Q0"], exp(true_params$log_q0), tolerance = 1.0)
  expect_equal(coefs$estimate[coefs$term == "k"], true_params$k, tolerance = 0.2)
  expect_equal(coefs$estimate[coefs$term == "alpha"], true_params$alpha, tolerance = 0.1)
})

test_that("fit_demand_hurdle fits 3-RE model on simulated data", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  # Simulate larger dataset - 3-RE model needs more subjects (12 params)
  sim_data <- simulate_hurdle_data(
    n_subjects = 100,
    n_random_effects = 3,
    seed = 456
  )

  # Fit model - suppress optimization warnings that can occur during search
  fit <- suppressMessages(suppressWarnings(fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )))

  expect_s3_class(fit, "beezdemand_hurdle")
  expect_equal(fit$param_info$n_random_effects, 3)
  expect_true("c_i" %in% colnames(fit$random_effects))
})

test_that("fit_demand_hurdle errors on invalid inputs", {
  skip_on_cran()

  sim_data <- simulate_hurdle_data(n_subjects = 20, seed = 123)

  # Missing column

  expect_error(
    fit_demand_hurdle(
      sim_data,
      y_var = "nonexistent",
      x_var = "x",
      id_var = "id",
      verbose = 0
    ),
    "Missing columns"
  )

  # Invalid random_effects
  expect_error(
    fit_demand_hurdle(
      sim_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      random_effects = c("invalid"),
      verbose = 0
    ),
    "random_effects must be a subset"
  )

  # Missing required random effects
  expect_error(
    fit_demand_hurdle(
      sim_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      random_effects = c("zeros"),
      verbose = 0
    ),
    "must include at least"
  )
})

test_that("fit_demand_hurdle handles missing data with warning", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)

  # Add some NAs
  sim_data$y[1:5] <- NA

  expect_warning(
    fit <- fit_demand_hurdle(
      sim_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      random_effects = c("zeros", "q0"),
      verbose = 0
    ),
    "rows removed due to missing"
  )

  expect_s3_class(fit, "beezdemand_hurdle")
})

test_that("fit_demand_hurdle accepts custom start values", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)

  custom_starts <- list(
    beta0 = -3.0,
    beta1 = 1.5,
    log_q0 = log(8),
    log_k = log(2.5),
    log_alpha = log(0.3),  # log-space alpha per EQUATIONS_CONTRACT.md
    logsigma_a = 0.3,
    logsigma_b = -0.3,
    logsigma_e = -0.8,
    rho_ab_raw = 0.1
  )

  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    start_values = custom_starts,
    verbose = 0
  )

  expect_s3_class(fit, "beezdemand_hurdle")
})

test_that("fit_demand_hurdle works with different column names", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)

  # Rename columns
  names(sim_data)[names(sim_data) == "id"] <- "subject_id"
  names(sim_data)[names(sim_data) == "x"] <- "price"
  names(sim_data)[names(sim_data) == "y"] <- "consumption"

  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "consumption",
    x_var = "price",
    id_var = "subject_id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  expect_s3_class(fit, "beezdemand_hurdle")
  expect_equal(fit$param_info$y_var, "consumption")
  expect_equal(fit$param_info$x_var, "price")
  expect_equal(fit$param_info$id_var, "subject_id")
})

test_that("fit_demand_hurdle respects tmb_control options", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)

  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    tmb_control = list(max_iter = 50, eval_max = 200),
    verbose = 0
  )

  # Should still return a fit object (may or may not converge with limited iterations)
  expect_s3_class(fit, "beezdemand_hurdle")
})

test_that("fit_demand_hurdle returns correct structure", {
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

  # Check structure
  expect_true(is.list(fit$model))
  expect_true("coefficients" %in% names(fit$model))
  expect_true("se" %in% names(fit$model))
  expect_true(is.matrix(fit$random_effects))
  expect_true(is.data.frame(fit$subject_pars))
  expect_true(is.logical(fit$converged))
  expect_true(is.numeric(fit$loglik))
  expect_true(is.numeric(fit$AIC))
  expect_true(is.numeric(fit$BIC))

  # Check subject_pars columns
  expect_true(all(
    c("id", "a_i", "b_i", "Q0", "alpha", "breakpoint", "Pmax", "Omax") %in%
      names(fit$subject_pars)
  ))
})

test_that("validate_hurdle_data catches invalid data", {
  # Non-data.frame
  expect_error(
    validate_hurdle_data(list(x = 1), "y", "x", "id"),
    "must be a data.frame"
  )

  # Missing columns
  df <- data.frame(x = 1:10, y = 1:10)
  expect_error(
    validate_hurdle_data(df, "y", "x", "id"),
    "Missing columns"
  )

  # Non-numeric y
  df <- data.frame(id = 1, x = 1:10, y = letters[1:10])
  expect_error(
    validate_hurdle_data(df, "y", "x", "id"),
    "must be numeric"
  )

  # Real negative consumption (beyond tolerance)
  df <- data.frame(id = 1, x = 1:10, y = c(-1, 1:9))
  expect_error(
    validate_hurdle_data(df, "y", "x", "id"),
    "negative value"
  )

  # Insufficient observations (< 10)
  df <- data.frame(id = rep(1:2, each = 3), x = rep(1:3, 2), y = rep(1:3, 2))
  expect_error(
    validate_hurdle_data(df, "y", "x", "id"),
    "Insufficient data"
  )

  # Single subject (need at least 2 for mixed effects)
  df <- data.frame(id = rep(1, 15), x = 1:15, y = 1:15)
  expect_error(
    validate_hurdle_data(df, "y", "x", "id"),
    "at least 2 subjects"
  )
})

test_that("validate_hurdle_data clamps near-zero negatives with message", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  data(apt, package = "beezdemand")
  apt_mod <- apt
  # Set some zero values to near-zero negatives
  zero_idx <- which(apt_mod$y == 0)
  if (length(zero_idx) >= 3) {
    apt_mod$y[zero_idx[1:3]] <- c(-1e-15, -5e-12, -1e-11)
  }

  expect_message(
    fit_demand_hurdle(apt_mod, y_var = "y", x_var = "x", id_var = "id",
                      random_effects = c("zeros", "q0"), verbose = 0),
    "clamped to 0"
  )
})

test_that("validate_hurdle_data rejects real negatives with informative error", {
  data(apt, package = "beezdemand")
  apt_mod <- apt
  apt_mod$y[1:3] <- c(-5, -10, -2)

  expect_error(
    fit_demand_hurdle(apt_mod, y_var = "y", x_var = "x", id_var = "id"),
    "3 negative value"
  )
})

test_that("validate_hurdle_data rejects NaN values", {
  data(apt, package = "beezdemand")
  apt_mod <- apt
  apt_mod$y[1] <- NaN

  expect_error(
    fit_demand_hurdle(apt_mod, y_var = "y", x_var = "x", id_var = "id"),
    "NaN"
  )
})
