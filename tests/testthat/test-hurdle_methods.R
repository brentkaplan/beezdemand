# Tests for S3 methods for beezdemand_hurdle class

test_that("print.beezdemand_hurdle produces output", {
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

  expect_output(print(fit), "Two-Part Mixed Effects")
  expect_output(print(fit), "Convergence")
  expect_output(print(fit), "Fixed Effects")
})

test_that("summary.beezdemand_hurdle returns expected structure", {
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

  summ <- summary(fit)

  expect_s3_class(summ, "summary.beezdemand_hurdle")
  expect_true("coefficients" %in% names(summ))
  expect_true("variance_components" %in% names(summ))
  expect_true("correlations" %in% names(summ))
  expect_true("logLik" %in% names(summ))
  expect_true("AIC" %in% names(summ))
  expect_true("BIC" %in% names(summ))
  expect_true("group_metrics" %in% names(summ))
  expect_true("individual_metrics" %in% names(summ))
})

test_that("print.summary.beezdemand_hurdle produces formatted output", {
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

  summ <- summary(fit)

  expect_output(print(summ), "Two-Part Mixed Effects")
  expect_output(print(summ), "Fixed Effects")
  expect_output(print(summ), "Variance Components")
  expect_output(print(summ), "Model Fit")
  expect_output(print(summ), "Demand Metrics")
})

test_that("coef.beezdemand_hurdle extracts coefficients", {
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

  coefs <- coef(fit)

  expect_true(is.numeric(coefs))
  expect_true(length(coefs) > 0)
  expect_true(all(c("beta0", "beta1", "logQ0", "k", "alpha") %in% names(coefs)))
})

test_that("logLik returns correct class and attributes", {
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

  ll <- logLik(fit)

  expect_s3_class(ll, "logLik")
  expect_true(!is.null(attr(ll, "df")))
  expect_true(!is.null(attr(ll, "nobs")))
  expect_equal(as.numeric(ll), fit$loglik)
})

test_that("AIC and BIC compute correctly", {
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

  aic_val <- AIC(fit)
  bic_val <- BIC(fit)

  expect_true(is.numeric(aic_val))
  expect_true(is.numeric(bic_val))
  expect_equal(aic_val, fit$AIC)
  expect_equal(bic_val, fit$BIC)

  # BIC should generally be larger than AIC for reasonable sample sizes
  expect_true(bic_val >= aic_val || fit$param_info$n_obs < 8)
})

test_that("predict type='parameters' returns subject_pars", {
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

  pars <- predict(fit, type = "parameters")

  expect_true(is.data.frame(pars))
  expect_equal(nrow(pars), fit$param_info$n_subjects)
  expect_true(all(
    c("Q0", "alpha", "breakpoint", "Pmax", "Omax") %in% names(pars)
  ))
})

test_that("predict type='demand' returns predictions", {
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

  prices <- c(0, 1, 2, 5)
  pred <- predict(fit, type = "demand", prices = prices)

  expect_true(is.data.frame(pred))
  expect_equal(nrow(pred), fit$param_info$n_subjects * length(prices))
  expect_true(all(
    c("predicted_consumption", "prob_zero", "expected_consumption") %in%
      names(pred)
  ))
})

test_that("predict type='probability' returns probabilities", {
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

  prices <- c(0, 1, 2, 5)
  prob <- predict(fit, type = "probability", prices = prices)

  expect_true(is.data.frame(prob))
  expect_true("prob_zero" %in% names(prob))
  expect_true(all(prob$prob_zero >= 0 & prob$prob_zero <= 1))
})

test_that("get_hurdle_param_summary returns summary tibble", {
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

  summ <- get_hurdle_param_summary(fit)

  expect_true(is.data.frame(summ))
  expect_true(all(
    c("parameter", "mean", "sd", "median", "lcl", "ucl") %in% names(summ)
  ))
  expect_true(all(
    c("Q0", "alpha", "breakpoint", "Pmax", "Omax") %in% summ$parameter
  ))
})
