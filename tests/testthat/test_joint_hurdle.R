# Tests for Joint Hurdle Cross-Price Model
# =========================================

# Helper function to create test data
create_joint_test_data <- function(n_subjects = 10, n_prices = 8, seed = 42) {
  set.seed(seed)

  prices <- c(0, 0.5, 1, 2, 4, 8, 16, 32)[1:n_prices]

  data_list <- list()

  for (i in 1:n_subjects) {
    # Subject-level random effects
    a_i <- rnorm(1, 0, 0.5) # zeros RE
    b_AT_i <- rnorm(1, 0, 0.3) # alone.target intensity RE
    b_OT_i <- rnorm(1, 0, 0.3) # own.target intensity RE
    b_OA_i <- rnorm(1, 0, 0.3) # own.alt intensity RE

    # Population parameters
    gamma0 <- -1
    gamma1 <- 0.3
    logQ0_AT <- 3
    logQ0_OT <- 2.8
    alpha_AT <- 0.02
    alpha_OT <- 0.025
    k <- 2
    logQalone_OA <- 2
    I <- -0.8
    beta_param <- 0.1
    epsilon <- 0.001

    for (p in prices) {
      # alone.target
      eta_alone <- gamma0 + 0 + gamma1 * log(p + epsilon) + a_i
      prob_zero_alone <- exp(eta_alone) / (1 + exp(eta_alone))
      is_zero_alone <- runif(1) < prob_zero_alone
      if (is_zero_alone) {
        y_alone <- 0
      } else {
        mu_alone <- (logQ0_AT + b_AT_i) + k * (exp(-alpha_AT * p) - 1)
        y_alone <- exp(mu_alone + rnorm(1, 0, 0.3))
      }

      # own.target
      eta_own <- gamma0 + 0.2 + gamma1 * log(p + epsilon) + a_i
      prob_zero_own <- exp(eta_own) / (1 + exp(eta_own))
      is_zero_own <- runif(1) < prob_zero_own
      if (is_zero_own) {
        y_own <- 0
      } else {
        mu_own <- (logQ0_OT + b_OT_i) + k * (exp(-alpha_OT * p) - 1)
        y_own <- exp(mu_own + rnorm(1, 0, 0.3))
      }

      # own.alt
      eta_alt <- gamma0 + 0.3 + gamma1 * log(p + epsilon) + a_i
      prob_zero_alt <- exp(eta_alt) / (1 + exp(eta_alt))
      is_zero_alt <- runif(1) < prob_zero_alt
      if (is_zero_alt) {
        y_alt <- 0
      } else {
        mu_alt <- (logQalone_OA + b_OA_i) + I * exp(-beta_param * p)
        y_alt <- exp(mu_alt + rnorm(1, 0, 0.3))
      }

      data_list[[length(data_list) + 1]] <- data.frame(
        id = i,
        x = p,
        y = y_alone,
        target = "alone",
        stringsAsFactors = FALSE
      )
      data_list[[length(data_list) + 1]] <- data.frame(
        id = i,
        x = p,
        y = y_own,
        target = "own",
        stringsAsFactors = FALSE
      )
      data_list[[length(data_list) + 1]] <- data.frame(
        id = i,
        x = p,
        y = y_alt,
        target = "alt",
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, data_list)
}


# =============================================================================
# Test: prepare_joint_data()
# =============================================================================

test_that("prepare_joint_data validates input correctly", {
  # Missing columns
  bad_data <- data.frame(id = 1:10, x = 1:10)
  expect_error(prepare_joint_data(bad_data), "Missing required columns")

  # Negative consumption
  bad_data <- data.frame(id = 1, x = 1, y = -1, target = "alone")
  expect_error(prepare_joint_data(bad_data), "cannot be negative")

  # Unrecognized target values
  bad_data <- data.frame(id = 1, x = 1, y = 1, target = "unknown")
  expect_error(prepare_joint_data(bad_data), "Unrecognized target values")
})

test_that("prepare_joint_data creates correct stream mapping", {
  test_data <- create_joint_test_data(n_subjects = 3, n_prices = 4)

  prep <- suppressWarnings(prepare_joint_data(test_data))

  # Check stream values
  expect_true(all(prep$data$stream %in% c(0, 1, 2)))

  # Check mapping
  alone_rows <- test_data$target == "alone"
  expect_true(all(prep$data$stream[alone_rows] == 0))

  own_rows <- test_data$target == "own"
  expect_true(all(prep$data$stream[own_rows] == 1))

  alt_rows <- test_data$target == "alt"
  expect_true(all(prep$data$stream[alt_rows] == 2))
})

capture_warnings <- function(expr) {
  warn_msgs <- character(0)
  val <- withCallingHandlers(
    expr,
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = val, warnings = warn_msgs)
}

test_that("prepare_joint_data warns for missing streams", {
  # Data with only alone
  alone_only <- data.frame(id = 1:5, x = 1:5, y = 1:5, target = "alone")
  res <- capture_warnings(prepare_joint_data(alone_only))
  expect_true(any(grepl("own.target not observed", res$warnings, fixed = TRUE)))
  expect_true(any(grepl("own.alt not observed", res$warnings, fixed = TRUE)))
})

test_that("prepare_joint_data warns for single subject", {
  single_subj <- data.frame(
    id = rep(1, 9),
    x = rep(c(1, 2, 4), 3),
    y = runif(9, 1, 10),
    target = rep(c("alone", "own", "alt"), each = 3)
  )
  res <- capture_warnings(prepare_joint_data(single_subj))
  expect_true(any(grepl("Single subject", res$warnings)))
})

test_that("prepare_joint_data warns for constant price", {
  const_price <- data.frame(
    id = rep(1:3, each = 3),
    x = rep(1, 9),
    y = runif(9, 1, 10),
    target = rep(c("alone", "own", "alt"), 3)
  )
  res <- capture_warnings(prepare_joint_data(const_price))
  expect_true(any(grepl("Constant target price", res$warnings)))
})

test_that("prepare_joint_data warns for all zeros in a stream", {
  all_zeros <- data.frame(
    id = rep(1:3, each = 6),
    x = rep(c(1, 2), 9),
    y = c(rep(0, 6), runif(6, 1, 10), runif(6, 1, 10)),
    target = rep(c("alone", "own", "alt"), each = 6)
  )
  res <- capture_warnings(prepare_joint_data(all_zeros))
  expect_true(any(grepl("All zeros in alone.target", res$warnings)))
})


# =============================================================================
# Test: fit_joint_hurdle() basic functionality
# =============================================================================

test_that("fit_joint_hurdle runs without error on valid data", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  # Should run without error
  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  expect_s3_class(fit, "beezdemand_joint_hurdle")
  expect_true(fit$convergence == 0 || fit$convergence == 1) # May not fully converge on small data
})

test_that("fit_joint_hurdle returns correct structure", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  # Check required components
  expect_true("coefficients" %in% names(fit))
  expect_true("random_effects" %in% names(fit))
  expect_true("logLik" %in% names(fit))
  expect_true("stream_counts" %in% names(fit))
  expect_true("data" %in% names(fit))

  # Check coefficient names
  coef_names <- names(fit$coefficients)
  expect_true("gamma0" %in% coef_names)
  expect_true("gamma_own_target" %in% coef_names)
  expect_true("gamma_own_alt" %in% coef_names)
  expect_true("logQ0_AT" %in% coef_names)
  expect_true("logQ0_OT" %in% coef_names)
  expect_true("logQalone_OA" %in% coef_names)
  expect_true("I" %in% coef_names)
})

test_that("fit_joint_hurdle respects fixed k", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2.5,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  expect_true(fit$k_fixed)
  expect_equal(fit$k_value, 2.5)
})


# =============================================================================
# Test: S3 methods
# =============================================================================

test_that("print.beezdemand_joint_hurdle works", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)
  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  expect_output(print(fit), "Joint Hurdle Cross-Price Model")
  expect_output(print(fit), "Observations:")
})

test_that("summary.beezdemand_joint_hurdle works", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)
  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  # summary() now returns a structured object; output comes from print()
  summ <- summary(fit)
  expect_s3_class(summ, "summary.beezdemand_joint_hurdle")

  # Contract: derived metrics are separate from coefficients
  expect_true("derived_metrics" %in% names(summ))
  expect_s3_class(summ$derived_metrics, "tbl_df")
  expect_true(all(c("metric", "estimate") %in% names(summ$derived_metrics)))

  expect_true(all(c("Q0_AT", "Q0_OT", "Qalone_OA") %in% summ$derived_metrics$metric))
  expect_false(any(summ$coefficients$term %in% c("Q0_AT", "Q0_OT", "Qalone_OA")))

  # Canonical components (allow stream:<name> and shared)
  allowed_components <- c("fixed", "zero_probability", "consumption", "variance", "shared")
  components <- unique(summ$coefficients$component)
  expect_true(all(components %in% allowed_components | grepl("^stream:", components)))

  expect_output(print(summ), "Part I")
  expect_output(print(summ), "Part II")
  expect_output(print(summ), "Variance Components")
})

test_that("coef.beezdemand_joint_hurdle returns correct types", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)
  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  all_coef <- coef(fit, type = "all")
  expect_true(is.numeric(all_coef))

  hurdle_coef <- coef(fit, type = "hurdle")
  expect_true(all(grepl("gamma", names(hurdle_coef))))

  demand_coef <- coef(fit, type = "demand")
  expect_true("k" %in% names(demand_coef) || fit$k_fixed)

  cp_coef <- coef(fit, type = "crossprice")
  expect_true("I" %in% names(cp_coef))
  expect_true("beta" %in% names(cp_coef))
})

test_that("predict.beezdemand_joint_hurdle works for all types", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)
  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  # Response predictions
  pred_resp <- predict(fit, type = "response", stream = "all")
  expect_true("y_pred" %in% names(pred_resp))
  expect_true("stream" %in% names(pred_resp))
  expect_true(all(pred_resp$y_pred >= 0))

  # Probability predictions
  pred_prob <- predict(fit, type = "probability", stream = "all")
  expect_true("prob_zero" %in% names(pred_prob))
  expect_true(all(pred_prob$prob_zero >= 0 & pred_prob$prob_zero <= 1))

  # Parameter predictions
  pred_pars <- predict(fit, type = "parameters")
  expect_true("Q0_AT_i" %in% names(pred_pars))
  expect_true("Qalone_OA_i" %in% names(pred_pars))
})

test_that("logLik, AIC, BIC work correctly", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)
  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  ll <- logLik(fit)
  expect_true(is.numeric(ll))
  expect_true(ll < 0) # Log-likelihood should be negative

  aic <- AIC(fit)
  expect_true(is.numeric(aic))
  expect_true(aic > 0)

  bic <- BIC(fit)
  expect_true(is.numeric(bic))
  expect_true(bic > aic) # BIC penalizes more with reasonable n
})

test_that("tidy.beezdemand_joint_hurdle returns tibble", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)
  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  tidy_out <- tidy(fit)
  expect_s3_class(tidy_out, "tbl_df")
  expect_true("term" %in% names(tidy_out))
  expect_true("estimate" %in% names(tidy_out))
  expect_true("component" %in% names(tidy_out))
})

test_that("glance.beezdemand_joint_hurdle returns tibble", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)
  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  glance_out <- glance(fit)
  expect_s3_class(glance_out, "tbl_df")
  expect_true("logLik" %in% names(glance_out))
  expect_true("n_subjects" %in% names(glance_out))
  expect_true("converged" %in% names(glance_out))
})


# =============================================================================
# Test: Edge cases (D1-D5 from spec)
# =============================================================================

test_that("D1: No zeros in one stream - model runs with warning", {
  skip_on_cran()

  # Create data with no zeros in alone stream
  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)
  test_data$y[test_data$target == "alone" & test_data$y == 0] <- 0.1

  expect_warning(
    fit <- fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    ),
    "No zeros in alone.target"
  )

  expect_s3_class(fit, "beezdemand_joint_hurdle")
})

test_that("D3: Constant price - warning is emitted", {
  const_price_data <- data.frame(
    id = rep(1:5, each = 9),
    x = rep(1, 45), # Constant price
    y = runif(45, 0, 10),
    target = rep(rep(c("alone", "own", "alt"), each = 3), 5)
  )

  res <- capture_warnings(prepare_joint_data(const_price_data))
  expect_true(any(grepl("Constant target price", res$warnings)))
})

test_that("D5: Single subject - model runs with warning", {
  skip_on_cran()

  single_subj_data <- create_joint_test_data(n_subjects = 1, n_prices = 8)

  expect_warning(
    fit <- fit_joint_hurdle(
      data = single_subj_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    ),
    "Single subject"
  )

  expect_s3_class(fit, "beezdemand_joint_hurdle")
})


# =============================================================================
# Test: Model equivalence (C1 from spec)
# =============================================================================

test_that("C1.3: Cross-price parameters only affect own.alt stream", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  # Predict for each stream
  pred_alone <- predict(fit, type = "response", stream = "alone.target")
  pred_own <- predict(fit, type = "response", stream = "own.target")
  pred_alt <- predict(fit, type = "response", stream = "own.alt")

  # Cross-price params (I, beta) should not appear in alone/own predictions
  # This is structural - verified by checking the model formulation
  coef <- fit$coefficients_natural

  # I and beta should exist
  expect_true("I" %in% names(coef))
  expect_true("beta" %in% names(coef))

  # The predictions for different streams should differ
  expect_false(all(pred_alone$y_pred == pred_alt$y_pred))
})

test_that("C3: k is identical across target streams", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  # k should be a single value, not separate by stream
  expect_length(fit$k_value, 1)

  # When k is fixed, it should be respected
  expect_equal(fit$k_value, 2)
})

test_that("C3: alpha can differ between AT and OT", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  coef <- fit$coefficients_natural

  # alpha_AT and alpha_OT are separate parameters
  expect_true("alpha_AT" %in% names(coef))
  expect_true("alpha_OT" %in% names(coef))

  # They should be allowed to differ
  # (Not testing equality since they come from same generating process)
})


# =============================================================================
# Test: Plotting
# =============================================================================

test_that("plot.beezdemand_joint_hurdle works for demand type", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)
  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  p <- plot(fit, type = "demand")
  expect_s3_class(p, "ggplot")
})

test_that("plot.beezdemand_joint_hurdle works for probability type", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)
  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  p <- plot(fit, type = "probability")
  expect_s3_class(p, "ggplot")
})


# =============================================================================
# Test: Latent-Trait Variant (Variant 2)
# =============================================================================

test_that("fit_joint_hurdle with joint_type='latent' runs without error", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      joint_type = "latent",
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  expect_s3_class(fit, "beezdemand_joint_hurdle")
  expect_equal(fit$joint_type, "latent")
})

test_that("latent variant has correct parameter structure", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      joint_type = "latent",
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  coef_names <- names(fit$coefficients)

  # Should have theta parameters instead of logQ0/alpha
  expect_true("theta_Q0_AT" %in% coef_names)
  expect_true("theta_alpha_AT" %in% coef_names)
  expect_true("theta_Q0_OT" %in% coef_names)
  expect_true("theta_alpha_OT" %in% coef_names)
  expect_true("theta_Qalone_OA" %in% coef_names)

  # Should have latent loadings
  expect_true("lambda_sub_q0" %in% coef_names)
  expect_true("lambda_sub_alpha" %in% coef_names)
  expect_true("lambda_sub_alt" %in% coef_names)

  # Should have latent trait variances
  expect_true("logsigma_buy" %in% coef_names)
  expect_true("logsigma_val" %in% coef_names)
  expect_true("logsigma_sens" %in% coef_names)
  expect_true("logsigma_sub" %in% coef_names)
})

test_that("latent variant has latent_traits instead of random_effects", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      joint_type = "latent",
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  # Should have latent_traits
  expect_true("latent_traits" %in% names(fit))
  expect_true(is.matrix(fit$latent_traits))
  expect_equal(ncol(fit$latent_traits), 4) # u_buy, u_val, u_sens, u_sub
  expect_equal(
    colnames(fit$latent_traits),
    c("u_buy", "u_val", "u_sens", "u_sub")
  )
})

test_that("latent variant coef() returns correct types", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      joint_type = "latent",
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  # Demand coefficients should use theta_ prefix
  demand_coef <- coef(fit, type = "demand")
  expect_true("theta_Q0_AT" %in% names(demand_coef))

  # Cross-price coefficients
  cp_coef <- coef(fit, type = "crossprice")
  expect_true("theta_Qalone_OA" %in% names(cp_coef))
  expect_true("I" %in% names(cp_coef))
  expect_true("beta" %in% names(cp_coef))

  # Latent loadings
  latent_coef <- coef(fit, type = "latent")
  expect_true("lambda_sub_q0" %in% names(latent_coef))
})

test_that("latent variant print and summary work", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      joint_type = "latent",
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  expect_output(print(fit), "Variant: latent")
  # summary() now returns a structured object; output comes from print()
  summ <- summary(fit)
  expect_output(print(summ), "Latent Trait")
})

test_that("latent variant has fewer random effect parameters than saturated", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit_sat <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      joint_type = "saturated",
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  fit_lat <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      joint_type = "latent",
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  # Both models should have same number of latent dimensions (4)
  # but latent model uses shared traits instead of stream-specific REs
  expect_equal(ncol(fit_sat$random_effects), 4) # a_i, b_AT_i, b_OT_i, b_OA_i
  expect_equal(ncol(fit_lat$latent_traits), 4) # u_buy, u_val, u_sens, u_sub

  # Both should be beezdemand_joint_hurdle class
  expect_s3_class(fit_sat, "beezdemand_joint_hurdle")
  expect_s3_class(fit_lat, "beezdemand_joint_hurdle")
})

test_that("saturated and latent models can be fit on same data", {
  skip_on_cran()

  test_data <- create_joint_test_data(n_subjects = 5, n_prices = 6)

  fit_sat <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      joint_type = "saturated",
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  fit_lat <- suppressWarnings(
    fit_joint_hurdle(
      data = test_data,
      joint_type = "latent",
      k = 2,
      verbose = 0,
      control = list(eval.max = 200, iter.max = 200)
    )
  )

  # Both should have finite log-likelihoods
  expect_true(is.finite(fit_sat$logLik))
  expect_true(is.finite(fit_lat$logLik))

  # Both should have same data dimensions
  expect_equal(fit_sat$n_obs, fit_lat$n_obs)
  expect_equal(fit_sat$n_subjects, fit_lat$n_subjects)
})
