# Test suite for fit_demand_tmb()

test_that("fit_demand_tmb converges with exponential equation", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  expect_s3_class(fit, "beezdemand_tmb")
  expect_true(fit$converged)
  expect_true(is.finite(fit$loglik))
  expect_true(is.finite(fit$AIC))
  expect_true(is.finite(fit$BIC))
  expect_equal(fit$param_info$equation, "exponential")
})

test_that("fit_demand_tmb converges with simplified equation", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "simplified", verbose = 0
  )

  expect_s3_class(fit, "beezdemand_tmb")
  expect_true(fit$converged)
  expect_false(fit$param_info$has_k)
  # k should not be in estimated parameters
  expect_false("log_k" %in% names(fit$opt$par))
})

test_that("fit_demand_tmb converges with zben equation", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_tmb(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", verbose = 0
  )

  expect_s3_class(fit, "beezdemand_tmb")
  expect_true(fit$converged)
  expect_false(fit$param_info$has_k)
})

test_that("fit_demand_tmb converges with exponentiated equation", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated", verbose = 0
  )

  expect_s3_class(fit, "beezdemand_tmb")
  # May or may not converge with small data, but should produce estimates
  expect_true(is.finite(fit$loglik))
})

test_that("exponential equation drops zeros", {
  data(apt, package = "beezdemand")
  n_zeros <- sum(apt$y == 0)

  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  expect_equal(fit$param_info$n_dropped, n_zeros)
  expect_equal(fit$param_info$n_obs, nrow(apt) - n_zeros)
})

test_that("exponentiated and simplified equations accept zeros", {
  data(apt, package = "beezdemand")

  for (eq in c("exponentiated", "simplified")) {
    fit <- fit_demand_tmb(
      apt, y_var = "y", x_var = "x", id_var = "id",
      equation = eq, verbose = 0
    )
    expect_equal(fit$param_info$n_dropped, 0L)
    expect_equal(fit$param_info$n_obs, nrow(apt))
  }
})

test_that("1 RE vs 2 RE models work", {
  data(apt, package = "beezdemand")

  fit_1re <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", random_effects = "q0", verbose = 0
  )
  fit_2re <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", random_effects = c("q0", "alpha"), verbose = 0
  )

  expect_equal(fit_1re$param_info$n_random_effects, 1)
  expect_equal(fit_2re$param_info$n_random_effects, 2)

  # 1 RE should have fewer parameters
  expect_lt(length(fit_1re$opt$par), length(fit_2re$opt$par))

  # Both should converge
  expect_true(fit_1re$converged)
  expect_true(fit_2re$converged)
})

test_that("estimate_k = FALSE fixes k", {
  data(apt, package = "beezdemand")

  fit_free <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", estimate_k = TRUE, verbose = 0
  )
  fit_fixed <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", estimate_k = FALSE, k = 2, verbose = 0
  )

  # Fixed k model should have fewer parameters
  expect_lt(length(fit_fixed$opt$par), length(fit_free$opt$par))

  # AIC should typically be higher for fixed k (worse fit)
  # But direction depends on data, so just check both are finite
  expect_true(is.finite(fit_free$AIC))
  expect_true(is.finite(fit_fixed$AIC))
})

test_that("multi_start selects best NLL", {
  data(apt, package = "beezdemand")

  fit_ms <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", multi_start = TRUE, verbose = 0
  )
  fit_ss <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", multi_start = FALSE, verbose = 0
  )

  # Multi-start should have equal or better NLL
  expect_lte(fit_ms$opt$objective, fit_ss$opt$objective + 0.01)
})

test_that("subject_pars has correct structure", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  spars <- fit$subject_pars
  expect_true(is.data.frame(spars))
  expect_true(all(c("id", "b_i", "Q0", "alpha", "Pmax", "Omax") %in% names(spars)))
  expect_equal(nrow(spars), fit$param_info$n_subjects)

  # Q0 and alpha should be positive
  expect_true(all(spars$Q0 > 0))
  expect_true(all(spars$alpha > 0))
})

test_that("subject_pars with 2 RE includes c_i", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", random_effects = c("q0", "alpha"), verbose = 0
  )

  expect_true("c_i" %in% names(fit$subject_pars))
})

test_that("print method works", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  output <- capture.output(print(fit))
  expect_true(any(grepl("TMB Mixed-Effects", output)))
  expect_true(any(grepl("Convergence", output)))
})

test_that("summary method works", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  s <- summary(fit)
  expect_s3_class(s, "summary.beezdemand_tmb")
  expect_true("coefficients" %in% names(s))
  expect_true(is.finite(s$logLik))
  expect_true(is.finite(s$AIC))

  # Should print without error
  output <- capture.output(print(s))
  expect_true(length(output) > 0)
})

test_that("predict methods work", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  # Response predictions
  pred_resp <- predict(fit, type = "response")
  expect_true(".fitted" %in% names(pred_resp))
  expect_equal(nrow(pred_resp), nrow(fit$data))

  # Demand curve
  pred_demand <- predict(fit, type = "demand", prices = c(0, 1, 5, 10))
  expect_true("price" %in% names(pred_demand))
  expect_true(".fitted" %in% names(pred_demand))
  expect_equal(nrow(pred_demand), 4)

  # Parameters
  pred_pars <- predict(fit, type = "parameters")
  expect_true("Q0" %in% names(pred_pars))
  expect_equal(nrow(pred_pars), fit$param_info$n_subjects)
})

test_that("plot method produces ggplot object", {
  skip_if_not_installed("ggplot2")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  p <- plot(fit, type = "demand")
  expect_s3_class(p, "ggplot")

  p2 <- plot(fit, type = "individual")
  expect_s3_class(p2, "ggplot")

  p3 <- plot(fit, type = "parameters")
  expect_s3_class(p3, "ggplot")
})

test_that("coef, logLik, AIC, BIC work", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  expect_true(is.numeric(coef(fit)))
  expect_true(length(coef(fit)) > 0)

  ll <- logLik(fit)
  expect_s3_class(ll, "logLik")
  expect_true(is.finite(as.numeric(ll)))

  expect_true(is.finite(AIC(fit)))
  expect_true(is.finite(BIC(fit)))
})

test_that("fixef and ranef work", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  fe <- fixef(fit)
  expect_true(is.numeric(fe))
  expect_equal(fe, coef(fit))

  re <- ranef(fit)
  expect_true(is.data.frame(re))
  expect_true("id" %in% names(re))
  expect_true("b_i" %in% names(re))
})

test_that("confint works", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  ci <- confint(fit)
  expect_true(is.data.frame(ci))
  expect_true(all(c("term", "estimate", "conf.low", "conf.high") %in% names(ci)))
  expect_true(all(ci$conf.low <= ci$estimate, na.rm = TRUE))
  expect_true(all(ci$conf.high >= ci$estimate, na.rm = TRUE))
})

test_that("get_subject_pars works", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  spars <- get_subject_pars(fit)
  expect_true(is.data.frame(spars))
  expect_equal(nrow(spars), fit$param_info$n_subjects)
})

test_that("check_demand_model works for TMB model", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  diag <- check_demand_model(fit)
  expect_s3_class(diag, "beezdemand_diagnostics")
  expect_true("convergence" %in% names(diag))
  expect_true("random_effects" %in% names(diag))
})

test_that("compare_models works with TMB models", {
  data(apt, package = "beezdemand")

  fit1 <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", random_effects = "q0", verbose = 0
  )
  fit2 <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", random_effects = c("q0", "alpha"), verbose = 0
  )

  comp <- compare_models(fit1, fit2)
  expect_s3_class(comp, "beezdemand_model_comparison")
})

test_that("invalid inputs produce errors", {
  data(apt, package = "beezdemand")

  # Missing required column
  expect_error(
    fit_demand_tmb(apt[, c("x", "y")], y_var = "y", x_var = "x", id_var = "id",
                   verbose = 0),
    "Missing required columns"
  )

  # Invalid equation
  expect_error(
    fit_demand_tmb(apt, equation = "invalid", verbose = 0)
  )

  # Invalid random_effects
  expect_error(
    fit_demand_tmb(apt, random_effects = "zeros", verbose = 0),
    "random_effects must be a subset"
  )

  # random_effects without q0
  expect_error(
    fit_demand_tmb(apt, random_effects = "alpha", verbose = 0),
    "must include at least"
  )
})

test_that("factors work with TMB model", {
  skip_if_not(exists("apt_full", where = asNamespace("beezdemand")))
  data(apt_full, package = "beezdemand")

  fit <- fit_demand_tmb(
    apt_full, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", factors = "gender", verbose = 0
  )

  expect_s3_class(fit, "beezdemand_tmb")
  # Design matrices should have more columns with factors
  expect_gt(ncol(fit$formula_details$X_q0), 1)
  expect_gt(ncol(fit$formula_details$X_alpha), 1)
})

test_that("collapse_levels works with TMB model", {
  skip_if_not(exists("apt_full", where = asNamespace("beezdemand")))
  data(apt_full, package = "beezdemand")

  # Create a factor with 3+ levels for collapsing
  apt_full$age_group <- cut(apt_full$age, breaks = c(0, 25, 35, Inf),
                            labels = c("young", "mid", "old"))

  fit <- fit_demand_tmb(
    apt_full, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential",
    factors = "age_group",
    collapse_levels = list(
      Q0 = list(age_group = list(younger = c("young", "mid"), older = "old")),
      alpha = list(age_group = list(younger = "young", older = c("mid", "old")))
    ),
    verbose = 0
  )

  expect_s3_class(fit, "beezdemand_tmb")
  expect_true(!is.null(fit$collapse_info))
})


# --- Factor-stratified subject parameter correctness ---

test_that("subject_pars Q0 matches design-matrix computation for each subject", {
  skip_if_not(exists("apt_full", where = asNamespace("beezdemand")))
  data(apt_full, package = "beezdemand")

  fit <- fit_demand_tmb(
    apt_full, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", factors = "gender", verbose = 0
  )

  spars <- get_subject_pars(fit)
  coefs <- coef(fit)
  beta_q0 <- coefs[names(coefs) == "beta_q0"]
  X_q0 <- fit$formula_details$X_q0
  data_used <- fit$data
  subj_levels <- fit$param_info$subject_levels
  n_re <- fit$param_info$n_random_effects

  # Model should have factor betas
  expect_gt(length(beta_q0), 1)

  # Extract random effects
  re_summary <- summary(fit$sdr, "random")
  u_hat <- matrix(re_summary[, "Estimate"],
                  nrow = fit$param_info$n_subjects, ncol = n_re)

  # Reconstruct Cholesky-transformed RE for Q0
  sigma_b <- exp(coefs[["logsigma_b"]])
  if (n_re == 2) {
    sigma_c <- exp(coefs[["logsigma_c"]])
    rho_bc <- tanh(coefs[["rho_bc_raw"]])
    Sigma <- matrix(c(sigma_b^2, sigma_b * sigma_c * rho_bc,
                      sigma_b * sigma_c * rho_bc, sigma_c^2), 2)
    L <- t(chol(Sigma))
    re_mat <- t(L %*% t(u_hat))
  } else {
    re_mat <- matrix(sigma_b * u_hat[, 1], ncol = 1)
  }

  # Build subject_id mapping
  ids <- data_used[[fit$param_info$id_var]]
  subject_map <- setNames(seq_along(subj_levels) - 1L, subj_levels)
  subject_id <- as.integer(subject_map[as.character(ids)])

  # Verify EVERY subject's Q0 against independent computation
  for (i in seq_along(subj_levels)) {
    first_obs <- which(subject_id == (i - 1L))[1]
    expected_log_q0 <- sum(X_q0[first_obs, ] * beta_q0) + re_mat[i, 1]
    expected_Q0 <- exp(expected_log_q0)

    actual_Q0 <- spars$Q0[spars$id == subj_levels[i]]
    expect_equal(actual_Q0, expected_Q0, tolerance = 1e-8,
                 label = paste("Q0 for subject", subj_levels[i]))
  }
})


test_that("predict(type='response') is vectorized and consistent with subject_pars", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  pred <- predict(fit, type = "response")
  expect_s3_class(pred, "tbl_df")
  expect_true(".fitted" %in% names(pred))
  expect_equal(nrow(pred), nrow(fit$data))
  # No NAs in fitted values
  expect_true(all(is.finite(pred$.fitted)))
})


test_that("predict warns for unknown subjects", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  new_data <- data.frame(id = "UNKNOWN", x = c(0, 1, 5), y = c(10, 8, 2))
  expect_warning(
    predict(fit, newdata = new_data, type = "response"),
    "unknown subject"
  )
})


test_that("optimizer warnings are captured, not silently dropped", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  # opt_warnings field should exist on the result
  expect_true("opt_warnings" %in% names(fit))
  expect_type(fit$opt_warnings, "character")
})


test_that("se_available flag is set correctly", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  expect_true("se_available" %in% names(fit))
  # For a successful fit, se_available should be TRUE
  if (fit$converged) {
    expect_true(fit$se_available)
  }
})


test_that("backend name distinguishes TMB_mixed from TMB_hurdle", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  g <- glance(fit)
  expect_equal(g$backend, "TMB_mixed")
})


# ==============================================================================
# Optimizer control tests
# ==============================================================================

test_that("L-BFGS-B optimizer produces valid fit", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated",
    tmb_control = list(optimizer = "L-BFGS-B"),
    multi_start = FALSE, verbose = 0
  )

  expect_s3_class(fit, "beezdemand_tmb")
  expect_true(is.finite(fit$AIC))
  expect_true(is.finite(fit$opt$objective))
  expect_true(is.character(fit$opt$message))
  expect_false(is.null(fit$opt$message))
})

test_that("warm_start from previous fit works", {
  data(apt, package = "beezdemand")
  fit1 <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated", multi_start = FALSE, verbose = 0
  )

  fit2 <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated",
    tmb_control = list(warm_start = fit1$opt$par),
    multi_start = FALSE, verbose = 0
  )

  expect_s3_class(fit2, "beezdemand_tmb")
  expect_true(is.finite(fit2$opt$objective))
})

test_that("warm_start disables multi_start with message", {
  data(apt, package = "beezdemand")
  fit1 <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated", multi_start = FALSE, verbose = 0
  )

  expect_message(
    fit_demand_tmb(
      apt, y_var = "y", x_var = "x", id_var = "id",
      equation = "exponentiated",
      tmb_control = list(warm_start = fit1$opt$par),
      multi_start = TRUE, verbose = 0
    ),
    "multi_start disabled"
  )
})

test_that("warm_start length validation errors on mismatch", {
  data(apt, package = "beezdemand")

  expect_error(
    fit_demand_tmb(
      apt, y_var = "y", x_var = "x", id_var = "id",
      equation = "exponentiated",
      tmb_control = list(warm_start = c(1, 2, 3)),
      multi_start = FALSE, verbose = 0
    ),
    "warm_start has 3 elements"
  )
})

test_that("rel_tol is passed to nlminb", {
  data(apt, package = "beezdemand")
  # Tighter tolerance should still work
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated",
    tmb_control = list(rel_tol = 1e-12),
    multi_start = FALSE, verbose = 0
  )

  expect_s3_class(fit, "beezdemand_tmb")
  expect_true(is.finite(fit$opt$objective))
})

test_that("invalid optimizer is rejected", {
  data(apt, package = "beezdemand")

  expect_error(
    fit_demand_tmb(
      apt, y_var = "y", x_var = "x", id_var = "id",
      equation = "exponentiated",
      tmb_control = list(optimizer = "fake"),
      verbose = 0
    ),
    "must be one of"
  )
})

test_that("bounds with L-BFGS-B are applied", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated",
    tmb_control = list(
      optimizer = "L-BFGS-B",
      lower = c(log_k = -2),
      upper = c(log_k = 4)
    ),
    multi_start = FALSE, verbose = 0
  )

  expect_s3_class(fit, "beezdemand_tmb")
  expect_true(is.finite(fit$opt$objective))
  # k should be within bounds
  if ("log_k" %in% names(fit$opt$par)) {
    expect_gte(fit$opt$par[["log_k"]], -2)
    expect_lte(fit$opt$par[["log_k"]], 4)
  }
})

test_that("S3 methods work with L-BFGS-B fit", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated",
    tmb_control = list(optimizer = "L-BFGS-B"),
    multi_start = FALSE, verbose = 0
  )

  expect_true(is.numeric(coef(fit)))
  expect_true(length(coef(fit)) > 0)

  s <- summary(fit)
  expect_s3_class(s, "summary.beezdemand_tmb")

  pred <- predict(fit, type = "parameters")
  expect_true(is.data.frame(pred))

  g <- glance(fit)
  expect_true(is.finite(g$AIC))

  t <- tidy(fit)
  expect_true(is.data.frame(t))
})

test_that("rel_tol warning only when user-specified with L-BFGS-B", {
  data(apt, package = "beezdemand")

  # No rel_tol warning when rel_tol is not user-specified
  ws <- testthat::capture_warnings(
    fit_demand_tmb(
      apt, y_var = "y", x_var = "x", id_var = "id",
      equation = "exponentiated",
      tmb_control = list(optimizer = "L-BFGS-B"),
      multi_start = FALSE, verbose = 0
    )
  )
  expect_false(any(grepl("rel_tol", ws)))

  # Warning when rel_tol IS user-specified
  ws2 <- testthat::capture_warnings(
    fit_demand_tmb(
      apt, y_var = "y", x_var = "x", id_var = "id",
      equation = "exponentiated",
      tmb_control = list(optimizer = "L-BFGS-B", rel_tol = 1e-12),
      multi_start = FALSE, verbose = 0
    )
  )
  expect_true(any(grepl("rel_tol is ignored", ws2)))
})

test_that("input validation catches bad tmb_control values", {
  data(apt, package = "beezdemand")

  # Non-numeric rel_tol
  expect_error(
    fit_demand_tmb(
      apt, equation = "exponentiated",
      tmb_control = list(rel_tol = "small"), verbose = 0
    ),
    "rel_tol must be a single positive finite number"
  )

  # Negative trace
  expect_error(
    fit_demand_tmb(
      apt, equation = "exponentiated",
      tmb_control = list(trace = -1), verbose = 0
    ),
    "trace must be a single non-negative number"
  )

  # Non-numeric warm_start
  expect_error(
    fit_demand_tmb(
      apt, equation = "exponentiated",
      tmb_control = list(warm_start = "bad"), verbose = 0
    ),
    "warm_start must be a numeric vector"
  )

  # Non-numeric lower
  expect_error(
    fit_demand_tmb(
      apt, equation = "exponentiated",
      tmb_control = list(lower = "bad"), verbose = 0
    ),
    "lower must be a named numeric vector"
  )
})

test_that("nobs returns correct observation count", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated", multi_start = FALSE, verbose = 0
  )

  n <- nobs(fit)
  expect_true(is.numeric(n))
  expect_equal(n, fit$param_info$n_obs)
  expect_equal(n, nrow(fit$data))
})

test_that("nobs fallback works when param_info$n_obs is NULL", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated", multi_start = FALSE, verbose = 0
  )

  # Simulate legacy object without n_obs
  fit2 <- fit
  fit2$param_info$n_obs <- NULL
  expect_equal(nobs(fit2), nrow(fit2$data))
})

test_that("unnamed bounds are rejected", {
  data(apt, package = "beezdemand")

  # Unnamed lower bounds

  expect_error(
    fit_demand_tmb(
      apt, equation = "exponentiated",
      tmb_control = list(lower = c(-2, 4)),
      verbose = 0
    ),
    "lower must be a named numeric vector"
  )

  # Unnamed upper bounds
  expect_error(
    fit_demand_tmb(
      apt, equation = "exponentiated",
      tmb_control = list(upper = c(1, 2)),
      verbose = 0
    ),
    "upper must be a named numeric vector"
  )
})

test_that("normalized opt$message is never NULL", {
  data(apt, package = "beezdemand")

  # L-BFGS-B fit
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated",
    tmb_control = list(optimizer = "L-BFGS-B"),
    multi_start = FALSE, verbose = 0
  )

  expect_true(!is.null(fit$opt$message))
  expect_true(is.character(fit$opt$message))
  expect_true(nchar(fit$opt$message) > 0)
})


# ==============================================================================
# Non-convergence and failure mode tests (#4)
# ==============================================================================

test_that("degenerate data produces non-converged model with graceful S3 methods", {
  # 2 subjects, 3 prices: too little data for a mixed model to converge well

  degen <- data.frame(
    id = rep(c("S1", "S2"), each = 3),
    x = rep(c(0, 1, 10), 2),
    y = c(10, 8, 1, 12, 7, 0.5)
  )

  # Should not error — should produce a result even if not converged
  fit <- tryCatch(
    fit_demand_tmb(
      degen, y_var = "y", x_var = "x", id_var = "id",
      equation = "exponentiated", multi_start = FALSE, verbose = 0,
      tmb_control = list(iter_max = 5)  # Force early stop
    ),
    error = function(e) NULL
  )

  # If it returns a result (may error with only 2 subjects), verify graceful handling
  if (!is.null(fit)) {
    expect_s3_class(fit, "beezdemand_tmb")

    # S3 methods should not error on non-converged models
    expect_no_error(print(fit))
    expect_no_error(coef(fit))
    expect_no_error(summary(fit))
    expect_no_error(glance(fit))

    # tidy should return a tibble
    t <- tidy(fit)
    expect_s3_class(t, "tbl_df")

    # confint should return a tibble (may have NA bounds)
    ci <- confint(fit)
    expect_s3_class(ci, "tbl_df")
    expect_true(all(c("term", "estimate", "conf.low", "conf.high") %in% names(ci)))
  }
})

test_that("summary notes non-convergence and SE unavailability", {
  data(apt, package = "beezdemand")

  # Force non-convergence by limiting iterations
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated", multi_start = FALSE, verbose = 0,
    tmb_control = list(iter_max = 2)
  )

  if (!fit$converged) {
    s <- summary(fit)
    expect_true(any(grepl("did not converge", s$notes, ignore.case = TRUE)))
  }

  # Simulate SE unavailability
  fit2 <- fit
  fit2$se_available <- FALSE
  s2 <- summary(fit2)
  expect_true(any(grepl("Standard errors unavailable", s2$notes)))
})


# ==============================================================================
# Equation-specific prediction and parameter correctness tests (#7)
# ==============================================================================

test_that("exponential equation ln(10) fix: predictions match HS formula", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  coefs <- coef(fit)
  beta_q0 <- coefs[names(coefs) == "beta_q0"][1]
  beta_alpha <- coefs[names(coefs) == "beta_alpha"][1]
  Q0 <- exp(beta_q0)
  alpha_val <- exp(beta_alpha)
  k_val <- exp(coefs[["log_k"]])

  # Predict at specific prices
  prices <- c(0, 1, 5, 10)
  pred <- predict(fit, type = "demand", prices = prices)

  # Manual HS in natural log: ln(Q) = ln(Q0) + k*ln(10)*(exp(-α*Q0*C) - 1)
  expected <- unname(beta_q0 + k_val * log(10) * (exp(-alpha_val * Q0 * prices) - 1))
  expect_equal(pred$.fitted, expected, tolerance = 1e-10)

  # At price = 0, prediction should equal ln(Q0)
  expect_equal(pred$.fitted[1], unname(beta_q0), tolerance = 1e-10)
})

test_that("exponentiated equation predictions match Koffarnus formula", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated", verbose = 0
  )

  coefs <- coef(fit)
  beta_q0 <- coefs[names(coefs) == "beta_q0"][1]
  beta_alpha <- coefs[names(coefs) == "beta_alpha"][1]
  Q0 <- exp(beta_q0)
  alpha_val <- exp(beta_alpha)
  k_val <- exp(coefs[["log_k"]])

  prices <- c(0, 1, 5, 10)
  pred <- predict(fit, type = "demand", prices = prices)

  # Koffarnus: Q_pred = exp(ln(Q0) + k*ln(10)*(exp(-α*Q0*C) - 1))
  log_Q_pred <- beta_q0 + k_val * log(10) * (exp(-alpha_val * Q0 * prices) - 1)
  expected <- unname(exp(log_Q_pred))
  expect_equal(pred$.fitted, expected, tolerance = 1e-10)

  # At price = 0, prediction should equal Q0
  expect_equal(pred$.fitted[1], unname(Q0), tolerance = 1e-6)
})

test_that("simplified equation predictions match Q0*exp(-α*Q0*C)", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "simplified", verbose = 0
  )

  coefs <- coef(fit)
  beta_q0 <- coefs[names(coefs) == "beta_q0"][1]
  beta_alpha <- coefs[names(coefs) == "beta_alpha"][1]
  Q0 <- exp(beta_q0)
  alpha_val <- exp(beta_alpha)

  prices <- c(0, 1, 5, 10)
  pred <- predict(fit, type = "demand", prices = prices)

  expected <- unname(Q0 * exp(-alpha_val * Q0 * prices))
  expect_equal(pred$.fitted, expected, tolerance = 1e-10)

  # At price = 0, prediction should equal Q0
  expect_equal(pred$.fitted[1], unname(Q0), tolerance = 1e-10)
})

test_that("zben equation predictions match formula with singularity protection", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_tmb(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", verbose = 0
  )

  coefs <- coef(fit)
  beta_q0 <- coefs[names(coefs) == "beta_q0"][1]
  beta_alpha <- coefs[names(coefs) == "beta_alpha"][1]
  Q0 <- exp(beta_q0)
  alpha_val <- exp(beta_alpha)

  prices <- c(0, 1, 5, 10)
  pred <- predict(fit, type = "demand", prices = prices)

  # zben: Q0_log10 = ln(Q0)/ln(10), rate = (α/Q0_log10)*Q0, y = Q0_log10*exp(-rate*C)
  Q0_log10 <- unname(beta_q0 / log(10))
  # Positive clamp to prevent singularity and sign-flip divergence
  Q0_log10 <- max(Q0_log10, 1e-3)
  rate <- unname((alpha_val / Q0_log10) * Q0)
  expected <- Q0_log10 * exp(-rate * prices)
  expect_equal(pred$.fitted, expected, tolerance = 1e-10)
})

test_that("exponential backtransform to natural scale includes retransformation correction", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  # Model scale should be log(Q)
  pred_model <- predict(fit, type = "demand", prices = c(0, 1, 5))
  # Natural scale: exp(log(Q)) * exp(sigma_e^2/2) — lognormal mean correction
  pred_natural <- predict(fit, type = "demand", prices = c(0, 1, 5),
                          scale = "natural")
  sigma_e <- exp(fit$model$coefficients[["logsigma_e"]])
  correction <- exp(sigma_e^2 / 2)
  expect_equal(pred_natural$.fitted, exp(pred_model$.fitted) * correction,
               tolerance = 1e-10)
})

test_that("exponentiated Pmax/Omax are computed correctly", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated", verbose = 0
  )

  # All subjects should have finite Pmax/Omax
  spars <- get_subject_pars(fit)
  expect_true(all(is.finite(spars$Pmax)))
  expect_true(all(is.finite(spars$Omax)))
  expect_true(all(spars$Pmax > 0))
  expect_true(all(spars$Omax > 0))

  # Group metrics should also be finite
  gm <- calc_group_metrics(fit)
  expect_true(is.finite(gm$Pmax))
  expect_true(is.finite(gm$Omax))
})

test_that("simplified Pmax/Omax use SND model type", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "simplified", verbose = 0
  )

  spars <- get_subject_pars(fit)
  expect_true(all(is.finite(spars$Pmax)))
  expect_true(all(is.finite(spars$Omax)))

  gm <- calc_group_metrics(fit)
  expect_true(is.finite(gm$Pmax))
  expect_true(is.finite(gm$Omax))
})


# ==============================================================================
# confint parm matching tests (#6)
# ==============================================================================

test_that("confint parm filters by display names", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  # Full confint for reference
  ci_all <- confint(fit)
  expect_gt(nrow(ci_all), 0)

  # Filter by display name (e.g., "Q0:(Intercept)")
  q0_display <- ci_all$term[grepl("^Q0:", ci_all$term)][1]
  ci_display <- confint(fit, parm = q0_display)
  expect_equal(nrow(ci_display), 1)
  expect_equal(ci_display$term, q0_display)

  # Filter by raw name should still work
  ci_raw <- confint(fit, parm = "log_k")
  expect_equal(nrow(ci_raw), 1)
  expect_true(grepl("log_k", ci_raw$term))
})


# ==============================================================================
# Population prediction warning with factors (#3)
# ==============================================================================

test_that("predict(type='demand') warns when factors are present", {
  skip_if_not(exists("apt_full", where = asNamespace("beezdemand")))
  data(apt_full, package = "beezdemand")

  fit <- fit_demand_tmb(
    apt_full, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", factors = "gender", verbose = 0
  )

  expect_warning(
    predict(fit, type = "demand", prices = c(0, 1, 5)),
    "reference level"
  )
})

test_that("predict(type='demand') does NOT warn without factors", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  expect_no_warning(
    predict(fit, type = "demand", prices = c(0, 1, 5))
  )
})
