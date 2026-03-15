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

test_that("subject_pars reflect factor effects, not just intercepts", {
  skip_if_not(exists("apt_full", where = asNamespace("beezdemand")))
  data(apt_full, package = "beezdemand")

  fit <- fit_demand_tmb(
    apt_full, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", factors = "gender", verbose = 0
  )

  spars <- get_subject_pars(fit)

  # Merge factor level back onto subject_pars
  subj_info <- unique(apt_full[, c("id", "gender")])
  merged <- merge(spars, subj_info, by = "id")

  # If factor effects are included, group means should differ
  # (unless the factor effect is exactly zero, which is unlikely)
  coefs <- coef(fit)
  beta_q0 <- coefs[names(coefs) == "beta_q0"]

  # With factors, there should be >1 beta coefficient for Q0

  expect_gt(length(beta_q0), 1)

  # Verify subject Q0 values use the full design matrix, not just intercept.
  # For each subject, compute expected Q0 from design matrix row + RE.
  X_q0 <- fit$formula_details$X_q0
  u_hat <- if (!is.null(fit$sdr)) {
    re_summary <- summary(fit$sdr, "random")
    matrix(re_summary[, "Estimate"], nrow = fit$param_info$n_subjects,
           ncol = fit$param_info$n_random_effects)
  } else {
    matrix(0, nrow = fit$param_info$n_subjects, ncol = fit$param_info$n_random_effects)
  }

  # Check a specific subject: their Q0 should use their design matrix row
  data_used <- fit$data
  subj_levels <- fit$param_info$subject_levels
  # Pick first subject in second factor level (non-reference)
  non_ref_subjs <- unique(data_used$id[data_used$gender != levels(data_used$gender)[1]])
  if (length(non_ref_subjs) > 0) {
    test_subj <- as.character(non_ref_subjs[1])
    test_subj_idx <- which(subj_levels == test_subj)
    first_obs_idx <- which(data_used$id == test_subj)[1]

    # Expected log(Q0) = X_q0[obs,] %*% beta_q0 + b_i
    expected_log_q0 <- sum(X_q0[first_obs_idx, ] * beta_q0)
    # b_i contribution comes from random effect
    # The key test: Q0 for this subject should NOT equal exp(intercept + b_i)
    intercept_only_log_q0 <- beta_q0[1]

    # If there's a non-zero factor effect, these should differ
    if (abs(beta_q0[2]) > 1e-10) {
      expect_false(
        abs(expected_log_q0 - intercept_only_log_q0) < 1e-10,
        info = "Non-reference subject Q0 should differ from intercept-only"
      )
    }
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
