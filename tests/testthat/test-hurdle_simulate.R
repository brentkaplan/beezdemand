# Tests for simulation functions

test_that("simulate_hurdle_data returns correct structure", {
  sim_data <- simulate_hurdle_data(n_subjects = 50, seed = 123)

  expect_true(is.data.frame(sim_data))
  expect_true(all(
    c("id", "x", "y", "delta", "a_i", "b_i") %in% names(sim_data)
  ))
  expect_true(is.factor(sim_data$id))
  expect_equal(length(unique(sim_data$id)), 50)
})

test_that("simulate_hurdle_data is reproducible with seed", {
  sim1 <- simulate_hurdle_data(n_subjects = 20, seed = 456)
  sim2 <- simulate_hurdle_data(n_subjects = 20, seed = 456)

  expect_identical(sim1, sim2)
})

test_that("simulate_hurdle_data works for 2-RE and 3-RE", {
  # 2 RE
  sim2 <- simulate_hurdle_data(
    n_subjects = 20,
    n_random_effects = 2,
    seed = 123
  )
  expect_false("c_i" %in% names(sim2))

  # 3 RE
  sim3 <- simulate_hurdle_data(
    n_subjects = 20,
    n_random_effects = 3,
    seed = 123
  )
  expect_true("c_i" %in% names(sim3))
})

test_that("simulate_hurdle_data respects stop_at_zero parameter", {
  # With stop_at_zero = TRUE (default), subjects have varying obs
  sim_stop <- simulate_hurdle_data(
    n_subjects = 50,
    stop_at_zero = TRUE,
    seed = 123
  )

  # With stop_at_zero = FALSE, all subjects have same number of obs
  sim_nostop <- simulate_hurdle_data(
    n_subjects = 50,
    stop_at_zero = FALSE,
    seed = 123
  )

  # Check that stop_at_zero produces fewer total observations (on average)
  # because subjects stop when they hit zero
  expect_true(nrow(sim_stop) <= nrow(sim_nostop))
})

test_that("simulate_hurdle_data stores true_params as attribute", {
  sim_data <- simulate_hurdle_data(n_subjects = 20, seed = 123)

  true_params <- attr(sim_data, "true_params")

  expect_true(is.list(true_params))
  expect_true(all(
    c("beta0", "beta1", "log_q0", "k", "alpha") %in% names(true_params)
  ))
})

test_that("simulate_hurdle_data accepts custom prices", {
  custom_prices <- c(0, 0.5, 1, 2, 5, 10)
  sim_data <- simulate_hurdle_data(
    n_subjects = 20,
    prices = custom_prices,
    stop_at_zero = FALSE,
    seed = 123
  )

  # Each subject should have observations at all prices
  obs_per_subject <- table(sim_data$id)
  expect_true(all(obs_per_subject == length(custom_prices)))
})

test_that("simulate_hurdle_data errors on invalid correlation parameters", {
  # Correlations that would make covariance matrix non-positive-definite
  expect_error(
    simulate_hurdle_data(
      n_subjects = 20,
      rho_ab = 0.99,
      rho_ac = 0.99,
      rho_bc = -0.99,
      n_random_effects = 3,
      seed = 123
    ),
    "positive definite"
  )
})

test_that("run_hurdle_monte_carlo completes without error", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  # Very small simulation for testing
  mc_results <- run_hurdle_monte_carlo(
    n_sim = 3,
    n_subjects = 30,
    n_random_effects = 2,
    verbose = FALSE,
    seed = 123
  )

  expect_true(is.list(mc_results))
  expect_true("estimates" %in% names(mc_results))
  expect_true("summary" %in% names(mc_results))
  expect_true("n_converged" %in% names(mc_results))
  expect_true("n_sim" %in% names(mc_results))
})

# TICKET-062: run_hurdle_monte_carlo() must retain per-replicate diagnostics
# (rather than collapsing failures to NULL) and must exclude converged-but-
# non-PD-Hessian replicates from the SE-dependent summary statistics.

test_that("run_hurdle_monte_carlo returns a per-replicate diagnostics table (real fixture)", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  mc_results <- run_hurdle_monte_carlo(
    n_sim = 3,
    n_subjects = 30,
    n_random_effects = 2,
    verbose = FALSE,
    seed = 123
  )

  expect_true("diagnostics" %in% names(mc_results))
  expect_true("n_hessian_not_pd" %in% names(mc_results))
  expect_equal(nrow(mc_results$diagnostics), 3)
  expect_true(all(
    c("sim_id", "status", "converged", "hessian_pd", "opt_convergence", "opt_message") %in%
      names(mc_results$diagnostics)
  ))
  expect_true(all(
    mc_results$diagnostics$status %in%
      c("error", "nonconverged", "converged_non_pd", "converged_hessian_unavailable", "clean")
  ))
})

# Codex 2D review (blocking #2): the original version of this test used
# UNNAMED `se` vectors, so `se[param_names]` silently returned all-NA and
# `valid_idx <- !is.na(est_vals) & !is.na(se_vals)` was FALSE for every
# replicate regardless of hessian_pd -- the exclusion assertion
# (`n_valid <= 1`) passed vacuously, not because exclusion actually worked.
# Rewritten with NAMED coefficient/se vectors, two DISTINCT clean replicates
# (so n_valid == n_clean == 2, not the degenerate n = 1 case where
# empirical_se is NA), and a by-hand recomputation of the summary row from
# only the clean estimates.
test_that("run_hurdle_monte_carlo distinguishes error/nonconverged/non-PD/clean replicates and excludes non-PD from the summary", {
  call_id <- 0L
  param_names_2re <- c(
    "beta0", "beta1", "log_q0", "k", "alpha",
    "logsigma_a", "logsigma_b", "logsigma_e", "rho_ab_raw"
  )
  mock_coefs <- stats::setNames(
    c(-2, 1, log(10), 2, 0.5, 0, 0, 0, 0),
    param_names_2re
  )
  se_named <- stats::setNames(rep(0.2, length(mock_coefs)), param_names_2re)

  # Two clean replicates with DIFFERENT beta0 estimates so bias/empirical_se
  # can be recomputed by hand from exactly these two values.
  clean_a <- mock_coefs
  clean_a["beta0"] <- -1.5 # deviation from true (-2): +0.5
  clean_b <- mock_coefs
  clean_b["beta0"] <- -1.9 # deviation from true (-2): +0.1

  # Non-PD replicate: a wildly different beta0 so if it were wrongly
  # INCLUDED the bias/mean_estimate would be detectably contaminated.
  nonpd_bad <- mock_coefs
  nonpd_bad["beta0"] <- 999

  testthat::local_mocked_bindings(
    fit_demand_hurdle = function(...) {
      call_id <<- call_id + 1L
      if (call_id == 1L) {
        stop("forced fit error")
      } else if (call_id == 2L) {
        list(
          converged = FALSE,
          hessian_pd = NA,
          opt = list(convergence = 1L, message = "forced nonconvergence"),
          model = list(coefficients = mock_coefs, se = se_named)
        )
      } else if (call_id == 3L) {
        list(
          converged = TRUE,
          hessian_pd = FALSE,
          opt = list(convergence = 0L, message = "relative convergence (4)"),
          model = list(coefficients = nonpd_bad, se = se_named)
        )
      } else if (call_id == 4L) {
        list(
          converged = TRUE,
          hessian_pd = TRUE,
          opt = list(convergence = 0L, message = "relative convergence (4)"),
          model = list(coefficients = clean_a, se = se_named)
        )
      } else {
        list(
          converged = TRUE,
          hessian_pd = TRUE,
          opt = list(convergence = 0L, message = "relative convergence (4)"),
          model = list(coefficients = clean_b, se = se_named)
        )
      }
    }
  )

  warned_msg <- NULL
  mc <- withCallingHandlers(
    run_hurdle_monte_carlo(
      n_sim = 5,
      n_subjects = 10,
      n_random_effects = 2,
      verbose = FALSE,
      seed = 1
    ),
    beezdemand_hurdle_mc_hessian_excluded_warning = function(w) {
      warned_msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(nrow(mc$diagnostics), 5)
  expect_setequal(
    mc$diagnostics$status,
    c("error", "nonconverged", "converged_non_pd", "clean")
  )
  expect_equal(mc$n_converged, 3L) # sims 3, 4, 5 converged
  expect_equal(mc$n_hessian_not_pd, 1L) # sim 3
  n_clean <- sum(mc$diagnostics$status == "clean")
  expect_equal(n_clean, 2L) # sims 4, 5

  # (c) excluded count is named in the warning.
  expect_false(is.null(warned_msg))
  expect_match(warned_msg, "1 converged replicate")
  expect_match(warned_msg, "1 non-PD")

  beta0_row <- mc$summary[mc$summary$parameter == "beta0", ]

  # (a) n_valid == n_clean EXACTLY -- not just "<= 1", which the original
  # (broken) assertion allowed to pass vacuously.
  expect_equal(beta0_row$n_valid, n_clean)

  # (b) the summary statistics for beta0 match a by-hand computation from
  # ONLY the two clean estimates (clean_a, clean_b); the excluded
  # non-PD replicate's beta0 = 999 must NOT appear anywhere in this
  # computation.
  hand_est <- c(unname(clean_a["beta0"]), unname(clean_b["beta0"]))
  hand_se <- rep(unname(se_named["beta0"]), 2)
  hand_true <- -2
  hand_bias <- mean(hand_est) - hand_true
  hand_emp_se <- sd(hand_est)
  hand_mean_se <- mean(hand_se)
  hand_lower <- hand_est - 1.96 * hand_se
  hand_upper <- hand_est + 1.96 * hand_se
  hand_coverage <- mean(hand_lower <= hand_true & hand_true <= hand_upper)

  expect_equal(unname(beta0_row$mean_estimate), mean(hand_est))
  expect_equal(unname(beta0_row$bias), hand_bias)
  expect_equal(unname(beta0_row$empirical_se), hand_emp_se)
  expect_equal(unname(beta0_row$mean_se), hand_mean_se)
  expect_equal(unname(beta0_row$se_ratio), hand_mean_se / hand_emp_se)
  expect_equal(unname(beta0_row$coverage_95), hand_coverage)
})

# Codex 2D review (recommended #3): hessian_pd = NA (sdreport() itself
# failed) must be preserved as its own status, not coerced to
# "converged_non_pd" -- it is excluded from the summary the same way, but
# counted and reported separately.
test_that("run_hurdle_monte_carlo distinguishes hessian_pd = NA from hessian_pd = FALSE", {
  call_id <- 0L
  param_names_2re <- c(
    "beta0", "beta1", "log_q0", "k", "alpha",
    "logsigma_a", "logsigma_b", "logsigma_e", "rho_ab_raw"
  )
  mock_coefs <- stats::setNames(
    c(-2, 1, log(10), 2, 0.5, 0, 0, 0, 0),
    param_names_2re
  )
  se_named <- stats::setNames(rep(0.2, length(mock_coefs)), param_names_2re)

  testthat::local_mocked_bindings(
    fit_demand_hurdle = function(...) {
      call_id <<- call_id + 1L
      if (call_id == 1L) {
        list(
          converged = TRUE,
          hessian_pd = FALSE,
          opt = list(convergence = 0L, message = "ok"),
          model = list(coefficients = mock_coefs, se = se_named)
        )
      } else if (call_id == 2L) {
        list(
          converged = TRUE,
          hessian_pd = NA,
          opt = list(convergence = 0L, message = "ok, but sdreport failed"),
          model = list(coefficients = mock_coefs, se = se_named)
        )
      } else {
        list(
          converged = TRUE,
          hessian_pd = TRUE,
          opt = list(convergence = 0L, message = "ok"),
          model = list(coefficients = mock_coefs, se = se_named)
        )
      }
    }
  )

  warned_msg <- NULL
  mc <- withCallingHandlers(
    run_hurdle_monte_carlo(
      n_sim = 3,
      n_subjects = 10,
      n_random_effects = 2,
      verbose = FALSE,
      seed = 1
    ),
    beezdemand_hurdle_mc_hessian_excluded_warning = function(w) {
      warned_msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  expect_setequal(
    mc$diagnostics$status,
    c("converged_non_pd", "converged_hessian_unavailable", "clean")
  )
  expect_true(is.na(mc$diagnostics$hessian_pd[mc$diagnostics$status == "converged_hessian_unavailable"]))
  expect_equal(mc$n_converged, 3L)
  expect_equal(mc$n_hessian_not_pd, 1L)
  expect_equal(mc$n_hessian_unavailable, 1L)

  expect_false(is.null(warned_msg))
  expect_match(warned_msg, "1 non-PD")
  expect_match(warned_msg, "1 Hessian unavailable")

  # Only the clean (call 3) replicate contributes to the summary.
  beta0_row <- mc$summary[mc$summary$parameter == "beta0", ]
  expect_equal(beta0_row$n_valid, 1L)
})

test_that("run_hurdle_monte_carlo summary has expected columns", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  mc_results <- run_hurdle_monte_carlo(
    n_sim = 3,
    n_subjects = 30,
    n_random_effects = 2,
    verbose = FALSE,
    seed = 456
  )

  if (!is.null(mc_results$summary)) {
    expect_true(all(
      c(
        "parameter",
        "true_value",
        "mean_estimate",
        "bias",
        "empirical_se",
        "mean_se",
        "coverage_95"
      ) %in%
        names(mc_results$summary)
    ))
  }
})

test_that("print_mc_summary produces output", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  mc_results <- run_hurdle_monte_carlo(
    n_sim = 3,
    n_subjects = 30,
    n_random_effects = 2,
    verbose = FALSE,
    seed = 789
  )

  expect_output(print_mc_summary(mc_results), "Monte Carlo Simulation Summary")
})

test_that("run_hurdle_monte_carlo accepts custom true_params", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  custom_params <- list(
    beta0 = -3,
    beta1 = 1.5,
    log_q0 = log(8),
    k = 2.5,
    alpha = 0.3,
    sigma_a = 0.8,
    sigma_b = 0.4,
    sigma_c = 0.1,
    rho_ab = 0.2,
    rho_ac = 0,
    rho_bc = 0,
    sigma_e = 0.25
  )

  mc_results <- run_hurdle_monte_carlo(
    n_sim = 2,
    n_subjects = 30,
    true_params = custom_params,
    n_random_effects = 2,
    verbose = FALSE,
    seed = 999
  )

  expect_equal(mc_results$true_params$beta0, -3)
  expect_equal(mc_results$true_params$alpha, 0.3)
})

# =============================================================================
# TICKET-044: simulate_hurdle_data(part2 = "snd") -- SND positive-part
# generator matching src/HurdleDemand3RE_SND.h / HurdleDemand2RE_SND.h
# exactly (lognormal positive part, no k; rho_bc via LKJ-Cholesky partial
# correlation).
# =============================================================================

test_that("(a) part2 argument validation", {
  expect_error(
    simulate_hurdle_data(n_subjects = 5, part2 = "bogus"),
    "should be one of"
  )
})

test_that("(a) part2 = 'snd' returns the standard structure", {
  sim_data <- simulate_hurdle_data(
    n_subjects = 30, part2 = "snd", n_random_effects = 3, seed = 123
  )
  expect_true(is.data.frame(sim_data))
  expect_true(all(
    c("id", "x", "y", "delta", "a_i", "b_i", "c_i") %in% names(sim_data)
  ))
  expect_equal(length(unique(sim_data$id)), 30)

  attrs <- attr(sim_data, "true_params")
  expect_identical(attrs$part2, "snd")
  expect_true(all(
    c("rho_ab_raw", "rho_ac_raw", "rho_bc_raw", "rho_ab", "rho_ac", "rho_bc") %in%
      names(attrs)
  ))
  # default raw correlations reproduce the koff generator's default ACTUAL
  # correlations (rho_ab = 0.3, rho_ac = 0, rho_bc = 0)
  expect_equal(attrs$rho_ab, 0.3, tolerance = 1e-8)
  expect_equal(attrs$rho_ac, 0, tolerance = 1e-8)
  expect_equal(attrs$rho_bc, 0, tolerance = 1e-8)
})

test_that("(a) part2 = 'snd' works for 2-RE and 3-RE", {
  sim2 <- simulate_hurdle_data(
    n_subjects = 20, part2 = "snd", n_random_effects = 2, seed = 123
  )
  expect_false("c_i" %in% names(sim2))

  sim3 <- simulate_hurdle_data(
    n_subjects = 20, part2 = "snd", n_random_effects = 3, seed = 123
  )
  expect_true("c_i" %in% names(sim3))
})

test_that("(b) structural: zero fraction tracks the specified hurdle probability", {
  skip_on_cran()
  set.seed(2026)
  beta0 <- -1
  beta1 <- 0.5
  prices <- seq(0, 10, by = 0.5)
  sim_data <- simulate_hurdle_data(
    n_subjects = 400, prices = prices, part2 = "snd",
    n_random_effects = 2, beta0 = beta0, beta1 = beta1,
    sigma_a = 1e-6, sigma_b = 1e-6, stop_at_zero = FALSE, seed = 2026
  )
  # sigma_a ~ 0 collapses Part I to the fixed-effect logistic curve
  expected_p0 <- 1 / (1 + exp(-(beta0 + beta1 * log(prices + 0.001))))
  observed_p0 <- vapply(prices, function(p) {
    mean(sim_data$y[sim_data$x == p] == 0)
  }, numeric(1))
  expect_equal(observed_p0, expected_p0, tolerance = 0.08)
})

test_that("(b) structural: positive part follows the SND lognormal shape", {
  skip_on_cran()
  log_q0 <- log(10)
  alpha <- 0.05
  price <- 3
  sim_data <- simulate_hurdle_data(
    n_subjects = 2000, prices = price, part2 = "snd",
    n_random_effects = 2, log_q0 = log_q0, alpha = alpha,
    sigma_a = 1e-6, sigma_b = 1e-6, sigma_e = 0.2,
    beta0 = -10, beta1 = 0, stop_at_zero = FALSE, seed = 321
  )
  # beta0 = -10 -> essentially never zero at any price
  pos <- sim_data$y[sim_data$y > 0]
  expect_gt(length(pos), 1900)
  q0 <- exp(log_q0)
  expected_mu <- log_q0 - alpha * q0 * price
  expect_equal(mean(log(pos)), expected_mu, tolerance = 0.02)
  expect_equal(sd(log(pos)), 0.2, tolerance = 0.03)
})

test_that("(c) identity check: fit_demand_hurdle(part2 = 'snd') recovers population truth", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  # Codex 2F review fold, item 7: exercise NONZERO raw correlations (not
  # just the rho_ac_raw = rho_bc_raw = 0 defaults) and add RE-SD/
  # correlation recovery assertions, loose tolerances (correlations are the
  # hardest population quantities to recover at this N).
  truth <- list(
    beta0 = -2, beta1 = 1, log_q0 = log(10), alpha = 0.5,
    sigma_a = 1, sigma_b = 0.5, sigma_c = 0.1, sigma_e = 0.3,
    rho_ab_raw = atanh(0.3), rho_ac_raw = atanh(0.15), rho_bc_raw = atanh(0.1)
  )
  d <- simulate_hurdle_data(
    n_subjects = 300, prices = seq(0, 11, by = 1), seed = 20260817,
    part2 = "snd", n_random_effects = 3,
    beta0 = truth$beta0, beta1 = truth$beta1, log_q0 = truth$log_q0,
    alpha = truth$alpha, sigma_a = truth$sigma_a, sigma_b = truth$sigma_b,
    sigma_c = truth$sigma_c, sigma_e = truth$sigma_e,
    rho_ab_raw = truth$rho_ab_raw, rho_ac_raw = truth$rho_ac_raw,
    rho_bc_raw = truth$rho_bc_raw, stop_at_zero = FALSE
  )

  fit <- fit_demand_hurdle(
    d, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"), part2 = "snd", verbose = 0,
    tmb_control = list(max_iter = 300)
  )
  expect_true(fit$converged)

  co <- fit$model$coefficients
  expect_equal(unname(co[["beta0"]]), truth$beta0, tolerance = 0.5)
  expect_equal(unname(co[["beta1"]]), truth$beta1, tolerance = 0.5)
  expect_equal(unname(co[["log_q0"]]), truth$log_q0, tolerance = 0.3)
  expect_equal(unname(co[["log_alpha"]]), log(truth$alpha), tolerance = 0.3)
  expect_equal(exp(unname(co[["logsigma_e"]])), truth$sigma_e, tolerance = 0.15)

  # RE-SD recovery (natural scale, loose tolerances)
  expect_equal(exp(unname(co[["logsigma_a"]])), truth$sigma_a, tolerance = 0.5)
  expect_equal(exp(unname(co[["logsigma_b"]])), truth$sigma_b, tolerance = 0.3)
  expect_equal(exp(unname(co[["logsigma_c"]])), truth$sigma_c, tolerance = 0.3)

  # Raw-correlation recovery: truth values are small in magnitude, so use
  # an ABSOLUTE tolerance (expect_equal()'s default relative tolerance
  # would be unreasonably tight near zero) -- very loose, correlations are
  # the hardest population quantities to recover here.
  expect_lt(abs(unname(co[["rho_ab_raw"]]) - truth$rho_ab_raw), 0.6)
  expect_lt(abs(unname(co[["rho_ac_raw"]]) - truth$rho_ac_raw), 0.6)
  expect_lt(abs(unname(co[["rho_bc_raw"]]) - truth$rho_bc_raw), 0.6)
})

test_that("(d) part2 = 'koff' output is byte-identical to before TICKET-044", {
  # Golden values captured from the pre-TICKET-044 simulate_hurdle_data()
  # (develop @ 556331f) with n_subjects = 5, seed = 123, all other args
  # default.
  golden_y <- c(
    24.23241845, 0, 14.19131998, 6.54532277, 0, 8.02330741, 4.36619251,
    2.72977581, 2.25459211, 0, 5.25848878, 3.87925532, 3.17401286,
    2.65461189, 2.52639435, 1.98675426, 1.28650758, 1.81582762, 0,
    10.1350845, 2.01136935, 3.42356896, 1.98971329, 2.18757744, 1.88370644,
    0
  )
  golden_x <- c(
    0, 0.5, 0, 0.5, 1, 0, 0.5, 1, 1.5, 2, 0, 0.5, 1, 1.5, 2, 2.5,
    3, 3.5, 4, 0, 0.5, 1, 1.5, 2, 2.5, 3
  )
  golden_id <- c(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5)

  d_default <- simulate_hurdle_data(n_subjects = 5, seed = 123)
  d_explicit <- simulate_hurdle_data(n_subjects = 5, seed = 123, part2 = "koff")

  expect_equal(nrow(d_default), 26)
  expect_equal(round(d_default$y, 8), golden_y)
  expect_equal(round(d_default$x, 8), golden_x)
  expect_equal(as.numeric(as.character(d_default$id)), golden_id)
  expect_identical(d_default, d_explicit)

  attrs <- attr(d_default, "true_params")
  expect_false("part2" %in% names(attrs))
  expect_identical(
    names(attrs),
    c(
      "beta0", "beta1", "log_q0", "k", "alpha", "sigma_a", "sigma_b",
      "sigma_c", "rho_ab", "rho_ac", "rho_bc", "sigma_e", "n_random_effects"
    )
  )
})

test_that("(d) item 7: part2 = 'koff' is FULLY byte-identical to before TICKET-044 (rds fixture)", {
  # Codex 2F review fold, item 7: the rounded spot-checks above only cover
  # `y`/`x`/`id`; this compares the ENTIRE unrounded data frame (all
  # columns, all attributes, including "delta"/"a_i"/"b_i" and the full
  # true_params list) via identical() against a golden fixture regenerated
  # from the actual pre-TICKET-044 simulate_hurdle_data() source (`git show
  # 6861cdc:R/hurdle-simulate.R`, sourced standalone and run with the exact
  # same n_subjects = 5 / seed = 123 call) -- not hand-transcribed values.
  golden <- readRDS(test_path("fixtures", "golden-hurdle-koff-n5-seed123.rds"))
  current <- simulate_hurdle_data(n_subjects = 5, seed = 123)
  expect_identical(current, golden)
})

# =============================================================================
# Codex 2F review fold (TICKET-044): blocking item 2
# =============================================================================

test_that("item 2: positional calls through `seed` are unaffected by the new part2/rho_*_raw args", {
  # Pre-fold, part2/rho_ab_raw/rho_ac_raw/rho_bc_raw were inserted BEFORE
  # seed, so an old positional call's 19th argument (seed) would bind to
  # part2 instead, and match.arg() would error (or silently misbehave).
  golden_y <- c(
    24.23241845, 0, 14.19131998, 6.54532277, 0, 8.02330741, 4.36619251,
    2.72977581, 2.25459211, 0, 5.25848878, 3.87925532, 3.17401286,
    2.65461189, 2.52639435, 1.98675426, 1.28650758, 1.81582762, 0,
    10.1350845, 2.01136935, 3.42356896, 1.98971329, 2.18757744, 1.88370644,
    0
  )

  d_positional <- simulate_hurdle_data(
    5, seq(0, 11, by = 0.5), -2, 1, log(10), lifecycle::deprecated(), 2, 0.5,
    1, 0.5, 0.1, 0.3, 0, 0, 0.3, 0.001, 2, TRUE, 123
  )

  expect_equal(nrow(d_positional), 26)
  expect_equal(round(d_positional$y, 8), golden_y)

  d_named <- simulate_hurdle_data(n_subjects = 5, seed = 123)
  expect_identical(d_positional, d_named)
})

# Release 0.3.0 Codex whole-release fold: supplying `seed` must not overwrite
# the caller's RNG stream (same guarantee power_demand()/boot_demand() give).
test_that("simulate_hurdle_data(seed = ) restores the caller's RNG state", {
  set.seed(999)
  before <- .Random.seed
  d1 <- simulate_hurdle_data(n_subjects = 3, seed = 42)
  expect_identical(.Random.seed, before)
  # Same seed reproduces; and the caller's stream continues where it was.
  d2 <- simulate_hurdle_data(n_subjects = 3, seed = 42)
  expect_identical(d1, d2)
  set.seed(999)
  x_ref <- runif(1)
  set.seed(999)
  invisible(simulate_hurdle_data(n_subjects = 3, seed = 42))
  expect_identical(runif(1), x_ref)
})

test_that("simulate_hurdle_data(seed = ) leaves no RNG state when none existed", {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(simulate_hurdle_data(n_subjects = 3, seed = 42))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("run_hurdle_monte_carlo(seed = ) restores the caller's RNG state", {
  skip_on_cran()
  set.seed(999)
  before <- .Random.seed
  suppressWarnings(run_hurdle_monte_carlo(
    n_sim = 1, n_subjects = 30, n_random_effects = 2, seed = 42, verbose = FALSE
  ))
  expect_identical(.Random.seed, before)
})
