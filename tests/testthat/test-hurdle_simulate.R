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
    c("beta0", "beta1", "logQ0", "k", "alpha") %in% names(true_params)
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
    logQ0 = log(8),
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
