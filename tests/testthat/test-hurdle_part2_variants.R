# Tests for hurdle Part II variants added in WS4.4 / WS4.5

simulate_hurdle_part2_data <- function(
  n_subjects,
  prices,
  part2 = c("exponential", "simplified_exponential"),
  seed = 123
) {
  part2 <- match.arg(part2)
  set.seed(seed)

  # Fixed effects (Part I)
  beta0 <- -3
  beta1 <- 2
  epsilon <- 0.001

  # Part II parameters (natural scale)
  log_q0 <- 0 # Q0 centered around 1
  alpha <- 0.8
  k <- 4

  # Random effects + residual
  sigma_a <- 0.6
  sigma_b <- 0.6
  sigma_e <- 0.15

  ids <- seq_len(n_subjects)
  a_i <- stats::rnorm(n_subjects, mean = 0, sd = sigma_a)
  b_i <- stats::rnorm(n_subjects, mean = 0, sd = sigma_b)

  grid <- expand.grid(
    id = ids,
    x = prices,
    stringsAsFactors = FALSE
  )

  y <- numeric(nrow(grid))

  for (row_idx in seq_len(nrow(grid))) {
    subj <- grid$id[[row_idx]]
    p <- grid$x[[row_idx]]

    eta <- beta0 + beta1 * log(p + epsilon) + a_i[[subj]]
    prob_zero <- stats::plogis(eta)
    delta <- stats::rbinom(1, size = 1, prob = prob_zero)

    if (delta == 1) {
      y[[row_idx]] <- 0
      next
    }

    Q0_i <- exp(log_q0 + b_i[[subj]])
    mu <- if (identical(part2, "simplified_exponential")) {
      (log_q0 + b_i[[subj]]) - alpha * Q0_i * p
    } else {
      (log_q0 + b_i[[subj]]) + k * (exp(-alpha * Q0_i * p) - 1)
    }

    logQ <- stats::rnorm(1, mean = mu, sd = sigma_e)
    y[[row_idx]] <- exp(logQ)
  }

  grid$y <- y
  grid
}

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

