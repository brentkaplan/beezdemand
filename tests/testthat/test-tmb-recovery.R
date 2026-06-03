# Guard the compiled TMB template (src/MixedDemand.h) end-to-end, not just the R
# prediction mirror (.tmb_predict_equation). Simulate from each equation's
# published MEAN function with a small perturbation, fit through the compiled
# TMB objective, and confirm the population parameters are recovered. These are
# mean-function recovery probes: the small multiplicative perturbation keeps
# simulated consumption positive and is a convenience, not a claim about the
# equation-specific error model -- the point is that the compiled mean must
# match the published equation for the optimizer to recover the truth. If a C++
# mean branch drifted from its published form, recovery would fail here even
# though the R-mirror equation tests still passed. (Codex audit recommendation;
# complements test-statistical-corrections.R.)
#
# Coverage: eqn_type 0 (exponential/HS), 1 (exponentiated/Koffarnus),
# 2 (simplified/SND). The zben branch (eqn_type 3) is guarded by the R
# prediction mirror; a compiled-recovery probe was excluded because its analytic
# start heuristic places beta_q0 at log(ln(Q0)) rather than log(Q0), which would
# make a recovery failure ambiguous (start quality vs template) rather than a
# clean template guard.

test_that("fit_demand_tmb recovers known simplified-equation population parameters", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  set.seed(20260603)
  n_sub <- 60
  prices <- c(0, 1, 2, 4, 8, 16, 24, 36)
  Q0_true <- 20
  alpha_true <- 0.006
  sd_b <- 0.30 # log-Q0 random-effect SD

  dat <- do.call(rbind, lapply(seq_len(n_sub), function(i) {
    q0_i <- Q0_true * exp(stats::rnorm(1, 0, sd_b)) # multiplicative RE on Q0
    mu <- q0_i * exp(-alpha_true * q0_i * prices) # SND mean (eqn_type = 2)
    y <- mu * exp(stats::rnorm(length(prices), 0, 0.04)) # small noise, stays > 0
    data.frame(id = i, x = prices, y = y)
  }))

  fit <- fit_demand_tmb(
    dat, y_var = "y", x_var = "x", id_var = "id",
    equation = "simplified", random_effects = "q0",
    multi_start = FALSE, verbose = 0
  )
  co <- coef(fit)
  Q0_hat <- exp(unname(co[["beta_q0"]]))
  alpha_hat <- exp(unname(co[["beta_alpha"]]))

  # Population parameters recovered (the RE is centred, so beta_q0 targets the
  # median/geometric-mean Q0 = Q0_true).
  expect_equal(Q0_hat, Q0_true, tolerance = 0.15)
  expect_equal(alpha_hat, alpha_true, tolerance = 0.15)
})

test_that("fit_demand_tmb recovers known exponential (HS) population parameters", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  set.seed(20260604)
  n_sub <- 60
  prices <- c(0, 1, 2, 4, 8, 16, 24, 36)
  Q0_true <- 20
  alpha_true <- 0.006
  k_true <- 2.5
  sd_b <- 0.30
  ln10 <- log(10)

  # Internal response for eqn_type 0 is log(Q); the compiled mean is
  #   mu = log(Q0) + k*ln10*(exp(-alpha*Q0*price) - 1).
  # Simulate consumption Q (fit_demand_tmb log-transforms it internally).
  dat <- do.call(rbind, lapply(seq_len(n_sub), function(i) {
    q0_i <- Q0_true * exp(stats::rnorm(1, 0, sd_b))
    mu <- log(q0_i) + k_true * ln10 * (exp(-alpha_true * q0_i * prices) - 1)
    y <- exp(mu + stats::rnorm(length(prices), 0, 0.04)) # consumption > 0
    data.frame(id = i, x = prices, y = y)
  }))

  fit <- fit_demand_tmb(
    dat, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", random_effects = "q0",
    estimate_k = FALSE, k = k_true,
    multi_start = FALSE, verbose = 0
  )
  co <- coef(fit)
  expect_equal(exp(unname(co[["beta_q0"]])), Q0_true, tolerance = 0.15)
  expect_equal(exp(unname(co[["beta_alpha"]])), alpha_true, tolerance = 0.15)
})

test_that("fit_demand_tmb recovers known exponentiated (Koffarnus) population parameters", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  set.seed(20260605)
  n_sub <- 60
  prices <- c(0, 1, 2, 4, 8, 16, 24, 36)
  Q0_true <- 20
  alpha_true <- 0.006
  k_true <- 2.5
  sd_b <- 0.30

  # eqn_type 1 mean on raw Q: Q = Q0 * 10^(k*(exp(-alpha*Q0*price) - 1)).
  dat <- do.call(rbind, lapply(seq_len(n_sub), function(i) {
    q0_i <- Q0_true * exp(stats::rnorm(1, 0, sd_b))
    mu <- q0_i * 10^(k_true * (exp(-alpha_true * q0_i * prices) - 1))
    y <- mu * exp(stats::rnorm(length(prices), 0, 0.04)) # stays > 0
    data.frame(id = i, x = prices, y = y)
  }))

  fit <- fit_demand_tmb(
    dat, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponentiated", random_effects = "q0",
    estimate_k = FALSE, k = k_true,
    multi_start = FALSE, verbose = 0
  )
  co <- coef(fit)
  expect_equal(exp(unname(co[["beta_q0"]])), Q0_true, tolerance = 0.15)
  expect_equal(exp(unname(co[["beta_alpha"]])), alpha_true, tolerance = 0.15)
})
