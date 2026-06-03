# Guard the compiled TMB template (src/MixedDemand.h) end-to-end, not just the R
# prediction mirror (.tmb_predict_equation). Simulate from a known simplified-
# equation data-generating process, fit through the compiled TMB objective, and
# confirm the population parameters are recovered. If the C++ mean function
# drifted from the published SND equation Q = Q0*exp(-alpha*Q0*price), recovery
# would fail here even though the R-mirror equation tests still passed.
# (Codex audit recommendation, complements test-statistical-corrections.R.)

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
