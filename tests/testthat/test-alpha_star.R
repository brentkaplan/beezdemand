# Tests for Strategy B alpha_star (WS3.8)

test_that(".calc_alpha_star matches closed-form (natural space, base 10)", {
  res <- beezdemand:::.calc_alpha_star(
    params = list(alpha = 0.01, k = 3),
    param_scales = list(alpha = "natural", k = "natural"),
    base = "10"
  )

  expected <- -0.01 / log(1 - 1 / (3 * log(10)))
  expect_equal(res$estimate, expected, tolerance = 1e-12)
  expect_true(is.na(res$se))
})

test_that(".calc_alpha_star returns NA with note on domain violation", {
  res <- beezdemand:::.calc_alpha_star(
    params = list(alpha = 0.01, k = 0.4),
    param_scales = list(alpha = "natural", k = "natural"),
    base = "10"
  )

  expect_true(is.na(res$estimate))
  expect_true(is.na(res$se))
  expect_match(res$note, "must be > 1")
})

test_that(".calc_alpha_star delta-method SE matches finite-difference gradient (log space)", {
  theta_alpha <- log(0.5)
  theta_k <- log(3)
  Sigma <- matrix(
    c(0.02^2, 0.0001,
      0.0001, 0.03^2),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("log_alpha", "log_k"), c("log_alpha", "log_k"))
  )

  res <- beezdemand:::.calc_alpha_star(
    params = list(log_alpha = theta_alpha, log_k = theta_k),
    param_scales = list(log_alpha = "log", log_k = "log"),
    vcov = Sigma,
    base = "e"
  )

  fn <- function(ta, tk) {
    beezdemand:::.calc_alpha_star(
      params = list(log_alpha = ta, log_k = tk),
      param_scales = list(log_alpha = "log", log_k = "log"),
      base = "e"
    )$estimate
  }

  h <- 1e-6
  g_alpha <- (fn(theta_alpha + h, theta_k) - fn(theta_alpha - h, theta_k)) / (2 * h)
  g_k <- (fn(theta_alpha, theta_k + h) - fn(theta_alpha, theta_k - h)) / (2 * h)
  g <- c(log_alpha = g_alpha, log_k = g_k)

  expected_se <- sqrt(as.numeric(t(g) %*% Sigma %*% g))
  expect_equal(res$se, expected_se, tolerance = 1e-6)
})

test_that(".calc_alpha_star raw partials match finite difference (natural space)", {
  # Natural space has identity chain-rule Jacobian, so finite-differencing the
  # ACTUAL .calc_alpha_star() estimate isolates the hand-coded raw partials
  # (R/alpha-star.R:160-161) -- the log/log10 tests above cannot separate the
  # partials from the d(natural)/d(theta) factor. Hand derivation (base 10):
  #   L_k = ln(1 - 1/(k*c)),  c = ln(10)
  #   d(alpha*)/d(alpha) = -1 / L_k
  #   d(alpha*)/dk       =  alpha / (k*(c*k - 1)*L_k^2)
  alpha <- 0.01
  k <- 3
  Sigma <- matrix(
    c(0.002^2, 1e-7,
      1e-7, 0.05^2),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("alpha", "k"), c("alpha", "k"))
  )
  res <- beezdemand:::.calc_alpha_star(
    params = list(alpha = alpha, k = k),
    param_scales = list(alpha = "natural", k = "natural"),
    vcov = Sigma,
    base = "10"
  )

  fn <- function(a, kk) {
    beezdemand:::.calc_alpha_star(
      params = list(alpha = a, k = kk),
      param_scales = list(alpha = "natural", k = "natural"),
      base = "10"
    )$estimate
  }
  h <- 1e-7
  g_alpha <- (fn(alpha + h, k) - fn(alpha - h, k)) / (2 * h)
  g_k <- (fn(alpha, k + h) - fn(alpha, k - h)) / (2 * h)

  c_const <- log(10)
  L_k <- log(1 - 1 / (k * c_const))
  expect_equal(g_alpha, -1 / L_k, tolerance = 1e-5)
  expect_equal(g_k, alpha / (k * (c_const * k - 1) * L_k^2), tolerance = 1e-5)

  g <- c(alpha = g_alpha, k = g_k)
  expected_se <- sqrt(as.numeric(t(g) %*% Sigma %*% g))
  expect_equal(res$se, expected_se, tolerance = 1e-6)
})

test_that(".calc_alpha_star delta-method SE matches finite-difference gradient (log10 space)", {
  theta_alpha <- log10(0.01)
  theta_k <- log10(3)
  Sigma <- matrix(
    c(0.01^2, 0.00005,
      0.00005, 0.02^2),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("log10_alpha", "log10_k"), c("log10_alpha", "log10_k"))
  )

  res <- beezdemand:::.calc_alpha_star(
    params = list(log10_alpha = theta_alpha, log10_k = theta_k),
    param_scales = list(log10_alpha = "log10", log10_k = "log10"),
    vcov = Sigma,
    base = "10"
  )

  fn <- function(ta, tk) {
    beezdemand:::.calc_alpha_star(
      params = list(log10_alpha = ta, log10_k = tk),
      param_scales = list(log10_alpha = "log10", log10_k = "log10"),
      base = "10"
    )$estimate
  }

  h <- 1e-6
  g_alpha <- (fn(theta_alpha + h, theta_k) - fn(theta_alpha - h, theta_k)) / (2 * h)
  g_k <- (fn(theta_alpha, theta_k + h) - fn(theta_alpha, theta_k - h)) / (2 * h)
  g <- c(log10_alpha = g_alpha, log10_k = g_k)

  expected_se <- sqrt(as.numeric(t(g) %*% Sigma %*% g))
  expect_equal(res$se, expected_se, tolerance = 1e-6)
})
