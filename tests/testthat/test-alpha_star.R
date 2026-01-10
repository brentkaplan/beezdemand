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
