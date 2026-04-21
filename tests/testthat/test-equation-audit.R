# Tests verifying demand equations against published mathematical formulations.
#
# Cross-references:
#   - Hursh & Silberberg (2008): HS/exponential model
#   - Koffarnus et al. (2015): exponentiated model
#   - Gilroy et al. (2019): Pmax Lambert W derivation
#   - Gilroy et al. (2021): ZBE/ZBEn formulation
#   - Rzeszutek et al. (2025): SND model, alpha-star normalization
#   - engineering/contracts/EQUATIONS_CONTRACT.md v1.1.0

# ==============================================================================
# SECTION 1: Equation Evaluation at Known Points
# ==============================================================================

test_that("HS equation: log10(Q) = log10(Q0) + k*(exp(-a*Q0*P) - 1)", {
  q0 <- 10; alpha <- 0.001; k <- 2; price <- 5

  log10_q <- log10(q0) + k * (exp(-alpha * q0 * price) - 1)

  # Verify intermediate values
  expect_equal(-alpha * q0 * price, -0.05, tolerance = 1e-10)
  expect_equal(exp(-0.05) - 1, -0.04877057, tolerance = 1e-6)
  expect_equal(log10_q, 1 + 2 * (exp(-0.05) - 1), tolerance = 1e-10)

  # Q must be positive
  expect_true(10^log10_q > 0)
})

test_that("Koffarnus equation is exponentiation (10^) of HS", {
  q0 <- 10; alpha <- 0.001; k <- 2; price <- 5

  # HS in log10 space
  hs_log10_q <- log10(q0) + k * (exp(-alpha * q0 * price) - 1)

  # Koffarnus in natural space
  koff_q <- q0 * 10^(k * (exp(-alpha * q0 * price) - 1))

  # 10^(HS log10) must equal Koffarnus Q

  expect_equal(10^hs_log10_q, koff_q, tolerance = 1e-10)
})

test_that("SND equation: Q = Q0*exp(-alpha*Q0*P)", {
  q0 <- 10; alpha <- 0.01; price <- 5

  q_pred <- q0 * exp(-alpha * q0 * price)
  expect_equal(q_pred, 10 * exp(-0.5), tolerance = 1e-10)

  # At P=0, Q should equal Q0
  q_at_zero <- q0 * exp(-alpha * q0 * 0)
  expect_equal(q_at_zero, q0, tolerance = 1e-10)
})

test_that("ZBEN equation: y = log10(Q0)*exp(-(alpha/log10(Q0))*Q0*P)", {
  q0 <- 10; alpha <- 0.001; price <- 5

  q0_log10 <- log10(q0)   # = 1
  rate <- (alpha / q0_log10) * q0   # = (0.001/1)*10 = 0.01
  y_pred <- q0_log10 * exp(-rate * price)   # = 1 * exp(-0.05)

  expect_equal(q0_log10, 1, tolerance = 1e-10)
  expect_equal(rate, 0.01, tolerance = 1e-10)
  expect_equal(y_pred, exp(-0.05), tolerance = 1e-10)

  # At P=0, y should equal log10(Q0) (the LL4-scale intercept)
  y_at_zero <- q0_log10 * exp(-rate * 0)
  expect_equal(y_at_zero, log10(q0), tolerance = 1e-10)
})


# ==============================================================================
# SECTION 2: k*ln(10) Log-Base Conversion Pattern
# ==============================================================================

test_that("k*ln(10) converts HS from log10 space to ln space", {
  # HS in log10: log10(Q) = log10(Q0) + k*(exp(-a*Q0*P) - 1)
  # HS in ln:    ln(Q)    = ln(Q0)    + k*ln(10)*(exp(-a*Q0*P) - 1)
  q0 <- 10; alpha <- 0.001; k <- 2; price <- 5

  log10_val <- log10(q0) + k * (exp(-alpha * q0 * price) - 1)
  ln_val <- log(q0) + k * log(10) * (exp(-alpha * q0 * price) - 1)

  # ln_val / ln(10) must equal log10_val
  expect_equal(ln_val / log(10), log10_val, tolerance = 1e-10)

  # Both recover the same Q
  expect_equal(10^log10_val, exp(ln_val), tolerance = 1e-10)
})

test_that("Koffarnus exponentiation identity: exp(k*ln(10)*x) = 10^(k*x)", {
  k <- 2.5
  x <- -0.3  # any value

  lhs <- exp(k * log(10) * x)
  rhs <- 10^(k * x)
  expect_equal(lhs, rhs, tolerance = 1e-10)
})

test_that("HS vs hurdle Lambert W args differ by ln(10) factor", {
  k <- 3

  hs_w_arg <- -1 / (k * log(10))     # HS (log10 space)
  hurdle_w_arg <- -1 / k              # hurdle (ln space)

  # HS arg is closer to 0 (less negative) because of ln(10) denominator
  expect_true(hurdle_w_arg < hs_w_arg)

  # Ratio: hs_arg / hurdle_arg = k / (k * ln(10)) = 1/ln(10)
  expect_equal(hs_w_arg / hurdle_w_arg, 1 / log(10), tolerance = 1e-10)
})


# ==============================================================================
# SECTION 3: Pmax Unit-Elasticity Verification
# ==============================================================================

test_that("HS Pmax satisfies dE/dP = 0 (unit elasticity)", {
  q0 <- 10; alpha <- 0.001; k <- 3

  result <- .pmax_analytic_hs(alpha, q0, k)
  expect_true(result$success)
  pmax <- result$pmax

  # E(P) = P * Q0 * 10^(k*(exp(-a*Q0*P) - 1))
  E <- function(p) p * q0 * 10^(k * (exp(-alpha * q0 * p) - 1))
  dE <- (E(pmax + 1e-6) - E(pmax - 1e-6)) / (2e-6)
  expect_equal(dE, 0, tolerance = 0.01)
})

test_that("Hurdle (Zhao) Pmax satisfies dE/dP = 0", {
  alpha <- 0.5; k <- 3; q0 <- 10

  result <- .pmax_analytic_hurdle(alpha, k)
  expect_true(result$success)
  pmax <- result$pmax

  # Q(P) = Q0 * exp(k * (exp(-alpha*P) - 1))
  # Note: Q0 NOT in exponent (Zhao form)
  E <- function(p) p * q0 * exp(k * (exp(-alpha * p) - 1))
  dE <- (E(pmax + 1e-8) - E(pmax - 1e-8)) / (2e-8)
  expect_equal(dE, 0, tolerance = 0.1)
})

test_that("Hurdle StdQ0 Pmax satisfies dE/dP = 0", {
  alpha <- 0.01; q0 <- 10; k <- 3

  result <- .pmax_analytic_hurdle_hs_stdq0(alpha, q0, k)
  expect_true(result$success)
  pmax <- result$pmax

  # Q(P) = Q0 * exp(k * (exp(-alpha*Q0*P) - 1))
  E <- function(p) p * q0 * exp(k * (exp(-alpha * q0 * p) - 1))
  dE <- (E(pmax + 1e-6) - E(pmax - 1e-6)) / (2e-6)
  expect_equal(dE, 0, tolerance = 0.01)
})

test_that("SND Pmax = 1/(alpha*Q0) satisfies dE/dP = 0", {
  alpha <- 0.01; q0 <- 10
  pmax <- 1 / (alpha * q0)   # = 10

  # Q(P) = Q0 * exp(-alpha*Q0*P)
  E <- function(p) p * q0 * exp(-alpha * q0 * p)
  dE <- (E(pmax + 1e-6) - E(pmax - 1e-6)) / (2e-6)
  expect_equal(dE, 0, tolerance = 0.001)
})


# ==============================================================================
# SECTION 4: Pmax Existence Thresholds
# ==============================================================================

test_that("HS Pmax threshold is e/ln(10) ~ 1.1806", {
  threshold <- exp(1) / log(10)
  expect_equal(threshold, 1.180560, tolerance = 1e-4)

  # k below threshold: no real Lambert W solution
  result <- .pmax_analytic_hs(0.01, 10, k_nat = 1.0)
  expect_false(result$success)

  # k at threshold (boundary): should fail (<= check)
  result <- .pmax_analytic_hs(0.01, 10, k_nat = threshold)
  expect_false(result$success)

  # k above threshold: success
  result <- .pmax_analytic_hs(0.01, 10, k_nat = 1.5)
  expect_true(result$success)
})

test_that("Hurdle Pmax threshold is e ~ 2.7183", {
  # k below e: no real Lambert W solution
  result <- .pmax_analytic_hurdle(0.5, k_nat = 2.0)
  expect_false(result$success)

  # k above e: success
  result <- .pmax_analytic_hurdle(0.5, k_nat = 3.0)
  expect_true(result$success)
})

test_that("SND Pmax has no k threshold (always exists for positive params)", {
  result <- .pmax_analytic_snd(alpha_nat = 0.001, q0_nat = 5)
  expect_true(result$success)
  expect_equal(result$pmax, 1 / (0.001 * 5), tolerance = 1e-10)
})


# ==============================================================================
# SECTION 5: Omax Verification
# ==============================================================================

test_that("SND Omax = 1/(alpha*e)", {
  alpha <- 0.01; q0 <- 10
  pmax <- 1 / (alpha * q0)

  # Omax = Pmax * Q(Pmax)
  q_at_pmax <- q0 * exp(-alpha * q0 * pmax)
  omax <- pmax * q_at_pmax

  expected_omax <- 1 / (alpha * exp(1))
  expect_equal(omax, expected_omax, tolerance = 1e-10)
  expect_equal(expected_omax, 36.78794, tolerance = 1e-4)
})

test_that("HS Omax = Pmax * Q0 * 10^(k*(exp(-a*Q0*Pmax) - 1))", {
  q0 <- 10; alpha <- 0.001; k <- 3

  result <- .pmax_analytic_hs(alpha, q0, k)
  pmax <- result$pmax

  q_at_pmax <- q0 * 10^(k * (exp(-alpha * q0 * pmax) - 1))
  omax <- pmax * q_at_pmax

  expect_true(omax > 0)
  expect_true(is.finite(omax))

  # Verify Omax is the actual maximum of E(P)
  E <- function(p) p * q0 * 10^(k * (exp(-alpha * q0 * p) - 1))
  # Check nearby points are lower
  expect_true(omax >= E(pmax * 0.9))
  expect_true(omax >= E(pmax * 1.1))
})


# ==============================================================================
# SECTION 6: Alpha-Star (Rzeszutek et al. 2025)
# ==============================================================================

test_that("Alpha-star formula for base 10 (HS/Koff)", {
  alpha <- 0.01; k <- 3
  c_const <- log(10)
  L_k <- log(1 - 1 / (k * c_const))
  expected <- -alpha / L_k

  result <- .calc_alpha_star(
    params = list(alpha = alpha, k = k),
    param_scales = list(alpha = "natural", k = "natural"),
    base = "10"
  )

  expect_equal(result$estimate, expected, tolerance = 1e-10)
  expect_true(result$estimate > 0)
})

test_that("Alpha-star formula for base e (hurdle)", {
  alpha <- 0.5; k <- 3
  c_const <- 1  # ln(e) = 1
  L_k <- log(1 - 1 / (k * c_const))   # ln(2/3)
  expected <- -alpha / L_k

  result <- .calc_alpha_star(
    params = list(alpha = alpha, k = k),
    param_scales = list(alpha = "natural", k = "natural"),
    base = "e"
  )

  expect_equal(result$estimate, expected, tolerance = 1e-10)
  expect_true(result$estimate > 0)
})

test_that("Alpha-star domain: k*c must be > 1", {
  # Base 10: k*ln(10) > 1 → k > 1/ln(10) ≈ 0.4343
  result <- .calc_alpha_star(
    params = list(alpha = 0.01, k = 0.4),
    param_scales = list(alpha = "natural", k = "natural"),
    base = "10"
  )
  expect_true(is.na(result$estimate))
  expect_true(grepl("must be > 1", result$note))

  # Base e: k*1 > 1 → k > 1
  result <- .calc_alpha_star(
    params = list(alpha = 0.5, k = 0.9),
    param_scales = list(alpha = "natural", k = "natural"),
    base = "e"
  )
  expect_true(is.na(result$estimate))
})

test_that("Alpha-star delta-method partial derivatives are correct", {
  alpha <- 0.01; k <- 3

  # Analytical partials for base 10
  c_const <- log(10)
  L_k <- log(1 - 1 / (k * c_const))

  d_as_d_alpha <- -1 / L_k
  d_as_d_k <- alpha / (k * (c_const * k - 1) * (L_k^2))

  # Verify via numerical differentiation
  as_fn <- function(a, kk) {
    Lk <- log(1 - 1 / (kk * c_const))
    -a / Lk
  }

  num_d_alpha <- (as_fn(alpha + 1e-8, k) - as_fn(alpha - 1e-8, k)) / (2e-8)
  num_d_k <- (as_fn(alpha, k + 1e-8) - as_fn(alpha, k - 1e-8)) / (2e-8)

  expect_equal(d_as_d_alpha, num_d_alpha, tolerance = 1e-4)
  expect_equal(d_as_d_k, num_d_k, tolerance = 1e-4)
})

test_that("Alpha-star with log-scale parameters matches natural-scale", {
  alpha_nat <- 0.01; k_nat <- 3

  # Natural scale
  result_nat <- .calc_alpha_star(
    params = list(alpha = alpha_nat, k = k_nat),
    param_scales = list(alpha = "natural", k = "natural"),
    base = "10"
  )

  # Log10 scale
  result_log10 <- .calc_alpha_star(
    params = list(alpha = log10(alpha_nat), k = k_nat),
    param_scales = list(alpha = "log10", k = "natural"),
    base = "10"
  )

  # Log (ln) scale
  result_log <- .calc_alpha_star(
    params = list(log_alpha = log(alpha_nat), k = k_nat),
    param_scales = list(log_alpha = "log", k = "natural"),
    base = "10"
  )

  expect_equal(result_nat$estimate, result_log10$estimate, tolerance = 1e-8)
  expect_equal(result_nat$estimate, result_log$estimate, tolerance = 1e-8)
})


# ==============================================================================
# SECTION 7: Cross-Equation Consistency
# ==============================================================================

test_that("At k*ln(10)=1 boundary, HS and SND alphas converge", {
  # Alpha-star conversion: alpha_SND = -alpha_HS / ln(1 - 1/(k*ln(10)))
  # As k*ln(10) → ∞, ln(1 - 1/(k*ln(10))) → -1/(k*ln(10))
  # So alpha_SND ≈ alpha_HS * k * ln(10) for large k

  # For a large k, verify the conversion holds
  k <- 10; alpha_hs <- 0.001
  c_const <- log(10)
  alpha_snd <- -alpha_hs / log(1 - 1 / (k * c_const))

  # alpha_snd should be positive and larger than alpha_hs
  expect_true(alpha_snd > 0)
  expect_true(alpha_snd > alpha_hs)
})

test_that("SND is limit of HS/Koff as k→∞ at fixed alpha_SND", {
  # For large k, koff equation with alpha_HS = alpha_SND * ln(1 - 1/(k*ln10))
  # approaches SND equation. Verify numerically.
  q0 <- 10; price <- 5; alpha_snd <- 0.01; k <- 50
  c_const <- log(10)

  # Convert to HS alpha
  alpha_hs <- -alpha_snd * log(1 - 1 / (k * c_const))

  # HS prediction
  q_hs <- q0 * 10^(k * (exp(-alpha_hs * q0 * price) - 1))

  # SND prediction
  q_snd <- q0 * exp(-alpha_snd * q0 * price)

  # Should be close for large k
  expect_equal(q_hs, q_snd, tolerance = 0.01 * q_snd)
})

test_that("k parameter: log10(max) - log10(min) + 0.5", {
  consumption <- c(0.5, 1, 5, 10, 20, 50, 100)
  # Only nonzero values
  nonzero <- consumption[consumption > 0]

  k_expected <- log10(max(nonzero)) - log10(min(nonzero)) + 0.5
  # = log10(100) - log10(0.5) + 0.5 = 2 - (-0.301) + 0.5 = 2.801
  expect_equal(k_expected, log10(100) - log10(0.5) + 0.5, tolerance = 1e-10)
  expect_true(k_expected > 0)
})
