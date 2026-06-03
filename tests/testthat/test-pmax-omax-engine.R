# Tests for pmax-omax-engine.R

test_that("Parameter scale conversion works correctly", {
  # Test natural -> natural (no change)
  expect_equal(.to_natural_scale(5, "natural"), 5)
  

  # Test log -> natural
  expect_equal(.to_natural_scale(0, "log"), 1)
  expect_equal(.to_natural_scale(1, "log"), exp(1), tolerance = 1e-10)
  expect_equal(.to_natural_scale(log(10), "log"), 10, tolerance = 1e-10)
  
  # Test log10 -> natural
  expect_equal(.to_natural_scale(0, "log10"), 1)

  expect_equal(.to_natural_scale(1, "log10"), 10)
  expect_equal(.to_natural_scale(2, "log10"), 100)
  expect_equal(.to_natural_scale(-2, "log10"), 0.01, tolerance = 1e-10)
  
  # Test NA handling
  expect_true(is.na(.to_natural_scale(NA, "natural")))
})

test_that("Standardize params handles mixed scales", {
  params <- list(alpha = -2, q0 = 1, k = 3)
  scales <- list(alpha = "log10", q0 = "log10", k = "natural")
  
  result <- .standardize_params_to_natural(params, scales)
  
  expect_equal(result$params_natural$alpha, 0.01, tolerance = 1e-10)
  expect_equal(result$params_natural$q0, 10, tolerance = 1e-10)
  expect_equal(result$params_natural$k, 3)
  
  # Check notes were generated for converted params
  expect_true(length(result$notes) >= 2)
})

test_that("HS analytic pmax matches expected formula", {
  # Test case: alpha=0.01, Q0=10, k=3
  # Pmax = -W_0(-1/(k*ln(10))) / (alpha * Q0)
  alpha_nat <- 0.01
  q0_nat <- 10
  k_nat <- 3
  
  result <- .pmax_analytic_hs(alpha_nat, q0_nat, k_nat)
  
  expect_true(result$success)
  expect_equal(result$method, "analytic_lambert_w")
  expect_true(result$pmax > 0)
  
  # Verify against direct calculation
  w_arg <- -1 / (k_nat * log(10))
  w_val <- lambertW(z = w_arg)
  expected_pmax <- -as.numeric(w_val) / (alpha_nat * q0_nat)
  
  expect_equal(result$pmax, expected_pmax, tolerance = 1e-8)
})

test_that("HS analytic pmax fails correctly when k <= threshold", {
  # k must be > e/ln(10) ≈ 1.18 for real solution
  alpha_nat <- 0.01
  q0_nat <- 10
  k_nat <- 1.0  # Below threshold
  
  result <- .pmax_analytic_hs(alpha_nat, q0_nat, k_nat)
  
  expect_false(result$success)
  expect_true(is.na(result$pmax))
  expect_true(grepl("Lambert W", result$note))
})

test_that("Hurdle analytic pmax uses correct formula (no Q0 normalization)", {
  # Hurdle: Pmax = -W_0(-1/k) / alpha
  alpha_nat <- 0.5
  k_nat <- 3
  
  result <- .pmax_analytic_hurdle(alpha_nat, k_nat)
  
  expect_true(result$success)
  expect_equal(result$method, "analytic_lambert_w_hurdle")
  expect_true(result$pmax > 0)
  
  # Verify against direct calculation
  w_arg <- -1 / k_nat
  w_val <- lambertW(z = w_arg)
  expected_pmax <- -as.numeric(w_val) / alpha_nat
  
  expect_equal(result$pmax, expected_pmax, tolerance = 1e-8)
})

test_that("Hurdle analytic pmax fails when k < e", {
  alpha_nat <- 0.5
  k_nat <- 2.0  # Below e ≈ 2.718
  
  result <- .pmax_analytic_hurdle(alpha_nat, k_nat)
  
  expect_false(result$success)
  expect_true(is.na(result$pmax))
  expect_true(grepl("k.*< e", result$note))
})

test_that("SND analytic pmax is correct closed form", {
  # SND: Pmax = 1 / (alpha * Q0)
  alpha_nat <- 0.01
  q0_nat <- 10
  
  result <- .pmax_analytic_snd(alpha_nat, q0_nat)
  
  expect_true(result$success)
  expect_equal(result$method, "analytic_snd")
  expect_equal(result$pmax, 1 / (alpha_nat * q0_nat), tolerance = 1e-10)
  expect_equal(result$pmax, 10, tolerance = 1e-10)
})

test_that("Parameter space conversions yield identical pmax for HS model", {
  # Same underlying natural parameters, provided in different scales
  alpha_nat <- 0.01
  q0_nat <- 10
  k_nat <- 3
  
  # Natural scale input
  result_nat <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = alpha_nat, q0 = q0_nat, k = k_nat),
    param_scales = list(alpha = "natural", q0 = "natural", k = "natural")
  )
  
  # Log10 scale input (same underlying values)
  result_log10 <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = log10(alpha_nat), q0 = log10(q0_nat), k = k_nat),
    param_scales = list(alpha = "log10", q0 = "log10", k = "natural")
  )
  
  # Log (ln) scale input
  result_log <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = log(alpha_nat), q0 = log(q0_nat), k = k_nat),
    param_scales = list(alpha = "log", q0 = "log", k = "natural")
  )
  
  # All should produce same pmax
  expect_equal(result_nat$pmax_model, result_log10$pmax_model, tolerance = 1e-8)
  expect_equal(result_nat$pmax_model, result_log$pmax_model, tolerance = 1e-8)
  
  # Method should be the same
  expect_equal(result_nat$method_model, "analytic_lambert_w")
  expect_equal(result_log10$method_model, "analytic_lambert_w")
  expect_equal(result_log$method_model, "analytic_lambert_w")
})

test_that("Parameter space conversions yield identical pmax for SND model", {
  alpha_nat <- 0.01
  q0_nat <- 10
  
  # Natural scale input
  result_nat <- beezdemand_calc_pmax_omax(
    model_type = "snd",
    params = list(alpha = alpha_nat, q0 = q0_nat),
    param_scales = list(alpha = "natural", q0 = "natural")
  )
  
  # Log10 scale input
  result_log10 <- beezdemand_calc_pmax_omax(
    model_type = "snd",
    params = list(alpha = log10(alpha_nat), q0 = log10(q0_nat)),
    param_scales = list(alpha = "log10", q0 = "log10")
  )
  
  # Log (ln) scale input
  result_log <- beezdemand_calc_pmax_omax(
    model_type = "snd",
    params = list(alpha = log(alpha_nat), q0 = log(q0_nat)),
    param_scales = list(alpha = "log", q0 = "log")
  )
  
  # All should produce same pmax
  expect_equal(result_nat$pmax_model, result_log10$pmax_model, tolerance = 1e-8)
  expect_equal(result_nat$pmax_model, result_log$pmax_model, tolerance = 1e-8)
  expect_equal(result_nat$pmax_model, 10, tolerance = 1e-8)  # 1/(0.01 * 10) = 10
})

test_that("Hurdle model with ln-space params produces correct pmax", {
  # Hurdle uses log (ln) parameterization internally
  alpha_nat <- 0.5
  k_nat <- 3
  q0_nat <- 10
  
  result <- beezdemand_calc_pmax_omax(
    model_type = "hurdle",
    params = list(
      log_alpha = log(alpha_nat),
      log_q0 = log(q0_nat),
      log_k = log(k_nat)
    ),
    param_scales = list(
      log_alpha = "log",
      log_q0 = "log",
      log_k = "log"
    )
  )
  
  expect_true(!is.na(result$pmax_model))
  expect_equal(result$alpha_scale_in, "log")
  
  # Verify pmax formula: -W_0(-1/k) / alpha
  w_val <- lambertW(z = -1/k_nat)
  expected_pmax <- -as.numeric(w_val) / alpha_nat
  expect_equal(result$pmax_model, expected_pmax, tolerance = 1e-8)
})

test_that("Observed pmax/omax calculation is correct", {
  price <- c(0.5, 1, 2, 5, 10)
  consumption <- c(100, 80, 40, 10, 0)
  
  result <- .calc_observed_pmax_omax(price, consumption)
  
  # Expenditure: 50, 80, 80, 50, 0 -> max is 80 at prices 1 and 2
  expect_equal(result$omax_obs, 80)
  # Tie-break: min price among maxes -> 1
  expect_equal(result$pmax_obs, 1)
  expect_equal(result$n_max_ties, 2)
  expect_false(result$has_duplicate_prices)
})

test_that("Observed pmax/omax handles duplicate prices with warning", {
  price <- c(1, 1, 2, 2, 5)  # Duplicates
  consumption <- c(100, 90, 50, 45, 10)
  
  result <- .calc_observed_pmax_omax(price, consumption)
  
  expect_true(result$has_duplicate_prices)
  expect_equal(result$n_unique_prices, 3)
  expect_equal(result$n_obs_rows, 5)
  expect_true(grepl("Duplicate prices", result$note_obs))
})

test_that("Observed pmax/omax returns correct values for single maximum", {
  price <- c(1, 2, 3, 4, 5)
  consumption <- c(10, 20, 15, 5, 1)
  # Expenditure: 10, 40, 45, 20, 5 -> max is 45 at price 3
  
  result <- .calc_observed_pmax_omax(price, consumption)
  
  expect_equal(result$omax_obs, 45)
  expect_equal(result$pmax_obs, 3)
  expect_equal(result$n_max_ties, 1)
})

test_that("Numerical fallback works when analytic fails", {
  # k < e/ln(10) for HS model -> analytic will fail
  alpha_nat <- 0.01
  q0_nat <- 10
  k_nat <- 1.0  # Below threshold
  
  price_obs <- seq(0.1, 50, length.out = 100)
  
  result <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = alpha_nat, q0 = q0_nat, k = k_nat),
    param_scales = list(alpha = "natural", q0 = "natural", k = "natural"),
    price_obs = price_obs
  )
  
  # Should fall back to numerical
  expect_equal(result$method_model, "numerical_optimize_observed_domain")
  expect_true(!is.na(result$pmax_model))
  expect_true(!is.na(result$omax_model))
})

test_that("Numerical fallback respects observed domain", {
  alpha_nat <- 0.01
  q0_nat <- 10
  k_nat <- 1.0
  
  # Narrow price range
  price_obs <- c(1, 2, 3, 4, 5)
  
  result <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = alpha_nat, q0 = q0_nat, k = k_nat),
    param_scales = list(alpha = "natural", q0 = "natural", k = "natural"),
    price_obs = price_obs
  )
  
  # pmax should be within observed domain
  expect_true(result$pmax_model >= min(price_obs) - 0.01)
  expect_true(result$pmax_model <= max(price_obs) + 0.01)
  expect_equal(result$domain_model, "[1, 5]")
})

test_that("Elasticity at pmax is near -1 for valid solutions", {
  # Use SND for clean closed-form
  alpha_nat <- 0.01
  q0_nat <- 10
  
  result <- beezdemand_calc_pmax_omax(
    model_type = "snd",
    params = list(alpha = alpha_nat, q0 = q0_nat),
    param_scales = list(alpha = "natural", q0 = "natural"),
    tol = 0.1
  )
  
  expect_true(!is.na(result$elasticity_at_pmax_model))
  # For SND, elasticity at pmax should be exactly -1
  expect_equal(result$elasticity_at_pmax_model, -1, tolerance = 0.05)
  expect_true(result$unit_elasticity_pass_model)
})

test_that("calc_observed_pmax_omax works on grouped data", {
  data <- data.frame(
    id = c(rep("A", 5), rep("B", 5)),
    x = rep(c(1, 2, 3, 4, 5), 2),
    y = c(100, 80, 40, 10, 0, 50, 60, 45, 20, 5)
  )
  
  result <- calc_observed_pmax_omax(data)
  
  expect_equal(nrow(result), 2)
  expect_true("A" %in% result$id)
  expect_true("B" %in% result$id)
  
  # Subject A: expenditure = 100, 160, 120, 40, 0 -> max 160 at price 2
  result_a <- result[result$id == "A", ]
  expect_equal(result_a$omax_obs, 160)
  expect_equal(result_a$pmax_obs, 2)
  
  # Subject B: expenditure = 50, 120, 135, 80, 25 -> max 135 at price 3
  result_b <- result[result$id == "B", ]
  expect_equal(result_b$omax_obs, 135)
  expect_equal(result_b$pmax_obs, 3)
})

test_that("Boundary detection works correctly", {
  # Create a case where maximum is at the boundary
  expenditure_fn <- function(p) p * exp(-0.01 * p)  # Monotonic in test range
  
  result <- .pmax_numerical(expenditure_fn, price_range = c(0.1, 5))
  
  # This function's maximum is at p = 100, so within [0.1, 5] it's at boundary
  expect_true(result$is_boundary)
})

test_that("beezdemand_calc_pmax_omax_vec works for multiple subjects", {
  params_df <- data.frame(
    alpha = c(0.01, 0.02, 0.005),
    q0 = c(10, 15, 20),
    k = c(3, 3, 3)
  )
  
  result <- beezdemand_calc_pmax_omax_vec(
    params_df,
    model_type = "hs",
    param_scales = list(alpha = "natural", q0 = "natural", k = "natural")
  )
  
  expect_equal(nrow(result), 3)
  expect_true(all(!is.na(result$pmax_model)))
  expect_true(all(result$method_model == "analytic_lambert_w"))
})

test_that("Full engine returns all expected fields", {
  result <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = 0.01, q0 = 10, k = 3),
    param_scales = list(alpha = "natural", q0 = "natural", k = "natural"),
    price_obs = c(1, 2, 3, 4, 5),
    consumption_obs = c(10, 8, 5, 2, 0.5)
  )
  
  # Model-based fields
  expect_true("pmax_model" %in% names(result))
  expect_true("omax_model" %in% names(result))
  expect_true("q_at_pmax_model" %in% names(result))
  expect_true("method_model" %in% names(result))
  expect_true("domain_model" %in% names(result))
  expect_true("is_boundary_model" %in% names(result))
  expect_true("elasticity_at_pmax_model" %in% names(result))
  expect_true("unit_elasticity_pass_model" %in% names(result))
  
  # Observed fields
  expect_true("pmax_obs" %in% names(result))
  expect_true("omax_obs" %in% names(result))
  expect_true("method_obs" %in% names(result))
  expect_true("has_duplicate_prices" %in% names(result))
  expect_true("n_max_ties" %in% names(result))
  
  # Parameter space fields
  expect_true("alpha_scale_in" %in% names(result))
  expect_true("q0_scale_in" %in% names(result))
  expect_true("k_scale_in" %in% names(result))
})

# ============================================================================
# Independent (non-circular) verification added for the v0.3.0 audit.
#
# The expected Pmax values below come from brute-force maximization of the
# expenditure function E(P) = P * Q(P), with Q(P) re-derived directly from the
# published demand equation -- they NEVER call .pmax_analytic_*() or lambertW(),
# unlike the circular checks above (e.g. L51-56 feed w_arg back through
# lambertW). For HS and the hurdle variants demand has a positive lower
# asymptote, so E(P) -> Inf as P -> Inf; the analytic Pmax is therefore the
# FIRST interior local maximum (the W0 / unit-elasticity point), not the global
# maximum. The oracle scans for that first local max. SND expenditure -> 0, so
# its first local max is also the global max.
# ============================================================================

# First interior local maximum of E on a fine grid, refined with optimize().
# Never references the engine or lambertW.
.audit_first_local_max <- function(Efn, p_grid) {
  E <- vapply(p_grid, Efn, numeric(1))
  for (i in 2:(length(p_grid) - 1L)) {
    if (E[i] > E[i - 1L] && E[i] >= E[i + 1L]) {
      opt <- stats::optimize(
        Efn,
        lower = p_grid[i - 1L],
        upper = p_grid[i + 1L],
        maximum = TRUE,
        tol = 1e-10
      )
      return(opt$maximum)
    }
  }
  NA_real_
}

test_that("HS analytic Pmax matches an independent brute-force local maximum", {
  alpha <- 0.01; q0 <- 10; k <- 3
  demand <- function(p) q0 * 10^(k * (exp(-alpha * q0 * p) - 1))
  oracle <- .audit_first_local_max(
    function(p) p * demand(p),
    seq(1e-3, 50, length.out = 8000)
  )
  res <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = alpha, q0 = q0, k = k),
    param_scales = list(alpha = "natural", q0 = "natural", k = "natural")
  )
  expect_equal(res$method_model, "analytic_lambert_w")
  expect_equal(res$pmax_model, oracle, tolerance = 1e-4)
})

test_that("Zhao hurdle analytic Pmax matches an independent brute-force local maximum", {
  alpha <- 0.5; q0 <- 10; k <- 3
  demand <- function(p) q0 * exp(k * (exp(-alpha * p) - 1))
  oracle <- .audit_first_local_max(
    function(p) p * demand(p),
    seq(1e-3, 30, length.out = 8000)
  )
  res <- beezdemand_calc_pmax_omax(
    model_type = "hurdle",
    params = list(alpha = alpha, q0 = q0, k = k),
    param_scales = list(alpha = "natural", q0 = "natural", k = "natural")
  )
  expect_equal(res$pmax_model, oracle, tolerance = 1e-4)
})

test_that("StdQ0 hurdle analytic Pmax matches an independent brute-force local maximum", {
  alpha <- 0.05; q0 <- 8; k <- 3
  demand <- function(p) q0 * exp(k * (exp(-alpha * q0 * p) - 1))
  oracle <- .audit_first_local_max(
    function(p) p * demand(p),
    seq(1e-3, 40, length.out = 8000)
  )
  res <- beezdemand_calc_pmax_omax(
    model_type = "hurdle_hs_stdq0",
    params = list(alpha = alpha, q0 = q0, k = k),
    param_scales = list(alpha = "natural", q0 = "natural", k = "natural")
  )
  expect_equal(res$pmax_model, oracle, tolerance = 1e-4)
})

test_that("SND analytic Pmax matches an independent global maximum", {
  alpha <- 0.001; q0 <- 5
  demand <- function(p) q0 * exp(-alpha * q0 * p)
  opt <- stats::optimize(
    function(p) p * demand(p),
    lower = 1, upper = 5000, maximum = TRUE, tol = 1e-10
  )
  res <- beezdemand_calc_pmax_omax(
    model_type = "snd",
    params = list(alpha = alpha, q0 = q0),
    param_scales = list(alpha = "natural", q0 = "natural")
  )
  expect_equal(res$pmax_model, 1 / (alpha * q0), tolerance = 1e-8) # = 200
  expect_equal(res$pmax_model, opt$maximum, tolerance = 1e-4)
})

test_that("SND Omax equals 1/(e*alpha) and is invariant to Q0", {
  alpha <- 0.001
  res5 <- beezdemand_calc_pmax_omax(
    model_type = "snd", params = list(alpha = alpha, q0 = 5),
    param_scales = list(alpha = "natural", q0 = "natural")
  )
  res50 <- beezdemand_calc_pmax_omax(
    model_type = "snd", params = list(alpha = alpha, q0 = 50),
    param_scales = list(alpha = "natural", q0 = "natural")
  )
  expect_equal(res5$omax_model, 1 / (exp(1) * alpha), tolerance = 1e-6)
  expect_equal(res50$omax_model, 1 / (exp(1) * alpha), tolerance = 1e-6)
  expect_equal(res5$omax_model, res50$omax_model, tolerance = 1e-8)
})

test_that("Analytic Pmax succeeds above threshold and is NA below threshold", {
  hs_thr <- exp(1) / log(10)
  hu_thr <- exp(1)
  # strictly above threshold -> analytic solution exists
  expect_true(.pmax_analytic_hs(0.01, 10, hs_thr + 0.1)$success)
  expect_true(.pmax_analytic_hurdle(0.5, hu_thr + 0.1)$success)
  expect_true(.pmax_analytic_hurdle_hs_stdq0(0.05, 8, hu_thr + 0.1)$success)
  # strictly below threshold -> no real solution
  expect_false(.pmax_analytic_hs(0.01, 10, hs_thr - 0.1)$success)
  expect_false(.pmax_analytic_hurdle(0.5, hu_thr - 0.1)$success)
  expect_false(.pmax_analytic_hurdle_hs_stdq0(0.05, 8, hu_thr - 0.1)$success)
  # NOTE (audit FLAG, not asserted here): the EXACT boundary k == threshold is
  # handled inconsistently -- HS uses `k <= threshold` -> NA, while both hurdle
  # functions use `k < threshold` -> finite. The contract (EQUATIONS_CONTRACT.md
  # L542,572) specifies k >= e (inclusive, matching the hurdle behavior), but the
  # strict interior-maximum interpretation (an inflection, not a max, at exactly
  # the threshold) matches HS. Convention to be reconciled by the maintainer.
})
