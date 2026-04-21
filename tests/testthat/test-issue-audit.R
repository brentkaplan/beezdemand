# Tests for package issue audit (Issues 1-10)
# Verifies fixes for blocking/required/suggestion issues from comprehensive audit.

# ==============================================================================
# Issue 1: Division by zero in hurdle summary (SE = 0)
# ==============================================================================

test_that("summary.beezdemand_hurdle handles zero SE without Inf/NaN", {
  # Create a minimal mock hurdle object with SE = 0 for a boundary parameter
  mock_obj <- structure(
    list(
      model = list(
        coefficients = c(
          beta0 = -1.5, beta1 = 0.3, log_q0 = 2.0, logsigma_a = -15
        ),
        se = c(
          beta0 = 0.2, beta1 = 0.05, log_q0 = 0.1, logsigma_a = 0
        ),
        variance_components = list(sigma_b = 0.5, sigma_e = 1.0)
      ),
      converged = TRUE,
      loglik = -100,
      AIC = 210,
      BIC = 220,
      param_info = list(
        equation = "exponential",
        part2 = "zhao_exponential",
        n_subjects = 10,
        n_obs = 100,
        n_zeros = 30,
        random_effects = c("zeros", "q0"),
        has_k = FALSE,
        y_var = "y", x_var = "x", id_var = "id"
      ),
      sdr = NULL,
      subject_pars = NULL,
      data = data.frame(id = rep(1:10, each = 10),
                        x = rep(0:9, 10),
                        y = stats::rpois(100, 5))
    ),
    class = "beezdemand_hurdle"
  )

  # Should warn about zero SE, not error
  expect_warning(
    s <- summary(mock_obj, report_space = "internal"),
    "Standard error is zero|zero for"
  )

  # Coefficient table t-values and p-values must not have Inf/NaN
  coef_tbl <- s$coefficients
  expect_false(is.null(coef_tbl), info = "summary must return coefficients")
  expect_true(
    all(is.finite(coef_tbl$statistic) | is.na(coef_tbl$statistic)),
    info = "t-values must not contain Inf"
  )
  expect_true(
    all(is.finite(coef_tbl$p.value) | is.na(coef_tbl$p.value)),
    info = "p-values must not contain NaN"
  )
  # The zero-SE parameter should have NA statistic
  boundary_row <- coef_tbl[coef_tbl$term == "logsigma_a", ]
  if (nrow(boundary_row) > 0) {
    expect_true(is.na(boundary_row$statistic[1]))
  }
})

# ==============================================================================
# Issue 2: Degenerate hurdle data validation
# ==============================================================================

test_that("validate_hurdle_data errors on all-positive consumption", {
  data_no_zeros <- data.frame(
    id = rep(1:5, each = 8),
    x = rep(c(0, 1, 2, 5, 10, 20, 50, 100), 5),
    y = rep(c(10, 8, 6, 4, 3, 2, 1, 0.5), 5)
  )
  expect_error(
    beezdemand:::validate_hurdle_data(data_no_zeros, "y", "x", "id"),
    "positive|zero"
  )
})

test_that("validate_hurdle_data errors on all-zero consumption", {
  data_all_zero <- data.frame(
    id = rep(1:5, each = 8),
    x = rep(c(0, 1, 2, 5, 10, 20, 50, 100), 5),
    y = 0
  )
  expect_error(
    beezdemand:::validate_hurdle_data(data_all_zero, "y", "x", "id"),
    "positive|zero"
  )
})

test_that("validate_hurdle_data errors with single subject", {
  data_one_subj <- data.frame(
    id = rep(1, 16),
    x = rep(c(0, 1, 2, 5, 10, 20, 50, 100), 2),
    y = c(10, 8, 5, 3, 0, 0, 0, 0, 12, 9, 6, 4, 0, 0, 0, 0)
  )
  expect_error(
    beezdemand:::validate_hurdle_data(data_one_subj, "y", "x", "id"),
    "at least 2 subject"
  )
})

test_that("validate_hurdle_data errors with single price level", {
  data_one_price <- data.frame(
    id = rep(1:5, each = 2),
    x = 1,
    y = c(10, 0, 8, 0, 5, 0, 3, 0, 7, 0)
  )
  expect_error(
    beezdemand:::validate_hurdle_data(data_one_price, "y", "x", "id"),
    "price"
  )
})

# ==============================================================================
# Issue 4: pivot_demand_data silent NA prices
# ==============================================================================

test_that("pivot_demand_data produces no NA prices from valid input", {
  wide <- data.frame(
    id = 1:3,
    `0` = c(10, 12, 8),
    `1` = c(8, 9, 6),
    `5` = c(3, 4, 2),
    check.names = FALSE
  )
  long <- pivot_demand_data(wide, id_var = "id", price_cols = c("0", "1", "5"))
  expect_false(any(is.na(long$x)), info = "No NA prices should exist after pivot")
})

test_that("pivot_demand_data handles whitespace in column names", {
  # Simulate an Excel-derived column with leading space
  wide <- data.frame(
    id = 1:3,
    ` 0` = c(10, 12, 8),
    `1` = c(8, 9, 6),
    check.names = FALSE
  )
  # With trimming, " 0" should map correctly
  long <- pivot_demand_data(wide, id_var = "id", price_cols = c(" 0", "1"))
  expect_false(any(is.na(long$x)))
})

# ==============================================================================
# Issue 5: Equation name aliases across tiers
# ==============================================================================

test_that("normalize_equation accepts legacy aliases for tmb tier", {
  # "hs" is legacy name for "exponential"
  expect_warning(
    result <- beezdemand:::normalize_equation("hs", tier = "tmb"),
    "deprecated|equation"
  )
  expect_equal(result, "exponential")
})

test_that("normalize_equation accepts legacy aliases for fixed tier", {
  # "exponential" should map to "hs" for fixed tier
  expect_warning(
    result <- beezdemand:::normalize_equation("exponential", tier = "fixed"),
    "deprecated|equation"
  )
  expect_equal(result, "hs")
})

test_that("normalize_equation passes through canonical names unchanged", {
  expect_equal(
    beezdemand:::normalize_equation("exponential", tier = "tmb"),
    "exponential"
  )
  expect_equal(
    beezdemand:::normalize_equation("hs", tier = "fixed"),
    "hs"
  )
})

# ==============================================================================
# Issue 7: sapply type instability in simulate.R (nruns = 1)
# ==============================================================================

test_that("SimulateDemand works with nruns = 1", {
  setparams <- c(-2.5547, .702521, 1.239893, .320221, 3.096, 1.438231)
  names(setparams) <- c("alphalm", "alphalsd", "q0lm", "q0lsd", "k", "yvalssd")
  sdindex <- c(2.1978, 1.9243, 1.5804, 1.2465, 0.8104, 0.1751, 0.0380, 0.0270)
  x <- c(.1, 1, 3, 10, 30, 100, 300, 1000)
  set.seed(1234)
  result <- SimulateDemand(
    nruns = 1,
    setparams = setparams, sdindex = sdindex, x = x
  )
  expect_type(result, "list")
})

test_that("SimulateDemand works with nruns > 1", {
  setparams <- c(-2.5547, .702521, 1.239893, .320221, 3.096, 1.438231)
  names(setparams) <- c("alphalm", "alphalsd", "q0lm", "q0lsd", "k", "yvalssd")
  sdindex <- c(2.1978, 1.9243, 1.5804, 1.2465, 0.8104, 0.1751, 0.0380, 0.0270)
  x <- c(.1, 1, 3, 10, 30, 100, 300, 1000)
  set.seed(1234)
  result <- SimulateDemand(
    nruns = 3,
    setparams = setparams, sdindex = sdindex, x = x
  )
  expect_type(result, "list")
})

# ==============================================================================
# Issue 9: Parameter registry consistency
# ==============================================================================

test_that("NLS equation specs have consistent structure", {
  specs <- beezdemand:::.beezdemand_equation_registry
  nls_ids <- c("hs", "koff", "simplified", "linear")
  required_fields <- c("id", "name", "params_estimated", "default_param_space",
                        "response_scale", "zero_handling")

  for (spec_name in nls_ids) {
    spec <- specs[[spec_name]]
    skip_if(is.null(spec), message = paste("Spec", spec_name, "not found"))
    for (field in required_fields) {
      expect_true(
        field %in% names(spec),
        info = paste(spec_name, "missing required field:", field)
      )
    }
    # params_default_fixed should be present (not params_fixed) in NLS specs
    expect_true(
      "params_default_fixed" %in% names(spec),
      info = paste(spec_name, "should use params_default_fixed, not params_fixed")
    )
  }
})

# ==============================================================================
# Issue 10: TMB version constraint
# ==============================================================================

test_that("TMB version meets minimum requirement", {
  skip_if_not_installed("TMB")
  tmb_ver <- packageVersion("TMB")
  expect_true(
    tmb_ver >= "1.9.0",
    info = paste("TMB version", tmb_ver, "is below minimum required")
  )
})
