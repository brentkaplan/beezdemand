# =============================================================================
# Tests for get_demand_param_emms, get_observed_demand_param_emms, and
# get_demand_comparisons functions
# =============================================================================

# Helper function to create test demand data for EMM/comparison tests
create_emm_test_data <- function(
    n_subjects = 10,
    n_prices = 6,
    n_levels_factor1 = 3,
    n_levels_factor2 = NULL,
    seed = 42
) {
  set.seed(seed)
  prices <- 10^seq(-1, 1.5, length.out = n_prices)
  factor_levels1 <- paste0("level", seq_len(n_levels_factor1))

  if (!is.null(n_levels_factor2)) {
    factor_levels2 <- paste0("group", seq_len(n_levels_factor2))
    test_data <- expand.grid(
      id = seq_len(n_subjects),
      x = prices,
      factor1 = factor_levels1,
      factor2 = factor_levels2
    )
    test_data$factor2 <- factor(test_data$factor2)
  } else {
    test_data <- expand.grid(
      id = seq_len(n_subjects),
      x = prices,
      factor1 = factor_levels1
    )
  }

  test_data$y <- with(test_data, {
    q0 <- 80 + rnorm(nrow(test_data), 0, 5)
    alpha <- 0.002
    q0 * exp(-alpha * q0 * x) + rnorm(nrow(test_data), 0, 2)
  })
  test_data$y[test_data$y < 0] <- 0.1
  test_data$id <- factor(test_data$id)
  test_data$factor1 <- factor(test_data$factor1)

  test_data
}


# =============================================================================
# Tests for get_demand_param_emms
# =============================================================================

test_that("get_demand_param_emms works with single factor (no collapse)", {
  skip_on_cran()

  test_data <- create_emm_test_data(n_subjects = 8, n_levels_factor1 = 3)

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  emms <- get_demand_param_emms(fit, factors_in_emm = "factor1")

  # Should have 3 rows (one per level)
  expect_equal(nrow(emms), 3)

  # Should have expected columns
  expect_true("factor1" %in% names(emms))
  expect_true("Q0_param_log10" %in% names(emms))
  expect_true("alpha_param_log10" %in% names(emms))
  expect_true("Q0_natural" %in% names(emms))
  expect_true("alpha_natural" %in% names(emms))

  # Factor levels should be correct
  expect_equal(
    sort(as.character(emms$factor1)),
    sort(c("level1", "level2", "level3"))
  )
})


test_that("get_demand_param_emms works with collapse_levels (asymmetric)", {
  skip_on_cran()

  test_data <- create_emm_test_data(n_subjects = 10, n_levels_factor1 = 3)

  # Collapse Q0 to 2 levels, alpha to 1 level (intercept-only)
  collapse_spec <- list(
    Q0 = list(factor1 = list(low = c("level1", "level2"), high = c("level3"))),
    alpha = list(factor1 = list(all = c("level1", "level2", "level3")))
  )

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    collapse_levels = collapse_spec,
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  emms <- get_demand_param_emms(fit, factors_in_emm = "factor1")

  # Q0 has 2 levels, alpha is intercept-only
  # Output should have 2 rows (based on Q0 levels)
  expect_equal(nrow(emms), 2)

  # Factor levels should be the collapsed levels
  expect_equal(
    sort(as.character(emms$factor1)),
    sort(c("high", "low"))
  )

  # Q0 values should differ between rows
  expect_false(emms$Q0_param_log10[1] == emms$Q0_param_log10[2])

  # alpha values should be the same for both rows (intercept-only)
  expect_equal(emms$alpha_param_log10[1], emms$alpha_param_log10[2])
})


test_that("get_demand_param_emms returns correct types for all columns", {
  skip_on_cran()

  test_data <- create_emm_test_data(n_subjects = 8, n_levels_factor1 = 2)

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    equation_form = "simplified"
  )

  emms <- get_demand_param_emms(fit, factors_in_emm = "factor1")

  # factor1 should be a factor

  expect_true(is.factor(emms$factor1))

  # Numeric columns should be numeric
  expect_true(is.numeric(emms$Q0_param_log10))
  expect_true(is.numeric(emms$alpha_param_log10))
  expect_true(is.numeric(emms$Q0_natural))
  expect_true(is.numeric(emms$alpha_natural))
})


test_that("get_demand_param_emms includes EV when requested", {
  skip_on_cran()

  test_data <- create_emm_test_data(n_subjects = 8, n_levels_factor1 = 2)

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    equation_form = "simplified"
  )

  emms <- get_demand_param_emms(fit, factors_in_emm = "factor1", include_ev = TRUE)

  # Should have EV columns
  expect_true("EV" %in% names(emms))
  expect_true("LCL_EV" %in% names(emms))
  expect_true("UCL_EV" %in% names(emms))

  # EV should be numeric and positive
  expect_true(all(is.numeric(emms$EV)))
  expect_true(all(emms$EV > 0))
})


# =============================================================================
# Tests for get_observed_demand_param_emms
# =============================================================================

test_that("get_observed_demand_param_emms works with collapse_levels", {
  skip_on_cran()

  test_data <- create_emm_test_data(n_subjects = 10, n_levels_factor1 = 3)

  collapse_spec <- list(
    Q0 = list(factor1 = list(low = c("level1", "level2"), high = c("level3"))),
    alpha = list(factor1 = list(all = c("level1", "level2", "level3")))
  )

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    collapse_levels = collapse_spec,
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  obs_emms <- get_observed_demand_param_emms(fit, factors_in_emm = "factor1")

  # Should have same number of rows as full EMMs (all combinations observed)
  full_emms <- get_demand_param_emms(fit, factors_in_emm = "factor1")
  expect_equal(nrow(obs_emms), nrow(full_emms))
})


# =============================================================================
# Tests for get_demand_comparisons
# =============================================================================

test_that("get_demand_comparisons works with single factor (no collapse)", {
  skip_on_cran()

  test_data <- create_emm_test_data(n_subjects = 8, n_levels_factor1 = 3)

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  comps <- get_demand_comparisons(
    fit,
    compare_specs = ~ factor1,
    params_to_compare = c("Q0", "alpha")
  )

  # Should return results for both Q0 and alpha
  expect_true("Q0" %in% names(comps))
  expect_true("alpha" %in% names(comps))

  # Each should have emmeans and contrasts
  expect_true("emmeans" %in% names(comps$Q0))
  expect_true("contrasts_log10" %in% names(comps$Q0))

  # 3 levels = 3 pairwise comparisons
  expect_equal(nrow(comps$Q0$contrasts_log10), 3)
  expect_equal(nrow(comps$alpha$contrasts_log10), 3)
})


test_that("get_demand_comparisons works with collapse_levels (asymmetric)", {
  skip_on_cran()

  test_data <- create_emm_test_data(n_subjects = 10, n_levels_factor1 = 3)

  collapse_spec <- list(
    Q0 = list(factor1 = list(low = c("level1", "level2"), high = c("level3"))),
    alpha = list(factor1 = list(all = c("level1", "level2", "level3")))
  )

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    collapse_levels = collapse_spec,
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  comps <- get_demand_comparisons(
    fit,
    compare_specs = ~ factor1,
    params_to_compare = c("Q0", "alpha")
  )

  # Q0 should have 1 comparison (high vs low)
  expect_equal(nrow(comps$Q0$contrasts_log10), 1)

  # alpha should have empty contrasts (intercept-only)
  expect_equal(nrow(comps$alpha$contrasts_log10), 0)
})


test_that("get_demand_comparisons handles multiple factors with different levels", {
  skip_on_cran()

  test_data <- create_emm_test_data(
    n_subjects = 8,
    n_levels_factor1 = 3,
    n_levels_factor2 = 2
  )

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = c("factor1", "factor2"),
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  # Compare just factor1
  comps_f1 <- get_demand_comparisons(
    fit,
    compare_specs = ~ factor1,
    params_to_compare = "Q0"
  )

  # Should have 3 pairwise comparisons for factor1
  expect_equal(nrow(comps_f1$Q0$contrasts_log10), 3)

  # Comparisons should be for factor1 levels only
  contrast_defs <- comps_f1$Q0$contrasts_log10$contrast_definition
  expect_true(all(grepl("level", contrast_defs)))
  expect_false(any(grepl("group", contrast_defs)))
})


test_that("get_demand_comparisons EMMs have correct factor levels", {
  skip_on_cran()

  # Test with two factors that have overlapping level names
  set.seed(42)
  test_data <- expand.grid(
    id = factor(1:6),
    x = c(0.1, 1, 10, 30),
    FactorA = c("Level1", "Level2", "Level3"),
    FactorB = c("Level1", "Level2", "Level4")  # Shares some names!
  )
  test_data$y <- 80 * exp(-0.002 * 80 * test_data$x) + rnorm(nrow(test_data), 0, 2)
  test_data$y[test_data$y < 0.1] <- 0.1
  test_data$FactorA <- factor(test_data$FactorA)
  test_data$FactorB <- factor(test_data$FactorB)

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = c("FactorA", "FactorB"),
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  # Get EMMs for both factors
  emms <- get_demand_param_emms(fit, factors_in_emm = c("FactorA", "FactorB"))

  # FactorA should have its own levels only
  unique_A <- unique(as.character(emms$FactorA))
  expect_true("Level3" %in% unique_A)  # Unique to FactorA
  expect_false("Level4" %in% unique_A) # Should NOT appear in FactorA

  # FactorB should have its own levels only
  unique_B <- unique(as.character(emms$FactorB))
  expect_true("Level4" %in% unique_B)  # Unique to FactorB
  expect_false("Level3" %in% unique_B) # Should NOT appear in FactorB
})


# =============================================================================
# Error handling tests
# =============================================================================

test_that("get_demand_param_emms errors on invalid fit object", {
  expect_error(
    get_demand_param_emms(list(a = 1)),
    "must be a 'beezdemand_nlme' object"
  )
})


test_that("get_demand_param_emms errors on NULL model", {
  fake_fit <- structure(
    list(model = NULL),
    class = "beezdemand_nlme"
  )
  expect_error(
    get_demand_param_emms(fake_fit),
    "No model found"
  )
})


test_that("get_demand_comparisons errors on invalid fit object", {
  expect_error(
    get_demand_comparisons(list(a = 1)),
    "must be a 'beezdemand_nlme' object"
  )
})
