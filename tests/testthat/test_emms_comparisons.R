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


test_that("get_demand_param_emms includes EV when requested (k-free 'simplified' form: EV = 1/alpha)", {
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

  emms <- get_demand_param_emms(
    fit,
    factors_in_emm = "factor1",
    include_ev = TRUE
  )

  # Should have EV columns
  expect_true("EV" %in% names(emms))
  expect_true("LCL_EV" %in% names(emms))
  expect_true("UCL_EV" %in% names(emms))

  # EV should be numeric and positive
  expect_true(all(is.numeric(emms$EV)))
  expect_true(all(emms$EV > 0))

  # "simplified" is a k-free (SND) equation form: EV must match analyze.R's
  # own "simplified"-branch formula, 1/alpha (no k, no /100) -- NOT the
  # historical buggy 1/(100*alpha).
  expect_equal(emms$EV, 1 / emms$alpha_natural, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(emms$EV, 1 / (100 * emms$alpha_natural))))
})


test_that("get_demand_param_emms EV matches analyze.R's literature formula for the k-bearing 'exponentiated' form", {
  skip_on_cran()

  test_data <- create_emm_test_data(n_subjects = 8, n_levels_factor1 = 2)
  k_supplied <- 3

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    equation_form = "exponentiated",
    k = k_supplied
  )

  emms <- get_demand_param_emms(
    fit,
    factors_in_emm = "factor1",
    include_ev = TRUE
  )

  expect_true("EV" %in% names(emms))
  expect_equal(fit$param_info$k, k_supplied)

  # k-bearing form: EV must match analyze.R's literature (Hursh & Silberberg)
  # formula, 1/(100*alpha*k^1.5) -- NOT the historical dropped-k 1/(100*alpha).
  expect_equal(
    emms$EV, 1 / (100 * emms$alpha_natural * (k_supplied^1.5)),
    tolerance = 1e-8
  )
  expect_false(isTRUE(all.equal(emms$EV, 1 / (100 * emms$alpha_natural))))

  # CI bounds: EV is decreasing in alpha (k fixed), so the bounds swap exactly.
  expect_equal(
    emms$LCL_EV, 1 / (100 * emms$UCL_alpha_natural * (k_supplied^1.5)),
    tolerance = 1e-8
  )
  expect_equal(
    emms$UCL_EV, 1 / (100 * emms$LCL_alpha_natural * (k_supplied^1.5)),
    tolerance = 1e-8
  )
  expect_true(all(emms$LCL_EV <= emms$EV & emms$EV <= emms$UCL_EV))
})


test_that("get_demand_param_emms EV is NA (with a warning) for custom_model_formula fits", {
  skip_on_cran()

  # A custom formula makes the equation form unknowable to the EV branch:
  # `equation_form_selected` keeps its default ("zben") even though the fitted
  # model may be k-bearing. The honest output is NA, not a guessed formula.
  test_data <- create_emm_test_data(n_subjects = 8, n_levels_factor1 = 2)
  test_data$y_ll4 <- ll4(test_data$y)
  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y_ll4",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    equation_form = "zben",
    custom_model_formula = "y_ll4 ~ (10^Q0) * exp(-(10^alpha) * (10^Q0) * x)"
  )
  skip_if(inherits(fit$model, "try-error") || is.null(fit$model), "custom-formula NLME fit failed")

  expect_warning(
    emms <- get_demand_param_emms(fit, factors_in_emm = "factor1", include_ev = TRUE),
    "custom_model_formula"
  )
  expect_true("EV" %in% names(emms))
  expect_true(all(is.na(emms$EV)))
  expect_true(all(is.na(emms$LCL_EV)))
  expect_true(all(is.na(emms$UCL_EV)))
  # alpha itself is still reported
  expect_true(all(is.finite(emms$alpha_natural)))
})


# -----------------------------------------------------------------------------
# TICKET-012: `param` argument for NLME EMMs (parity with TMB tier)
# -----------------------------------------------------------------------------

test_that("get_demand_param_emms.beezdemand_nlme default preserves back-compat", {
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

  emms_default <- get_demand_param_emms(fit, factors_in_emm = "factor1")
  emms_both <- get_demand_param_emms(
    fit,
    factors_in_emm = "factor1",
    param = "both"
  )

  expect_identical(emms_default, emms_both)
  expect_true(all(
    c("Q0_param_log10", "alpha_param_log10", "Q0_natural", "alpha_natural") %in%
      names(emms_default)
  ))
})


test_that("param = 'Q0' returns only Q0 columns (NLME)", {
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

  emms <- get_demand_param_emms(
    fit,
    factors_in_emm = "factor1",
    param = "Q0"
  )

  expect_true(all(c("Q0_param_log10", "Q0_natural") %in% names(emms)))
  expect_false(any(grepl("alpha", names(emms))))
  expect_false(any(names(emms) %in% c("EV", "LCL_EV", "UCL_EV")))
})


test_that("param = 'alpha' with include_ev returns alpha + EV (NLME)", {
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

  emms <- get_demand_param_emms(
    fit,
    factors_in_emm = "factor1",
    param = "alpha",
    include_ev = TRUE
  )

  expect_true(all(
    c("alpha_param_log10", "alpha_natural", "EV", "LCL_EV", "UCL_EV") %in%
      names(emms)
  ))
  expect_false(any(grepl("^Q0_|^LCL_Q0_|^UCL_Q0_", names(emms))))
})


test_that("param = 'Q0' with include_ev warns and drops EV (NLME)", {
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

  expect_warning(
    emms <- get_demand_param_emms(
      fit,
      factors_in_emm = "factor1",
      param = "Q0",
      include_ev = TRUE
    ),
    "EV"
  )
  expect_false(any(names(emms) %in% c("EV", "LCL_EV", "UCL_EV")))
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
    compare_specs = ~factor1,
    param = c("Q0", "alpha")
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
    compare_specs = ~factor1,
    param = c("Q0", "alpha")
  )

  # Q0 should have 1 comparison (high vs low)
  expect_equal(nrow(comps$Q0$contrasts_log10), 1)

  # alpha should have empty contrasts (intercept-only)
  expect_equal(nrow(comps$alpha$contrasts_log10), 0)
})


test_that("compare_specs_used records the user request, not the last param's collapsed formula (release-audit C4)", {
  skip_on_cran()

  test_data <- create_emm_test_data(n_subjects = 10, n_levels_factor1 = 3)

  # Asymmetric collapse: Q0 keeps factor1 (2 levels), alpha collapses to a
  # single level (intercept-only). The per-parameter emm formula thus differs,
  # so the recorded `compare_specs_used` must NOT depend on which parameter
  # happens to be processed last.
  collapse_spec <- list(
    Q0 = list(factor1 = list(low = c("level1", "level2"), high = c("level3"))),
    alpha = list(factor1 = list(all = c("level1", "level2", "level3")))
  )

  fit <- fit_demand_mixed(
    data = test_data, y_var = "y", x_var = "x", id_var = "id",
    factors = "factor1", collapse_levels = collapse_spec,
    equation_form = "simplified"
  )
  skip_if(is.null(fit$model), "NLME fit did not converge")

  # alpha LAST: old code stored alpha's collapsed "~1"; must be the user's spec.
  comps <- get_demand_comparisons(fit, compare_specs = ~factor1,
                                  param = c("Q0", "alpha"))
  expect_equal(attr(comps, "compare_specs_used"), "~factor1")

  # Order-independence: Q0 LAST must give the same recorded spec.
  comps_rev <- get_demand_comparisons(fit, compare_specs = ~factor1,
                                      param = c("alpha", "Q0"))
  expect_equal(attr(comps_rev, "compare_specs_used"), "~factor1")
})


test_that("compare_specs_used default label matches the TMB backend when no spec given (release-audit C4)", {
  skip_on_cran()

  test_data <- create_emm_test_data(n_subjects = 10, n_levels_factor1 = 3)
  fit <- fit_demand_mixed(
    data = test_data, y_var = "y", x_var = "x", id_var = "id",
    factors = "factor1", equation_form = "simplified"
  )
  skip_if(is.null(fit$model), "NLME fit did not converge")

  comps <- get_demand_comparisons(fit, param = c("Q0", "alpha"))
  # Mirror get_demand_comparisons.beezdemand_tmb()'s NULL-spec label.
  expect_equal(attr(comps, "compare_specs_used"), "all fitted factors")
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
    compare_specs = ~factor1,
    param = "Q0"
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
    FactorB = c("Level1", "Level2", "Level4") # Shares some names!
  )
  test_data$y <- 80 *
    exp(-0.002 * 80 * test_data$x) +
    rnorm(nrow(test_data), 0, 2)
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
  expect_true("Level3" %in% unique_A) # Unique to FactorA
  expect_false("Level4" %in% unique_A) # Should NOT appear in FactorA

  # FactorB should have its own levels only
  unique_B <- unique(as.character(emms$FactorB))
  expect_true("Level4" %in% unique_B) # Unique to FactorB
  expect_false("Level3" %in% unique_B) # Should NOT appear in FactorB
})


# =============================================================================
# Error handling tests
# =============================================================================

test_that("get_demand_param_emms errors on invalid fit object", {
  expect_error(
    get_demand_param_emms(list(a = 1)),
    "must be a 'beezdemand_nlme' or 'beezdemand_tmb' object"
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
    "must be a 'beezdemand_nlme' or 'beezdemand_tmb' object"
  )
})


test_that("get_demand_comparisons nested by-column uses the user-original name under collapse (TICKET-033)", {
  skip_on_cran()

  # Create data with two factors (3 levels each for more meaningful collapse)
  set.seed(123)
  test_data <- expand.grid(
    id = factor(1:6),
    x = c(0.1, 1, 10),
    factor1 = c("A", "B", "C"),
    factor2 = c("low", "mid", "high")
  )
  test_data$y <- 80 *
    exp(-0.002 * 80 * test_data$x) +
    rnorm(nrow(test_data), 0, 3)
  test_data$y[test_data$y < 0.1] <- 0.1
  test_data$factor1 <- factor(test_data$factor1)
  test_data$factor2 <- factor(test_data$factor2)

  # Collapse factor2 for alpha only into 2 groups (aa and bb)
  # This mirrors the user's scenario: dose collapsed to aa/bb for alpha only
  collapse_spec <- list(
    alpha = list(factor2 = list(aa = c("low", "mid"), bb = c("high")))
  )

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = c("factor1", "factor2"),
    collapse_levels = collapse_spec,
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  # User selects: compare factor1, within factor2
  # For Q0: contrast_by = "factor2" (original levels: low, mid, high)

  # For alpha: contrast_by should be mapped to "factor2_alpha" (collapsed: aa, bb)
  comps <- get_demand_comparisons(
    fit,
    compare_specs = ~ factor1 * factor2,
    param = c("Q0", "alpha"),
    contrast_by = "factor2"
  )

  # Q0 (not collapsed) keeps the original factor name as 'by' variable.
  expect_true("Q0" %in% names(comps))
  expect_true(is.data.frame(comps$Q0$contrasts_log10))
  expect_true(nrow(comps$Q0$contrasts_log10) > 0)
  expect_true("factor2" %in% names(comps$Q0$contrasts_log10))

  # alpha (collapsed to factor2_alpha internally) now reports the USER-ORIGINAL
  # name `factor2` in the nested object too (TICKET-033) -- harmonized with the
  # TMB backend and the flat tidy() output. The collapse-mapped name is gone.
  expect_true("alpha" %in% names(comps))
  expect_true(is.data.frame(comps$alpha$contrasts_log10))
  expect_true(nrow(comps$alpha$contrasts_log10) > 0)
  expect_true("factor2" %in% names(comps$alpha$contrasts_log10))
  expect_false("factor2_alpha" %in% names(comps$alpha$contrasts_log10))

  # ratio block carries the same renamed by-column.
  expect_true("factor2" %in% names(comps$alpha$contrasts_ratio))
  expect_false("factor2_alpha" %in% names(comps$alpha$contrasts_ratio))

  # Values are preserved (the collapsed level labels), only the name moved.
  expect_false(any(is.na(comps$alpha$contrasts_log10$factor2)))
})


# =============================================================================
# Tests for unbalanced factor filtering in get_demand_comparisons
# =============================================================================

test_that("get_demand_comparisons restricts to observed factor combinations", {
  skip_on_cran()

  # Create unbalanced data: drug A has doses 1,2,3; drug B has doses 2,3,4
  set.seed(99)
  make_rows <- function(drug_label, dose_levels, n_ids, id_offset) {
    expand.grid(
      id = factor(seq(id_offset + 1, id_offset + n_ids)),
      x = c(0.1, 1, 10),
      drug = drug_label,
      dose = dose_levels
    )
  }
  dat_a <- make_rows("A", c("d1", "d2", "d3"), n_ids = 5, id_offset = 0)
  dat_b <- make_rows("B", c("d2", "d3", "d4"), n_ids = 5, id_offset = 5)
  test_data <- rbind(dat_a, dat_b)
  test_data$drug <- factor(test_data$drug)
  test_data$dose <- factor(test_data$dose)
  test_data$y <- 80 *
    exp(-0.002 * 80 * test_data$x) +
    rnorm(nrow(test_data), 0, 3)
  test_data$y[test_data$y < 0.1] <- 0.1

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = c("drug", "dose"),
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  comps <- get_demand_comparisons(
    fit,
    compare_specs = ~ dose * drug,
    contrast_by = "drug",
    param = "Q0"
  )

  contrasts_df <- comps$Q0$contrasts_log10
  expect_true(is.data.frame(contrasts_df))
  expect_true(nrow(contrasts_df) > 0)

  # Drug A should only have C(3,2)=3 comparisons (doses d1, d2, d3)
  drug_a_rows <- contrasts_df[contrasts_df$drug == "A", ]
  expect_equal(nrow(drug_a_rows), 3)


  # Drug B should only have C(3,2)=3 comparisons (doses d2, d3, d4)
  drug_b_rows <- contrasts_df[contrasts_df$drug == "B", ]
  expect_equal(nrow(drug_b_rows), 3)

  # Total should be 6 (3 per drug), NOT 6 * 2 = 12 (C(4,2) per drug if unfiltered)
  expect_equal(nrow(contrasts_df), 6)
})


test_that("get_demand_comparisons does not filter balanced designs", {
  skip_on_cran()

  # Balanced: both drugs have the same doses
  set.seed(101)
  test_data <- expand.grid(
    id = factor(1:8),
    x = c(0.1, 1, 10),
    drug = c("A", "B"),
    dose = c("d1", "d2", "d3")
  )
  test_data$drug <- factor(test_data$drug)
  test_data$dose <- factor(test_data$dose)
  test_data$y <- 80 *
    exp(-0.002 * 80 * test_data$x) +
    rnorm(nrow(test_data), 0, 3)
  test_data$y[test_data$y < 0.1] <- 0.1

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = c("drug", "dose"),
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  comps <- get_demand_comparisons(
    fit,
    compare_specs = ~ dose * drug,
    contrast_by = "drug",
    param = "Q0"
  )

  contrasts_df <- comps$Q0$contrasts_log10

  # Each drug should have C(3,2) = 3 comparisons
  drug_a_rows <- contrasts_df[contrasts_df$drug == "A", ]
  drug_b_rows <- contrasts_df[contrasts_df$drug == "B", ]
  expect_equal(nrow(drug_a_rows), 3)
  expect_equal(nrow(drug_b_rows), 3)
  expect_equal(nrow(contrasts_df), 6)
})


# =============================================================================
# TMB fits with continuous covariates in EMMs/comparisons (codex Bug 3)
# =============================================================================

test_that("get_demand_param_emms.beezdemand_tmb handles continuous covariates", {
  skip_on_cran()
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  # Subsample for fast CI runs (keep ~50 subjects, balanced by gender)
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ids_g <- unique(d$id[d$gender == g])
    head(ids_g[order(ids_g)], 25)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  fit <- fit_demand_tmb(d, equation = "exponential",
                        factors = "gender",
                        continuous_covariates = "age", verbose = 0)
  emms <- get_demand_param_emms(fit, param = "Q0")
  expect_s3_class(emms, "tbl_df")
  expect_equal(nrow(emms), 2)
  expect_true(all(is.finite(emms$std.error)))
  expect_true(all(emms$std.error > 0))
})

test_that("get_demand_comparisons.beezdemand_tmb handles continuous covariates", {
  skip_on_cran()
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  # Subsample for fast CI runs (keep ~50 subjects, balanced by gender)
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ids_g <- unique(d$id[d$gender == g])
    head(ids_g[order(ids_g)], 25)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  fit <- fit_demand_tmb(d, equation = "exponential",
                        factors = "gender",
                        continuous_covariates = "age", verbose = 0)
  cmp <- get_demand_comparisons(fit, param = "Q0")
  expect_s3_class(cmp, "beezdemand_comparison")
  td <- broom::tidy(cmp)
  expect_true(is.finite(td$estimate[1]))
})

test_that("EMM `at` overrides continuous covariate value for TMB fits", {
  skip_on_cran()
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  # Subsample for fast CI runs (keep ~50 subjects, balanced by gender)
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ids_g <- unique(d$id[d$gender == g])
    head(ids_g[order(ids_g)], 25)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  fit <- fit_demand_tmb(d, equation = "exponential",
                        factors = "gender",
                        continuous_covariates = "age", verbose = 0)
  emm_low  <- get_demand_param_emms(fit, param = "Q0", at = list(age = 25))
  emm_high <- get_demand_param_emms(fit, param = "Q0", at = list(age = 35))
  expect_false(isTRUE(all.equal(emm_low$estimate, emm_high$estimate)))
})

# TICKET-016 (Decision 10, Option A): factors_in_emm that omits a fitted
# factor now MARGINALIZES equal-weight over the full omitted-factor grid
# (emmeans default weights = "equal") rather than erroring. This lifts the
# former TICKET-011 Phase 0.3 guard; the public get_demand_param_emms() gains
# marginalization too (the shared .tmb_build_emm_ref_grid() builder).
test_that("TMB EMMs marginalize when factors_in_emm drops a fitted factor", {
  skip_on_cran()
  dat <- create_emm_test_data(
    n_subjects = 10,
    n_levels_factor1 = 2,
    n_levels_factor2 = 2,
    seed = 99
  )
  fit <- suppressWarnings(fit_demand_tmb(
    dat,
    equation = "simplified",
    factors = c("factor1", "factor2"),
    verbose = 0
  ))
  skip_if_not(isTRUE(fit$converged), "TMB fit did not converge")

  # Dropping factor2 marginalizes over it: one EMM row per factor1 level.
  emm_full <- get_demand_param_emms(fit, param = "Q0")
  emm_marg <- get_demand_param_emms(fit, param = "Q0", factors_in_emm = "factor1")
  expect_equal(nrow(emm_full), 4L)   # 2 x 2 observed cells
  expect_equal(nrow(emm_marg), 2L)   # marginalized to factor1 levels

  # The marginalized EMM equals the equal-weight average over the FULL
  # factor2 grid of the cell log-predictors (Option A), back-transformed.
  coefs <- fit$model$coefficients
  beta_q0 <- unname(coefs[names(coefs) == "beta_q0"])
  xnames <- colnames(fit$formula_details$X_q0)
  l1 <- levels(fit$data$factor1)
  l2 <- levels(fit$data$factor2)
  full <- expand.grid(
    factor1 = factor(l1, levels = l1),
    factor2 = factor(l2, levels = l2)
  )
  Xf <- stats::model.matrix(~ factor1 + factor2, data = full)[, xnames, drop = FALSE]
  pred <- as.numeric(Xf %*% beta_q0)
  marg <- tapply(pred, full$factor1, mean)
  expect_equal(emm_marg$estimate, as.numeric(exp(marg[l1])), tolerance = 1e-7)

  # alpha path marginalizes symmetrically (drop factor1).
  emm_alpha <- get_demand_param_emms(fit, param = "alpha", factors_in_emm = "factor2")
  expect_equal(nrow(emm_alpha), 2L)
})

# TICKET-011 Phase 0.2: covariate-only TMB EMMs must honor `at`.
# Adversarial review flagged an early return at
# get_demand_param_emms.beezdemand_tmb() that fired whenever factors was
# empty — even when continuous_covariates was non-empty — so every EMM
# collapsed to the intercept, ignoring the requested covariate value.
test_that("TMB EMMs honor `at` for covariate-only fits (no factors)", {
  skip_on_cran()
  data(apt_full, package = "beezdemand")
  d <- apt_full
  ids_keep <- head(sort(unique(d$id)), 40)
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  fit <- fit_demand_tmb(d, equation = "exponential",
                        continuous_covariates = "age", verbose = 0)

  emm_low  <- get_demand_param_emms(fit, param = "Q0", at = list(age = 20))
  emm_high <- get_demand_param_emms(fit, param = "Q0", at = list(age = 60))

  expect_s3_class(emm_low, "tbl_df")
  expect_s3_class(emm_high, "tbl_df")
  expect_false(isTRUE(all.equal(emm_low$estimate, emm_high$estimate)))

  # Verify the at-values flow through the design: exp(beta_q0 %*% [1, age]).
  coefs <- fit$model$coefficients
  beta_q0 <- unname(coefs[names(coefs) == "beta_q0"])
  x_cols <- colnames(fit$formula_details$X_q0)
  age_idx <- which(x_cols == "age")
  int_idx <- which(x_cols == "(Intercept)")
  expect_equal(length(age_idx), 1L)
  expect_equal(length(int_idx), 1L)
  expected_low  <- exp(beta_q0[int_idx] + beta_q0[age_idx] * 20)
  expected_high <- exp(beta_q0[int_idx] + beta_q0[age_idx] * 60)
  expect_equal(emm_low$estimate,  expected_low,  tolerance = 1e-6)
  expect_equal(emm_high$estimate, expected_high, tolerance = 1e-6)
})

# Codex review P2: get_demand_comparisons.beezdemand_tmb() accepts `...`
# but never forwards it to the internal get_demand_param_emms() call, so
# caller-supplied `at` and `factors_in_emm` were silently ignored.
test_that("get_demand_comparisons.beezdemand_tmb forwards `at` and `factors_in_emm`", {
  skip_on_cran()
  dat <- create_emm_test_data(
    n_subjects = 10,
    n_levels_factor1 = 2,
    n_levels_factor2 = 2,
    seed = 99
  )
  fit <- suppressWarnings(fit_demand_tmb(
    dat,
    equation = "simplified",
    factors = c("factor1", "factor2"),
    verbose = 0
  ))
  skip_if_not(isTRUE(fit$converged), "TMB fit did not converge")

  # TICKET-016: the former hard error is gone (marginalization now succeeds),
  # so we re-prove `...`-forwarding with a result-CHANGING assertion. Without
  # forwarding, factors_in_emm would be silently dropped and both calls would
  # return identical contrast sets over all 4 cells (6 pairwise contrasts).
  # With forwarding, factors_in_emm = "factor1" marginalizes factor2 down to a
  # single pairwise contrast between factor1 levels.
  res_full <- suppressMessages(get_demand_comparisons(fit, param = "Q0"))
  res_marg <- suppressMessages(get_demand_comparisons(
    fit, param = "Q0", factors_in_emm = "factor1"))
  expect_equal(nrow(broom::tidy(res_full)), 6L)  # choose(4, 2)
  expect_equal(nrow(broom::tidy(res_marg)), 1L)  # choose(2, 2)
})

# =============================================================================
# TICKET-011 Phase 0.4: get_demand_comparisons.beezdemand_tmb() must build
# its contrast reference grid from the SAME conditioned grid that
# get_demand_param_emms() uses. Codex adversarial review (rounds 2 + 3 + 4)
# confirmed the wrapper forwards `...` to emms but rebuilds level_combos /
# ref_X from the unfiltered training data, so `at` filtering produces
# off-grid contrasts (silent statistical corruption) and NA labels when
# the filtered EMM has fewer rows than the unfiltered grid.
#
# Note on continuous-covariate `at` testing: fit_demand_tmb() does not
# currently support factor:covariate interactions, so the contrast estimate
# is invariant to the covariate choice in any main-effects-only model
# (the covariate column cancels in ref_X[i,] - ref_X[j,]). The bug Codex
# flagged for the covariate path is therefore unobservable in the estimate
# until interaction support lands. Phase 5 should add the missing covariate
# `at` regression test once factor:covariate interactions are supported.
# =============================================================================

test_that("get_demand_comparisons honors factor-level `at` (no NA labels, correct row count)", {
  skip_on_cran()
  dat <- create_emm_test_data(
    n_subjects = 10,
    n_levels_factor1 = 2,
    n_levels_factor2 = 2,
    seed = 99
  )
  fit <- suppressWarnings(fit_demand_tmb(
    dat,
    equation = "simplified",
    factors = c("factor1", "factor2"),
    verbose = 0
  ))
  skip_if_not(isTRUE(fit$converged), "TMB fit did not converge")

  # Unconditional baseline: 4 cells (2 levels x 2 levels) -> 6 pairwise.
  cmp_unconditional <- get_demand_comparisons(fit, param = "Q0")
  td_unc <- broom::tidy(cmp_unconditional)
  expect_equal(nrow(td_unc), 6L)
  expect_false(any(is.na(td_unc$contrast)))

  # Conditioned on factor2 = "group1": only factor1 contrasts at that
  # level remain. With 2 levels of factor1, that is 1 pairwise contrast.
  cmp_at <- get_demand_comparisons(
    fit, param = "Q0",
    at = list(factor2 = "group1")
  )
  td_at <- broom::tidy(cmp_at)
  expect_equal(nrow(td_at), 1L)
  # Catch both NA values and the "NA" literal string that paste(NA, "-", NA)
  # would emit when emms$level[i] is NA for indices past the filtered grid.
  expect_false(any(is.na(td_at$contrast)))
  expect_false(any(grepl("\\bNA\\b", td_at$contrast)))
})

test_that("get_demand_comparisons agrees with EMM differences under `at`", {
  skip_on_cran()
  dat <- create_emm_test_data(
    n_subjects = 10,
    n_levels_factor1 = 2,
    n_levels_factor2 = 2,
    seed = 99
  )
  fit <- suppressWarnings(fit_demand_tmb(
    dat,
    equation = "simplified",
    factors = c("factor1", "factor2"),
    verbose = 0
  ))
  skip_if_not(isTRUE(fit$converged), "TMB fit did not converge")

  # Under the same `at`, the comparison estimate must equal the difference
  # of the corresponding EMM rows. This invariant fails today because the
  # comparison function rebuilds ref_X from the unfiltered grid, so its
  # diff_x can correspond to factor2=group2 cells while emms reports
  # factor2=group1 estimates -> labels say one thing, math does another.
  emms_at <- get_demand_param_emms(
    fit, param = "Q0",
    at = list(factor2 = "group1")
  )
  cmp_at <- get_demand_comparisons(
    fit, param = "Q0",
    at = list(factor2 = "group1")
  )
  td_at <- broom::tidy(cmp_at)
  expect_equal(nrow(emms_at), 2L)
  expect_equal(nrow(td_at), 1L)

  # Pairwise contrast with two levels: emms[1] - emms[2] (i=1, j=2).
  # The comparison frame reports on the log10 scale; EMMs carry natural-log
  # `estimate_log`, so the expected difference divides by log(10).
  expected_diff_log10 <- (emms_at$estimate_log[1] - emms_at$estimate_log[2]) / log(10)
  expect_equal(td_at$estimate, expected_diff_log10, tolerance = 1e-8)
})


# --- TICKET-063: hessian_pd gate on TMB emms/comparisons surfaces ----------

test_that("get_demand_param_emms.beezdemand_tmb warns once when hessian_pd is FALSE", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .weak_pd_tmb_fit()
  skip_if(!isFALSE(fit$hessian_pd),
          "platform numerics did not produce a non-PD Hessian")

  conds <- .capture_warning_conditions(e <- get_demand_param_emms(fit, param = "Q0"))
  expect_identical(.n_hessian_pd_warnings(conds), 1L)
  expect_true(nrow(e) > 0)
})

test_that("get_demand_param_emms.beezdemand_tmb: healthy fit raises no hessian_pd warning", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  expect_no_warning(get_demand_param_emms(fit, param = "Q0"))
})

test_that("get_demand_comparisons.beezdemand_tmb warns exactly once per call (not once per param)", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .weak_pd_tmb_fit()
  skip_if(!isFALSE(fit$hessian_pd),
          "platform numerics did not produce a non-PD Hessian")

  conds <- .capture_warning_conditions(
    res <- get_demand_comparisons(fit, param = c("Q0", "alpha"))
  )
  expect_identical(.n_hessian_pd_warnings(conds), 1L)
})

test_that("get_demand_comparisons.beezdemand_tmb: healthy fit raises no hessian_pd warning", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ids_g <- unique(d$id[d$gender == g])
    head(ids_g[order(ids_g)], 25)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  fit <- fit_demand_tmb(d, equation = "exponential", factors = "gender", verbose = 0)
  expect_true(isTRUE(fit$hessian_pd))
  expect_no_warning(get_demand_comparisons(fit, param = "Q0"))
})


# --- TICKET-074: NLME EMMs back-transform correctly for param_space="natural" -

test_that("get_demand_param_emms.beezdemand_nlme: natural-space fit agrees with log10-space fit (simplified)", {
  skip_on_cran()
  skip_if_not_installed("emmeans")
  data(apt, package = "beezdemand")
  d <- subset(apt, y > 0)

  f_nat <- fit_demand_mixed(d, y_var = "y", x_var = "x", id_var = "id",
                            equation_form = "simplified", param_space = "natural")
  f_log <- fit_demand_mixed(d, y_var = "y", x_var = "x", id_var = "id",
                            equation_form = "simplified", param_space = "log10")

  e_nat <- get_demand_param_emms(f_nat, include_ev = TRUE)
  e_log <- get_demand_param_emms(f_log, include_ev = TRUE)

  # Two independently-optimized NLME parameterizations of the same data
  # (natural vs. log10) converge to slightly different optima; ~8% tolerance
  # confirms they agree in magnitude (the bug produced errors of orders of
  # magnitude -- see this ticket's RED evidence), not bit-identity.
  expect_equal(e_nat$alpha_natural, e_log$alpha_natural, tolerance = 0.08)
  expect_equal(e_nat$Q0_natural, e_log$Q0_natural, tolerance = 0.08)
  expect_equal(e_nat$EV, e_log$EV, tolerance = 0.08)

  expect_true(e_nat$LCL_alpha_natural <= e_nat$alpha_natural)
  expect_true(e_nat$alpha_natural <= e_nat$UCL_alpha_natural)
  expect_true(e_nat$LCL_Q0_natural <= e_nat$Q0_natural)
  expect_true(e_nat$Q0_natural <= e_nat$UCL_Q0_natural)

  # EV = 1/alpha_natural exactly for the k-free SND ("simplified") form
  expect_equal(e_nat$EV, 1 / e_nat$alpha_natural, tolerance = 1e-8)

  # param_log10 columns still present and consistent (log10 of the natural values)
  expect_equal(e_nat$alpha_param_log10, log10(e_nat$alpha_natural), tolerance = 1e-8)
  expect_equal(e_nat$Q0_param_log10, log10(e_nat$Q0_natural), tolerance = 1e-8)
})

test_that("get_demand_param_emms.beezdemand_nlme: natural-space exponentiated fit computes EV with k", {
  skip_on_cran()
  skip_if_not_installed("emmeans")
  data(apt, package = "beezdemand")
  d <- subset(apt, y > 0)

  f_nat <- fit_demand_mixed(d, y_var = "y", x_var = "x", id_var = "id",
                            equation_form = "exponentiated", k = 3,
                            param_space = "natural")
  e_nat <- get_demand_param_emms(f_nat, include_ev = TRUE)

  expect_equal(e_nat$EV, 1 / (100 * e_nat$alpha_natural * (3^1.5)), tolerance = 1e-8)
})

test_that("get_demand_comparisons.beezdemand_nlme: natural-space fit does not error and agrees in sign with log10 fit", {
  skip_on_cran()
  skip_if_not_installed("emmeans")

  test_data <- create_emm_test_data(n_subjects = 8, n_levels_factor1 = 3)

  fit_nat <- fit_demand_mixed(
    data = test_data, y_var = "y", x_var = "x", id_var = "id",
    factors = "factor1", equation_form = "simplified", param_space = "natural"
  )
  fit_log <- fit_demand_mixed(
    data = test_data, y_var = "y", x_var = "x", id_var = "id",
    factors = "factor1", equation_form = "simplified", param_space = "log10"
  )

  comps_nat <- get_demand_comparisons(fit_nat, compare_specs = ~factor1, param = "Q0")
  comps_log <- get_demand_comparisons(fit_log, compare_specs = ~factor1, param = "Q0")

  expect_equal(nrow(comps_nat$Q0$contrasts_log10), nrow(comps_log$Q0$contrasts_log10))
  expect_equal(
    sign(comps_nat$Q0$contrasts_log10$estimate),
    sign(comps_log$Q0$contrasts_log10$estimate)
  )
})


# --- Codex 2C review fold: BLOCKING 2 (TICKET-074) --------------------------
# A converged param_space = "natural" fit is an unconstrained parameterization
# -- a Wald CI bound (here, alpha's lower bound) can be non-positive. Before
# the fold, unconditional log10() of that bound raised a raw "NaNs produced"
# warning and returned NaN instead of NA.

test_that("get_demand_param_emms.beezdemand_nlme: natural-space fit with a non-positive Wald bound emits no warning and reports NA (not NaN) in *_param_log10", {
  skip_on_cran()
  skip_if_not_installed("emmeans")

  set.seed(7)
  d <- expand.grid(id = factor(1:5), x = c(0.1, 1, 5, 20, 50))
  d$y <- pmax(0, 8 * exp(-0.005 * d$x) + rnorm(nrow(d), 0, 3))
  fit <- fit_demand_mixed(
    d, y_var = "y", x_var = "x", id_var = "id",
    equation_form = "simplified", param_space = "natural"
  )
  skip_if(is.null(fit$model), "fit did not converge")

  e <- expect_no_warning(get_demand_param_emms(fit, include_ev = TRUE))

  # organic fixture: alpha's lower Wald bound on the natural scale is <= 0
  expect_true(e$LCL_alpha_natural <= 0)
  expect_true(is.na(e$LCL_alpha_param_log10))
  expect_false(is.nan(e$LCL_alpha_param_log10))  # NA_real_, not NaN
  # the natural-scale columns (what alpha_natural/EV consume) are unaffected
  expect_true(is.finite(e$alpha_natural))
  expect_true(is.finite(e$EV))
})

test_that("get_demand_param_emms.beezdemand_nlme: log10-space (default) fit output is byte-identical to the pre-fold value (RECOMMENDED 6)", {
  skip_on_cran()
  skip_if_not_installed("emmeans")

  data(apt, package = "beezdemand")
  d <- subset(apt, y > 0)
  fit <- fit_demand_mixed(d, y_var = "y", x_var = "x", id_var = "id",
                          equation_form = "simplified")
  e <- get_demand_param_emms(fit, include_ev = TRUE)

  # Pinned by computing with git stash against the pre-fold committed code
  # (commit 59240f9, i.e. before the BLOCKING-2 .safe_log10() change) --
  # the log10-space branch is untouched by that change, so this must be
  # byte-identical.
  expected <- list(
    Q0_param_log10 = 0.815617163829254, LCL_Q0_param_log10 = 0.711179527637501,
    UCL_Q0_param_log10 = 0.920054800021007, Q0_natural = 6.54059358134015,
    LCL_Q0_natural = 5.14256189671449, UCL_Q0_natural = 8.31868731100721,
    alpha_param_log10 = -1.799340469525, LCL_alpha_param_log10 = -1.91475032312893,
    UCL_alpha_param_log10 = -1.68393061592107, alpha_natural = 0.0158730187943233,
    LCL_alpha_natural = 0.0121688538972499, UCL_alpha_natural = 0.0207047210667786,
    EV = 62.999988405333, LCL_EV = 48.2981633403664, UCL_EV = 82.1770076659395
  )
  # expect_equal at a tight tolerance rather than expect_identical: this
  # NLME fit is reproducible to ~1e-12 relative but not bit-identical
  # across repeated runs (optimizer/BLAS summation-order noise), while a
  # real regression from this fold would differ by orders of magnitude.
  expect_equal(as.list(e)[names(expected)], expected, tolerance = 1e-8)
})


# --- Codex 2C review fold: NEW TICKET-075 -----------------------------------
# get_demand_comparisons.beezdemand_nlme()'s $contrasts_ratio block
# unconditionally computed ratio_estimate = 10^estimate. For a
# param_space = "natural" fit, `estimate` is already a natural-scale
# difference (not log10-scale), so 10^estimate is meaningless.

.ticket075_natural_fit <- function() {
  set.seed(42)
  prices <- 10^seq(-1, 1.5, length.out = 6)
  d <- expand.grid(id = seq_len(8), x = prices, factor1 = paste0("level", 1:3))
  d$id <- factor(paste0(d$id, "_", d$factor1))
  d$y <- pmax(0.01, 8 * exp(-0.01 * d$x) + rnorm(nrow(d), 0, 2))
  suppressWarnings(fit_demand_mixed(
    d, y_var = "y", x_var = "x", id_var = "id",
    factors = "factor1", equation_form = "simplified", param_space = "natural"
  ))
}

test_that("get_demand_comparisons.beezdemand_nlme: natural-space contrasts_ratio reports the DIFFERENCE, not 10^difference", {
  skip_on_cran()
  skip_if_not_installed("emmeans")

  fit <- .ticket075_natural_fit()
  skip_if(is.null(fit$model), "fit did not converge")

  res <- get_demand_comparisons(fit, compare_specs = ~factor1, param = "Q0")

  expect_identical(attr(res, "contrasts_ratio_scale"), "difference")
  expect_equal(
    res$Q0$contrasts_ratio$ratio_estimate,
    res$Q0$contrasts_log10$estimate
  )
  expect_equal(res$Q0$contrasts_ratio$LCL_ratio, res$Q0$contrasts_log10$lower.CL)
  expect_equal(res$Q0$contrasts_ratio$UCL_ratio, res$Q0$contrasts_log10$upper.CL)
  # the old (wrong) computation would have given 10^estimate here instead
  expect_false(isTRUE(all.equal(
    res$Q0$contrasts_ratio$ratio_estimate,
    10^res$Q0$contrasts_log10$estimate
  )))
})

test_that("get_demand_comparisons.beezdemand_nlme: log10-space (default) fit contrasts_ratio_scale is 'ratio' and ratio_estimate is 10^difference", {
  skip_on_cran()
  skip_if_not_installed("emmeans")

  test_data <- create_emm_test_data(n_subjects = 8, n_levels_factor1 = 3)
  fit <- fit_demand_mixed(
    data = test_data, y_var = "y", x_var = "x", id_var = "id",
    factors = "factor1", equation_form = "simplified"
  )
  res <- get_demand_comparisons(fit, compare_specs = ~factor1, param = "Q0")

  expect_identical(attr(res, "contrasts_ratio_scale"), "ratio")
  expect_equal(res$Q0$contrasts_ratio$ratio_estimate, 10^res$Q0$contrasts_log10$estimate)
})

test_that("get_demand_comparisons.beezdemand_nlme: log10-space contrasts_ratio output is byte-identical to the pre-fold value", {
  skip_on_cran()
  skip_if_not_installed("emmeans")

  test_data <- create_emm_test_data(n_subjects = 8, n_levels_factor1 = 3)
  fit <- fit_demand_mixed(
    data = test_data, y_var = "y", x_var = "x", id_var = "id",
    factors = "factor1", equation_form = "simplified"
  )
  res <- get_demand_comparisons(fit, compare_specs = ~factor1, param = "Q0")

  # Pinned via git stash against the pre-fold committed code (commit
  # d6d5a40, i.e. before TICKET-075's internal_space branch); the
  # log10-space path is the untouched `else` branch, so this must match.
  expected <- list(
    ratio_estimate = c(1.0010081365038, 1.01206922238537, 1.01104994602762),
    LCL_ratio = c(0.965477826227737, 0.975931875394098, 0.974936730322645),
    UCL_ratio = c(1.03784598892534, 1.04954468311235, 1.04850085299807)
  )
  actual <- list(
    ratio_estimate = res$Q0$contrasts_ratio$ratio_estimate,
    LCL_ratio = res$Q0$contrasts_ratio$LCL_ratio,
    UCL_ratio = res$Q0$contrasts_ratio$UCL_ratio
  )
  expect_equal(actual, expected, tolerance = 1e-6)
})
