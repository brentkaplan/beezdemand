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
    compare_specs = ~factor1,
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
    compare_specs = ~factor1,
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


test_that("get_demand_comparisons maps contrast_by to collapsed factor name", {
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
    params_to_compare = c("Q0", "alpha"),
    contrast_by = "factor2"
  )

  # Q0 should have comparisons with factor2 as 'by' variable
  expect_true("Q0" %in% names(comps))
  expect_true(is.data.frame(comps$Q0$contrasts_log10))
  expect_true(nrow(comps$Q0$contrasts_log10) > 0)
  expect_true("factor2" %in% names(comps$Q0$contrasts_log10))

  # alpha should have comparisons with factor2_alpha as 'by' variable
  expect_true("alpha" %in% names(comps))
  expect_true(is.data.frame(comps$alpha$contrasts_log10))
  expect_true(nrow(comps$alpha$contrasts_log10) > 0)
  # The mapped factor should appear in the results
  expect_true("factor2_alpha" %in% names(comps$alpha$contrasts_log10))
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
    params_to_compare = "Q0"
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
    params_to_compare = "Q0"
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
  expect_s3_class(cmp, "tbl_df")
  expect_true("estimate_log" %in% names(cmp))
  expect_true(is.finite(cmp$estimate_log[1]))
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

# TICKET-011 Phase 0.3: factors_in_emm that omits any fitted factor must
# error cleanly rather than silently recycling a shorter x_ref vector
# against the full beta vector. Proper marginalization over omitted
# factors is planned for TICKET-011 Phase 5.
test_that("TMB EMMs error when factors_in_emm drops a fitted factor", {
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

  expect_error(
    get_demand_param_emms(fit, param = "Q0", factors_in_emm = "factor1"),
    regexp = "factors_in_emm.*must include every fitted factor"
  )
  expect_error(
    get_demand_param_emms(fit, param = "alpha", factors_in_emm = "factor2"),
    regexp = "factors_in_emm.*must include every fitted factor"
  )
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

  # Phase 0.3 landed a hard error in get_demand_param_emms() when
  # factors_in_emm drops any fitted factor. If the wrapper forwards
  # `...` correctly, the same error surfaces through get_demand_comparisons().
  # Before this fix, factors_in_emm was silently dropped and the call
  # returned a (wrong) tibble instead of erroring.
  expect_error(
    get_demand_comparisons(
      fit,
      param = "Q0",
      factors_in_emm = "factor1"
    ),
    regexp = "factors_in_emm.*must include every fitted factor"
  )
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
  expect_equal(nrow(cmp_unconditional), 6L)
  expect_false(any(is.na(cmp_unconditional$contrast)))

  # Conditioned on factor2 = "group1": only factor1 contrasts at that
  # level remain. With 2 levels of factor1, that is 1 pairwise contrast.
  cmp_at <- get_demand_comparisons(
    fit, param = "Q0",
    at = list(factor2 = "group1")
  )
  expect_equal(nrow(cmp_at), 1L)
  # Catch both NA values and the "NA" literal string that paste(NA, "-", NA)
  # would emit when emms$level[i] is NA for indices past the filtered grid.
  expect_false(any(is.na(cmp_at$contrast)))
  expect_false(any(grepl("\\bNA\\b", cmp_at$contrast)))
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
  expect_equal(nrow(emms_at), 2L)
  expect_equal(nrow(cmp_at), 1L)

  # Pairwise contrast with two levels: emms[1] - emms[2] (i=1, j=2).
  expected_diff <- emms_at$estimate_log[1] - emms_at$estimate_log[2]
  expect_equal(cmp_at$estimate_log, expected_diff, tolerance = 1e-10)
})
