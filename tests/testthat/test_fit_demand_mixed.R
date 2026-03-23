# Tests for fit_demand_mixed() function
# Covers: basic functionality, collapse_levels, edge cases, and error handling

# =============================================================================
# Helper function to create simulated demand data for testing
# =============================================================================

create_test_demand_data <- function(
    n_subjects = 10,
    n_prices = 8,
    n_levels_factor1 = 3,
    n_levels_factor2 = NULL,
    seed = 42
) {
  set.seed(seed)

  prices <- 10^seq(-2, 2, length.out = n_prices)
  factor1_levels <- paste0("level", seq_len(n_levels_factor1))

  if (!is.null(n_levels_factor2)) {
    factor2_levels <- paste0("cond", seq_len(n_levels_factor2))
    combinations <- expand.grid(
      id = seq_len(n_subjects),
      x = prices,
      factor1 = factor1_levels,
      factor2 = factor2_levels,
      stringsAsFactors = FALSE
    )
  } else {
    combinations <- expand.grid(
      id = seq_len(n_subjects),
      x = prices,
      factor1 = factor1_levels,
      stringsAsFactors = FALSE
    )
  }

  # Generate consumption data following a demand curve pattern
  combinations$y <- with(combinations, {
    # Base Q0 and alpha with some variation
    q0_base <- 100 + rnorm(nrow(combinations), 0, 10)
    alpha_base <- 0.001 + rnorm(nrow(combinations), 0, 0.0001)
    alpha_base[alpha_base <= 0] <- 0.0001

    # Demand curve: Q = Q0 * exp(-alpha * Q0 * x)
    consumption <- q0_base * exp(-alpha_base * q0_base * x)
    consumption[consumption < 0] <- 0
    consumption + rnorm(nrow(combinations), 0, 2)
  })

  combinations$y[combinations$y < 0] <- 0

  # Convert to factors

  combinations$id <- factor(combinations$id)
  combinations$factor1 <- factor(combinations$factor1, levels = factor1_levels)
  if (!is.null(n_levels_factor2)) {
    combinations$factor2 <- factor(combinations$factor2, levels = factor2_levels)
  }

  combinations
}

# =============================================================================
# Tests for helper functions
# =============================================================================

test_that("build_fixed_rhs constructs correct formula strings", {
  # No factors - intercept only
  expect_equal(build_fixed_rhs(), "~ 1")
  expect_equal(build_fixed_rhs(factors = NULL), "~ 1")

  # Single factor
  expect_equal(build_fixed_rhs(factors = "dose"), "~ dose")

  # Two factors without interaction
  expect_equal(
    build_fixed_rhs(factors = c("dose", "drug")),
    "~ dose + drug"
  )

  # Two factors with interaction
  expect_equal(
    build_fixed_rhs(factors = c("dose", "drug"), factor_interaction = TRUE),
    "~ dose*drug"
  )

  # With continuous covariates
  expect_equal(
    build_fixed_rhs(factors = "dose", continuous_covariates = c("age", "weight")),
    "~ dose + age + weight"
  )

  # Only continuous covariates
  expect_equal(
    build_fixed_rhs(continuous_covariates = c("age")),
    "~ age"
  )
})


test_that("validate_collapse_levels catches invalid structures", {
  # NULL is valid
  expect_true(validate_collapse_levels(NULL))

  # Invalid: not a list
  expect_error(
    validate_collapse_levels("not_a_list"),
    "must be a named list"
  )

  # Invalid: unnamed list
  expect_error(
    validate_collapse_levels(list(list(a = "b"))),
    "must have named elements"
  )

 # Invalid: wrong keys
  expect_error(
    validate_collapse_levels(list(wrong_key = list(a = "b"))),
    "Invalid keys"
  )

  # Valid: Q0 only
  expect_true(validate_collapse_levels(list(Q0 = list(dose = list(low = "a")))))

  # Valid: alpha only
  expect_true(validate_collapse_levels(list(alpha = list(dose = list(low = "a")))))

  # Valid: both Q0 and alpha
  expect_true(validate_collapse_levels(list(
    Q0 = list(dose = list(low = "a")),
    alpha = list(dose = list(high = "b"))
  )))
})


test_that("collapse_factor_levels creates new columns correctly", {
  test_data <- data.frame(
    id = factor(1:6),
    dose = factor(c("5mg", "10mg", "20mg", "5mg", "10mg", "20mg")),
    y = c(10, 8, 6, 9, 7, 5),
    x = c(1, 1, 1, 2, 2, 2)
  )

  collapse_spec <- list(
    dose = list(
      low = c("5mg", "10mg"),
      high = c("20mg")
    )
  )

  result <- collapse_factor_levels(
    data = test_data,
    collapse_spec = collapse_spec,
    factors = "dose",
    suffix = "Q0"
  )

  # Check new column was created
  expect_true("dose_Q0" %in% names(result$data))

  # Check levels are collapsed correctly
  expect_equal(levels(result$data$dose_Q0), c("high", "low"))

  # Check values are mapped correctly
  expect_equal(
    as.character(result$data$dose_Q0),
    c("low", "low", "high", "low", "low", "high")
  )

  # Check new_factor_names is updated
  expect_equal(result$new_factor_names, "dose_Q0")

  # Check info is populated
  expect_equal(result$info$dose$original_levels, c("10mg", "20mg", "5mg"))
  expect_equal(result$info$dose$new_levels, c("high", "low"))
})


test_that("collapse_factor_levels detects overlapping levels", {
  test_data <- data.frame(
    dose = factor(c("5mg", "10mg", "20mg")),
    y = c(10, 8, 6),
    x = c(1, 1, 1)
  )

  # "10mg" appears in both low and medium
  collapse_spec_overlap <- list(
    dose = list(
      low = c("5mg", "10mg"),
      medium = c("10mg", "20mg")
    )
  )

  expect_error(
    collapse_factor_levels(
      data = test_data,
      collapse_spec = collapse_spec_overlap,
      factors = "dose",
      suffix = "Q0"
    ),
    "Overlapping old levels"
  )
})


test_that("collapse_factor_levels warns for unknown factors", {
  test_data <- data.frame(
    dose = factor(c("5mg", "10mg")),
    y = c(10, 8),
    x = c(1, 1)
  )

  collapse_spec <- list(
    unknown_factor = list(low = c("a", "b"))
  )

  expect_warning(
    collapse_factor_levels(
      data = test_data,
      collapse_spec = collapse_spec,
      factors = "dose",
      suffix = "Q0"
    ),
    "not in the 'factors' list"
  )
})


# =============================================================================
# Tests for fit_demand_mixed() - basic functionality
# =============================================================================

test_that("fit_demand_mixed runs without factors (intercept only)", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 5, n_prices = 6, n_levels_factor1 = 1)
  test_data$factor1 <- NULL  # Remove factor

  result <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    equation_form = "simplified"
  )

  expect_s3_class(result, "beezdemand_nlme")
  expect_equal(result$param_info$num_params_Q0, 1)
  expect_equal(result$param_info$num_params_alpha, 1)
})

test_that("fit_demand_mixed recovers known parameters (simplified, natural)", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  set.seed(1)
  n_subjects <- 12
  prices <- 10^seq(-2, 2, length.out = 8)

  Q0_true <- 100
  alpha_true <- 0.001

  dat <- expand.grid(id = factor(seq_len(n_subjects)), x = prices)
  dat$y <- Q0_true * exp(-(alpha_true) * (Q0_true) * dat$x)
  dat$y <- dat$y + rnorm(nrow(dat), 0, 1)
  dat$y[dat$y < 0] <- 0

  fit <- suppressMessages(fit_demand_mixed(
    data = dat,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    equation_form = "simplified",
    param_space = "natural",
    random_effects = nlme::pdDiag(Q0 + alpha ~ 1),
    verbose = FALSE
  ))

  s <- suppressMessages(suppressWarnings(summary(fit, report_space = "natural")))
  coefs <- s$coefficients

  q0_hat <- as.numeric(coefs$estimate[coefs$term == "Q0"][1])
  alpha_hat <- as.numeric(coefs$estimate[coefs$term == "alpha"][1])

  expect_lt(abs(q0_hat - Q0_true), 5)
  expect_lt(abs(alpha_hat - alpha_true), 1e-4)
})


test_that("fit_demand_mixed runs with single factor", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 5, n_prices = 6, n_levels_factor1 = 2)

  result <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    equation_form = "simplified"
  )

  expect_s3_class(result, "beezdemand_nlme")
  # 2 levels = intercept + 1 contrast = 2 params per variable
  expect_equal(result$param_info$num_params_Q0, 2)
  expect_equal(result$param_info$num_params_alpha, 2)
})


# =============================================================================
# Tests for collapse_levels functionality (NEW FEATURE)
# =============================================================================

test_that("fit_demand_mixed applies collapse_levels for Q0 only", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 5, n_prices = 6, n_levels_factor1 = 3)

  # Collapse 3 levels to 2 for Q0 only
  collapse_spec <- list(
    Q0 = list(
      factor1 = list(
        combined = c("level1", "level2"),
        separate = c("level3")
      )
    )
  )

  result <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    collapse_levels = collapse_spec,
    equation_form = "simplified"
  )

  expect_s3_class(result, "beezdemand_nlme")

  # Q0 should have 2 params (2 collapsed levels)
  expect_equal(result$param_info$num_params_Q0, 2)

  # alpha should have 3 params (3 original levels)
  expect_equal(result$param_info$num_params_alpha, 3)

  # Check collapse_info is stored
  expect_false(is.null(result$collapse_info))
  expect_true("factor1" %in% names(result$collapse_info$Q0))
})


test_that("fit_demand_mixed applies collapse_levels for alpha only", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 5, n_prices = 6, n_levels_factor1 = 3)

  # Collapse 3 levels to 1 for alpha only (all levels combined)
  collapse_spec <- list(
    alpha = list(
      factor1 = list(
        all = c("level1", "level2", "level3")
      )
    )
  )

  result <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    collapse_levels = collapse_spec,
    equation_form = "simplified"
  )

  expect_s3_class(result, "beezdemand_nlme")

  # Q0 should have 3 params (3 original levels)
  expect_equal(result$param_info$num_params_Q0, 3)

  # alpha should have 1 param (all collapsed to single level)
  expect_equal(result$param_info$num_params_alpha, 1)
})


test_that("fit_demand_mixed applies different collapse_levels for Q0 and alpha", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 5, n_prices = 6, n_levels_factor1 = 3)

  # Different collapsing for each parameter
  collapse_spec <- list(
    Q0 = list(
      factor1 = list(
        low = c("level1"),
        high = c("level2", "level3")
      )
    ),
    alpha = list(
      factor1 = list(
        all = c("level1", "level2", "level3")
      )
    )
  )

  result <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    collapse_levels = collapse_spec,
    equation_form = "simplified"
  )

  expect_s3_class(result, "beezdemand_nlme")

  # Q0: 2 collapsed levels = 2 params
  expect_equal(result$param_info$num_params_Q0, 2)

  # alpha: 1 collapsed level = 1 param
  expect_equal(result$param_info$num_params_alpha, 1)

  # Check formula strings are different
  expect_true(
    result$formula_details$fixed_effects_formula_str_Q0 !=
      result$formula_details$fixed_effects_formula_str_alpha
  )
})


test_that("fit_demand_mixed stores separate factor names for Q0 and alpha", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 5, n_prices = 6, n_levels_factor1 = 3)

  collapse_spec <- list(
    Q0 = list(
      factor1 = list(low = c("level1", "level2"), high = c("level3"))
    )
  )

  result <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    collapse_levels = collapse_spec,
    equation_form = "simplified"
  )

  # factors_Q0 should reference the collapsed column
  expect_equal(result$param_info$factors_Q0, "factor1_Q0")

  # factors_alpha should reference the original column
  expect_equal(result$param_info$factors_alpha, "factor1")

  # Both columns should exist in the data
  expect_true("factor1_Q0" %in% names(result$data))
  expect_true("factor1" %in% names(result$data))
})


# =============================================================================
# Tests for edge cases
# =============================================================================

test_that("fit_demand_mixed ignores collapse_levels when fixed_rhs is provided", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 5, n_prices = 6, n_levels_factor1 = 3)

  collapse_spec <- list(
    Q0 = list(
      factor1 = list(all = c("level1", "level2", "level3"))
    )
  )

  # Expect a message about collapse_levels being ignored
  expect_message(
    result <- fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      fixed_rhs = "~ 1 + factor1",
      collapse_levels = collapse_spec,
      equation_form = "simplified"
    ),
    "collapse_levels.*ignored"
  )

  # Both Q0 and alpha should have same params (from fixed_rhs)
  expect_equal(result$param_info$num_params_Q0, result$param_info$num_params_alpha)
})


test_that("fit_demand_mixed handles empty collapse_levels gracefully", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 5, n_prices = 6, n_levels_factor1 = 2)

  # Empty specs for both parameters
  collapse_spec <- list(
    Q0 = list(),
    alpha = list()
  )

  result <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    collapse_levels = collapse_spec,
    equation_form = "simplified"
  )

  # Should work normally with original levels
  expect_equal(result$param_info$num_params_Q0, 2)
  expect_equal(result$param_info$num_params_alpha, 2)
})


test_that("fit_demand_mixed handles single-level collapsed factors correctly", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 8, n_prices = 6, n_levels_factor1 = 3)

  # Collapse all levels to 1 for alpha (removes factor from formula)
  collapse_spec <- list(
    Q0 = list(
      factor1 = list(low = c("level1", "level2"), high = c("level3"))
    ),
    alpha = list(
      factor1 = list(all = c("level1", "level2", "level3"))
    )
  )

  # Should produce a message about single-level factor
  expect_message(
    result <- fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      collapse_levels = collapse_spec,
      equation_form = "simplified"
    ),
    "only 1 level removed"
  )

  # Q0: 2 collapsed levels = 2 params
  expect_equal(result$param_info$num_params_Q0, 2)

  # alpha: single level = intercept only = 1 param
  expect_equal(result$param_info$num_params_alpha, 1)

  # alpha formula should be intercept-only
  expect_equal(result$formula_details$fixed_effects_formula_str_alpha, "~ 1")

  # Model should have fitted successfully
  expect_false(is.null(result$model))
})


# =============================================================================
# Tests for error handling
# =============================================================================

test_that("fit_demand_mixed errors on invalid collapse_levels keys", {
  test_data <- create_test_demand_data(n_subjects = 3, n_prices = 4, n_levels_factor1 = 2)

  # Invalid key "Q1" instead of "Q0"
  collapse_spec <- list(
    Q1 = list(factor1 = list(all = c("level1", "level2")))
  )

  expect_error(
    fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      collapse_levels = collapse_spec,
      equation_form = "simplified"
    ),
    "Invalid keys"
  )
})


test_that("fit_demand_mixed errors on overlapping collapse levels", {
  test_data <- create_test_demand_data(n_subjects = 3, n_prices = 4, n_levels_factor1 = 3)

  # level2 appears in both groups
  collapse_spec <- list(
    Q0 = list(
      factor1 = list(
        low = c("level1", "level2"),
        high = c("level2", "level3")
      )
    )
  )

  expect_error(
    fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      collapse_levels = collapse_spec,
      equation_form = "simplified"
    ),
    "Overlapping old levels"
  )
})


test_that("fit_demand_mixed errors on missing required columns", {
  test_data <- data.frame(
    id = factor(1:5),
    x = 1:5,
    y = 5:1
  )

  expect_error(
    fit_demand_mixed(
      data = test_data,
      y_var = "consumption",  # doesn't exist
      x_var = "x",
      id_var = "id",
      equation_form = "simplified"
    ),
    "Missing required columns"
  )
})


test_that("fit_demand_mixed errors on incorrect start_values length with asymmetric params", {
  test_data <- create_test_demand_data(n_subjects = 3, n_prices = 4, n_levels_factor1 = 3)

  # No collapsing - just test that wrong start_values length is caught
  # 3 factor levels = 3 params for Q0 + 3 params for alpha = 6 total
  expect_error(
    fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      start_values = list(fixed = c(1, 0, 0)),  # Only 3 values, need 6
      equation_form = "simplified"
    ),
    "incorrect length"
  )
})


# =============================================================================
# Tests for data validation
# =============================================================================

test_that("fit_demand_mixed validates data is a data frame", {
  expect_error(
    fit_demand_mixed(
      data = "not_a_dataframe",
      y_var = "y",
      x_var = "x",
      id_var = "id"
    ),
    "must be a data frame"
  )
})


test_that("fit_demand_mixed converts id and factors to factor type", {
  skip_on_cran()

  # Create proper demand data with numeric id and character factor
  test_data <- create_test_demand_data(n_subjects = 5, n_prices = 6, n_levels_factor1 = 2)

  # Convert to non-factor types to test conversion

  test_data$id <- as.numeric(as.character(test_data$id))
  test_data$factor1 <- as.character(test_data$factor1)

  result <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    equation_form = "simplified"
  )

  # Check data was converted
  expect_true(is.factor(result$data$id))
  expect_true(is.factor(result$data$factor1))
})

test_that("fit_demand_mixed validates x_var/y_var are numeric", {
  test_data <- create_test_demand_data(n_subjects = 5, n_prices = 6, n_levels_factor1 = 2)

  test_data$x <- factor(test_data$x)

  expect_error(
    fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      equation_form = "simplified"
    ),
    "must be numeric"
  )
})


# =============================================================================
# Tests for print and plot methods
# =============================================================================

test_that("print.beezdemand_nlme shows both formulas when they differ", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 8, n_levels_factor1 = 3)

  # Asymmetric collapse_levels
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

  # Capture print output
  output <- capture.output(print(fit))
  output_str <- paste(output, collapse = "\n")

  # Should show separate Q0 and alpha formulas

  expect_true(grepl("Fixed Effects Structure \\(Q0\\)", output_str))
  expect_true(grepl("Fixed Effects Structure \\(alpha\\)", output_str))
})


test_that("print.beezdemand_nlme shows combined formula when Q0 and alpha are same", {
  skip_on_cran()

  test_data <- create_test_demand_data(n_subjects = 6, n_levels_factor1 = 2)

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "factor1",
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  # Capture print output
  output <- capture.output(print(fit))
  output_str <- paste(output, collapse = "\n")

  # Should show combined formula (Q0 & alpha)
  expect_true(grepl("Fixed Effects Structure \\(Q0 & alpha\\)", output_str))
})


test_that("plot.beezdemand_nlme works with continuous covariates in fixed_rhs", {
  skip_on_cran()

  # Create data with a continuous covariate
  set.seed(42)
  test_data <- expand.grid(
    id = factor(1:6),
    x = c(0.1, 1, 10, 30),
    drug = c("A", "B")
  )
  test_data$dose_num <- runif(nrow(test_data), 0.001, 0.01)
  test_data$y <- 80 * exp(-0.002 * 80 * test_data$x) + rnorm(nrow(test_data), 0, 3)
  test_data$y[test_data$y < 0.1] <- 0.1
  test_data$drug <- factor(test_data$drug)

  fit <- fit_demand_mixed(
    data = test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "drug",
    fixed_rhs = "~ 1 + drug + dose_num",
    equation_form = "simplified"
  )

  expect_false(is.null(fit$model))

  # Plot should work with continuous covariate conditioning
  expect_no_error({
    p <- plot(fit, at = list(dose_num = 0.005), show_pred_lines = "population")
  })

  # Result should be a ggplot object
  p <- plot(fit, at = list(dose_num = 0.005), show_pred_lines = "population")
  expect_s3_class(p, "ggplot")
})


# =============================================================================
# Tests for warning capture and convergence detection (Bug 3)
# =============================================================================

test_that("fit_demand_mixed stores fit_warnings field", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  expect_true("fit_warnings" %in% names(fit))
  expect_type(fit$fit_warnings, "character")
})

test_that("glance.beezdemand_nlme reports actual convergence status", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  skip_if(is.null(fit$model), "Model fitting failed")

  g <- glance(fit)
  expect_type(g$converged, "logical")
})

test_that(".check_nlme_convergence detects known warning patterns", {
  fake <- structure(list(
    model = list(apVar = matrix(1)),
    fit_warnings = c("false convergence (8)")
  ), class = "beezdemand_nlme")

  result <- beezdemand:::.check_nlme_convergence(fake)
  expect_false(result$converged)
  expect_match(result$message, "false convergence")
})

test_that(".check_nlme_convergence handles old objects without fit_warnings", {
  # Backward compatibility: old saved objects lack fit_warnings
  fake <- structure(list(
    model = list(apVar = matrix(1))
    # no fit_warnings field
  ), class = "beezdemand_nlme")

  result <- beezdemand:::.check_nlme_convergence(fake)
  expect_true(result$converged)
  expect_null(result$message)
})

test_that(".check_nlme_convergence detects apVar character (singular Hessian)", {
  fake <- structure(list(
    model = list(apVar = "Non-positive definite approximate variance-covariance"),
    fit_warnings = character(0)
  ), class = "beezdemand_nlme")

  result <- beezdemand:::.check_nlme_convergence(fake)
  expect_false(result$converged)
  expect_match(result$message, "Hessian")
})

# =============================================================================
# Regression tests: NA handling and start-value naming
# =============================================================================

test_that("fit_demand_mixed succeeds when some y values are NA", {
  d <- create_test_demand_data(n_subjects = 10, n_prices = 8, seed = 42)
  # Inject ~10% NAs into y
  set.seed(123)
  na_idx <- sample(nrow(d), size = ceiling(nrow(d) * 0.1))
  d$y[na_idx] <- NA

  result <- suppressMessages(
    fit_demand_mixed(
      data = d, id_var = "id", x_var = "x", y_var = "y",
      factors = "factor1", equation_form = "simplified"
    )
  )
  expect_s3_class(result, "beezdemand_nlme")
  expect_false(is.null(result$model))
})

test_that("fit_demand_mixed emits message about dropped NA rows", {
  d <- create_test_demand_data(n_subjects = 10, n_prices = 8, seed = 42)
  set.seed(123)
  na_idx <- sample(nrow(d), size = 5)
  d$y[na_idx] <- NA

  expect_message(
    fit_demand_mixed(
      data = d, id_var = "id", x_var = "x", y_var = "y",
      factors = "factor1", equation_form = "simplified"
    ),
    "Removed 5 row.*missing values"
  )
})

test_that("fit_demand_mixed errors when all rows have NA", {
  d <- create_test_demand_data(n_subjects = 10, n_prices = 8, seed = 42)
  d$y <- NA_real_

  expect_error(
    suppressMessages(
      fit_demand_mixed(
        data = d, id_var = "id", x_var = "x", y_var = "y",
        factors = "factor1", equation_form = "simplified"
      )
    ),
    "No complete cases remain"
  )
})

test_that("start_values_used is an unnamed numeric vector", {
  d <- create_test_demand_data(n_subjects = 10, n_prices = 8, seed = 42)

  result <- suppressMessages(
    fit_demand_mixed(
      data = d, id_var = "id", x_var = "x", y_var = "y",
      factors = "factor1", equation_form = "simplified",
      start_value_method = "pooled_nls"
    )
  )
  expect_s3_class(result, "beezdemand_nlme")
  expect_null(names(result$start_values_used))
  expect_type(result$start_values_used, "double")
})
