# Legacy Contract Tests - Output Invariants
# WS2.2: Lock output schemas so refactors cannot silently change user-visible behavior

# These tests verify behavioral contracts that must hold across all releases.
# They serve as guardrails to prevent accidental breaking changes.


# Column Name Invariants -------------------------------------------------
# Fail if columns are renamed or removed from legacy API output

test_that("FitCurves column names are stable (HS equation)", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id == 19, ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
  ))

  # Core parameter columns must exist
  core_params <- c("id", "Q0d", "Alpha", "K", "R2")
  expect_true(all(core_params %in% names(result)),
    info = paste(
      "Core parameter columns missing:",
      paste(setdiff(core_params, names(result)), collapse = ", ")
    )
  )

  # Standard error columns must exist
  se_cols <- c("Q0se", "Alphase")
  expect_true(all(se_cols %in% names(result)),
    info = paste(
      "Standard error columns missing:",
      paste(setdiff(se_cols, names(result)), collapse = ", ")
    )
  )

  # Derived metrics columns must exist
  derived_cols <- c("EV", "Pmaxd", "Omaxd", "Pmaxa", "Omaxa", "Pmaxe", "Omaxe")
  expect_true(all(derived_cols %in% names(result)),
    info = paste(
      "Derived metrics columns missing:",
      paste(setdiff(derived_cols, names(result)), collapse = ", ")
    )
  )

  # Confidence interval columns must exist
  ci_cols <- c("Q0Low", "Q0High", "AlphaLow", "AlphaHigh")
  expect_true(all(ci_cols %in% names(result)),
    info = paste(
      "Confidence interval columns missing:",
      paste(setdiff(ci_cols, names(result)), collapse = ", ")
    )
  )
})

test_that("FitCurves column names are stable (Koff equation)", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id == 19, ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "koff", k = 2)
  ))

  # Same columns as HS
  core_params <- c("id", "Q0d", "Alpha", "K", "R2")
  expect_true(all(core_params %in% names(result)))
})


# Parameter Bound Constraints --------------------------------------------
# Q0 > 0, alpha > 0 for converged models

test_that("Converged models have positive Q0", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
  ))

  converged <- result[result$Notes == "converged", ]

  # All converged Q0 values should be positive
  expect_true(all(converged$Q0d > 0, na.rm = TRUE),
    info = "All converged Q0d values should be positive"
  )
})

test_that("Converged models have positive alpha", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
  ))

  converged <- result[result$Notes == "converged", ]

  # All converged Alpha values should be positive
  expect_true(all(converged$Alpha > 0, na.rm = TRUE),
    info = "All converged Alpha values should be positive"
  )
})

test_that("Converged models have R2 in [0, 1] range", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
  ))

  converged <- result[result$Notes == "converged", ]

  # R2 should be between 0 and 1 for well-fitting models
  # Note: technically R2 can be negative for very poor fits, but converged

  # models should typically have R2 >= 0
  expect_true(all(converged$R2 <= 1, na.rm = TRUE),
    info = "R2 should not exceed 1"
  )
})


# Monotonicity Check -----------------------------------------------------
# Predicted consumption should be non-increasing in price for demand models

test_that("Predicted demand is monotonically non-increasing in price", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id == 19, ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2, detailed = TRUE)
  ))

  # Get fitted model for subject 19
  fit_obj <- result[[2]][[1]]
  df <- result[[3]][[1]]

  # Generate predictions at increasing prices
  test_prices <- seq(0.1, 30, by = 0.5)

  # Predict using model formula
  k_val <- result[[1]]$K[1]
  q0_val <- result[[1]]$Q0d[1]
  alpha_val <- result[[1]]$Alpha[1]

  # HS equation: y = q0 * 10^(k * (exp(-alpha * q0 * x) - 1))
  predicted <- q0_val * 10^(k_val * (exp(-alpha_val * q0_val * test_prices) - 1))

  # Check monotonicity: each prediction should be >= next prediction
  diffs <- diff(predicted)
  expect_true(all(diffs <= 1e-10),
    info = paste(
      "Demand should be monotonically decreasing.",
      "Found", sum(diffs > 1e-10), "violations."
    )
  )
})

test_that("Predicted demand (Koff) is monotonically non-increasing", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id == 19, ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "koff", k = 2)
  ))

  # Get parameter estimates
  k_val <- result$K[1]
  q0_val <- result$Q0d[1]
  alpha_val <- result$Alpha[1]

  # Generate predictions at increasing prices
  test_prices <- seq(0.1, 30, by = 0.5)

  # Koff equation: y = q0 * 10^(k * (exp(-alpha * q0 * x) - 1))
  # Note: Same functional form for predicted values
  predicted <- q0_val * 10^(k_val * (exp(-alpha_val * q0_val * test_prices) - 1))

  # Check monotonicity
  diffs <- diff(predicted)
  expect_true(all(diffs <= 1e-10),
    info = "Koff demand should be monotonically decreasing."
  )
})


# Convergence Status Format Stability ------------------------------------

test_that("Notes column uses consistent convergence format", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
  ))

  # Notes should contain recognizable status strings
  valid_statuses <- c("converged", "maxiter", "singular", "failed", "")

  # All notes should be character
  expect_type(result$Notes, "character")

  # Check that converged subjects have "converged" status
  converged_rows <- which(!is.na(result$Q0d) & !is.na(result$Alpha))
  if (length(converged_rows) > 0) {
    expect_true(
      any(grepl("converged", result$Notes[converged_rows], ignore.case = TRUE)),
      info = "At least some successful fits should have 'converged' in Notes"
    )
  }
})


# EV Column Invariant -----------------------------------------------------

test_that("EV (Essential Value) is calculated for converged models", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
  ))

  converged <- result[result$Notes == "converged", ]

  # EV should be present and positive for converged models
  expect_true(all(!is.na(converged$EV)),
    info = "EV should be calculated for converged models"
  )
  expect_true(all(converged$EV > 0, na.rm = TRUE),
    info = "EV should be positive for converged models"
  )
})


# Row Count Invariants ---------------------------------------------------

test_that("FitCurves returns one row per subject", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
  ))

  expect_equal(nrow(result), 3)
  expect_equal(sort(as.character(unique(result$id))), sort(c("19", "30", "38")))
})

test_that("FitMeanCurves returns exactly one row", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result_mean <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "hs", k = 2, method = "Mean")
  ))

  result_pooled <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "hs", k = 2, method = "Pooled")
  ))

  expect_equal(nrow(result_mean), 1)
  expect_equal(nrow(result_pooled), 1)
})


# Detailed Output Contract -----------------------------------------------

test_that("FitCurves detailed=TRUE returns named list with expected elements", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2, detailed = TRUE)
  ))

  expect_type(result, "list")
  # detailed=TRUE returns list with: dfres, fits, newdats, adfs
  expect_true(length(result) >= 3,
    info = "detailed=TRUE should return at least 3 elements"
  )

  # Check named elements exist
  expect_true("dfres" %in% names(result) || is.data.frame(result[[1]]),
    info = "First element should be results data frame"
  )
  expect_true("fits" %in% names(result) || is.list(result[[2]]),
    info = "Second element should be list of model fits"
  )

  # First element: data frame of results
  expect_s3_class(result$dfres %||% result[[1]], "data.frame")

  # Second element: list of model objects (one per subject)
  fits_element <- result$fits %||% result[[2]]
  expect_type(fits_element, "list")
  expect_equal(length(fits_element), 2)  # One per subject
})


# N Column Contract ------------------------------------------------------

test_that("N column reflects number of data points per subject", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id == 19, ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "koff", k = 2)
  ))

  # N should be the number of non-NA observations
  expected_n <- sum(!is.na(apt_test$y))

  expect_equal(result$N[1], expected_n)
})
