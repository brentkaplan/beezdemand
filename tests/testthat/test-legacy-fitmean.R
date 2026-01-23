# Legacy Tests for FitMeanCurves() - Schema Stability and Numeric Regression
# WS2.1: Freeze FitMeanCurves() output schema with golden tests

# Expected columns from FitMeanCurves() output (HS/Koff equations)
# These must remain stable across refactors
FITMEAN_EXPECTED_COLUMNS <- c(
  "id", "Intensity", "BP0", "BP1", "Omaxe", "Pmaxe",
  "Equation", "Q0d", "K", "R2", "Alpha", "Q0se",
  "Alphase", "N", "AbsSS", "SdRes", "Q0Low", "Q0High",
  "AlphaLow", "AlphaHigh", "EV", "Omaxd", "Pmaxd", "Omaxa",
  "Pmaxa", "Notes"
)


# Schema Stability Tests -------------------------------------------------

test_that("FitMeanCurves output has expected columns (Mean method, HS)", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "hs", k = 2, method = "Mean")
  ))

  expect_s3_class(result, "data.frame")
  expect_true(all(FITMEAN_EXPECTED_COLUMNS %in% names(result)),
    info = paste(
      "Missing columns:",
      paste(setdiff(FITMEAN_EXPECTED_COLUMNS, names(result)), collapse = ", ")
    )
  )
})

test_that("FitMeanCurves output has expected columns (Pooled method, HS)", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "hs", k = 2, method = "Pooled")
  ))

  expect_s3_class(result, "data.frame")
  expect_true(all(FITMEAN_EXPECTED_COLUMNS %in% names(result)),
    info = paste(
      "Missing columns:",
      paste(setdiff(FITMEAN_EXPECTED_COLUMNS, names(result)), collapse = ", ")
    )
  )
})

test_that("FitMeanCurves output has expected columns (Koff equation)", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "koff", k = 2, method = "Mean")
  ))

  expect_s3_class(result, "data.frame")
  expect_true(all(FITMEAN_EXPECTED_COLUMNS %in% names(result)),
    info = paste(
      "Missing columns:",
      paste(setdiff(FITMEAN_EXPECTED_COLUMNS, names(result)), collapse = ", ")
    )
  )
})

test_that("FitMeanCurves output column types are stable", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "hs", k = 2, method = "Mean")
  ))

  # Character columns
  expect_type(result$id, "character")
  expect_type(result$Equation, "character")
  expect_type(result$Notes, "character")

  # Numeric columns
  numeric_cols <- c(
    "Intensity", "Q0d", "K", "Alpha", "R2", "Q0se", "Alphase",
    "AbsSS", "SdRes", "Q0Low", "Q0High", "AlphaLow", "AlphaHigh",
    "EV", "Omaxd", "Pmaxd", "Omaxa", "Pmaxa", "Omaxe", "Pmaxe"
  )
  for (col in numeric_cols) {
    expect_true(is.numeric(result[[col]]),
      info = paste("Column", col, "should be numeric")
    )
  }

  # Integer columns
  expect_true(is.integer(result$N) || is.numeric(result$N),
    info = "Column N should be integer/numeric"
  )
})


# Numeric Regression Tests (Golden Values) --------------------------------

test_that("FitMeanCurves reproduces Mean method golden values (HS)", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "hs", k = 2, method = "Mean")
  ))

  expect_equal(nrow(result), 1)
  expect_equal(result$id, "Mean")
  expect_equal(result$Q0d, 6.472078, tolerance = 0.01)
  expect_equal(result$Alpha, 0.00302151, tolerance = 1e-5)
  expect_equal(result$K, 2)
  expect_equal(result$R2, 0.9744355, tolerance = 0.01)
  expect_equal(result$Pmaxd, 14.7529, tolerance = 0.1)
  expect_equal(result$Omaxd, 30.10893, tolerance = 0.1)
})

test_that("FitMeanCurves reproduces Pooled method golden values (HS)", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "hs", k = 2, method = "Pooled")
  ))

  expect_equal(nrow(result), 1)
  expect_equal(result$id, "Pooled")
  expect_equal(result$Q0d, 5.343316, tolerance = 0.01)
  expect_equal(result$Alpha, 0.003487513, tolerance = 1e-5)
  expect_equal(result$K, 2)
  expect_equal(result$R2, 0.2875808, tolerance = 0.01)
  expect_equal(result$Pmaxd, 15.48169, tolerance = 0.1)
  expect_equal(result$Omaxd, 26.08576, tolerance = 0.1)
})


# Method Validation Tests ------------------------------------------------

test_that("FitMeanCurves requires method parameter", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  expect_error(
    FitMeanCurves(apt_test, equation = "hs", k = 2),
    regexp = "No method specified|Choose either"
  )
})

test_that("FitMeanCurves validates method parameter", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  expect_error(
    FitMeanCurves(apt_test, equation = "hs", k = 2, method = "Invalid"),
    regexp = "No method specified|Choose either"
  )
})

test_that("FitMeanCurves always returns single row", {
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


# Data Transformation Tests ----------------------------------------------

test_that("FitMeanCurves handles zero replacement with nrepl/replnum", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  # Add some zeros to test replacement
  apt_zero <- apt_test
  apt_zero$y[apt_zero$x == max(apt_zero$x)] <- 0

  # Use Koff equation which can handle zeros with replacement
  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_zero, equation = "koff", k = 2, method = "Mean",
                  nrepl = 1, replnum = 0.01)
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})


# Koff Equation Tests ----------------------------------------------------

test_that("FitMeanCurves works with Koff equation", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "koff", k = 2, method = "Mean")
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$Equation, "koff")
  expect_true(!is.na(result$Q0d))
  expect_true(!is.na(result$Alpha))
})


# k = "fit" Tests --------------------------------------------------------

test_that("FitMeanCurves works with k='fit'", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "koff", k = "fit", method = "Mean")
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  # K should be estimated, not necessarily 2
  expect_true(!is.na(result$K))
})
