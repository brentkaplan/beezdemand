# Legacy Tests for FitCurves() - Schema Stability and Numeric Regression
# WS2.1: Freeze FitCurves() output schema with golden tests

# Expected columns from FitCurves() output
# These must remain stable across refactors
FITCURVES_EXPECTED_COLUMNS <- c(

  "id", "Intensity", "BP0", "BP1", "Omaxe", "Pmaxe",

  "Equation", "Q0d", "K", "Alpha", "R2", "Q0se",
  "Alphase", "N", "AbsSS", "SdRes", "Q0Low", "Q0High",
  "AlphaLow", "AlphaHigh", "EV", "Omaxd", "Pmaxd", "Omaxa",
  "Pmaxa", "Notes"
)

# Additional columns when k = "fit"
FITCURVES_KFIT_EXTRA_COLUMNS <- c("Kse", "KLow", "KHigh")


# Schema Stability Tests -------------------------------------------------

test_that("FitCurves output has expected columns (HS equation)", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
  ))

  expect_s3_class(result, "data.frame")
  expect_true(all(FITCURVES_EXPECTED_COLUMNS %in% names(result)),
    info = paste(
      "Missing columns:",
      paste(setdiff(FITCURVES_EXPECTED_COLUMNS, names(result)), collapse = ", ")
    )
  )
})

test_that("FitCurves output has expected columns (Koff equation)", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "koff", k = 2)
  ))

  expect_s3_class(result, "data.frame")
  expect_true(all(FITCURVES_EXPECTED_COLUMNS %in% names(result)),
    info = paste(
      "Missing columns:",
      paste(setdiff(FITCURVES_EXPECTED_COLUMNS, names(result)), collapse = ", ")
    )
  )
})

test_that("FitCurves output column types are stable", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]


  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
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

test_that("FitCurves reproduces HS equation golden values", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
  ))

  # Subject 19 golden values
  row_19 <- result[result$id == "19", ]
  expect_equal(row_19$Q0d, 10.158664, tolerance = 0.01)
  expect_equal(row_19$Alpha, 0.002047574, tolerance = 1e-5)
  expect_equal(row_19$K, 2)
  expect_equal(row_19$R2, 0.9804182, tolerance = 0.01)
  expect_equal(row_19$Pmaxd, 13.86976, tolerance = 0.1)
  expect_equal(row_19$Omaxd, 44.43035, tolerance = 0.1)

  # Subject 30 golden values
  row_30 <- result[result$id == "30", ]
  expect_equal(row_30$Q0d, 2.807366, tolerance = 0.01)
  expect_equal(row_30$Alpha, 0.005865523, tolerance = 1e-5)

  # Subject 38 golden values
  row_38 <- result[result$id == "38", ]
  expect_equal(row_38$Q0d, 4.497456, tolerance = 0.01)
  expect_equal(row_38$Alpha, 0.004203441, tolerance = 1e-5)
})

test_that("FitCurves reproduces Koff equation golden values", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "koff", k = 2)
  ))

  # Subject 19 golden values
  row_19 <- result[result$id == "19", ]
  expect_equal(row_19$Q0d, 10.072114, tolerance = 0.01)
  expect_equal(row_19$Alpha, 0.002003155, tolerance = 1e-5)
  expect_equal(row_19$K, 2)
  expect_equal(row_19$R2, 0.9676372, tolerance = 0.01)
  expect_equal(row_19$Pmaxd, 14.29914, tolerance = 0.1)
  expect_equal(row_19$Omaxd, 45.41558, tolerance = 0.1)

  # Subject 30 golden values
  row_30 <- result[result$id == "30", ]
  expect_equal(row_30$Q0d, 2.967428, tolerance = 0.01)
  expect_equal(row_30$Alpha, 0.006381213, tolerance = 1e-5)

  # Subject 38 golden values
  row_38 <- result[result$id == "38", ]
  expect_equal(row_38$Q0d, 4.605634, tolerance = 0.01)
  expect_equal(row_38$Alpha, 0.004874198, tolerance = 1e-5)
})


# Edge Case Tests ---------------------------------------------------------

test_that("FitCurves handles single subject", {
  data(apt, package = "beezdemand")
  apt_single <- apt[apt$id == 19, ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_single, equation = "hs", k = 2)
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(FITCURVES_EXPECTED_COLUMNS %in% names(result)))
})

test_that("FitCurves handles all-zero consumption row gracefully", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id == 19, ]

  # Create subject with all zeros at high prices
  apt_zero <- apt_test
  apt_zero$y[apt_zero$x >= 10] <- 0

  # Should emit warning and handle zeros
  expect_warning(
    result <- suppressMessages(FitCurves(apt_zero, equation = "hs", k = 2)),
    regexp = "Zeros found|Dropping zeros"
  )

  expect_s3_class(result, "data.frame")
})

test_that("FitCurves works with k='fit' and param_space='log10'", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "koff", k = "fit", param_space = "log10")
  ))

  expect_s3_class(result, "data.frame")
  expect_true(all(FITCURVES_EXPECTED_COLUMNS %in% names(result)))

  # K should vary when k = "fit"
  expect_true(
    !all(is.na(result$K)),
    info = "K values should be estimated when k='fit'"
  )
})


# Aggregation Tests -------------------------------------------------------

test_that("FitCurves agg='Mean' produces single row", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2, agg = "Mean")
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$id[1], "mean")
})

test_that("FitCurves agg='Pooled' produces single row", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]

  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "koff", k = 2, agg = "Pooled")
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$id[1], "pooled")
})


# Deprecation Warning Test ------------------------------------------------

test_that("FitCurves emits deprecation warning", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id == 19, ]

  # Reset deprecation state for testing
  rlang::local_options(lifecycle_verbosity = "warning")

  expect_warning(
    FitCurves(apt_test, equation = "hs", k = 2),
    regexp = "superseded|deprecated|fit_demand_fixed",
    ignore.case = TRUE
  )
})
