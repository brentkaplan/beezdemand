# Golden tests for FitCurves (legacy analyze.R functions)
# These tests capture the expected behavior of FitCurves to prevent regressions
# during future modernization efforts (API-008)

# Helper function to compare numeric values with tolerance
expect_equal_numeric <- function(actual, expected, tolerance = 1e-4) {
  expect_equal(actual, expected, tolerance = tolerance)
}

# Test data setup
test_that("FitCurves test data is available", {
  data(apt, package = "beezdemand")
  expect_true(exists("apt"))
  expect_true(is.data.frame(apt))
  expect_true(all(c("id", "x", "y") %in% names(apt)))
})

# =============================================================================
# Golden Tests for FitCurves with HS equation
# =============================================================================

test_that("FitCurves with HS equation and fixed k produces correct results", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  # This should produce a warning about zeros being dropped
  result <- suppressWarnings(FitCurves(test_data, "hs", k = 2))

  # Check structure

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("id", "Equation", "Q0d", "K", "Alpha", "R2", "Pmaxd", "Omaxd") %in% names(result)))
  expect_true(all(c("alpha_star", "alpha_star_se") %in% names(result)))

  # Golden values for ID 19
  row_19 <- result[result$id == 19, ]
  expect_equal_numeric(row_19$Q0d, 10.158664, tolerance = 0.01)
  expect_equal(row_19$K, 2)
  expect_equal_numeric(row_19$Alpha, 0.002047574, tolerance = 1e-5)
  expect_equal_numeric(
    row_19$alpha_star,
    -row_19$Alpha / log(1 - 1 / (row_19$K * log(10))),
    tolerance = 1e-8
  )
  expect_true(is.finite(row_19$alpha_star_se) && row_19$alpha_star_se >= 0)
  expect_equal_numeric(row_19$R2, 0.9804182, tolerance = 0.01)
  expect_equal_numeric(row_19$Pmaxd, 13.86976, tolerance = 0.1)
  expect_equal_numeric(row_19$Omaxd, 44.43035, tolerance = 0.1)

  # Golden values for ID 30
  row_30 <- result[result$id == 30, ]
  expect_equal_numeric(row_30$Q0d, 2.807366, tolerance = 0.01)
  expect_equal_numeric(row_30$Alpha, 0.005865523, tolerance = 1e-5)
  expect_equal_numeric(row_30$R2, 0.7723159, tolerance = 0.01)

  # Golden values for ID 38
  row_38 <- result[result$id == 38, ]
  expect_equal_numeric(row_38$Q0d, 4.497456, tolerance = 0.01)
  expect_equal_numeric(row_38$Alpha, 0.004203441, tolerance = 1e-5)
})

test_that("FitCurves with HS equation warns about zeros", {
  data(apt, package = "beezdemand")
  # ID 38 has zeros in the data
  test_data <- apt[apt$id == 38, ]

  expect_warning(
    FitCurves(test_data, "hs", k = 2),
    "Zeros found in data"
  )
})

# =============================================================================
# Golden Tests for FitCurves with Koffarnus equation
# =============================================================================

test_that("FitCurves with Koff equation and fixed k produces correct results", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  result <- FitCurves(test_data, "koff", k = 2)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # Golden values for ID 19
  row_19 <- result[result$id == 19, ]
  expect_equal(row_19$Equation, "koff")
  expect_equal_numeric(row_19$Q0d, 10.072114, tolerance = 0.01)
  expect_equal_numeric(row_19$Alpha, 0.002003155, tolerance = 1e-5)
  expect_equal_numeric(
    row_19$alpha_star,
    -row_19$Alpha / log(1 - 1 / (row_19$K * log(10))),
    tolerance = 1e-8
  )
  expect_true(is.finite(row_19$alpha_star_se) && row_19$alpha_star_se >= 0)
  expect_equal_numeric(row_19$R2, 0.9676372, tolerance = 0.01)
  expect_equal_numeric(row_19$Pmaxd, 14.29914, tolerance = 0.1)

  # Golden values for ID 30
  row_30 <- result[result$id == 30, ]
  expect_equal_numeric(row_30$Q0d, 2.967428, tolerance = 0.01)
  expect_equal_numeric(row_30$Alpha, 0.006381213, tolerance = 1e-5)

  # Golden values for ID 38
  row_38 <- result[result$id == 38, ]
  expect_equal_numeric(row_38$Q0d, 4.605634, tolerance = 0.01)
  expect_equal_numeric(row_38$Alpha, 0.004874198, tolerance = 1e-5)
})

# =============================================================================
# Golden Tests for FitCurves with aggregation
# =============================================================================

test_that("FitCurves with Mean aggregation produces correct results", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  result <- FitCurves(test_data, "hs", k = 2, agg = "Mean")

  # Check structure - should be single row
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$id), "mean")

  # Golden values
  expect_equal_numeric(result$Q0d, 6.170368, tolerance = 0.01)
  expect_equal_numeric(result$Alpha, 0.003859777, tolerance = 1e-5)
  expect_equal_numeric(result$R2, 0.9729868, tolerance = 0.01)
})

test_that("FitCurves with Pooled aggregation produces correct results", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  result <- suppressWarnings(FitCurves(test_data, "hs", k = 2, agg = "Pooled"))

  # Check structure - should be single row
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$id), "pooled")

  # Golden values (lower R2 expected for pooled data)
  expect_equal_numeric(result$Q0d, 4.993143, tolerance = 0.01)
  expect_equal_numeric(result$Alpha, 0.003598877, tolerance = 1e-5)
  expect_equal_numeric(result$R2, 0.3060571, tolerance = 0.01)
})

# =============================================================================
# Golden Tests for FitCurves with k="range"
# =============================================================================

test_that("FitCurves with k='range' produces correct results", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  result <- suppressWarnings(FitCurves(test_data, "hs", k = "range"))

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # All should have the same K value (calculated from range)
  expect_equal_numeric(result$K[1], 1.077236, tolerance = 0.01)
  expect_equal(result$K[1], result$K[2])
  expect_equal(result$K[1], result$K[3])

  # Golden values for ID 19
  row_19 <- result[result$id == 19, ]
  expect_equal_numeric(row_19$Q0d, 10.454333, tolerance = 0.01)
  expect_equal_numeric(row_19$Alpha, 0.004395264, tolerance = 1e-5)
})

# =============================================================================
# Input validation tests
# =============================================================================

test_that("FitCurves validates required inputs", {
  data(apt, package = "beezdemand")

  # Missing data
  expect_error(FitCurves(), "Need to provide a dataframe")

  # Missing equation
  expect_error(FitCurves(apt), "Need to specify an equation")

  # Invalid aggregation
  expect_error(
    FitCurves(apt[apt$id == 19, ], "hs", k = 2, agg = "invalid"),
    "No correct agg specified"
  )
})

test_that("FitCurves handles constrainq0 validation", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  # constrainq0 must be numeric
  expect_error(
    FitCurves(test_data, "hs", k = 2, constrainq0 = "abc"),
    "Q0 constraint must be a number"
  )
})

# =============================================================================
# Detailed output tests
# =============================================================================

test_that("FitCurves detailed=TRUE returns list with model objects", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  result <- suppressWarnings(FitCurves(test_data, "hs", k = 2, detailed = TRUE))

  # Should be a list with 4 elements
  expect_type(result, "list")
  expect_length(result, 4)

  # First element is the results dataframe
  expect_s3_class(result[[1]], "data.frame")
  expect_equal(nrow(result[[1]]), 1)

  # Second element contains model fits
  expect_type(result[[2]], "list")

  # Third element contains individual data
  expect_type(result[[3]], "list")

  # Fourth element contains new data
  expect_type(result[[4]], "list")
})

# =============================================================================
# Column specification tests
# =============================================================================

test_that("FitCurves works with custom column names", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  # Rename columns
  names(test_data) <- c("subject", "price", "consumption")

  result <- suppressWarnings(
    FitCurves(test_data, "hs", k = 2,
              xcol = "price", ycol = "consumption", idcol = "subject")
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

# =============================================================================
# Edge case tests
# =============================================================================

test_that("FitCurves handles single subject", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  result <- suppressWarnings(FitCurves(test_data, "hs", k = 2))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  # Note: id is returned as character
  expect_equal(as.character(result$id), "19")
})

test_that("FitCurves handles data with no zeros (koff equation)", {
  data(apt, package = "beezdemand")
  # Filter to data without zeros
  test_data <- apt[apt$id == 19 & apt$y > 0, ]

  # Should not produce warning for koff equation
  result <- FitCurves(test_data, "koff", k = 2)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})
