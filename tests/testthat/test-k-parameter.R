# Tests for k-parameter.R (get_k)

# =============================================================================
# Basic functionality tests
# =============================================================================

test_that("get_k returns numeric value", {
  data(apt, package = "beezdemand")

  result <- get_k(apt)

  expect_type(result, "double")
  expect_length(result, 1)
})

test_that("get_k calculates correctly with use_means = TRUE", {
  # Create simple test data
  test_data <- data.frame(
    id = rep(c("S1", "S2"), each = 3),
    x = rep(c(0, 1, 2), 2),
    y = c(10, 5, 2,
          12, 6, 3)
  )

  result <- get_k(test_data, use_means = TRUE)

  # Calculate expected value manually
  # Means: x=0 -> (10+12)/2 = 11, x=1 -> (5+6)/2 = 5.5, x=2 -> (2+3)/2 = 2.5
  # k = log10(11) - log10(2.5) + 0.5
  expected <- log10(11) - log10(2.5) + 0.5

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("get_k calculates correctly with use_means = FALSE", {
  # Create simple test data
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(10, 5, 2)
  )

  result <- get_k(test_data, use_means = FALSE)

  # Calculate expected value manually
  # Individual values: max = 10, min = 2
  # k = log10(10) - log10(2) + 0.5
  expected <- log10(10) - log10(2) + 0.5

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("get_k uses custom adjustment parameter", {
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(10, 5, 2)
  )

  result_default <- get_k(test_data, adjustment = 0.5)
  result_custom <- get_k(test_data, adjustment = 1.0)

  # Custom adjustment should be 0.5 more than default
  expect_equal(result_custom, result_default + 0.5)
})

test_that("get_k excludes zero values", {
  # Data with zeros
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(0, 1, 2, 3, 4),
    y = c(10, 5, 2, 1, 0)  # Zero at highest price
  )

  result <- get_k(test_data, use_means = FALSE)

  # Should use max = 10, min = 1 (excluding zero)
  expected <- log10(10) - log10(1) + 0.5

  expect_equal(result, expected)
})

test_that("get_k excludes NA values", {
  # Data with NAs
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(0, 1, 2, 3, 4),
    y = c(10, 5, NA, 2, 1)
  )

  result <- get_k(test_data, use_means = FALSE)

  # Should calculate from non-NA values: max = 10, min = 1
  expected <- log10(10) - log10(1) + 0.5

  expect_equal(result, expected)
})

test_that("get_k accepts custom column names", {
  data(apt, package = "beezdemand")

  # Rename columns
  test_data <- apt
  names(test_data) <- c("subject", "price", "consumption")

  result <- get_k(test_data, x_var = "price", y_var = "consumption")

  expect_type(result, "double")
  expect_length(result, 1)
})

test_that("get_k matches GetK with use_means = TRUE", {
  data(apt, package = "beezdemand")

  legacy_result <- suppressWarnings(GetK(apt, mnrange = TRUE))
  modern_result <- get_k(apt, use_means = TRUE)

  expect_equal(modern_result, legacy_result)
})

test_that("get_k matches GetK with use_means = FALSE", {
  data(apt, package = "beezdemand")

  legacy_result <- suppressWarnings(GetK(apt, mnrange = FALSE))
  modern_result <- get_k(apt, use_means = FALSE)

  expect_equal(modern_result, legacy_result)
})

# =============================================================================
# Error handling tests
# =============================================================================

test_that("get_k errors with non-data.frame input", {
  expect_error(get_k(c(1, 2, 3)), "'data' must be a data frame")
})

test_that("get_k errors with missing columns", {
  test_data <- data.frame(
    id = c(1, 2, 3),
    x = c(0, 1, 2)
    # Missing y column
  )

  expect_error(get_k(test_data), "Missing required columns")
})

test_that("get_k errors when no positive values exist", {
  # All zeros
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(0, 0, 0)
  )

  expect_error(get_k(test_data), "No positive consumption values")
})

test_that("get_k errors when aggregated means have no positive values", {
  # This is a pathological case but should be handled
  test_data <- data.frame(
    id = rep(c("S1", "S2"), each = 3),
    x = rep(c(0, 1, 2), 2),
    y = c(0, 0, 0,
          0, 0, 0)
  )

  expect_error(get_k(test_data, use_means = TRUE),
               "No positive consumption values found after aggregating")
})

# =============================================================================
# Verbose output tests
# =============================================================================

test_that("get_k verbose = TRUE prints output with use_means = TRUE", {
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(10, 5, 2)
  )

  expect_output(get_k(test_data, use_means = TRUE, verbose = TRUE),
                "Calculating k from mean consumption values")
  expect_output(get_k(test_data, use_means = TRUE, verbose = TRUE),
                "Max mean:")
  expect_output(get_k(test_data, use_means = TRUE, verbose = TRUE),
                "Min mean:")
})

test_that("get_k verbose = TRUE prints output with use_means = FALSE", {
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(10, 5, 2)
  )

  expect_output(get_k(test_data, use_means = FALSE, verbose = TRUE),
                "Calculating k from individual consumption values")
  expect_output(get_k(test_data, use_means = FALSE, verbose = TRUE),
                "Max:")
  expect_output(get_k(test_data, use_means = FALSE, verbose = TRUE),
                "Min:")
})

test_that("get_k verbose = FALSE produces no output", {
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(10, 5, 2)
  )

  expect_silent(get_k(test_data, verbose = FALSE))
})

# =============================================================================
# Edge cases
# =============================================================================

test_that("get_k handles single non-zero value gracefully", {
  # Only one positive value
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(5, 0, 0)
  )

  # This should work - min and max will be the same value
  result <- get_k(test_data, use_means = FALSE)

  # k = log10(5) - log10(5) + 0.5 = 0.5
  expect_equal(result, 0.5)
})

test_that("get_k handles very large consumption ranges", {
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(1e6, 1e3, 1)  # Large range
  )

  result <- get_k(test_data, use_means = FALSE)

  # Should handle without overflow
  expect_true(is.finite(result))
  expect_true(result > 0)
})

test_that("get_k handles very small consumption ranges", {
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(1e-3, 1e-4, 1e-5)  # Small values
  )

  result <- get_k(test_data, use_means = FALSE)

  # Should handle without underflow
  expect_true(is.finite(result))
  expect_true(result > 0)
})

test_that("get_k with adjustment = 0 returns pure log range", {
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(100, 10, 1)
  )

  result <- get_k(test_data, use_means = FALSE, adjustment = 0)

  # k = log10(100) - log10(1) + 0 = 2 - 0 + 0 = 2
  expect_equal(result, 2)
})

test_that("get_k with negative adjustment works", {
  test_data <- data.frame(
    id = rep("S1", 3),
    x = c(0, 1, 2),
    y = c(100, 10, 1)
  )

  result <- get_k(test_data, use_means = FALSE, adjustment = -0.5)

  # k = log10(100) - log10(1) - 0.5 = 2 - 0 - 0.5 = 1.5
  expect_equal(result, 1.5)
})
