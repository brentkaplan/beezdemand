# Tests for empirical-measures.R (get_empirical_measures and methods)

# =============================================================================
# Tests for get_empirical_measures
# =============================================================================

test_that("get_empirical_measures returns correct S3 structure", {
  data(apt, package = "beezdemand")

  result <- get_empirical_measures(apt)

  expect_s3_class(result, "beezdemand_empirical")
  expect_true(all(c("measures", "call", "data_summary") %in% names(result)))
})

test_that("get_empirical_measures measures have correct columns", {
  data(apt, package = "beezdemand")

  result <- get_empirical_measures(apt)

  expect_true(all(
    c("id", "Intensity", "BP0", "BP1", "Omaxe", "Pmaxe") %in%
      names(result$measures)
  ))
})

test_that("get_empirical_measures returns one row per subject", {
  data(apt, package = "beezdemand")

  result <- get_empirical_measures(apt)

  expect_equal(nrow(result$measures), length(unique(apt$id)))
})

test_that("get_empirical_measures calculates Intensity correctly", {
  # Create simple test data
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(0, 1, 2, 3, 4),
    y = c(10, 8, 6, 3, 0)
  )

  result <- get_empirical_measures(test_data)

  # Intensity should be consumption at minimum price (x=0)
  expect_equal(result$measures$Intensity[1], 10)
})

test_that("get_empirical_measures calculates BP0 correctly", {
  # Test data where consumption reaches zero
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(0, 1, 2, 3, 4),
    y = c(10, 8, 6, 3, 0)
  )

  result <- get_empirical_measures(test_data)

  # BP0 should be first price where y = 0 (which is x=4)
  expect_equal(result$measures$BP0[1], 4)
})

test_that("get_empirical_measures BP0 is NA when no zeros", {
  # Test data without zeros
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(0, 1, 2, 3, 4),
    y = c(10, 8, 6, 4, 2)
  )

  result <- get_empirical_measures(test_data)

  # BP0 should be NA when consumption never reaches zero
  expect_true(is.na(result$measures$BP0[1]))
})

test_that("get_empirical_measures calculates BP1 correctly", {
  # Test data with zeros
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(0, 1, 2, 3, 4),
    y = c(10, 8, 6, 0, 0)
  )

  result <- get_empirical_measures(test_data)

  # BP1 should be last price with non-zero consumption (x=2)
  expect_equal(result$measures$BP1[1], 2)
})

test_that("get_empirical_measures BP1 is NA when all zeros", {
  # Test data with all zeros
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(0, 1, 2, 3, 4),
    y = c(0, 0, 0, 0, 0)
  )

  result <- get_empirical_measures(test_data)

  # BP1 should be NA when all consumption is zero
  expect_true(is.na(result$measures$BP1[1]))
})

test_that("get_empirical_measures calculates Omaxe correctly", {
  # Create test data
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(0, 1, 2, 3, 4),
    y = c(10, 8, 6, 3, 0)
  )

  result <- get_empirical_measures(test_data)

  # Calculate expected max expenditure manually
  expend <- test_data$x * test_data$y # 0, 8, 12, 9, 0
  expected_omax <- max(expend) # 12

  expect_equal(result$measures$Omaxe[1], expected_omax)
})

test_that("get_empirical_measures calculates Pmaxe correctly", {
  # Create test data
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(0, 1, 2, 3, 4),
    y = c(10, 8, 6, 3, 0)
  )

  result <- get_empirical_measures(test_data)

  # Max expenditure is at x=2 (2*6=12)
  expect_equal(result$measures$Pmaxe[1], 2)
})

test_that("get_empirical_measures Pmaxe is 0 when Omaxe is 0", {
  # Test data with all zeros
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(0, 1, 2, 3, 4),
    y = c(0, 0, 0, 0, 0)
  )

  result <- get_empirical_measures(test_data)

  expect_equal(result$measures$Omaxe[1], 0)
  expect_equal(result$measures$Pmaxe[1], 0)
})

test_that("get_empirical_measures handles multiple subjects", {
  # Create test data with 3 subjects
  test_data <- data.frame(
    id = rep(c("S1", "S2", "S3"), each = 5),
    x = rep(c(0, 1, 2, 3, 4), 3),
    y = c(10, 8, 6, 3, 0, 12, 10, 7, 4, 1, 8, 6, 4, 2, 0)
  )

  result <- get_empirical_measures(test_data)

  expect_equal(nrow(result$measures), 3)
  expect_equal(result$measures$id, c("S1", "S2", "S3"))
})

test_that("get_empirical_measures errors with duplicate prices", {
  # Create test data with duplicate prices
  test_data <- data.frame(
    id = rep("S1", 6),
    x = c(0, 1, 2, 2, 3, 4), # duplicate price 2
    y = c(10, 8, 6, 5, 3, 0)
  )

  expect_error(
    get_empirical_measures(test_data),
    "Duplicates found where id = S1"
  )
})

test_that("get_empirical_measures data_summary is correct", {
  data(apt, package = "beezdemand")

  result <- get_empirical_measures(apt)

  expect_equal(result$data_summary$n_subjects, length(unique(apt$id)))
  expect_type(result$data_summary$has_zeros, "logical")
  expect_type(result$data_summary$complete_cases, "integer")
})

test_that("get_empirical_measures accepts custom column names", {
  data(apt, package = "beezdemand")

  # Rename columns
  test_data <- apt
  names(test_data) <- c("subject", "price", "consumption")

  result <- get_empirical_measures(
    test_data,
    x_var = "price",
    y_var = "consumption",
    id_var = "subject"
  )

  expect_s3_class(result, "beezdemand_empirical")
  expect_equal(nrow(result$measures), length(unique(test_data$subject)))
})

test_that("get_empirical_measures errors with missing columns", {
  data(apt, package = "beezdemand")

  test_data <- apt[, c("id", "x")] # Missing y column

  expect_error(get_empirical_measures(test_data)) # CheckCols will throw error
})

test_that("get_empirical_measures errors with non-data.frame input", {
  expect_error(
    get_empirical_measures(c(1, 2, 3)),
    "'data' must be a data frame"
  )
})

test_that("get_empirical_measures matches GetEmpirical results", {
  data(apt, package = "beezdemand")

  # Suppress deprecation warning
  legacy_result <- suppressWarnings(GetEmpirical(apt))
  modern_result <- get_empirical_measures(apt)

  # Results should be identical
  expect_equal(modern_result$measures, legacy_result)
})

test_that("get_empirical_measures handles Pmaxe with tied maximum expenditures", {
  # Create data where multiple prices have same max expenditure
  test_data <- data.frame(
    id = rep("S1", 5),
    x = c(1, 2, 3, 4, 5),
    y = c(10, 5, 10 / 3, 2.5, 2) # All give expenditure of 10
  )

  result <- get_empirical_measures(test_data)

  # Should return the highest price with max expenditure
  expect_equal(result$measures$Pmaxe[1], 5)
})

# =============================================================================
# Tests for print method
# =============================================================================

test_that("print.beezdemand_empirical runs without error", {
  data(apt, package = "beezdemand")

  result <- get_empirical_measures(apt)

  expect_output(print(result), "Empirical Demand Measures")
  expect_output(print(result), "Data Summary")
  expect_output(print(result), "Subjects:")
})

# =============================================================================
# Tests for summary method
# =============================================================================

test_that("summary.beezdemand_empirical runs without error", {
  data(apt, package = "beezdemand")

  result <- get_empirical_measures(apt)

  expect_output(summary(result), "Extended Summary")
  expect_output(summary(result), "Descriptive Statistics")
})

test_that("summary.beezdemand_empirical calculates statistics correctly", {
  # Create simple test data
  test_data <- data.frame(
    id = rep(c("S1", "S2", "S3"), each = 5),
    x = rep(c(0, 1, 2, 3, 4), 3),
    y = c(10, 8, 6, 3, 0, 12, 10, 7, 4, 1, 8, 6, 4, 2, 0)
  )

  result <- get_empirical_measures(test_data)
  summ <- suppressMessages(summary(result))

  # Check that statistics are returned
  expect_true("measure_statistics" %in% names(summ))
  expect_true("Intensity" %in% names(summ$measure_statistics))
})

# =============================================================================
# Tests for plot method
# =============================================================================

test_that("plot.beezdemand_empirical histogram returns ggplot object", {
  skip_if_not_installed("ggplot2")

  data(apt, package = "beezdemand")

  result <- get_empirical_measures(apt)
  p <- plot(result, type = "histogram")

  expect_s3_class(p, "ggplot")
})

test_that("plot.beezdemand_empirical matrix returns ggplot with GGally", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("GGally")

  data(apt, package = "beezdemand")

  result <- get_empirical_measures(apt)
  p <- suppressMessages(plot(result, type = "matrix"))

  expect_true(inherits(p, "ggplot") || inherits(p, "ggmatrix"))
})

test_that("plot.beezdemand_empirical default type is histogram", {
  skip_if_not_installed("ggplot2")

  data(apt, package = "beezdemand")

  result <- get_empirical_measures(apt)
  p <- plot(result) # No type specified

  expect_s3_class(p, "ggplot")
})

test_that("plot.beezdemand_empirical errors with invalid type", {
  data(apt, package = "beezdemand")

  result <- get_empirical_measures(apt)

  expect_error(plot(result, type = "invalid"), "'arg' should be one of")
})
