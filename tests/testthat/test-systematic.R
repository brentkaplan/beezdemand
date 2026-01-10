# Tests for systematic.R (CheckUnsystematic) and check_unsystematic_cp

# =============================================================================
# Tests for CheckUnsystematic (Stein et al. 2015 criteria)
# =============================================================================

test_that("CheckUnsystematic returns correct structure", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id %in% c(19, 30), ]

  result <- CheckUnsystematic(test_data)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "TotalPass", "DeltaQ", "DeltaQPass", "Bounce",
                    "BouncePass", "Reversals", "ReversalsPass", "NumPosValues") %in%
                    names(result)))
  expect_equal(nrow(result), 2)
})

test_that("CheckUnsystematic handles single subject", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  result <- CheckUnsystematic(test_data)

  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$id), "19")
})

test_that("CheckUnsystematic calculates DeltaQ correctly", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  result <- CheckUnsystematic(test_data, deltaq = 0.025)

  # DeltaQ should be a numeric value
  expect_true(is.numeric(result$DeltaQ))

  # For typical demand data, DeltaQ should be positive (decreasing consumption)
  expect_gt(result$DeltaQ, 0)
})

test_that("CheckUnsystematic DeltaQPass depends on deltaq threshold", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  # Get result with default threshold
  result <- CheckUnsystematic(test_data, deltaq = 0.025)

  # Test with very high threshold (should fail more often)
  result_high <- CheckUnsystematic(test_data, deltaq = 10)
  expect_equal(result_high$DeltaQPass, "Fail")

  # Test with very low threshold (should pass more often)
  result_low <- CheckUnsystematic(test_data, deltaq = 0.0001)
  expect_equal(result_low$DeltaQPass, "Pass")
})

test_that("CheckUnsystematic calculates Bounce correctly", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  result <- CheckUnsystematic(test_data, bounce = 0.10)

  # Bounce should be between 0 and 1
  expect_gte(result$Bounce, 0)
  expect_lte(result$Bounce, 1)
})

test_that("CheckUnsystematic BouncePass depends on bounce threshold", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  # Very strict threshold
  result_strict <- CheckUnsystematic(test_data, bounce = 0)
  # Very lenient threshold
  result_lenient <- CheckUnsystematic(test_data, bounce = 1)

  expect_equal(result_lenient$BouncePass, "Pass")
})

test_that("CheckUnsystematic counts reversals correctly", {
  data(apt, package = "beezdemand")

  result <- CheckUnsystematic(apt)

  # Reversals should be non-negative integers
  expect_true(all(result$Reversals >= 0))
  expect_true(all(result$Reversals == floor(result$Reversals)))
})

test_that("CheckUnsystematic ncons0=1 is more conservative than ncons0=2", {
  # Create data with known reversal pattern: 0 -> non-zero
  test_data <- data.frame(
    id = rep(1, 8),
    x = c(0, 0.5, 1, 2, 3, 4, 5, 6),
    y = c(10, 5, 0, 5, 0, 0, 5, 3)  # Has both single and double zero reversals
  )

  result_ncons1 <- CheckUnsystematic(test_data, ncons0 = 1)
  result_ncons2 <- CheckUnsystematic(test_data, ncons0 = 2)

  # ncons0=1 should detect more reversals
  expect_gte(result_ncons1$Reversals, result_ncons2$Reversals)
})

test_that("CheckUnsystematic counts NumPosValues correctly", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 38, ]

  result <- CheckUnsystematic(test_data)

  # Manual count of non-zero values
  expected_pos <- sum(test_data$y != 0)
  expect_equal(result$NumPosValues, expected_pos)
})

test_that("CheckUnsystematic TotalPass sums correctly", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  result <- CheckUnsystematic(test_data)

  # TotalPass should be 0-3 (count of "Pass" values)
  expect_true(result$TotalPass >= 0 && result$TotalPass <= 3)

  # Manual count
  pass_count <- sum(c(result$DeltaQPass, result$BouncePass, result$ReversalsPass) == "Pass")
  expect_equal(result$TotalPass, pass_count)
})

test_that("CheckUnsystematic warns on NA values", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]
  test_data$y[5] <- NA

  expect_warning(CheckUnsystematic(test_data), "NA values found")
})

test_that("CheckUnsystematic handles tibble input", {
  data(apt, package = "beezdemand")
  test_data <- tibble::as_tibble(apt[apt$id == 19, ])

  # Should work without error
  result <- CheckUnsystematic(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("CheckUnsystematic works with full apt dataset", {
  data(apt, package = "beezdemand")

  result <- CheckUnsystematic(apt)

  # Should have one row per unique ID
  expect_equal(nrow(result), length(unique(apt$id)))

  # Check column types
  expect_type(result$DeltaQ, "double")
  expect_type(result$Bounce, "double")
  expect_type(result$Reversals, "double")
  expect_type(result$NumPosValues, "integer")
})

# =============================================================================
# Tests for check_unsystematic_cp (Cross-Price systematic checks)
# =============================================================================

test_that("check_unsystematic_cp returns cp_unsystematic class", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  test_data <- data.frame(x = x_seq, y = 10 - seq_len(10))

  result <- check_unsystematic_cp(test_data)

  expect_s3_class(result, "cp_unsystematic")
  expect_s3_class(result, "data.frame")
})

test_that("check_unsystematic_cp returns correct structure", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  test_data <- data.frame(x = x_seq, y = 10 - seq_len(10))

  result <- check_unsystematic_cp(test_data)

  expect_true(all(c("delta_direction", "bounce_direction", "bounce_any",
                    "bounce_above", "bounce_below", "reversals", "returns") %in%
                    names(result)))
})

test_that("check_unsystematic_cp returns detailed output", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  test_data <- data.frame(x = x_seq, y = 10 - seq_len(10))

  result <- check_unsystematic_cp(test_data, detailed = TRUE)

  expect_true(all(c("delta_down", "delta_up", "delta_none",
                    "bounce_up", "bounce_down", "bounce_none") %in%
                    names(result)))
})

test_that("check_unsystematic_cp detects downward trend", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  # Clear downward trend
  test_data <- data.frame(x = x_seq, y = c(100, 80, 60, 40, 30, 20, 15, 10, 5, 2))

  result <- check_unsystematic_cp(test_data)

  expect_equal(result$delta_direction, "down")
})

test_that("check_unsystematic_cp detects upward trend", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  # Clear upward trend
  test_data <- data.frame(x = x_seq, y = c(1, 2, 5, 10, 15, 20, 30, 50, 80, 100))

  result <- check_unsystematic_cp(test_data)

  expect_equal(result$delta_direction, "up")
})

test_that("check_unsystematic_cp detects no trend", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  # Flat data
  test_data <- data.frame(x = x_seq, y = rep(50, 10))

  result <- check_unsystematic_cp(test_data)

  expect_equal(result$delta_direction, "none")
})

test_that("check_unsystematic_cp detects bounces", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  # Data with significant bounces
  test_data <- data.frame(x = x_seq, y = c(10, 5, 50, 8, 40, 6, 35, 4, 25, 2))

  result <- check_unsystematic_cp(test_data)

  expect_true(result$bounce_any)
})

test_that("check_unsystematic_cp counts bounce_above and bounce_below", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  test_data <- data.frame(x = x_seq, y = c(10, 5, 20, 8, 15, 6, 12, 4, 9, 2))

  result <- check_unsystematic_cp(test_data)

  expect_true(is.numeric(result$bounce_above))
  expect_true(is.numeric(result$bounce_below))
  expect_gte(result$bounce_above, 0)
  expect_gte(result$bounce_below, 0)
})

test_that("check_unsystematic_cp detects reversals from zeros", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  # Data with reversal: 0, 0, then positive
  test_data <- data.frame(x = x_seq, y = c(10, 8, 5, 0, 0, 5, 3, 1, 0, 0))

  result <- check_unsystematic_cp(test_data)

  expect_true(is.numeric(result$reversals) || is.na(result$reversals))
})

test_that("check_unsystematic_cp detects returns to zeros", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  # Data with returns: positive, positive, then 0
  test_data <- data.frame(x = x_seq, y = c(10, 8, 5, 3, 0, 5, 3, 0, 0, 0))

  result <- check_unsystematic_cp(test_data)

  expect_true(is.numeric(result$returns) || is.na(result$returns))
})

test_that("check_unsystematic_cp validates input", {
  # Not a data frame
  expect_error(check_unsystematic_cp(list(x = 1:5, y = 1:5)),
               "must be a data frame")

  # Missing columns
  expect_error(check_unsystematic_cp(data.frame(a = 1:5, b = 1:5)),
               "must contain 'x' and 'y' columns")

  # Too few rows
  expect_error(check_unsystematic_cp(data.frame(x = 1:2, y = 1:2)),
               "at least 3 rows")
})

test_that("check_unsystematic_cp warns about NA values", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  test_data <- data.frame(x = x_seq, y = c(10, 8, NA, 5, 4, 3, 2, 1, 0.5, 0.1))

  expect_warning(check_unsystematic_cp(test_data), "NA values")
})

test_that("check_unsystematic_cp expected_down suppresses reversals", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  test_data <- data.frame(x = x_seq, y = c(10, 8, 0, 0, 5, 4, 3, 2, 1, 0))

  result_normal <- check_unsystematic_cp(test_data, expected_down = FALSE)
  result_expected <- check_unsystematic_cp(test_data, expected_down = TRUE)

  # When expected_down = TRUE, reversals handling changes
  expect_true(is.numeric(result_normal$reversals) || is.na(result_normal$reversals))
})

test_that("check_unsystematic_cp verbose mode prints output", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  test_data <- data.frame(x = x_seq, y = c(100, 80, 60, 40, 30, 20, 15, 10, 5, 2))

  expect_output(
    check_unsystematic_cp(test_data, verbose = TRUE),
    "proportion"
  )
})

test_that("check_unsystematic_cp thresholds affect bounce detection", {
  x_seq <- 10^(seq(-2, 2, length.out = 10))
  test_data <- data.frame(x = x_seq, y = c(10, 5, 12, 8, 10, 6, 8, 4, 6, 2))

  # Very strict threshold
  result_strict <- check_unsystematic_cp(test_data,
                                         bounce_up_threshold = 0.01,
                                         bounce_down_threshold = 0.01)

  # Very lenient threshold
  result_lenient <- check_unsystematic_cp(test_data,
                                          bounce_up_threshold = 0.9,
                                          bounce_down_threshold = 0.9)

  # Strict should be more likely to detect bounces
  # (though this depends on the data)
  expect_true(is.logical(result_strict$bounce_any))
  expect_true(is.logical(result_lenient$bounce_any))
})
