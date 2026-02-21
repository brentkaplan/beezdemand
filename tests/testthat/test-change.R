# Tests for change.R functions (ReplaceZeros, RecodeOutliers, ChangeData)

# =============================================================================
# Tests for ReplaceZeros
# =============================================================================

test_that("ReplaceZeros replaces first zero only by default", {
  data(apt, package = "beezdemand")
  # Select a subject with zeros
  test_data <- apt[apt$id == 38, ]

  # Count original zeros
  original_zeros <- sum(test_data$y == 0)
  expect_gt(original_zeros, 0)  # Confirm there are zeros

  result <- ReplaceZeros(test_data, nrepl = 1, replnum = 0.01)

  # Should have one fewer zero
expect_equal(sum(result$y == 0), original_zeros - 1)
  expect_equal(sum(result$y == 0.01), 1)
})

test_that("ReplaceZeros replaces all zeros when nrepl='all'", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 38, ]

  original_zeros <- sum(test_data$y == 0)
  expect_gt(original_zeros, 0)

  result <- ReplaceZeros(test_data, nrepl = "all", replnum = 0.01)

  # Should have no zeros remaining
  expect_equal(sum(result$y == 0), 0)
  expect_equal(sum(result$y == 0.01), original_zeros)
})

test_that("ReplaceZeros replaces specified number of zeros", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 38, ]

  original_zeros <- sum(test_data$y == 0)

  result <- ReplaceZeros(test_data, nrepl = 2, replnum = 0.05)

  # Should replace up to 2 zeros (or fewer if there are fewer zeros)
  expected_remaining <- max(0, original_zeros - 2)
  expect_equal(sum(result$y == 0), expected_remaining)
})

test_that("ReplaceZeros handles data with no zeros", {
  data(apt, package = "beezdemand")
  # ID 19 data filtered to positive values only
  test_data <- apt[apt$id == 19 & apt$y > 0, ]

  result <- ReplaceZeros(test_data, nrepl = "all", replnum = 0.01)

  # Data should be unchanged
  expect_equal(result$y, test_data$y)
})

test_that("ReplaceZeros uses custom replacement value", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 38, ]

  custom_replnum <- 0.001
  result <- ReplaceZeros(test_data, nrepl = 1, replnum = custom_replnum)

  expect_true(custom_replnum %in% result$y)
})

test_that("ReplaceZeros handles multiple subjects", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  result <- ReplaceZeros(test_data, nrepl = 1, replnum = 0.01)

  # Should have same number of rows
  expect_equal(nrow(result), nrow(test_data))

  # Each id should still be present
  expect_equal(sort(unique(result$id)), sort(test_ids))
})

# =============================================================================
# Tests for RecodeOutliers
# =============================================================================

test_that("RecodeOutliers returns dataframe with same dimensions", {
  # Create test data with known outlier
  test_df <- data.frame(
    col1 = c(1, 2, 3, 4, 5, 100),  # 100 is an outlier
    col2 = c(10, 11, 12, 13, 14, 15)
  )

  result <- suppressMessages(RecodeOutliers(test_df, outval = 2))

  expect_equal(nrow(result), nrow(test_df))
  expect_equal(ncol(result), ncol(test_df))
})

test_that("RecodeOutliers prints messages about outliers", {
  test_df <- data.frame(
    col1 = c(1, 2, 3, 4, 5, 100)  # 100 is an outlier
  )

  expect_message(RecodeOutliers(test_df, outval = 2), "outlier")
})

test_that("RecodeOutliers prints no outliers message when none exist", {
  test_df <- data.frame(
    col1 = c(1, 2, 3, 4, 5)
  )

  expect_message(RecodeOutliers(test_df, outval = 3.29), "No outliers detected")
})

test_that("RecodeOutliers handles NA values", {
  test_df <- data.frame(
    col1 = c(1, 2, NA, 4, 5)
  )

  result <- suppressMessages(RecodeOutliers(test_df, outval = 3.29))

  expect_equal(sum(is.na(result$col1)), sum(is.na(test_df$col1)))
})

# =============================================================================
# Tests for ChangeData
# =============================================================================

test_that("ChangeData replaces first zero by default", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 38, ]

  original_zeros <- sum(test_data$y == 0)

  result <- ChangeData(test_data, nrepl = 1, replnum = 0.01)

  expect_equal(sum(result$y == 0), original_zeros - 1)
})

test_that("ChangeData replaces all zeros with nrepl='all'", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 38, ]

  result <- ChangeData(test_data, nrepl = "all", replnum = 0.01)

  expect_equal(sum(result$y == 0), 0)
})

test_that("ChangeData removes zeros with rem0=TRUE", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 38, ]

  original_rows <- nrow(test_data)
  original_zeros <- sum(test_data$y == 0)

  result <- ChangeData(test_data, rem0 = TRUE)

  # Should have fewer rows
  expect_equal(nrow(result), original_rows - original_zeros)
  expect_equal(sum(result$y == 0), 0)
})

test_that("ChangeData removes free price data with remq0e=TRUE", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  # Check if there's a price of 0
  has_free <- any(test_data$x == 0)

  result <- ChangeData(test_data, remq0e = TRUE)

  # If there was free price, it should be removed
  if (has_free) {
    expect_false(any(result$x == 0))
    expect_lt(nrow(result), nrow(test_data))
  }
})

test_that("ChangeData replaces free price with replfree", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  # Only test if there's a price of 0
  skip_if(!any(test_data$x == 0), "No free price in test data")

  result <- suppressWarnings(ChangeData(test_data, remq0e = TRUE, replfree = 0.001))

  expect_false(any(result$x == 0))
  expect_true(0.001 %in% result$x)
})

test_that("ChangeData works with custom column names", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 38, ]

  # Rename columns
  colnames(test_data) <- c("subject", "price", "consumption")

  result <- ChangeData(test_data, nrepl = 1, replnum = 0.01,
                       xcol = "price", ycol = "consumption", idcol = "subject")

  expect_equal(colnames(result), c("subject", "price", "consumption"))
  expect_equal(nrow(result), nrow(test_data))
})

test_that("ChangeData errors with missing column names", {
  test_data <- data.frame(a = 1:5, b = 1:5, c = rep(1, 5))

  expect_error(
    ChangeData(test_data),
    "Can't find x, y, and id column names"
  )
})

test_that("ChangeData handles multiple subjects", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  result <- ChangeData(test_data, nrepl = "all", replnum = 0.01)

  # All IDs should still be present
  expect_equal(sort(unique(result$id)), sort(test_ids))

  # No zeros should remain
  expect_equal(sum(result$y == 0), 0)
})

test_that("ChangeData warns when remq0e and replfree both specified", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  # Only test if there's a price of 0
  skip_if(!any(test_data$x == 0), "No free price in test data")

  expect_warning(
    ChangeData(test_data, remq0e = TRUE, replfree = 0.01),
    "You asked to remove q0e and replace this value"
  )
})
