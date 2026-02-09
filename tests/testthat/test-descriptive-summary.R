# Tests for descriptive-summary.R (get_descriptive_summary and methods)

# =============================================================================
# Tests for get_descriptive_summary
# =============================================================================

test_that("get_descriptive_summary returns correct S3 structure", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  expect_s3_class(result, "beezdemand_descriptive")
  expect_true(all(c("statistics", "call", "data_summary", "data") %in% names(result)))
})

test_that("get_descriptive_summary statistics have correct columns", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  expect_true(all(c("Price", "Mean", "Median", "SD", "PropZeros", "NAs", "Min", "Max") %in%
                    names(result$statistics)))
})

test_that("get_descriptive_summary returns one row per unique price", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  expect_equal(nrow(result$statistics), length(unique(apt$x)))
})

test_that("get_descriptive_summary calculates Mean correctly", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # Manually calculate mean for first price
  first_price <- unique(apt$x)[1]
  expected_mean <- round(mean(apt$y[apt$x == first_price], na.rm = TRUE), 2)

  expect_equal(result$statistics$Mean[1], expected_mean)
})

test_that("get_descriptive_summary calculates Median correctly", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # Manually calculate median for first price
  first_price <- unique(apt$x)[1]
  expected_median <- round(median(apt$y[apt$x == first_price], na.rm = TRUE), 2)

  expect_equal(result$statistics$Median[1], expected_median)
})

test_that("get_descriptive_summary calculates SD correctly", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # Manually calculate SD for first price
  first_price <- unique(apt$x)[1]
  expected_sd <- round(sd(apt$y[apt$x == first_price], na.rm = TRUE), 2)

  expect_equal(result$statistics$SD[1], expected_sd)
})

test_that("get_descriptive_summary calculates PropZeros correctly", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # Manually calculate proportion of zeros for first price
  first_price <- unique(apt$x)[1]
  y_vals <- apt$y[apt$x == first_price]
  expected_prop <- round(sum(y_vals == 0, na.rm = TRUE) / length(y_vals), 2)

  expect_equal(result$statistics$PropZeros[1], expected_prop)
})

test_that("get_descriptive_summary NAs column is non-negative", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  expect_true(all(result$statistics$NAs >= 0))
  expect_true(all(result$statistics$NAs == floor(result$statistics$NAs)))
})

test_that("get_descriptive_summary handles data without NAs", {
  data(apt, package = "beezdemand")

  # Clean data with no NAs
  clean_data <- apt[!is.na(apt$y), ]

  result <- get_descriptive_summary(clean_data)

  # All NA counts should be 0
  expect_true(all(result$statistics$NAs == 0))
})

test_that("get_descriptive_summary calculates Min correctly", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # Manually calculate min for first price
  first_price <- unique(apt$x)[1]
  expected_min <- round(min(apt$y[apt$x == first_price], na.rm = TRUE), 2)

  expect_equal(result$statistics$Min[1], expected_min)
})

test_that("get_descriptive_summary calculates Max correctly", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # Manually calculate max for first price
  first_price <- unique(apt$x)[1]
  expected_max <- round(max(apt$y[apt$x == first_price], na.rm = TRUE), 2)

  expect_equal(result$statistics$Max[1], expected_max)
})

test_that("get_descriptive_summary handles single subject data", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  result <- get_descriptive_summary(test_data)

  # Should have one row per unique price in single subject data
  expect_equal(nrow(result$statistics), length(unique(test_data$x)))

  # For single subject, mean = median = the value itself
  first_price <- unique(test_data$x)[1]
  y_val <- test_data$y[test_data$x == first_price]
  expect_equal(result$statistics$Mean[1], y_val)
  expect_equal(result$statistics$Median[1], y_val)
})

test_that("get_descriptive_summary handles data with all zeros at a price", {
  # Create test data with all zeros at one price
  test_data <- data.frame(
    id = rep(1:3, each = 5),
    x = rep(c(0, 1, 2, 3, 4), 3),
    y = c(10, 5, 3, 0, 0,
          8, 4, 2, 0, 0,
          12, 6, 4, 0, 0)
  )

  result <- get_descriptive_summary(test_data)

  # Find rows where all values are zero (prices 3 and 4)
  price_4_row <- result$statistics[result$statistics$Price == "4", ]
  expect_equal(price_4_row$Mean, 0)
  expect_equal(price_4_row$PropZeros, 1)
})

test_that("get_descriptive_summary returns Price as character", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  expect_type(result$statistics$Price, "character")
})

test_that("get_descriptive_summary handles numeric price values with X prefix", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # Prices should be clean numbers (as characters)
  expect_false(any(grepl("^X", result$statistics$Price)))
})

test_that("get_descriptive_summary PropZeros is between 0 and 1", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  expect_true(all(result$statistics$PropZeros >= 0 & result$statistics$PropZeros <= 1))
})

test_that("get_descriptive_summary Min <= Mean <= Max", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # Min should be <= Mean (with tolerance for rounding)
  expect_true(all(result$statistics$Min <= result$statistics$Mean + 0.01))

  # Mean should be <= Max (with tolerance for rounding)
  expect_true(all(result$statistics$Mean <= result$statistics$Max + 0.01))
})

test_that("get_descriptive_summary Min <= Median <= Max", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # Min should be <= Median
  expect_true(all(result$statistics$Min <= result$statistics$Median + 0.01))

  # Median should be <= Max
  expect_true(all(result$statistics$Median <= result$statistics$Max + 0.01))
})

test_that("get_descriptive_summary SD is non-negative", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # SD should always be >= 0 (except for single value where it's NA)
  expect_true(all(result$statistics$SD >= 0 | is.na(result$statistics$SD)))
})

test_that("get_descriptive_summary data_summary contains correct values", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  expect_equal(result$data_summary$n_subjects, length(unique(apt$id)))
  expect_equal(result$data_summary$n_prices, length(unique(apt$x)))
  expect_equal(length(result$data_summary$prices), length(unique(apt$x)))
})

test_that("get_descriptive_summary accepts custom column names", {
  data(apt, package = "beezdemand")

  # Rename columns
  test_data <- apt
  names(test_data) <- c("subject", "price", "consumption")

  result <- get_descriptive_summary(test_data,
                                   x_var = "price",
                                   y_var = "consumption",
                                   id_var = "subject")

  expect_s3_class(result, "beezdemand_descriptive")
  expect_equal(nrow(result$statistics), length(unique(test_data$price)))
})

test_that("get_descriptive_summary errors with missing columns", {
  data(apt, package = "beezdemand")

  test_data <- apt[, c("id", "x")]  # Missing y column

  expect_error(get_descriptive_summary(test_data), "Missing required columns")
})

test_that("get_descriptive_summary errors with non-data.frame input", {
  expect_error(get_descriptive_summary(c(1, 2, 3)), "'data' must be a data frame")
})

# =============================================================================
# Tests for print method
# =============================================================================

test_that("print.beezdemand_descriptive runs without error", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  expect_output(print(result), "Descriptive Summary")
  expect_output(print(result), "Data Summary")
  expect_output(print(result), "Statistics by Price")
})

# =============================================================================
# Tests for summary method
# =============================================================================

test_that("summary.beezdemand_descriptive runs without error", {
  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  expect_output(summary(result), "Extended Summary")
  expect_output(summary(result), "Distribution of Mean Consumption")
})

test_that("summary.beezdemand_descriptive detects prices with all zeros", {
  # Create test data with all zeros at one price
  test_data <- data.frame(
    id = rep(1:3, each = 3),
    x = rep(c(0, 1, 2), 3),
    y = c(10, 5, 0,
          8, 4, 0,
          12, 6, 0)
  )

  result <- get_descriptive_summary(test_data)

  expect_output(summary(result), "Prices with all zeros: 1")
})

# =============================================================================
# Tests for plot method
# =============================================================================

test_that("plot.beezdemand_descriptive returns ggplot object", {
  skip_if_not_installed("ggplot2")

  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)
  p <- plot(result)

  expect_s3_class(p, "ggplot")
})

test_that("plot.beezdemand_descriptive accepts transformations", {
  skip_if_not_installed("ggplot2")

  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)

  # Test different transformations don't error
  expect_s3_class(plot(result, y_trans = "log10"), "ggplot")
  expect_s3_class(plot(result, y_trans = "pseudo_log"), "ggplot")
})

test_that("plot.beezdemand_descriptive with show_zeros adds labels", {
  skip_if_not_installed("ggplot2")

  data(apt, package = "beezdemand")

  result <- get_descriptive_summary(apt)
  p <- plot(result, show_zeros = TRUE)

  expect_s3_class(p, "ggplot")
  # Check that geom_text layer was added
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomText"))))
})
