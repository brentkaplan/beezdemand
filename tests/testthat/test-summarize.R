# Tests for summarize.R (GetDescriptives)

# =============================================================================
# Tests for GetDescriptives
# =============================================================================

test_that("GetDescriptives returns correct structure", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("Price", "Mean", "Median", "SD", "PropZeros", "NAs", "Min", "Max") %in%
                    names(result)))
})

test_that("GetDescriptives returns one row per unique price", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  expect_equal(nrow(result), length(unique(apt$x)))
})

test_that("GetDescriptives calculates Mean correctly", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # Manually calculate mean for first price
  first_price <- unique(apt$x)[1]
  expected_mean <- round(mean(apt$y[apt$x == first_price], na.rm = TRUE), 2)

  expect_equal(result$Mean[1], expected_mean)
})

test_that("GetDescriptives calculates Median correctly", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # Manually calculate median for first price
  first_price <- unique(apt$x)[1]
  expected_median <- round(median(apt$y[apt$x == first_price], na.rm = TRUE), 2)

  expect_equal(result$Median[1], expected_median)
})

test_that("GetDescriptives calculates SD correctly", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # Manually calculate SD for first price
  first_price <- unique(apt$x)[1]
  expected_sd <- round(sd(apt$y[apt$x == first_price], na.rm = TRUE), 2)

  expect_equal(result$SD[1], expected_sd)
})

test_that("GetDescriptives calculates PropZeros correctly", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # Manually calculate proportion of zeros for first price
  first_price <- unique(apt$x)[1]
  y_vals <- apt$y[apt$x == first_price]
  expected_prop <- round(sum(y_vals == 0, na.rm = TRUE) / length(y_vals), 2)

  expect_equal(result$PropZeros[1], expected_prop)
})

test_that("GetDescriptives NAs column is non-negative", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # NAs column should exist and be non-negative integers
  expect_true("NAs" %in% names(result))
  expect_true(all(result$NAs >= 0))
  expect_true(all(result$NAs == floor(result$NAs)))
})

test_that("GetDescriptives handles data without NAs", {
  data(apt, package = "beezdemand")

  # Clean data with no NAs
  clean_data <- apt[!is.na(apt$y), ]

  result <- GetDescriptives(clean_data)

  # All NA counts should be 0
  expect_true(all(result$NAs == 0))
})

test_that("GetDescriptives calculates Min correctly", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # Manually calculate min for first price
  first_price <- unique(apt$x)[1]
  expected_min <- round(min(apt$y[apt$x == first_price], na.rm = TRUE), 2)

  expect_equal(result$Min[1], expected_min)
})

test_that("GetDescriptives calculates Max correctly", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # Manually calculate max for first price
  first_price <- unique(apt$x)[1]
  expected_max <- round(max(apt$y[apt$x == first_price], na.rm = TRUE), 2)

  expect_equal(result$Max[1], expected_max)
})

test_that("GetDescriptives handles single subject data", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  result <- GetDescriptives(test_data)

  # Should have one row per unique price in single subject data
  expect_equal(nrow(result), length(unique(test_data$x)))

  # For single subject, mean = median = the value itself
  first_price <- unique(test_data$x)[1]
  y_val <- test_data$y[test_data$x == first_price]
  expect_equal(result$Mean[1], y_val)
  expect_equal(result$Median[1], y_val)
})

test_that("GetDescriptives handles data with all zeros at a price", {
  # Create test data with all zeros at one price
  test_data <- data.frame(
    id = rep(1:3, each = 5),
    x = rep(c(0, 1, 2, 3, 4), 3),
    y = c(10, 5, 3, 0, 0,
          8, 4, 2, 0, 0,
          12, 6, 4, 0, 0)
  )

  result <- GetDescriptives(test_data)

  # Find rows where all values are zero (prices 3 and 4)
  price_4_row <- result[result$Price == "4", ]
  expect_equal(price_4_row$Mean, 0)
  expect_equal(price_4_row$PropZeros, 1)
})

test_that("GetDescriptives returns Price as character", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  expect_type(result$Price, "character")
})

test_that("GetDescriptives handles numeric price values with X prefix", {
  # The function uses gsub to remove "X" prefix from prices
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # Prices should be clean numbers (as characters)
  expect_false(any(grepl("^X", result$Price)))
})

test_that("GetDescriptives PropZeros is between 0 and 1", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  expect_true(all(result$PropZeros >= 0 & result$PropZeros <= 1))
})

test_that("GetDescriptives Min <= Mean <= Max", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # Min should be <= Mean (with tolerance for rounding)
  expect_true(all(result$Min <= result$Mean + 0.01))

  # Mean should be <= Max (with tolerance for rounding)
  expect_true(all(result$Mean <= result$Max + 0.01))
})

test_that("GetDescriptives Min <= Median <= Max", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # Min should be <= Median
  expect_true(all(result$Min <= result$Median + 0.01))

  # Median should be <= Max
  expect_true(all(result$Median <= result$Max + 0.01))
})

test_that("GetDescriptives SD is non-negative", {
  data(apt, package = "beezdemand")

  result <- GetDescriptives(apt)

  # SD should always be >= 0 (except for single value where it's NA)
  expect_true(all(result$SD >= 0 | is.na(result$SD)))
})

# =============================================================================
# Box plot tests (bwplot = TRUE)
# =============================================================================

test_that("GetDescriptives with bwplot=TRUE creates plot without error", {
  skip_if_not_installed("ggplot2")

  data(apt, package = "beezdemand")

  # Use a temporary directory
  temp_dir <- tempdir()
  outdir <- file.path(temp_dir, "test_plots/")

  # This should not error, even though it creates a plot
  result <- suppressWarnings(
    GetDescriptives(apt, bwplot = TRUE, outdir = outdir, device = "png",
                    filename = "test_bwplot")
  )

  expect_s3_class(result, "data.frame")

  # Check if file was created
  expected_file <- file.path(outdir, "test_bwplot.png")
  if (file.exists(expected_file)) {
    expect_true(file.exists(expected_file))
    # Cleanup
    unlink(expected_file)
    unlink(outdir, recursive = TRUE)
  }
})

test_that("GetDescriptives with bwplot=TRUE and device='pdf' creates pdf", {
  skip_if_not_installed("ggplot2")

  data(apt, package = "beezdemand")

  temp_dir <- tempdir()
  outdir <- file.path(temp_dir, "test_plots_pdf/")

  result <- suppressWarnings(
    GetDescriptives(apt, bwplot = TRUE, outdir = outdir, device = "pdf",
                    filename = "test_bwplot_pdf")
  )

  expect_s3_class(result, "data.frame")

  # Check if file was created
  expected_file <- file.path(outdir, "test_bwplot_pdf.pdf")
  if (file.exists(expected_file)) {
    expect_true(file.exists(expected_file))
    # Cleanup
    unlink(expected_file)
    unlink(outdir, recursive = TRUE)
  }
})
