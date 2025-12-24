# Tests for trans.R functions (ll4, ll4_inv, scale_ll4, pseudo_ll4_trans)

# =============================================================================
# Tests for ll4 (Log-Logistic Transformation)
# =============================================================================

test_that("ll4 handles zero correctly", {
  expect_equal(ll4(0), 0)
  expect_equal(ll4(0, lambda = 2), 0)
})

test_that("ll4 transforms positive values correctly", {
  # Known values: ll4(x) = log10(x^4 + 1) / 4
  x <- 1
  expected <- log10(1^4 + 1) / 4  # log10(2) / 4
  expect_equal(ll4(x), expected)

  x <- 10
  expected <- log10(10^4 + 1) / 4  # ~= log10(10001) / 4 ~= 1
  expect_equal(ll4(x), expected, tolerance = 0.001)
})

test_that("ll4 is monotonically increasing for positive values", {
  x_vals <- c(0, 0.1, 1, 10, 100, 1000)
  y_vals <- ll4(x_vals)

  # Check that each successive value is greater
  for (i in 2:length(y_vals)) {
    expect_gt(y_vals[i], y_vals[i-1])
  }
})

test_that("ll4 works with different lambda values", {
  x <- 10
  result_lambda2 <- ll4(x, lambda = 2)
  result_lambda4 <- ll4(x, lambda = 4)
  result_lambda8 <- ll4(x, lambda = 8)

  # All should be positive
  expect_gt(result_lambda2, 0)
  expect_gt(result_lambda4, 0)
  expect_gt(result_lambda8, 0)

  # All should be different (different lambdas give different results)
  expect_false(result_lambda2 == result_lambda4)
  expect_false(result_lambda4 == result_lambda8)

  # Verify the mathematical relationship: ll4(x) = log10(x^lambda + 1) / lambda
  expected_lambda4 <- log10(10^4 + 1) / 4
  expect_equal(result_lambda4, expected_lambda4, tolerance = 1e-10)
})

test_that("ll4 works with different base values", {
  x <- 10
  result_base10 <- ll4(x, base = 10)
  result_baseE <- ll4(x, base = exp(1))

  # Natural log gives larger values than log10
  expect_gt(result_baseE, result_base10)
})

test_that("ll4 warns for negative values", {
  expect_warning(ll4(-1), "negative values")
})

test_that("ll4 handles vectors", {
  x_vals <- c(0, 1, 10, 100)
  result <- ll4(x_vals)

  expect_length(result, 4)
  expect_true(all(is.finite(result)))
})

test_that("ll4 handles NA values", {
  x_vals <- c(0, 1, NA, 10)
  result <- ll4(x_vals)

  expect_true(is.na(result[3]))
  expect_equal(result[1], 0)
})

# =============================================================================
# Tests for ll4_inv (Inverse Log-Logistic Transformation)
# =============================================================================

test_that("ll4_inv is the inverse of ll4", {
  original_values <- c(0, 0.1, 1, 10, 100, 1000)
  transformed <- ll4(original_values)
  back_transformed <- ll4_inv(transformed)

  expect_equal(back_transformed, original_values, tolerance = 1e-10)
})

test_that("ll4_inv handles zero correctly", {
  # ll4(0) = 0, so ll4_inv(0) should = 0
  expect_equal(ll4_inv(0), 0)
})

test_that("ll4_inv handles vectors", {
  y_vals <- c(0, 0.1, 0.5, 1, 1.5)
  result <- ll4_inv(y_vals)

  expect_length(result, 5)
})

test_that("ll4_inv works with different lambda values", {
  original <- c(1, 10, 100)

  # Test roundtrip with different lambda values
  for (lambda in c(2, 4, 8)) {
    transformed <- ll4(original, lambda = lambda)
    back <- ll4_inv(transformed, lambda = lambda)
    expect_equal(back, original, tolerance = 1e-10)
  }
})

test_that("ll4_inv works with different base values", {
  original <- c(1, 10, 100)

  # Test roundtrip with different bases
  for (base in c(10, exp(1), 2)) {
    transformed <- ll4(original, base = base)
    back <- ll4_inv(transformed, base = base)
    expect_equal(back, original, tolerance = 1e-10)
  }
})

test_that("ll4_inv handles NA values", {
  y_vals <- c(0, 0.5, NA, 1)
  result <- ll4_inv(y_vals)

  expect_true(is.na(result[3]))
  expect_equal(result[1], 0)
})

# =============================================================================
# Tests for scale_ll4 (ggplot2 scale)
# =============================================================================

test_that("scale_ll4 returns a ggplot2 scale object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  scale_obj <- scale_ll4()

  expect_s3_class(scale_obj, "Scale")
  expect_s3_class(scale_obj, "ScaleContinuous")
})

test_that("scale_ll4 accepts lambda parameter", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  scale_obj <- scale_ll4(lambda = 2)

  expect_s3_class(scale_obj, "Scale")
})

test_that("scale_ll4 works with ggplot2", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  df <- data.frame(x = 1:10, y = c(0, 1, 5, 10, 50, 100, 200, 500, 800, 1000))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    scale_ll4()

  expect_s3_class(p, "ggplot")

  # Build the plot to ensure it doesn't error
  built <- ggplot2::ggplot_build(p)
  expect_true(is.list(built))
})

# =============================================================================
# Tests for pseudo_ll4_trans (scales transformation object)
# =============================================================================

test_that("pseudo_ll4_trans returns a trans object", {
  skip_if_not_installed("scales")

  trans_obj <- pseudo_ll4_trans()

  # scales::trans_new returns a "transform" class object
  expect_s3_class(trans_obj, "transform")
})

test_that("pseudo_ll4_trans accepts lambda parameter", {
  skip_if_not_installed("scales")

  trans_obj <- pseudo_ll4_trans(lambda = 2)

  expect_s3_class(trans_obj, "transform")
})

test_that("pseudo_ll4_trans transformation works correctly", {
  skip_if_not_installed("scales")

  trans_obj <- pseudo_ll4_trans(lambda = 4)

  # Test the transform function
  x_vals <- c(0, 1, 10, 100)
  transformed <- trans_obj$transform(x_vals)
  expected <- ll4(x_vals, lambda = 4)

  expect_equal(transformed, expected)
})

test_that("pseudo_ll4_trans inverse works correctly", {
  skip_if_not_installed("scales")

  trans_obj <- pseudo_ll4_trans(lambda = 4)

  # Test roundtrip
  original <- c(0, 1, 10, 100)
  transformed <- trans_obj$transform(original)
  back <- trans_obj$inverse(transformed)

  expect_equal(back, original, tolerance = 1e-10)
})

test_that("pseudo_ll4_trans works with ggplot2 scale_y_continuous", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  df <- data.frame(x = 1:10, y = c(0, 1, 5, 10, 50, 100, 200, 500, 800, 1000))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(trans = pseudo_ll4_trans(lambda = 4))

  expect_s3_class(p, "ggplot")

  # Build the plot to ensure it doesn't error
  built <- ggplot2::ggplot_build(p)
  expect_true(is.list(built))
})

# =============================================================================
# Edge case and integration tests
# =============================================================================

test_that("ll4 transformation preserves ordering", {
  # For demand data, we want smaller values to remain smaller
  x_sorted <- sort(c(0, 0.01, 0.1, 1, 10, 100, 1000))
  y_transformed <- ll4(x_sorted)

  # Check ordering is preserved
  expect_equal(y_transformed, sort(y_transformed))
})

test_that("ll4 handles typical demand data values", {
  data(apt, package = "beezdemand")

  # Get consumption values
  y_vals <- apt$y

  # Transform
  y_transformed <- ll4(y_vals, lambda = 4)

  # Check no NaN or Inf values (for non-negative input)
  expect_false(any(is.nan(y_transformed[!is.na(y_vals)])))
  expect_false(any(is.infinite(y_transformed[!is.na(y_vals)])))

  # Zeros should transform to zeros
  expect_equal(y_transformed[y_vals == 0], rep(0, sum(y_vals == 0)))
})

test_that("ll4/ll4_inv roundtrip preserves original data structure", {
  data(apt, package = "beezdemand")

  # Get non-negative consumption values
  y_vals <- apt$y[apt$y >= 0]

  # Roundtrip
  transformed <- ll4(y_vals, lambda = 4)
  recovered <- ll4_inv(transformed, lambda = 4)

  expect_equal(recovered, y_vals, tolerance = 1e-10)
})
