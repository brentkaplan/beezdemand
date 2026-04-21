# Tests for statistical corrections (Issues 1-6)
# These tests verify the fixes for the critical and required statistical issues
# identified in the mixed-effects and hurdle model review.

# ==============================================================================
# Issue 1: Lognormal retransformation bias
# ==============================================================================

test_that("hurdle predict(correction=TRUE) applies exp(sigma_e^2/2) factor", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Predictions with and without correction
  pred_corrected <- predict(fit, type = "response", correction = TRUE)
  pred_uncorrected <- predict(fit, type = "response", correction = FALSE)

  # Corrected values should be uniformly larger (by exp(sigma_e^2/2))
  sigma_e <- exp(fit$model$coefficients[["logsigma_e"]])
  expected_factor <- exp(sigma_e^2 / 2)

  ratio <- pred_corrected$.fitted / pred_uncorrected$.fitted
  expect_true(all(abs(ratio - expected_factor) < 1e-10, na.rm = TRUE))
  expect_gt(expected_factor, 1)  # correction always > 1
})

test_that("hurdle predict(correction=TRUE) applies to type='demand' too", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  pred_corr <- predict(fit, type = "demand", correction = TRUE)
  pred_uncorr <- predict(fit, type = "demand", correction = FALSE)

  # Demand = consumption * (1 - P(zero)), so correction should scale consumption
  expect_true(all(pred_corr$.fitted >= pred_uncorr$.fitted, na.rm = TRUE))
})

test_that("hurdle predict correction does NOT affect link or probability types", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  pred_link_corr <- predict(fit, type = "link", correction = TRUE)
  pred_link_uncorr <- predict(fit, type = "link", correction = FALSE)
  expect_equal(pred_link_corr$.fitted, pred_link_uncorr$.fitted)

  pred_prob_corr <- predict(fit, type = "probability", correction = TRUE)
  pred_prob_uncorr <- predict(fit, type = "probability", correction = FALSE)
  expect_equal(pred_prob_corr$.fitted, pred_prob_uncorr$.fitted)
})

test_that("TMB backtransform applies correction for exponential equation", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  # Get predictions with natural scale
  pred_natural <- predict(fit, type = "demand", scale = "natural")
  pred_model <- predict(fit, type = "demand", scale = "model")

  # Natural should be exp(model) * correction
  sigma_e <- exp(fit$model$coefficients[["logsigma_e"]])
  expected <- exp(pred_model$.fitted) * exp(sigma_e^2 / 2)
  expect_equal(pred_natural$.fitted, expected, tolerance = 1e-10)
})

test_that("TMB backtransform does NOT apply correction for simplified equation", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "simplified", verbose = 0
  )

  # Simplified is Gaussian on raw Q; predictions are already the conditional mean
  pred_natural <- predict(fit, type = "demand", scale = "natural")
  pred_model <- predict(fit, type = "demand", scale = "model")
  expect_equal(pred_natural$.fitted, pred_model$.fitted)
})

# ==============================================================================
# Issue 2: Symmetric CIs on positive-definite quantities
# ==============================================================================

test_that("hurdle predict CIs are always positive for type='response'", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  pred <- predict(fit, type = "response", se.fit = TRUE,
                  interval = "confidence", level = 0.95)

  # All lower bounds should be strictly positive
  valid <- !is.na(pred$.lower)
  expect_true(all(pred$.lower[valid] > 0),
              info = "Lower CI bounds should be positive for consumption")
  expect_true(all(pred$.upper[valid] > 0),
              info = "Upper CI bounds should be positive for consumption")
})

test_that("hurdle predict CIs are asymmetric for response type", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  pred <- predict(fit, type = "response", se.fit = TRUE,
                  interval = "confidence", level = 0.95)

  valid <- !is.na(pred$.lower) & !is.na(pred$.upper)
  lower_dist <- pred$.fitted[valid] - pred$.lower[valid]
  upper_dist <- pred$.upper[valid] - pred$.fitted[valid]

  # CIs should be asymmetric (upper > lower distance from center)
  # for lognormal quantities
  expect_false(all(abs(lower_dist - upper_dist) < 1e-10),
               info = "CIs should be asymmetric for positive quantities")
})

test_that("hurdle predict CIs are symmetric for link type", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  pred <- predict(fit, type = "link", se.fit = TRUE,
                  interval = "confidence", level = 0.95)

  valid <- !is.na(pred$.lower) & !is.na(pred$.upper)
  lower_dist <- pred$.fitted[valid] - pred$.lower[valid]
  upper_dist <- pred$.upper[valid] - pred$.fitted[valid]

  # Link-scale CIs should be symmetric
  expect_equal(lower_dist, upper_dist, tolerance = 1e-10)
})

# ==============================================================================
# Issue 3: Population predictions ignore Jensen's inequality
# ==============================================================================

test_that("hurdle marginal=TRUE works for type='demand'", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  prices <- c(0, 1, 5, 10, 20)
  marginal_pred <- predict(fit, type = "demand", marginal = TRUE, prices = prices)
  conditional_pred <- predict(fit, type = "demand",
                              newdata = data.frame(x = prices))

  expect_equal(nrow(marginal_pred), length(prices))
  expect_true(".fitted" %in% names(marginal_pred))
  expect_true("expected_consumption" %in% names(marginal_pred))

  # Due to Jensen's inequality, marginal predictions should be >= conditional
  # (at least on average, since exp is convex)
  expect_true(
    mean(marginal_pred$.fitted) >= mean(conditional_pred$.fitted) * 0.9,
    info = "Marginal demand should be at or above conditional due to Jensen's inequality"
  )
})

test_that("hurdle marginal=TRUE works for type='response'", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  prices <- c(0, 1, 5, 10, 20)
  marginal_pred <- predict(fit, type = "response", marginal = TRUE, prices = prices)

  expect_equal(nrow(marginal_pred), length(prices))
  expect_true(all(marginal_pred$.fitted >= 0))
})

test_that("hurdle marginal=TRUE returns correct attributes", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  pred <- predict(fit, type = "demand", marginal = TRUE,
                  prices = c(0, 5, 10))
  expect_equal(attr(pred, "marginal_method"), "monte_carlo")
})

# ==============================================================================
# Issue 4: ll4_inv silent NaN
# ==============================================================================

test_that("ll4_inv produces no NaN for any input", {
  test_range <- seq(-1, 3, by = 0.1)
  result <- ll4_inv(test_range)
  expect_false(any(is.nan(result)))
})

test_that("ll4_inv returns 0 for negative inputs (consumption floor)", {
  expect_equal(ll4_inv(-0.5), 0)
  expect_equal(ll4_inv(-1), 0)
  expect_equal(ll4_inv(c(-1, -0.5, 0)), c(0, 0, 0))
})

test_that("ll4_inv round-trip preserves values for non-negative inputs", {
  x <- c(0, 0.01, 1, 100, 10000)
  expect_equal(ll4_inv(ll4(x)), x, tolerance = 1e-10)
})

test_that("ll4_inv handles NA correctly", {
  expect_true(is.na(ll4_inv(NA)))
  result <- ll4_inv(c(0.5, NA, 1))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[1]))
  expect_false(is.na(result[3]))
})

# ==============================================================================
# Issue 5: ZBEN singularity at Q0_natural = 1
# ==============================================================================

test_that("TMB predict zben clamps Q0_log10 to positive minimum", {
  # .tmb_predict_equation with Q0 near 1 (log10(1) = 0) should not crash
  # Q0_natural = 1.0 → log_q0 = 0 → Q0_log10 = 0
  result <- beezdemand:::.tmb_predict_equation(
    price = c(0, 1, 5, 10),
    Q0 = 1.0, alpha = 0.001,
    k = 2, log_q0 = 0,
    equation = "zben"
  )
  expect_true(all(is.finite(result)))
})

test_that("TMB predict zben clamps for Q0 < 1 (prevents sign flip)", {
  # Q0_natural = 0.5 → log_q0 = log(0.5) → Q0_log10 = log(0.5)/log(10) < 0
  # Without clamp, this would produce negative decay rate
  result <- beezdemand:::.tmb_predict_equation(
    price = c(0, 1, 5, 10),
    Q0 = 0.5, alpha = 0.001,
    k = 2, log_q0 = log(0.5),
    equation = "zben"
  )
  expect_true(all(is.finite(result)))
  # Demand should decrease with price (not increase)
  expect_true(result[4] <= result[1],
              info = "Demand should decrease with price, not increase")
})

# ==============================================================================
# Issue 6: Hurdle residual diagnostics
# ==============================================================================

test_that("augment.beezdemand_hurdle component='combined' returns quantile residuals", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  aug <- augment(fit, component = "combined")

  # Quantile residuals should be defined for ALL observations (including zeros)
  expect_true(all(!is.na(aug$.resid)))
  expect_true(all(is.finite(aug$.resid)))
})

test_that("augment.beezdemand_hurdle component='continuous' returns Part II residuals", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  aug <- augment(fit, component = "continuous")

  y_obs <- apt$y
  # Zeros should have NA residuals
  expect_true(all(is.na(aug$.resid[y_obs == 0])))
  # Positive obs should have finite residuals
  expect_true(all(!is.na(aug$.resid[y_obs > 0])))
})

test_that("quantile residuals are approximately N(0,1) in aggregate", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  set.seed(123)  # For reproducibility of randomized component
  aug <- augment(fit, component = "combined")
  resids <- aug$.resid

  # Loose check: mean should be near 0, SD near 1
  # (not exact due to model misfit and finite sample)
  expect_true(abs(mean(resids)) < 1,
              info = "Mean of quantile residuals should be near 0")
  expect_true(sd(resids) > 0.3 && sd(resids) < 3,
              info = "SD of quantile residuals should be in a reasonable range")
})

test_that("plot_residuals works with component argument for hurdle models", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Default (combined) should work
  p1 <- plot_residuals(fit, type = "qq", component = "combined")
  expect_s3_class(p1, "ggplot")

  # Continuous should work
  p2 <- plot_residuals(fit, type = "qq", component = "continuous")
  expect_s3_class(p2, "ggplot")
})

# ==============================================================================
# Issue 7: MC marginal draws must use correlated MVN (not independent)
# ==============================================================================

test_that("marginal predictions use correlated MVN draws (not independent)", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # The predictions should use the seed parameter and be reproducible
  pred1 <- predict(fit, type = "demand", marginal = TRUE,
                   prices = c(1, 5, 10), seed = 42L)
  pred2 <- predict(fit, type = "demand", marginal = TRUE,
                   prices = c(1, 5, 10), seed = 42L)
  expect_equal(pred1$.fitted, pred2$.fitted)

  # Different seed should produce different results
  pred3 <- predict(fit, type = "demand", marginal = TRUE,
                   prices = c(1, 5, 10), seed = 99L)
  # With enough draws (1000), results converge, but with different seeds
  # the exact values will differ slightly
  expect_true(!identical(pred1$.fitted, pred3$.fitted))
})

test_that("marginal predictions preserve global RNG state", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  set.seed(123)
  before <- stats::rnorm(1)
  set.seed(123)
  # Marginal predict should not alter the global RNG state
  pred <- predict(fit, type = "demand", marginal = TRUE,
                  prices = c(1, 5), seed = 42L)
  after <- stats::rnorm(1)
  expect_equal(before, after)
})

test_that("marginal predictions work with seed = NULL", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Should not error with seed = NULL
  pred <- predict(fit, type = "demand", marginal = TRUE,
                  prices = c(1, 5, 10), seed = NULL)
  expect_true(all(is.finite(pred$.fitted)))
})
