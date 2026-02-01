# Test model comparison framework

test_that("compare_models works with two hurdle models", {
  data(apt, package = "beezdemand")

  fit2 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  fit3 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )

  result <- compare_models(fit2, fit3)

  expect_s3_class(result, "beezdemand_model_comparison")
  expect_true("comparison" %in% names(result))
  expect_true("lrt_results" %in% names(result))
  expect_equal(nrow(result$comparison), 2)
  expect_true(all(c("Model", "logLik", "AIC", "BIC") %in% names(result$comparison)))
})

test_that("compare_models performs LRT by default for same backend", {
  data(apt, package = "beezdemand")

  fit2 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  fit3 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )

  result <- compare_models(fit2, fit3, test = "auto")

  expect_equal(result$test_type, "lrt")
  expect_true(!is.null(result$lrt_results))
  expect_true("p_value" %in% names(result$lrt_results))
})

test_that("compare_models with test = 'none' skips LRT", {
  data(apt, package = "beezdemand")

  fit2 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  fit3 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )

  result <- compare_models(fit2, fit3, test = "none")

  expect_equal(result$test_type, "none")
  expect_null(result$lrt_results)
})

test_that("compare_models identifies best model by BIC", {
  data(apt, package = "beezdemand")

  fit2 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  fit3 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )

  result <- compare_models(fit2, fit3)

  best_idx <- result$best_model
  expect_true(best_idx %in% c(1, 2))
  # Best model should have minimum BIC
  expect_equal(result$comparison$BIC[best_idx], min(result$comparison$BIC))
})

test_that("compare_models prints without error", {
  data(apt, package = "beezdemand")

  fit2 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  fit3 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )

  result <- compare_models(fit2, fit3)

  expect_output(print(result), "Model Comparison")
  expect_output(print(result), "Likelihood Ratio Tests")
})

test_that("compare_models errors with fewer than 2 models", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  expect_error(compare_models(fit), "At least two models")
})

test_that("compare_models errors with invalid model class", {
  expect_error(
    compare_models(list(a = 1), list(b = 2)),
    "not a recognized beezdemand model"
  )
})

test_that("anova.beezdemand_hurdle works", {
  data(apt, package = "beezdemand")

  fit2 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  fit3 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )

  result <- anova(fit2, fit3)

  expect_s3_class(result, "anova.beezdemand_hurdle")
  expect_true("table" %in% names(result))
  expect_true("lrt" %in% names(result))
  expect_equal(nrow(result$table), 2)
})

test_that("anova.beezdemand_hurdle prints without error", {
  data(apt, package = "beezdemand")

  fit2 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  fit3 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )

  result <- anova(fit2, fit3)

  expect_output(print(result), "Analysis of Variance Table")
})

test_that("anova.beezdemand_hurdle errors with non-hurdle model", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  expect_error(
    anova(fit, list(a = 1)),
    "not a beezdemand_hurdle object"
  )
})

test_that("anova.beezdemand_nlme dispatches correctly", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  # Fit a single NLME model
  fit1 <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  skip_if(is.null(fit1$model), "Model fitting failed")

  # Test that anova method exists and errors appropriately with single model
  expect_error(
    anova(fit1),
    "At least two models"
  )

  # Test that method validates model class
  expect_error(
    anova(fit1, list(a = 1)),
    "not a beezdemand_nlme object"
  )
})

test_that("compare_models computes delta IC columns", {
  data(apt, package = "beezdemand")

  fit2 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  fit3 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"),
    verbose = 0
  )

  result <- compare_models(fit2, fit3)

  expect_true("delta_AIC" %in% names(result$comparison))
  expect_true("delta_BIC" %in% names(result$comparison))
  # Minimum delta should be 0
  expect_equal(min(result$comparison$delta_AIC), 0)
  expect_equal(min(result$comparison$delta_BIC), 0)
})
