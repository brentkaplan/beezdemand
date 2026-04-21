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
  expect_true("nesting_verified" %in% names(result))
  expect_false(result$nesting_verified)
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

test_that("compare_models warns when models use different sample sizes", {
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

  fit3$data <- fit3$data[-1, , drop = FALSE]

  expect_warning(
    suppressMessages(compare_models(fit2, fit3, test = "lrt")),
    regexp = "different sample sizes|identical data"
  )
})

test_that("compare_models warns and returns NA p-value for negative LR statistics", {
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

  df2 <- length(fit2$model$coefficients)
  df3 <- length(fit3$model$coefficients)

  # Ensure a strict df ordering so LRT is attempted between reduced and full.
  if (df2 == df3) {
    fit3$model$coefficients <- c(fit3$model$coefficients, extra = 0)
    df3 <- df3 + 1
  }

  # Force the higher-df ("full") model to have a smaller logLik -> negative LR.
  if (df2 < df3) {
    fit3$loglik <- fit2$loglik - 1
  } else {
    fit2$loglik <- fit3$loglik - 1
  }

  result <- NULL
  expect_warning(
    result <- suppressMessages(compare_models(fit2, fit3, test = "lrt")),
    regexp = "Negative LR statistic"
  )

  expect_true(result$lrt_results$LR_stat[1] < 0)
  expect_true(is.na(result$lrt_results$p_value[1]))
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

test_that(".get_model_info handles NULL nlme model gracefully", {
  # Simulate a failed nlme fit (model = NULL)
  fake_nlme <- structure(list(
    model = NULL,
    data = data.frame(id = 1:10, x = 1:10, y = 1:10)
  ), class = "beezdemand_nlme")

  info <- beezdemand:::.get_model_info(fake_nlme, name = "test")

  expect_equal(info$backend, "nlme")
  expect_equal(info$nobs, 10L)  # Falls back to model$data
  expect_true(is.na(info$df))
  expect_true(is.na(info$logLik))
  expect_true(is.na(info$AIC))
  expect_true(is.na(info$BIC))
})

test_that("compare_models cross-backend returns descriptive comparison", {
  data(apt, package = "beezdemand")

  fit_fixed <- fit_demand_fixed(
    apt, y_var = "y", x_var = "x", id_var = "id"
  )
  fit_hurdle <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  result <- compare_models(fit_fixed, fit_hurdle)

  expect_s3_class(result, "beezdemand_model_comparison")
  expect_true(is.na(result$best_model))
  expect_true(all(is.na(result$comparison$delta_AIC)))
  expect_true(all(is.na(result$comparison$delta_BIC)))
  expect_true(any(grepl("not comparable across modeling frameworks", result$notes)))
})

test_that("compare_models same-backend still works with delta columns", {
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

  expect_true(!is.na(result$best_model))
  expect_equal(min(result$comparison$delta_BIC), 0)
})

test_that("compare_models works with legacy fixed models (IC unavailable)", {
  data(apt, package = "beezdemand")

  fit_hs <- fit_demand_fixed(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "hs"
  )
  fit_linear <- fit_demand_fixed(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "linear"
  )

  result <- compare_models(fit_hs, fit_linear)

  expect_s3_class(result, "beezdemand_model_comparison")
  expect_equal(result$test_type, "none")
  expect_true(is.na(result$best_model))
  expect_true(all(is.na(result$comparison$AIC)))
  expect_true(all(is.na(result$comparison$BIC)))
  expect_true(all(is.na(result$comparison$delta_AIC)))
  expect_true(all(is.na(result$comparison$delta_BIC)))
})
