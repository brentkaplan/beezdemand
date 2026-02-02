# Test model diagnostics suite

test_that("check_demand_model.beezdemand_hurdle returns expected structure", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_s3_class(diag, "beezdemand_diagnostics")
  expect_true("convergence" %in% names(diag))
  expect_true("boundary" %in% names(diag))
  expect_true("residuals" %in% names(diag))
  expect_true("random_effects" %in% names(diag))
  expect_true("issues" %in% names(diag))
  expect_true("recommendations" %in% names(diag))
  expect_equal(diag$model_class, "beezdemand_hurdle")
})

test_that("check_demand_model.beezdemand_hurdle detects convergence", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  # Model should converge on this data
  expect_true(diag$convergence$converged)
})

test_that("check_demand_model.beezdemand_hurdle provides residual stats", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_true(!is.na(diag$residuals$mean))
  expect_true(!is.na(diag$residuals$sd))
  expect_true(is.numeric(diag$residuals$n_outliers))
})

test_that("check_demand_model.beezdemand_nlme returns expected structure", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  skip_if(is.null(fit$model), "Model fitting failed")

  diag <- check_demand_model(fit)

  expect_s3_class(diag, "beezdemand_diagnostics")
  expect_equal(diag$model_class, "beezdemand_nlme")
  expect_true("convergence" %in% names(diag))
  expect_true("random_effects" %in% names(diag))
})

test_that("check_demand_model.beezdemand_fixed returns expected structure", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_fixed(
    apt, y_var = "y", x_var = "x", id_var = "id"
  )

  diag <- check_demand_model(fit)

  expect_s3_class(diag, "beezdemand_diagnostics")
  expect_equal(diag$model_class, "beezdemand_fixed")
  expect_true("convergence" %in% names(diag))
  expect_true(is.numeric(diag$convergence$n_total))
  expect_true(is.numeric(diag$convergence$n_failed))
})

test_that("print.beezdemand_diagnostics works without error", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_output(print(diag), "Model Diagnostics")
  expect_output(print(diag), "Convergence")
  expect_output(print(diag), "Residuals")
})

test_that("plot_residuals works for hurdle models", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Test single plot types
  p_fitted <- plot_residuals(fit, type = "fitted")
  expect_s3_class(p_fitted, "ggplot")

  p_hist <- plot_residuals(fit, type = "histogram")
  expect_s3_class(p_hist, "ggplot")

  p_qq <- plot_residuals(fit, type = "qq")
  expect_s3_class(p_qq, "ggplot")
})

test_that("plot_residuals type='all' returns list of plots", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  plots <- plot_residuals(fit, type = "all")

  expect_type(plots, "list")
  expect_true("fitted" %in% names(plots))
  expect_true("histogram" %in% names(plots))
  expect_true("qq" %in% names(plots))
})

test_that("plot_qq.beezdemand_hurdle works", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  p <- plot_qq(fit)
  expect_s3_class(p, "ggplot")
})

test_that("plot_qq.beezdemand_hurdle with specific effects", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Should work with specific effect
  p <- plot_qq(fit, which = "Q0")
  expect_s3_class(p, "ggplot")
})

test_that("plot_qq.beezdemand_nlme works", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  skip_if(is.null(fit$model), "Model fitting failed")

  p <- plot_qq(fit)
  expect_s3_class(p, "ggplot")
})

test_that("check_demand_model reports n_issues correctly", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_equal(diag$n_issues, length(diag$issues))
})

test_that("plot_residuals works for nlme models", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  skip_if(is.null(fit$model), "Model fitting failed")

  p <- plot_residuals(fit, type = "fitted")
  expect_s3_class(p, "ggplot")
})

test_that("plot_residuals works for fixed models", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_fixed(
    apt, y_var = "y", x_var = "x", id_var = "id"
  )

  p <- plot_residuals(fit, type = "fitted")
  expect_s3_class(p, "ggplot")
})
