# Test exponentiated (Koffarnus) equation support in fit_demand_mixed()

test_that("fit_demand_mixed() accepts equation_form = 'exponentiated'", {
  data(apt, package = "beezdemand")

  # Should run without error with exponentiated equation
  expect_no_error({
    fit <- fit_demand_mixed(
      data = apt,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      equation_form = "exponentiated",
      k = 2
    )
  })
})

test_that("exponentiated equation with log10 param_space produces valid results", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_mixed(
    data = apt,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    equation_form = "exponentiated",
    param_space = "log10",
    k = 2
  )

  expect_s3_class(fit, "beezdemand_nlme")
  expect_false(is.null(fit$model))
  expect_equal(fit$formula_details$equation_form_selected, "exponentiated")
  expect_equal(fit$param_info$k, 2)

  # Check coefficients are extracted
  coefs <- coef(fit)
  expect_true("Q0.(Intercept)" %in% names(coefs) || "Q0" %in% names(coefs))
  expect_true("alpha.(Intercept)" %in% names(coefs) || "alpha" %in% names(coefs))
})

test_that("exponentiated equation with natural param_space produces valid results", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_mixed(
    data = apt,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    equation_form = "exponentiated",
    param_space = "natural",
    k = 2
  )

  expect_s3_class(fit, "beezdemand_nlme")
  expect_false(is.null(fit$model))
  expect_equal(fit$param_info$param_space, "natural")
  expect_equal(fit$param_info$k, 2)
})

test_that("exponentiated equation calculates k automatically when not provided", {
  data(apt, package = "beezdemand")

  expect_message(
    fit <- fit_demand_mixed(
      data = apt,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      equation_form = "exponentiated",
      k = NULL
    ),
    "Using calculated k"
  )

  expect_s3_class(fit, "beezdemand_nlme")
  expect_true(!is.null(fit$param_info$k))
  expect_true(is.numeric(fit$param_info$k))
  expect_true(fit$param_info$k > 0)
})

test_that("exponentiated equation formula is correctly constructed", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_mixed(
    data = apt,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    equation_form = "exponentiated",
    param_space = "log10",
    k = 2.5
  )

  # Check formula contains the expected exponentiated pattern
  formula_str <- deparse(fit$formula_details$nlme_model_formula_obj)
  expect_true(grepl("10\\^\\(", paste(formula_str, collapse = "")))
  expect_true(grepl("2\\.5", paste(formula_str, collapse = "")))
})

test_that("predict works with exponentiated equation models", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_mixed(
    data = apt,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    equation_form = "exponentiated",
    k = 2
  )

  # Predict should work
  preds <- predict(fit)
  expect_true(is.data.frame(preds) || is.numeric(preds))
})

test_that("summary works with exponentiated equation models", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_mixed(
    data = apt,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    equation_form = "exponentiated",
    k = 2
  )

  # Summary should produce output without error
  expect_no_error({
    summ <- summary(fit)
  })
})

test_that("exponentiated equation works with factors", {
  data(apt, package = "beezdemand")

  # Create a simple grouping factor
  apt$group <- rep(c("A", "B"), length.out = nrow(apt))

  fit <- fit_demand_mixed(
    data = apt,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    factors = "group",
    equation_form = "exponentiated",
    k = 2
  )

  expect_s3_class(fit, "beezdemand_nlme")
  expect_false(is.null(fit$model))
})
