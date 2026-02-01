# Test augment() methods for all model classes

test_that("augment.beezdemand_hurdle returns expected columns", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  aug <- augment(fit)

  expect_s3_class(aug, "tbl_df")
  expect_true(".fitted" %in% names(aug))
  expect_true(".fitted_link" %in% names(aug))
  expect_true(".fitted_prob" %in% names(aug))
  expect_true(".resid" %in% names(aug))
  expect_true(".resid_response" %in% names(aug))

  # Original data columns should be preserved
  expect_true("y" %in% names(aug))
  expect_true("x" %in% names(aug))
  expect_true("id" %in% names(aug))

  # Same number of rows as original data
  expect_equal(nrow(aug), nrow(apt))
})

test_that("augment.beezdemand_hurdle residuals are NA for zeros",
{
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  aug <- augment(fit)

  # Residuals should be NA where y == 0
  zeros_idx <- apt$y == 0
  expect_true(all(is.na(aug$.resid[zeros_idx])))

  # Residuals should be non-NA where y > 0
  nonzeros_idx <- apt$y > 0
  expect_true(all(!is.na(aug$.resid[nonzeros_idx])))
})

test_that("augment.beezdemand_nlme returns expected columns", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  aug <- augment(fit)

  expect_s3_class(aug, "tbl_df")
  expect_true(".fitted" %in% names(aug))
  expect_true(".resid" %in% names(aug))
  expect_true(".fixed" %in% names(aug))

  # Original data columns should be preserved
  expect_true("y_ll4" %in% names(aug))
  expect_true("x" %in% names(aug))
  expect_true("id" %in% names(aug))
})

test_that("augment.beezdemand_nlme fitted + resid equals observed", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  aug <- augment(fit)

  # Fitted + resid should approximately equal observed
  reconstructed <- aug$.fitted + aug$.resid
  expect_equal(reconstructed, aug$y_ll4, tolerance = 1e-10)
})

test_that("augment.beezdemand_nlme respects newdata", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  skip_if(is.null(fit$model), "Model fitting failed")

  newdata <- apt[seq_len(min(50, nrow(apt))), , drop = FALSE]

  aug <- augment(fit, newdata = newdata)

  expect_equal(nrow(aug), nrow(newdata))
  expect_true(all(c(".fitted", ".resid", ".fixed") %in% names(aug)))

  pred_ind <- predict(fit, newdata = newdata, type = "individual")
  pred_pop <- predict(fit, newdata = newdata, type = "population")

  expect_equal(aug$.fitted, pred_ind$.fitted, tolerance = 1e-10)
  expect_equal(aug$.fixed, pred_pop$.fitted, tolerance = 1e-10)
  expect_equal(aug$.resid, aug$y_ll4 - aug$.fitted, tolerance = 1e-10)
})

test_that("augment.beezdemand_fixed returns expected columns", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_fixed(
    apt, y_var = "y", x_var = "x", id_var = "id"
  )

  aug <- augment(fit)

  expect_s3_class(aug, "tbl_df")
  expect_true(".fitted" %in% names(aug))
  expect_true(".resid" %in% names(aug))

  # Original data columns should be preserved
  expect_true("y" %in% names(aug))
  expect_true("x" %in% names(aug))
  expect_true("id" %in% names(aug))
})

test_that("augment works with broom generic", {
  data(apt, package = "beezdemand")

  fit_hurdle <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Should dispatch to the correct method via broom::augment
  aug <- broom::augment(fit_hurdle)
  expect_s3_class(aug, "tbl_df")
  expect_true(".fitted" %in% names(aug))
})

test_that("augment error handling works", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Clear data to test error
  fit_no_data <- fit
  fit_no_data$data <- NULL

  expect_error(
    augment(fit_no_data),
    "No data available"
  )
})
