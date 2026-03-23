# Test suite for broom contract compliance of beezdemand_tmb

test_that("tidy.beezdemand_tmb returns proper tibble", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  td <- tidy(fit)
  expect_s3_class(td, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value") %in% names(td)))
  expect_true(nrow(td) > 0)
  expect_true(all(!is.na(td$estimate)))
})

test_that("tidy report_space argument works", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  td_int <- tidy(fit, report_space = "internal")
  td_nat <- tidy(fit, report_space = "natural")

  expect_s3_class(td_int, "tbl_df")
  expect_s3_class(td_nat, "tbl_df")
  # Same number of rows
  expect_equal(nrow(td_int), nrow(td_nat))
})

test_that("glance.beezdemand_tmb returns one-row tibble", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  gl <- glance(fit)
  expect_s3_class(gl, "tbl_df")
  expect_equal(nrow(gl), 1)
  expect_true(all(c("model_class", "backend", "equation", "nobs", "n_subjects",
                     "converged", "logLik", "AIC", "BIC") %in% names(gl)))
  expect_equal(gl$model_class, "beezdemand_tmb")
  expect_equal(gl$backend, "TMB_mixed")
  expect_equal(gl$equation, "exponential")
})

test_that("augment.beezdemand_tmb returns proper tibble", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  aug <- augment(fit)
  expect_s3_class(aug, "tbl_df")
  expect_true(all(c(".fitted", ".resid") %in% names(aug)))
  expect_equal(nrow(aug), nrow(fit$data))
})

test_that("augment works with newdata", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  # Use subset of original data as newdata
  newdata <- fit$data[1:10, ]
  aug <- augment(fit, newdata = newdata)
  expect_equal(nrow(aug), 10)
  expect_true(all(c(".fitted", ".resid") %in% names(aug)))
})

test_that("broom methods work for simplified equation", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "simplified", verbose = 0
  )

  expect_s3_class(tidy(fit), "tbl_df")
  expect_s3_class(glance(fit), "tbl_df")
  expect_s3_class(augment(fit), "tbl_df")
})

test_that("broom methods work for zben equation", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_tmb(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", verbose = 0
  )

  expect_s3_class(tidy(fit), "tbl_df")
  expect_s3_class(glance(fit), "tbl_df")
  expect_s3_class(augment(fit), "tbl_df")
})

# --- Augment residual correctness (regression tests for Bug 1) ---

test_that("augment exponential residuals are on log scale", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  aug <- augment(fit)

  # .resid should be log(y) - .fitted (both on log scale)
  # So residuals should be moderate in magnitude, not huge cross-scale values
  resids <- aug$.resid[!is.na(aug$.resid)]
  expect_true(all(is.finite(resids)))
  # Log-scale residuals should typically be < 5 in magnitude
  expect_true(all(abs(resids) < 10),
              info = "Residuals appear cross-scale; expected log-scale values")
})

test_that("augment includes .std_resid (Pearson residuals)", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  aug <- augment(fit)
  expect_true(".std_resid" %in% names(aug))
  std_resids <- aug$.std_resid[!is.na(aug$.std_resid)]
  expect_true(length(std_resids) > 0)
  expect_true(all(is.finite(std_resids)))
})

test_that("augment residuals are correct for simplified equation", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "simplified", verbose = 0
  )

  aug <- augment(fit)
  # simplified: y and fitted on same (natural) scale
  resids <- aug$.resid[!is.na(aug$.resid)]
  expect_true(all(is.finite(resids)))
  expect_true(".std_resid" %in% names(aug))
})

test_that("augment residuals are correct for zben equation", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_tmb(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", verbose = 0
  )

  aug <- augment(fit)
  # zben: y_ll4 and fitted on same (LL4) scale
  resids <- aug$.resid[!is.na(aug$.resid)]
  expect_true(all(is.finite(resids)))
  expect_true(".std_resid" %in% names(aug))
})

test_that("augment exponential handles data with zeros without -Inf", {
  data(apt, package = "beezdemand")
  # Ensure data contains zeros (apt typically does)
  has_zeros <- any(apt$y == 0)

  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  # augment with original data (which may retain zeros in stored data)
  aug <- augment(fit)

  # No -Inf residuals should be present
  expect_true(all(aug$.resid[!is.na(aug$.resid)] > -Inf),
              info = "Residuals should not be -Inf for zero observations")
  # Zero-consumption observations should have NA residuals
  y_obs <- fit$data[[fit$param_info$y_var]]
  if (any(y_obs == 0)) {
    expect_true(all(is.na(aug$.resid[y_obs == 0])),
                info = "Zero observations should have NA residuals for exponential")
  }
})
