# TICKET-003: tests for the conditional + unconditional Pmax/Omax pair on
# hurdle models. The unconditional metrics integrate the Part-I logistic
# zero-inflation into the expenditure curve so plot_expenditure() and the
# Pmax/Omax reference lines align.

# --- Engine: p_zero_fn parameter --------------------------------------------

test_that("beezdemand_calc_pmax_omax with p_zero_fn populates unconditional fields", {
  result <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = 0.001, q0 = 10, k = 3),
    p_zero_fn = function(p) plogis(-2 + 0.5 * log(p + 0.001)),
    price_obs = c(0, 0.5, 1, 2, 4, 8, 16)
  )
  expect_true(is.finite(result$pmax_unconditional))
  expect_true(is.finite(result$omax_unconditional))
  expect_true(is.finite(result$p_zero_at_pmax))
  expect_equal(result$method_unconditional, "numerical_optimize_observed_domain")
})

test_that("beezdemand_calc_pmax_omax without p_zero_fn leaves unconditional fields NA", {
  result <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = 0.001, q0 = 10, k = 3),
    price_obs = c(0, 0.5, 1, 2, 4, 8, 16)
  )
  expect_true(is.na(result$pmax_unconditional))
  expect_true(is.na(result$omax_unconditional))
  expect_true(is.na(result$p_zero_at_pmax))
  # Conditional fields untouched
  expect_true(is.finite(result$pmax_model))
})

test_that("beezdemand_calc_pmax_omax: increasing P0(price) shifts unconditional Omax down", {
  cond <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = 0.001, q0 = 10, k = 3),
    price_obs = c(0.5, 1, 2, 4, 8, 16, 32)
  )
  uncond <- beezdemand_calc_pmax_omax(
    model_type = "hs",
    params = list(alpha = 0.001, q0 = 10, k = 3),
    p_zero_fn = function(p) plogis(-1 + 1.0 * log(p + 0.001)),
    price_obs = c(0.5, 1, 2, 4, 8, 16, 32)
  )
  # Adding a (1 - P0) factor that decreases with price suppresses expenditure
  # everywhere relative to the conditional curve, so unconditional Omax is
  # at most the conditional one.
  expect_lt(uncond$omax_unconditional, cond$omax_model)
})

# --- Hurdle group_metrics ---------------------------------------------------

test_that("calc_group_metrics.beezdemand_hurdle returns both metric sets", {
  skip_on_cran()
  data("apt", package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                           verbose = 0)
  metrics <- calc_group_metrics(fit)

  expect_true(all(c("Pmax", "Omax", "Pmax_unconditional", "Omax_unconditional",
                    "p_zero_at_pmax", "method_unconditional") %in% names(metrics)))
  expect_true(is.finite(metrics$Pmax))
  expect_true(is.finite(metrics$Pmax_unconditional))
  expect_true(is.finite(metrics$Omax))
  expect_true(is.finite(metrics$Omax_unconditional))
})

# --- Subject-level pars -----------------------------------------------------

test_that("hurdle subject_pars includes Pmax_unconditional and Omax_unconditional", {
  skip_on_cran()
  data("apt", package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                           verbose = 0)

  expect_true("Pmax_unconditional" %in% names(fit$subject_pars))
  expect_true("Omax_unconditional" %in% names(fit$subject_pars))
  # Existing conditional columns untouched
  expect_true("Pmax" %in% names(fit$subject_pars))
  expect_true("Omax" %in% names(fit$subject_pars))
})

# --- Summary derived_metrics ------------------------------------------------

test_that("summary.beezdemand_hurdle reports both conditional and unconditional metrics", {
  skip_on_cran()
  data("apt", package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                           verbose = 0)
  s <- summary(fit)
  dm <- s$derived_metrics

  expect_true("pmax_model" %in% dm$metric)
  expect_true("omax_model" %in% dm$metric)
  expect_true("pmax_unconditional" %in% dm$metric)
  expect_true("omax_unconditional" %in% dm$metric)
  expect_true("p_zero_at_pmax" %in% dm$metric)

  expect_true("unconditional" %in% dm$component)
})

# --- plot_expenditure demand_type --------------------------------------------

test_that("plot_expenditure.beezdemand_hurdle accepts demand_type argument", {
  skip_on_cran()
  data("apt", package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                           verbose = 0)
  p_uncond <- plot_expenditure(fit, demand_type = "unconditional")
  p_cond <- plot_expenditure(fit, demand_type = "conditional")
  expect_s3_class(p_uncond, "ggplot")
  expect_s3_class(p_cond, "ggplot")
})

test_that("plot_expenditure rejects unknown demand_type", {
  skip_on_cran()
  data("apt", package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                           verbose = 0)
  expect_error(plot_expenditure(fit, demand_type = "fancy"))
})
