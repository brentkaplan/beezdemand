# Release-correctness audit 2026-09-06 (F-BD9-4): residuals(type = "pearson")
# for a hurdle fit divided the RAW-scale response residual by the LOG-scale
# sigma_e, so the statistic changed with the consumption unit. Pearson
# residuals are now the Part-II standardized log-scale residuals
# (log(y) - mu_i) / sigma_e for positive observations and NA at zeros.

skip_if_not_installed("TMB")

test_that("hurdle Pearson residuals are unit-invariant and standardized on the log scale", {
  skip_on_cran()
  data("apt", package = "beezdemand")
  fit <- suppressWarnings(fit_demand_hurdle(apt, id_var = "id", x_var = "x",
                                            y_var = "y", verbose = 0))
  y <- fit$data[[fit$param_info$y_var]]
  rp <- residuals(fit, type = "pearson")
  expect_length(rp, length(y))
  expect_true(all(is.na(rp[y == 0])))
  sigma_e <- exp(fit$model$coefficients[["logsigma_e"]])
  mu <- predict(fit, newdata = fit$data, type = "link")$.fitted
  expect_equal(as.numeric(rp[y > 0]), as.numeric((log(y[y > 0]) - mu[y > 0]) / sigma_e),
               tolerance = 1e-8)
  # Standardized: no residual should be tens of SDs out on apt.
  expect_lt(max(abs(rp), na.rm = TRUE), 6)

  # Unit invariance: scaling consumption by 10 must leave Pearson residuals
  # unchanged up to optimizer noise (Q0 absorbs the scale).
  apt10 <- apt; apt10$y <- apt10$y * 10
  fit10 <- suppressWarnings(fit_demand_hurdle(apt10, id_var = "id", x_var = "x",
                                              y_var = "y", verbose = 0))
  rp10 <- residuals(fit10, type = "pearson")
  expect_equal(as.numeric(rp10[y > 0]), as.numeric(rp[y > 0]), tolerance = 1e-2)
})
