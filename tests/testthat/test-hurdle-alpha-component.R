test_that("hurdle alpha is classified as consumption for both naming variants", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  fit <- fit_demand_hurdle(
    apt_small,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    verbose = 0
  )

  t1 <- tidy(fit, report_space = "natural")
  expect_true(any(t1$term == "alpha"))
  expect_true(all(t1$component[t1$term == "alpha"] == "consumption"))

  # Simulate older naming variant where the coefficient is stored as `alpha`
  # instead of `log_alpha`.
  fit2 <- fit
  fit2$model$coefficients[["alpha"]] <- fit2$model$coefficients[["log_alpha"]]
  fit2$model$coefficients <- fit2$model$coefficients[
    names(fit2$model$coefficients) != "log_alpha"
  ]
  fit2$model$se[["alpha"]] <- fit2$model$se[["log_alpha"]]
  fit2$model$se <- fit2$model$se[names(fit2$model$se) != "log_alpha"]

  t2 <- tidy(fit2, report_space = "natural")
  expect_true(any(t2$term == "alpha"))
  expect_true(all(t2$component[t2$term == "alpha"] == "consumption"))

  s2 <- suppressWarnings(summary(fit2, report_space = "natural"))
  expect_true(any(s2$coefficients$term == "alpha"))
  expect_true(all(s2$coefficients$component[s2$coefficients$term == "alpha"] == "consumption"))
})
