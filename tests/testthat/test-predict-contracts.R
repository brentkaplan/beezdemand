test_that("predict methods return a tibble with .fitted (cross-class contract)", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  # Fixed
  fit_fixed <- fit_demand_fixed(apt_small, equation = "hs", k = 2)
  pred_fixed <- predict(fit_fixed, newdata = data.frame(x = c(1, 5, 10)), type = "response")
  expect_s3_class(pred_fixed, "tbl_df")
  expect_true(".fitted" %in% names(pred_fixed))

  # NLME
  skip_if_not_installed("nlme")
  apt_small$y_ll4 <- ll4(apt_small$y)
  fit_nlme <- fit_demand_mixed(apt_small, y_var = "y_ll4", x_var = "x", id_var = "id")
  pred_nlme <- predict(fit_nlme, newdata = data.frame(x = c(1, 5, 10)), type = "response")
  expect_s3_class(pred_nlme, "tbl_df")
  expect_true(".fitted" %in% names(pred_nlme))
  expect_equal(nrow(pred_nlme), 3)

  # Hurdle
  skip_if_not_installed("TMB")
  fit_hurdle <- fit_demand_hurdle(apt_small, y_var = "y", x_var = "x", id_var = "id", verbose = 0)
  pred_hurdle <- predict(fit_hurdle, newdata = data.frame(x = c(1, 5, 10)), type = "response")
  expect_s3_class(pred_hurdle, "tbl_df")
  expect_true(".fitted" %in% names(pred_hurdle))
  expect_equal(nrow(pred_hurdle), 3)

})

test_that("predict.beezdemand_hurdle supports se.fit and confidence intervals", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  fit <- fit_demand_hurdle(apt_small, y_var = "y", x_var = "x", id_var = "id", verbose = 0)
  newdata <- data.frame(x = c(1, 5, 10))

  pred_se <- predict(fit, newdata = newdata, type = "response", se.fit = TRUE)
  expect_true(".se.fit" %in% names(pred_se))
  expect_true(all(is.finite(pred_se$.se.fit)))
  expect_true(all(pred_se$.se.fit >= 0))

  pred_ci <- predict(fit, newdata = newdata, type = "response", interval = "confidence", level = 0.95)
  expect_true(all(c(".lower", ".upper") %in% names(pred_ci)))
  expect_true(all(pred_ci$.lower <= pred_ci$.fitted))
  expect_true(all(pred_ci$.fitted <= pred_ci$.upper))
})
