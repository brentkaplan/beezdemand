test_that("predict.beezdemand_fixed returns consistent schema", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small, equation = "hs", k = 2)
  newdata <- data.frame(x = c(0, 1, 5, 10))

  pred <- predict(fit, newdata = newdata, type = "response")

  expect_s3_class(pred, "tbl_df")
  expect_true(".fitted" %in% names(pred))
  expect_true("id" %in% names(pred))

  n_subjects <- length(unique(apt_small$id))
  expect_equal(nrow(pred), nrow(newdata) * n_subjects)
  expect_true(all(is.finite(pred$.fitted) | is.na(pred$.fitted)))
  expect_true(all(pred$.fitted[!is.na(pred$.fitted)] >= 0))
})

test_that("predict.beezdemand_fixed works with id column (subject-specific)", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small, equation = "hs", k = 2)
  id_one <- unique(apt_small$id)[1]
  newdata <- data.frame(x = c(1, 5, 10), id = id_one)

  pred <- predict(fit, newdata = newdata, type = "response")

  expect_equal(nrow(pred), 3)
  expect_true(all(pred$id == id_one))
})

test_that("predict.beezdemand_fixed link and response are consistent", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:2], ]

  fit <- fit_demand_fixed(apt_small, equation = "hs", k = 2)
  newdata <- data.frame(x = c(0, 1, 5))

  pred_resp <- predict(fit, newdata = newdata, type = "response")
  pred_link <- predict(fit, newdata = newdata, type = "link")

  expect_equal(nrow(pred_resp), nrow(pred_link))
  expect_equal(log10(pred_resp$.fitted), pred_link$.fitted, tolerance = 1e-6)
})

test_that("predict.beezdemand_fixed errors on missing price column", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small, equation = "hs", k = 2)
  newdata <- data.frame(wrong_col = c(1, 2, 3))

  expect_error(predict(fit, newdata = newdata), regexp = "x|price")
})

test_that("predict.beezdemand_fixed handles k variability correctly", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit_ind_k <- fit_demand_fixed(apt_small, equation = "hs", k = "ind")

  newdata_no_id <- data.frame(x = c(1, 5))
  expect_error(predict(fit_ind_k, newdata = newdata_no_id), regexp = "k|id|subject")

  id_one <- unique(apt_small$id)[1]
  newdata_with_id <- data.frame(x = c(1, 5), id = id_one)
  expect_silent(predict(fit_ind_k, newdata = newdata_with_id))
})

test_that("predict.beezdemand_fixed returns NA uncertainty when vcov unavailable", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small, equation = "hs", k = 2)
  newdata <- data.frame(x = c(1, 5))

  expect_warning(
    predict(fit, newdata = newdata, se.fit = TRUE),
    regexp = "vcov|Standard errors|interval"
  )
  pred <- suppressWarnings(predict(fit, newdata = newdata, se.fit = TRUE))

  expect_true(".se.fit" %in% names(pred))
  expect_true(all(is.na(pred$.se.fit)))

  expect_warning(
    predict(fit, newdata = newdata, interval = "confidence"),
    regexp = "vcov|interval"
  )
  pred_int <- suppressWarnings(predict(fit, newdata = newdata, interval = "confidence"))
  expect_true(all(c(".lower", ".upper") %in% names(pred_int)))
  expect_true(all(is.na(pred_int$.lower)))
  expect_true(all(is.na(pred_int$.upper)))
})
