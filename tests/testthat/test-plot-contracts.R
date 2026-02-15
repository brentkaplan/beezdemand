# Plot method smoke tests

test_that("plot.beezdemand_fixed returns ggplot for individual fits", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small, equation = "koff", k = 2)
  p <- plot(fit, type = "individual", ids = unique(apt_small$id)[1:2])

  expect_s3_class(p, "ggplot")
})

test_that("plot.beezdemand_fixed returns ggplot for aggregated fits", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_fixed(apt, equation = "koff", k = 2, agg = "Mean")

  p <- plot(fit, type = "population")
  expect_s3_class(p, "ggplot")
})

test_that("plot.beezdemand_nlme returns ggplot", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]
  apt_small$y_ll4 <- ll4(apt_small$y)

  fit <- tryCatch(
    fit_demand_mixed(apt_small, y_var = "y_ll4", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  p <- plot(fit, type = "population")
  expect_s3_class(p, "ggplot")
})

