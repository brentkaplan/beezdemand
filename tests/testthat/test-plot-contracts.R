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

test_that("plot.beezdemand_cp_hurdle returns ggplot", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  data(etm, package = "beezdemand")
  etm_small <- etm[etm$id %in% unique(etm$id)[1:5], ]
  fit <- fit_cp_hurdle(etm_small)

  expect_s3_class(plot(fit, type = "demand"), "ggplot")
  expect_s3_class(plot(fit, type = "individual", ids = unique(etm_small$id)[1:2]), "ggplot")
})

test_that("plot.beezdemand_joint_hurdle returns ggplot", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  # Create minimal joint data
  set.seed(42)
  prices <- c(0.5, 1, 2, 4)
  ids <- 1:3
  data_list <- list()
  for (i in ids) {
    for (p in prices) {
      data_list[[length(data_list) + 1]] <- data.frame(id = i, x = p, y = exp(3 - 0.1 * p), target = "alone")
      data_list[[length(data_list) + 1]] <- data.frame(id = i, x = p, y = exp(2.8 - 0.12 * p), target = "own")
      data_list[[length(data_list) + 1]] <- data.frame(id = i, x = p, y = exp(2 - 0.05 * p), target = "alt")
    }
  }
  joint_data <- do.call(rbind, data_list)

  fit <- suppressWarnings(
    fit_joint_hurdle(
      data = joint_data,
      k = 2,
      verbose = 0,
      control = list(eval.max = 10, iter.max = 10) # Very fast for smoke test
    )
  )

  expect_s3_class(plot(fit, type = "demand"), "ggplot")
  expect_s3_class(plot(fit, type = "probability"), "ggplot")
})
