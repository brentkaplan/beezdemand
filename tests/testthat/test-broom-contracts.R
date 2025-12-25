# Test tidy()/glance() contract compliance for all model classes

test_that("tidy.beezdemand_hurdle meets contract", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  fit <- tryCatch(
    fit_demand_hurdle(apt_small, y_var = "y", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  t <- tidy(fit)

  expect_s3_class(t, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value") %in%
                    names(t)))
  expect_true("component" %in% names(t))
})


test_that("glance.beezdemand_hurdle meets contract", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  fit <- tryCatch(
    fit_demand_hurdle(apt_small, y_var = "y", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  g <- glance(fit)

  expect_s3_class(g, "tbl_df")
  expect_equal(nrow(g), 1)
  expect_true(all(c("model_class", "backend", "nobs", "n_subjects",
                    "converged", "logLik", "AIC", "BIC") %in% names(g)))
  expect_equal(g$model_class, "beezdemand_hurdle")
  expect_equal(g$backend, "TMB")
})


test_that("tidy.beezdemand_cp_hurdle meets contract", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  set.seed(123)
  n_subj <- 5
  n_obs <- 8
  cp_data <- data.frame(
    id = rep(1:n_subj, each = n_obs),
    x = rep(seq(0.1, 10, length.out = n_obs), n_subj),
    y = rpois(n_subj * n_obs, lambda = 10)
  )

  fit <- tryCatch(fit_cp_hurdle(cp_data), error = function(e) NULL)
  skip_if(is.null(fit), "Model fitting failed")

  t <- tidy(fit)

  expect_s3_class(t, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value") %in%
                    names(t)))
  expect_true("component" %in% names(t))
})


test_that("glance.beezdemand_cp_hurdle meets contract", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  set.seed(123)
  n_subj <- 5
  n_obs <- 8
  cp_data <- data.frame(
    id = rep(1:n_subj, each = n_obs),
    x = rep(seq(0.1, 10, length.out = n_obs), n_subj),
    y = rpois(n_subj * n_obs, lambda = 10)
  )

  fit <- tryCatch(fit_cp_hurdle(cp_data), error = function(e) NULL)
  skip_if(is.null(fit), "Model fitting failed")

  g <- glance(fit)

  expect_s3_class(g, "tbl_df")
  expect_equal(nrow(g), 1)
  expect_true(all(c("model_class", "backend") %in% names(g)))
  expect_equal(g$model_class, "beezdemand_cp_hurdle")
  expect_equal(g$backend, "TMB")
})


test_that("tidy.beezdemand_nlme meets contract", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]
  apt_small$y_ll4 <- ll4(apt_small$y)

  fit <- tryCatch(
    fit_demand_mixed(apt_small, y_var = "y_ll4", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model fitting failed")

  t <- tidy(fit)

  expect_s3_class(t, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error") %in% names(t)))
})


test_that("glance.beezdemand_nlme meets contract", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]
  apt_small$y_ll4 <- ll4(apt_small$y)

  fit <- tryCatch(
    fit_demand_mixed(apt_small, y_var = "y_ll4", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model fitting failed")

  g <- glance(fit)

  expect_s3_class(g, "tbl_df")
  expect_equal(nrow(g), 1)
  expect_true(all(c("model_class", "backend") %in% names(g)))
  expect_equal(g$model_class, "beezdemand_nlme")
  expect_equal(g$backend, "nlme")
})


test_that("tidy.beezdemand_fixed meets contract", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)
  t <- tidy(fit)

  expect_s3_class(t, "tbl_df")
  expect_true(all(c("term", "estimate") %in% names(t)))
  expect_true("id" %in% names(t))
})


test_that("glance.beezdemand_fixed meets contract", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)
  g <- glance(fit)

  expect_s3_class(g, "tbl_df")
  expect_equal(nrow(g), 1)
  expect_true(all(c("model_class", "backend") %in% names(g)))
  expect_equal(g$model_class, "beezdemand_fixed")
  expect_equal(g$backend, "legacy")
})


test_that("tidy.beezdemand_systematicity meets contract", {
  data(apt, package = "beezdemand")

  check <- check_systematic_demand(apt)
  t <- tidy(check)

  expect_s3_class(t, "tbl_df")
  expect_true("type" %in% names(t))
  expect_true(all(t$type == "demand"))
  expect_true("systematic" %in% names(t))
})


test_that("glance.beezdemand_systematicity meets contract", {
  data(apt, package = "beezdemand")

  check <- check_systematic_demand(apt)
  g <- glance(check)

  expect_s3_class(g, "tbl_df")
  expect_equal(nrow(g), 1)
  expect_true("pct_systematic" %in% names(g))
  expect_equal(g$type, "demand")
})


test_that("demand and cp systematicity wrappers have identical column names", {
  data(apt, package = "beezdemand")
  demand_check <- check_systematic_demand(apt)

  # Create simple CP-like data
  cp_data <- data.frame(
    id = rep(1:3, each = 5),
    x = rep(c(0.1, 1, 2, 5, 10), 3),
    y = c(10, 8, 6, 3, 1, 10, 9, 7, 4, 2, 10, 5, 8, 2, 0)
  )
  cp_check <- check_systematic_cp(cp_data)

  demand_cols <- names(tidy(demand_check))
  cp_cols <- names(tidy(cp_check))

  expect_identical(demand_cols, cp_cols)
})
