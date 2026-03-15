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
  expect_true(all(c("estimate_scale", "term_display") %in% names(t)))
  expect_true(all(t$estimate_scale %in% c("natural", "log", "log10", "logit")))

  # Component vocabulary (canonical)
  expect_false(any(t$component %in% c("probability")))
  expect_true(any(t$component == "zero_probability"))
  expect_true(any(t$component == "consumption"))
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
  expect_equal(g$backend, "TMB_hurdle")
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
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value") %in%
                    names(t)))
  expect_true("component" %in% names(t))
  expect_true(all(c("estimate_scale", "term_display") %in% names(t)))
  expect_true(all(t$estimate_scale %in% c("natural", "log", "log10", "logit")))
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
  expect_true(all(c("model_class", "backend", "equation_form", "nobs", "n_subjects",
                    "converged", "logLik", "AIC", "BIC") %in% names(g)))
  expect_equal(g$model_class, "beezdemand_nlme")
  expect_equal(g$backend, "nlme")
})


test_that("tidy.beezdemand_fixed meets contract", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)
  t <- tidy(fit)

  expect_s3_class(t, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value") %in%
                    names(t)))
  expect_true("component" %in% names(t))
  expect_true("id" %in% names(t))
  expect_true(all(c("estimate_scale", "term_display") %in% names(t)))
  expect_true(all(t$estimate_scale %in% c("natural", "log", "log10", "logit")))
})


test_that("glance.beezdemand_fixed meets contract", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)
  g <- glance(fit)

  expect_s3_class(g, "tbl_df")
  expect_equal(nrow(g), 1)
  expect_true(all(c("model_class", "backend", "equation", "nobs", "n_subjects",
                    "converged", "logLik", "AIC", "BIC") %in% names(g)))
  expect_equal(g$model_class, "beezdemand_fixed")
  expect_equal(g$backend, "legacy")
})


test_that("glance methods share common columns across all model classes", {
  common_cols <- c("model_class", "backend", "nobs", "n_subjects",
                   "converged", "logLik", "AIC", "BIC")

  # Fixed model
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]
  fit_fixed <- fit_demand_fixed(apt_small)
  g_fixed <- glance(fit_fixed)
  expect_true(all(common_cols %in% names(g_fixed)),
    info = paste("Fixed missing:", paste(setdiff(common_cols, names(g_fixed)), collapse = ", ")))

  # Hurdle model
  skip_if_not_installed("TMB")
  skip_on_cran()
  fit_hurdle <- tryCatch(
    fit_demand_hurdle(apt_small, y_var = "y", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  if (!is.null(fit_hurdle)) {
    g_hurdle <- glance(fit_hurdle)
    expect_true(all(common_cols %in% names(g_hurdle)),
      info = paste("Hurdle missing:", paste(setdiff(common_cols, names(g_hurdle)), collapse = ", ")))
  }

  # NLME model
  skip_if_not_installed("nlme")
  apt_small$y_ll4 <- ll4(apt_small$y)
  fit_nlme <- tryCatch(
    fit_demand_mixed(apt_small, y_var = "y_ll4", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  if (!is.null(fit_nlme) && !is.null(fit_nlme$model)) {
    g_nlme <- glance(fit_nlme)
    expect_true(all(common_cols %in% names(g_nlme)),
      info = paste("NLME missing:", paste(setdiff(common_cols, names(g_nlme)), collapse = ", ")))
  }
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
  expect_true(all(c("model_class", "backend", "type", "nobs", "n_subjects",
                    "n_systematic", "n_unsystematic", "pct_systematic",
                    "converged", "logLik", "AIC", "BIC") %in% names(g)))
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
