# Tests for cross-price hurdle model functions

test_that("fit_cp_hurdle works with 2-RE model", {
  skip_on_cran()

  # Create test data
  set.seed(42)
  n_subj <- 10
  prices <- c(0.5, 1, 2, 4, 8, 16)

  test_data <- do.call(
    rbind,
    lapply(1:n_subj, function(i) {
      base <- rnorm(1, 5, 1)
      consumption <- pmax(
        0,
        round(base * exp(-0.1 * prices) + rnorm(length(prices), 0, 0.5))
      )
      data.frame(id = i, x = prices, y = consumption)
    })
  )

  # Fit model
  fit <- fit_cp_hurdle(
    test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "qalone"),
    verbose = 0
  )

  expect_s3_class(fit, "beezdemand_cp_hurdle")
  expect_equal(fit$param_info$n_random_effects, 2)
  expect_equal(fit$param_info$n_subjects, n_subj)
  expect_true(!is.null(fit$model$coefficients))
  expect_true(!is.null(fit$subject_pars))
})

test_that("fit_cp_hurdle works with 3-RE model", {
  skip_on_cran()

  set.seed(42)
  n_subj <- 10
  prices <- c(0.5, 1, 2, 4, 8, 16)

  test_data <- do.call(
    rbind,
    lapply(1:n_subj, function(i) {
      base <- rnorm(1, 5, 1)
      consumption <- pmax(
        0,
        round(base * exp(-0.1 * prices) + rnorm(length(prices), 0, 0.5))
      )
      data.frame(id = i, x = prices, y = consumption)
    })
  )

  fit <- fit_cp_hurdle(
    test_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "qalone", "I"),
    verbose = 0
  )

  expect_s3_class(fit, "beezdemand_cp_hurdle")
  expect_equal(fit$param_info$n_random_effects, 3)
})

test_that("fit_cp_hurdle validates inputs", {
  test_data <- data.frame(id = 1:10, x = 1:10, y = 1:10)

  # Missing column
  expect_error(
    fit_cp_hurdle(test_data, "consumption", "x", "id"),
    "Missing columns"
  )

  # Invalid random_effects
  expect_error(
    fit_cp_hurdle(test_data, "y", "x", "id", random_effects = c("invalid")),
    "Invalid random_effects"
  )

  # Must include zeros and qalone
  expect_error(
    fit_cp_hurdle(test_data, "y", "x", "id", random_effects = c("zeros")),
    "must include at least"
  )
})

test_that("print.beezdemand_cp_hurdle works", {
  skip_on_cran()

  data(etm, package = "beezdemand")
  test_data <- as.data.frame(etm[
    etm$group == "E-Cigarettes" & etm$target == "alt",
    c("id", "x", "y")
  ])

  fit <- fit_cp_hurdle(
    test_data,
    "y",
    "x",
    "id",
    random_effects = c("zeros", "qalone"),
    verbose = 0
  )

  expect_output(print(fit), "Two-Part Mixed Effects Hurdle Cross-Price")
  expect_output(print(fit), "Converged")
})

test_that("summary.beezdemand_cp_hurdle works", {
  skip_on_cran()

  data(etm, package = "beezdemand")
  test_data <- as.data.frame(etm[
    etm$group == "E-Cigarettes" & etm$target == "alt",
    c("id", "x", "y")
  ])

  fit <- fit_cp_hurdle(
    test_data,
    "y",
    "x",
    "id",
    random_effects = c("zeros", "qalone"),
    verbose = 0
  )

  # summary() now returns a structured object; output comes from print()
  summ <- summary(fit)
  expect_s3_class(summ, "summary.beezdemand_cp_hurdle")
  expect_output(print(summ), "Part I")
  expect_output(print(summ), "Part II")
  expect_output(print(summ), "Variance Components")
})

test_that("coef.beezdemand_cp_hurdle works", {
  skip_on_cran()

  data(etm, package = "beezdemand")
  test_data <- as.data.frame(etm[
    etm$group == "E-Cigarettes" & etm$target == "alt",
    c("id", "x", "y")
  ])

  fit <- fit_cp_hurdle(
    test_data,
    "y",
    "x",
    "id",
    random_effects = c("zeros", "qalone"),
    verbose = 0
  )

  coefs_all <- coef(fit, type = "all")
  expect_true("beta0" %in% names(coefs_all))
  expect_true("beta" %in% names(coefs_all))

  coefs_fixed <- coef(fit, type = "fixed")
  expect_true(is.numeric(coefs_fixed))

  coefs_random <- coef(fit, type = "random")
  expect_true(is.matrix(coefs_random))
})

test_that("predict.beezdemand_cp_hurdle works", {
  skip_on_cran()

  data(etm, package = "beezdemand")
  test_data <- as.data.frame(etm[
    etm$group == "E-Cigarettes" & etm$target == "alt",
    c("id", "x", "y")
  ])

  fit <- fit_cp_hurdle(
    test_data,
    "y",
    "x",
    "id",
    random_effects = c("zeros", "qalone"),
    verbose = 0
  )

  # Population level
  newdata <- data.frame(x = c(2, 16, 64))
  pred_demand <- predict(fit, newdata, type = "demand", level = "population")
  expect_length(pred_demand, 3)

  pred_response <- predict(
    fit,
    newdata,
    type = "response",
    level = "population"
  )
  expect_true(all(pred_response > 0))

  pred_prob <- predict(fit, newdata, type = "probability", level = "population")
  expect_true(all(pred_prob >= 0 & pred_prob <= 1))

  # Parameters
  params <- predict(fit, type = "parameters")
  expect_s3_class(params, "data.frame")
  expect_true("Qalone" %in% names(params))
})

test_that("tidy and glance methods work", {
  skip_on_cran()

  data(etm, package = "beezdemand")
  test_data <- as.data.frame(etm[
    etm$group == "E-Cigarettes" & etm$target == "alt",
    c("id", "x", "y")
  ])

  fit <- fit_cp_hurdle(
    test_data,
    "y",
    "x",
    "id",
    random_effects = c("zeros", "qalone"),
    verbose = 0
  )

  tidy_df <- tidy(fit)
  expect_s3_class(tidy_df, "data.frame")
  expect_true("term" %in% names(tidy_df))
  expect_true("estimate" %in% names(tidy_df))

  glance_df <- glance(fit)
  expect_s3_class(glance_df, "data.frame")
  expect_equal(nrow(glance_df), 1)
  expect_true("AIC" %in% names(glance_df))
})

test_that("compare_cp_hurdle_models works", {
  skip_on_cran()

  data(etm, package = "beezdemand")
  test_data <- as.data.frame(etm[
    etm$group == "E-Cigarettes" & etm$target == "alt",
    c("id", "x", "y")
  ])

  fit2 <- fit_cp_hurdle(
    test_data,
    "y",
    "x",
    "id",
    random_effects = c("zeros", "qalone"),
    verbose = 0
  )
  fit3 <- fit_cp_hurdle(
    test_data,
    "y",
    "x",
    "id",
    random_effects = c("zeros", "qalone", "I"),
    verbose = 0
  )

  comp <- compare_cp_hurdle_models(fit3, fit2)
  expect_s3_class(comp, "cp_hurdle_comparison")
  expect_true(!is.null(comp$lr_statistic))
  expect_true(!is.null(comp$p_value))
  expect_output(print(comp), "Likelihood Ratio Test")
})

test_that("get_cp_hurdle_param_summary works", {
  skip_on_cran()

  data(etm, package = "beezdemand")
  test_data <- as.data.frame(etm[
    etm$group == "E-Cigarettes" & etm$target == "alt",
    c("id", "x", "y")
  ])

  fit <- fit_cp_hurdle(
    test_data,
    "y",
    "x",
    "id",
    random_effects = c("zeros", "qalone"),
    verbose = 0
  )

  summary_df <- get_cp_hurdle_param_summary(fit)
  expect_s3_class(summary_df, "data.frame")
  expect_true("Qalone" %in% summary_df$parameter)
  expect_true("mean" %in% names(summary_df))
})

test_that("interpret_cp_interaction works", {
  expect_equal(interpret_cp_interaction(-1.0), "Strong substitute")
  expect_equal(interpret_cp_interaction(-0.3), "Weak substitute")
  expect_equal(interpret_cp_interaction(0.2), "Weak complement")
  expect_equal(interpret_cp_interaction(1.0), "Strong complement")
  expect_equal(interpret_cp_interaction(NA), "Unknown (NA)")
})
