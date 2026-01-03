test_that("check_unsystematic_cp handles basic trends correctly", {
  x <- 10^(seq(-2, 2, length.out = 10))

  flat_data <- data.frame(x = x, y = rep(10, 10))
  up_data <- data.frame(x = x, y = seq(1, 10))
  down_data <- data.frame(x = x, y = seq(10, 1))
  bounce_data <- data.frame(x = x, y = c(10, 10, 10, 9, 8, 5, 3, 2, 0, 10))
  zero_rev_data <- data.frame(x = x, y = c(0, 0, 1, 0, 0, 1, 1, 0, 0, 1))

  expect_equal(check_unsystematic_cp(flat_data)$delta_direction, "none")
  expect_equal(check_unsystematic_cp(up_data)$delta_direction, "up")
  expect_equal(check_unsystematic_cp(down_data)$delta_direction, "down")

  result <- check_unsystematic_cp(bounce_data, detailed = TRUE)
  expect_equal(result$delta_direction, "none")
  expect_true(is.logical(result$bounce_none))

  result <- check_unsystematic_cp(
    zero_rev_data,
    detailed = TRUE,
    expected_down = FALSE
  )
  expect_true(!is.na(result$reversals) || !is.na(result$returns))
})


test_that("summary.cp_unsystematic works with valid input", {
  # Simulate simple example data
  df <- data.frame(
    id = rep(1:3, each = 2),
    group = rep(c("A", "B"), 3),
    delta_direction = c("down", "up", "none", "up", "down", "none"),
    bounce_direction = c(
      "up",
      "down",
      "significant",
      "none_detected",
      "down",
      "significant"
    ),
    bounce_any = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
  )

  summary_obj <- summary.cp_unsystematic(df)
  expect_s3_class(summary_obj, "summary.cp_unsystematic")
  expect_true(is.list(summary_obj))
  expect_true(summary_obj$total_patterns == 6)
  expect_equal(summary_obj$unsystematic_count, 4)
  expect_equal(summary_obj$systematic_count, 2)
  expect_true("trend_counts" %in% names(summary_obj))
  expect_true("bounce_counts" %in% names(summary_obj))
})


test_that("fit_cp_nls uses log10 parameterization and predict returns natural scale", {
  skip_if_not_installed("minpack.lm")

  set.seed(1)
  x <- c(1, 2, 4, 8, 16, 32)
  qalone_true <- 5
  beta_true <- 0.2
  I_true <- -0.6

  log10_y_true <- log10(qalone_true) + I_true * exp(-beta_true * x)
  y <- 10^(log10_y_true + rnorm(length(x), mean = 0, sd = 0.01))
  dat <- data.frame(x = x, y = y)

  fit_exp <- fit_cp_nls(
    dat,
    equation = "exponential",
    start_vals = list(
      log10_qalone = log10(qalone_true),
      I = I_true,
      log10_beta = log10(beta_true)
    ),
    iter = 5,
    return_all = TRUE
  )
  expect_s3_class(fit_exp, "cp_model_nls")

  coefs <- coef(fit_exp$model)
  expect_true(all(c("log10_qalone", "I", "log10_beta") %in% names(coefs)))

  preds <- predict(fit_exp, newdata = data.frame(x = x))
  expect_true(all(c("y_pred", "y_pred_log10") %in% names(preds)))

  beta_hat <- 10^coefs[["log10_beta"]]
  log10_y_hat <- coefs[["log10_qalone"]] + coefs[["I"]] * exp(-beta_hat * x)
  expect_equal(preds$y_pred_log10, unname(log10_y_hat), tolerance = 1e-12)
  expect_equal(preds$y_pred, unname(10^log10_y_hat), tolerance = 1e-12)

  summ <- summary(fit_exp)
  dm <- summ$derived_metrics
  expect_true(all(c("qalone", "beta", "log10_qalone", "log10_beta") %in% names(dm)))
  if (is.finite(dm$log10_qalone_se) && is.finite(dm$qalone_se)) {
    expect_equal(dm$qalone_se, log(10) * dm$qalone * dm$log10_qalone_se, tolerance = 1e-12)
  }
  if (is.finite(dm$log10_beta_se) && is.finite(dm$beta_se)) {
    expect_equal(dm$beta_se, log(10) * dm$beta * dm$log10_beta_se, tolerance = 1e-12)
  }

  fit_expt <- fit_cp_nls(
    dat,
    equation = "exponentiated",
    start_vals = list(
      log10_qalone = log10(qalone_true),
      I = I_true,
      log10_beta = log10(beta_true)
    ),
    iter = 5,
    return_all = TRUE
  )
  preds_expt <- predict(fit_expt, newdata = data.frame(x = x))
  expect_true("y_pred" %in% names(preds_expt))
  expect_false("y_pred_log10" %in% names(preds_expt))
})


test_that("fit_cp_nls exponential filters y <= 0 with warning", {
  skip_if_not_installed("minpack.lm")

  dat <- data.frame(x = c(1, 2, 3, 4), y = c(1, 0, 2, 3))
  expect_warning(
    fit <- fit_cp_nls(
      dat,
      equation = "exponential",
      start_vals = list(log10_qalone = 0, I = 0, log10_beta = 0),
      iter = 5,
      return_all = TRUE
    ),
    "Removing"
  )
  expect_true(all(fit$data$y > 0))
})
