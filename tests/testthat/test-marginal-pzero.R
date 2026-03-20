# Tests for marginal P(zero) prediction

# ---- Shared fixture ----

.make_hurdle_fit <- function(n_subjects = 50, seed = 42) {
  sim <- simulate_hurdle_data(n_subjects = n_subjects, seed = seed)
  fit_demand_hurdle(
    sim,
    y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )
}

# ---- Internal helper tests ----

test_that("all three marginal methods return values in [0, 1]", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()
  prices <- c(0, 0.5, 1, 2, 5, 10)

  for (m in c("kde", "normal", "empirical")) {
    res <- .compute_marginal_pzero(fit, prices, method = m)
    expect_length(res, length(prices))
    expect_true(all(res >= 0 & res <= 1), info = paste("method:", m))
  }
})

test_that("KDE and empirical agree closely with large n_subjects", {

  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit(n_subjects = 200, seed = 99)
  prices <- c(0, 1, 5, 10)

  kde_res <- .compute_marginal_pzero(fit, prices, method = "kde")
  emp_res <- .compute_marginal_pzero(fit, prices, method = "empirical")

  expect_equal(kde_res, emp_res, tolerance = 0.05)
})

test_that("all methods converge to conditional curve at very small sigma_a", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim <- simulate_hurdle_data(
    n_subjects = 50, seed = 42,
    sigma_a = 0.01, sigma_b = 0.01
  )
  fit <- fit_demand_hurdle(
    sim,
    y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  prices <- c(0, 1, 5, 10)
  coefs <- fit$model$coefficients
  beta0 <- unname(coefs[["beta0"]])
  beta1 <- unname(coefs[["beta1"]])
  eps <- fit$param_info$epsilon
  conditional <- stats::plogis(beta0 + beta1 * log(prices + eps))

  for (m in c("kde", "normal", "empirical")) {
    res <- .compute_marginal_pzero(fit, prices, method = m)
    expect_equal(res, conditional, tolerance = 0.1, info = paste("method:", m))
  }
})

test_that("KDE falls back to normal with < 3 unique subjects", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim <- simulate_hurdle_data(n_subjects = 2, seed = 42)
  fit <- fit_demand_hurdle(
    sim,
    y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  expect_warning(
    .compute_marginal_pzero(fit, c(1, 5), method = "kde"),
    "falling back"
  )
})

# ---- predict(type = "probability", marginal = TRUE) tests ----

test_that("predict marginal returns tibble with correct columns", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()
  prices <- c(0, 1, 5, 10)

  out <- predict(fit, type = "probability", marginal = TRUE, prices = prices)

  expect_s3_class(out, "tbl_df")
  expect_true("x" %in% names(out))
  expect_true("prob_zero" %in% names(out))
  expect_true(".fitted" %in% names(out))
  expect_false("id" %in% names(out))
  expect_equal(nrow(out), length(prices))
  expect_true(all(out$prob_zero >= 0 & out$prob_zero <= 1))
  expect_equal(out$prob_zero, out$.fitted)
  expect_equal(attr(out, "marginal_method"), "kde")
})

test_that("predict marginal each method works", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()
  prices <- c(0, 1, 5)

  for (m in c("kde", "normal", "empirical")) {
    out <- predict(fit, type = "probability", marginal = TRUE,
                   marginal_method = m, prices = prices)
    expect_s3_class(out, "tbl_df")
    expect_equal(attr(out, "marginal_method"), m)
    expect_true(all(out$prob_zero >= 0 & out$prob_zero <= 1))
  }
})

test_that("predict marginal warns when newdata includes id column", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()
  nd <- data.frame(x = c(1, 5), id = c("1", "2"))

  expect_warning(
    predict(fit, type = "probability", marginal = TRUE, newdata = nd),
    "population-level"
  )
})

test_that("predict marginal warns for unsupported types (link, parameters)", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()

  # marginal is supported for probability, response, and demand
  # but warns for link
  expect_warning(
    predict(fit, type = "link", marginal = TRUE, prices = c(1, 5)),
    "only applies to type"
  )
})

test_that("predict marginal works for type='demand' without warning", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()

  out <- predict(fit, type = "demand", marginal = TRUE, prices = c(1, 5))
  expect_s3_class(out, "tbl_df")
  expect_true(".fitted" %in% names(out))
  expect_equal(nrow(out), 2)
})

test_that("predict marginal se.fit warns and returns NA", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()

  expect_warning(
    out <- predict(fit, type = "probability", marginal = TRUE,
                   prices = c(1, 5), se.fit = TRUE),
    "Standard errors not yet supported"
  )
  expect_true(all(is.na(out$.se.fit)))
})

test_that("predict marginal interval warns and returns NA", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()

  expect_warning(
    out <- predict(fit, type = "probability", marginal = TRUE,
                   prices = c(1, 5), interval = "confidence"),
    "Standard errors not yet supported"
  )
  expect_true(all(is.na(out$.lower)))
  expect_true(all(is.na(out$.upper)))
})

test_that("predict marginal=FALSE matches existing behavior exactly", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()
  prices <- c(0, 1, 5, 10)

  out_default <- predict(fit, type = "probability", prices = prices)
  out_explicit <- predict(fit, type = "probability", marginal = FALSE,
                          prices = prices)

  expect_equal(out_default, out_explicit)
})

# ---- plot(type = "probability") tests ----

test_that("plot probability marginal=TRUE produces ggplot", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()

  p <- plot(fit, type = "probability")
  expect_s3_class(p, "gg")
})

test_that("plot probability marginal=FALSE still works", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()

  p <- plot(fit, type = "probability", marginal = FALSE)
  expect_s3_class(p, "gg")
})

test_that("plot probability with different marginal_methods", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()

  for (m in c("kde", "normal", "empirical")) {
    p <- plot(fit, type = "probability", marginal = TRUE, marginal_method = m)
    expect_s3_class(p, "gg")
  }
})

test_that("plot probability individual + population with marginal", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .make_hurdle_fit()
  ids <- head(fit$param_info$subject_levels, 3)

  p <- plot(fit, type = "probability", ids = ids, show_pred = "both",
            marginal = TRUE)
  expect_s3_class(p, "gg")
})
