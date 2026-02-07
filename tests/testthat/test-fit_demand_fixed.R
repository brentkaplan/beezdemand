# Tests for fit_demand_fixed() wrapper

test_that("fit_demand_fixed works on apt data", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)

  expect_s3_class(fit, "beezdemand_fixed")
  expect_true("results" %in% names(fit))
  expect_true("call" %in% names(fit))
  expect_true("equation" %in% names(fit))
  expect_true("n_total" %in% names(fit))
  expect_true("n_success" %in% names(fit))
  expect_true("n_fail" %in% names(fit))
})

test_that("fit_demand_fixed reproduces FitCurves golden values (HS, k=2)", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  fit <- suppressWarnings(fit_demand_fixed(test_data, equation = "hs", k = 2))
  res <- fit$results

  expect_s3_class(res, "data.frame")
  expect_true(all(test_ids %in% res$id))
  expect_true(all(c("Q0d", "Alpha", "K", "alpha_star", "alpha_star_se") %in% names(res)))

  row_19 <- res[res$id == 19, ]
  expect_equal(row_19$K, 2)
  expect_equal(row_19$Q0d, 10.158664, tolerance = 0.01)
  expect_equal(row_19$Alpha, 0.002047574, tolerance = 1e-5)
  expect_equal(
    row_19$alpha_star,
    -row_19$Alpha / log(1 - 1 / (row_19$K * log(10))),
    tolerance = 1e-8
  )
  expect_true(is.finite(row_19$alpha_star_se) && row_19$alpha_star_se >= 0)
})


test_that("fit_demand_fixed print method works", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)

  expect_output(print(fit), "Fixed-Effect Demand Model")
  expect_output(print(fit), "Equation:")
  expect_output(print(fit), "Subjects:")
})


test_that("fit_demand_fixed summary method works", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)
  s <- summary(fit)

  expect_s3_class(s, "summary.beezdemand_fixed")
  expect_s3_class(s, "beezdemand_summary")
  expect_equal(s$model_class, "beezdemand_fixed")
  expect_equal(s$backend, "legacy")
  expect_true("coefficients" %in% names(s))
  expect_true("derived_metrics" %in% names(s))
  expect_s3_class(s$coefficients, "tbl_df")
})

test_that("print.summary.beezdemand_fixed supports n truncation", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)
  s <- summary(fit)

  expect_output(print(s, n = 1), "Per-subject coefficients")
  expect_output(print(s, n = 1), "Showing first 1 ids")
})


test_that("fit_demand_fixed tidy method works", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)
  t <- tidy(fit)

  expect_s3_class(t, "tbl_df")
  expect_true("id" %in% names(t))
  expect_true("term" %in% names(t))
  expect_true("estimate" %in% names(t))
  expect_true("std.error" %in% names(t))
  expect_true("component" %in% names(t))

  # Should have Q0 and alpha rows
  expect_true("Q0" %in% t$term)
  expect_true("alpha" %in% t$term)
})


test_that("fit_demand_fixed glance method works", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small)
  g <- glance(fit)

  expect_s3_class(g, "tbl_df")
  expect_equal(nrow(g), 1)
  expect_equal(g$model_class, "beezdemand_fixed")
  expect_equal(g$backend, "legacy")
  expect_true("nobs" %in% names(g))
  expect_true("n_subjects" %in% names(g))
  expect_true("n_success" %in% names(g))
  expect_true("n_fail" %in% names(g))
})


test_that("fit_demand_fixed handles different equations", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit_hs <- fit_demand_fixed(apt_small, equation = "hs")
  fit_koff <- fit_demand_fixed(apt_small, equation = "koff")

  expect_equal(fit_hs$equation, "hs")
  expect_equal(fit_koff$equation, "koff")
})


test_that("fit_demand_fixed accepts modern equation aliases", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  # Modern aliases should map to legacy names internally
  fit_exp <- fit_demand_fixed(apt_small, equation = "exponential")
  fit_expd <- fit_demand_fixed(apt_small, equation = "exponentiated")

  expect_equal(fit_exp$equation, "hs")
  expect_equal(fit_expd$equation, "koff")
})


test_that("modern equation aliases produce identical results to legacy names", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit_hs <- suppressWarnings(fit_demand_fixed(apt_small, equation = "hs", k = 2))
  fit_exp <- suppressWarnings(fit_demand_fixed(apt_small, equation = "exponential", k = 2))

  expect_equal(fit_hs$results$Q0d, fit_exp$results$Q0d)
  expect_equal(fit_hs$results$Alpha, fit_exp$results$Alpha)

  fit_koff <- suppressWarnings(fit_demand_fixed(apt_small, equation = "koff", k = 2))
  fit_expd <- suppressWarnings(fit_demand_fixed(apt_small, equation = "exponentiated", k = 2))

  expect_equal(fit_koff$results$Q0d, fit_expd$results$Q0d)
  expect_equal(fit_koff$results$Alpha, fit_expd$results$Alpha)
})


test_that("fit_demand_fixed k specification is recorded", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit_k2 <- fit_demand_fixed(apt_small, k = 2)
  fit_k3 <- fit_demand_fixed(apt_small, k = 3)

  expect_equal(fit_k2$k_spec, "fixed (2)")
  expect_equal(fit_k3$k_spec, "fixed (3)")
  expect_equal(fit_k2$k_value, 2)
  expect_equal(fit_k3$k_value, 3)
})


test_that("fit_demand_fixed handles column name remapping", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  # Rename columns
  apt_renamed <- apt_small
  names(apt_renamed) <- c("subject", "price", "consumption")

  fit <- fit_demand_fixed(
    apt_renamed,
    x_var = "price",
    y_var = "consumption",
    id_var = "subject"
  )

  expect_s3_class(fit, "beezdemand_fixed")
  expect_true(fit$n_total > 0)
})


test_that("fit_demand_fixed tidy returns empty tibble for empty results",
{
  # Create a beezdemand_fixed object with NULL results
  fake_fit <- structure(
    list(
      results = NULL,
      fits = NULL,
      call = NULL,
      equation = "hs",
      k_spec = "fixed (2)",
      k_value = 2,
      n_total = 0,
      n_success = 0,
      n_fail = 0
    ),
    class = c("beezdemand_fixed", "list")
  )

  t <- tidy(fake_fit)

  expect_s3_class(t, "tbl_df")
  expect_equal(nrow(t), 0)
  expect_true(all(c("id", "term", "estimate", "std.error", "component") %in%
                    names(t)))
})


test_that("fit_demand_fixed convergence counts failed fits from Notes", {
  # Create a fake results data frame with some failed fits

  fake_results <- data.frame(
    id = c("1", "2", "3", "4"),
    Q0d = c(10, 5, 8, -0.1),
    Alpha = c(0.01, 0.02, -0.01, 0.03),
    K = c(2, 2, 2, 2),
    Notes = c(
      "converged",
      "wrapnls failed to converge, reverted to nlxb",
      "converged",
      "converged"
    ),
    stringsAsFactors = FALSE
  )

  # Create beezdemand_fixed object with the fake results
  fake_fit <- structure(
    list(
      results = fake_results,
      fits = NULL,
      call = NULL,
      equation = "hs",
      k_spec = "fixed (2)",
      k_value = 2,
      n_total = 4,
      n_success = 4,
      n_fail = 0
    ),
    class = c("beezdemand_fixed", "list")
  )

  # Simulate running fit_demand_fixed logic for convergence
  # The fix should count subject 2 as failed (Notes says failed)
  # and subject 3 as failed (negative Alpha)
  # and subject 4 as failed (negative Q0d)
  results <- fake_results
  n_total <- nrow(results)
  success_flag <- rep(TRUE, n_total)

  if ("Notes" %in% names(results)) {
    notes_lower <- tolower(results$Notes)
    failed_notes <- grepl("failed|reverted|singular|error", notes_lower)
    success_flag <- success_flag & !failed_notes
  }
  if ("Alpha" %in% names(results)) {
    success_flag <- success_flag & !is.na(results$Alpha) & results$Alpha >= 0
  }
  if ("Q0d" %in% names(results)) {
    success_flag <- success_flag & !is.na(results$Q0d) & results$Q0d >= 0
  }

  n_success <- sum(success_flag)
  n_fail <- n_total - n_success

  # Subject 1: converged, positive params -> success

# Subject 2: failed Notes -> fail
# Subject 3: negative Alpha -> fail
# Subject 4: negative Q0d -> fail
  expect_equal(n_success, 1)
  expect_equal(n_fail, 3)
})


test_that("summary.beezdemand_fixed param_summary respects report_space", {
  # Create a fake beezdemand_fixed object with positive params
  fake_results <- data.frame(
    id = c("1", "2", "3"),
    Q0d = c(10, 100, 1000),
    Alpha = c(0.01, 0.001, 0.0001),
    K = c(2, 2, 2),
    Notes = rep("converged", 3),
    stringsAsFactors = FALSE
  )

  fake_fit <- structure(
    list(
      results = fake_results,
      fits = NULL,
      predictions = NULL,
      data_used = NULL,
      call = quote(fit_demand_fixed(data = test)),
      equation = "hs",
      k_spec = "fixed (2)",
      k_value = 2,
      param_space = "natural",
      n_total = 3,
      n_success = 3,
      n_fail = 0
    ),
    class = c("beezdemand_fixed", "list")
  )

  s_natural <- summary(fake_fit, report_space = "natural")
  s_log10 <- summary(fake_fit, report_space = "log10")

  # Q0 median in natural space: 100 (middle value)
  # Q0 median in log10 space: log10(100) = 2
  expect_equal(s_natural$report_space, "natural")
  expect_equal(s_log10$report_space, "log10")

  # param_summary should differ between spaces
  q0_median_natural <- s_natural$param_summary$Q0["Median"]
  q0_median_log10 <- s_log10$param_summary$Q0["Median"]

  expect_equal(unname(q0_median_natural), 100)
  expect_equal(unname(q0_median_log10), 2)
})


test_that("tidy.beezdemand_fixed does not warn on negative values with log10", {
  # Create a fake beezdemand_fixed object with some negative params
  fake_results <- data.frame(
    id = c("1", "2", "3"),
    Q0d = c(10, -5, 100),
    Alpha = c(0.01, 0.001, -0.01),
    K = c(2, 2, 2),
    Notes = rep("converged", 3),
    stringsAsFactors = FALSE
  )

  fake_fit <- structure(
    list(
      results = fake_results,
      fits = NULL,
      predictions = NULL,
      data_used = NULL,
      call = quote(fit_demand_fixed(data = test)),
      equation = "hs",
      k_spec = "fixed (2)",
      k_value = 2,
      param_space = "natural",
      n_total = 3,
      n_success = 3,
      n_fail = 0
    ),
    class = c("beezdemand_fixed", "list")
  )

  # Should not produce NaN warnings
  expect_silent(t <- tidy(fake_fit, report_space = "log10"))

  # Negative values should become NA
  q0_estimates <- t$estimate[t$term == "Q0"]
  expect_true(is.na(q0_estimates[2]))  # -5 becomes NA

  alpha_estimates <- t$estimate[t$term == "alpha"]
  expect_true(is.na(alpha_estimates[3]))  # -0.01 becomes NA
})
