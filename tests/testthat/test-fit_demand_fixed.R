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
  expect_s3_class(s$coefficients, "tbl_df")
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
