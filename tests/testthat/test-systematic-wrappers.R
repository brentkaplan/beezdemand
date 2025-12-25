# Tests for systematicity wrapper functions

test_that("check_systematic_demand works", {
  data(apt, package = "beezdemand")

  check <- check_systematic_demand(apt)

  expect_s3_class(check, "beezdemand_systematicity")
  expect_equal(check$type, "demand")
  expect_true(check$n_total > 0)
  expect_equal(check$n_systematic + check$n_unsystematic, check$n_total)
})


test_that("check_systematic_demand results have correct columns", {
  data(apt, package = "beezdemand")

  check <- check_systematic_demand(apt)
  results <- check$results

  expected_cols <- c(
    "id", "type", "trend_stat", "trend_threshold", "trend_direction",
    "trend_pass", "bounce_stat", "bounce_threshold", "bounce_direction",
    "bounce_pass", "reversals", "reversals_pass", "returns", "n_positive",
    "systematic"
  )

  expect_true(all(expected_cols %in% names(results)))
  expect_true(all(results$type == "demand"))
})


test_that("check_systematic_demand respects thresholds", {
  data(apt, package = "beezdemand")

  # Run with different thresholds
  check_strict <- check_systematic_demand(apt, trend_threshold = 0.1)
  check_lenient <- check_systematic_demand(apt, trend_threshold = 0.01)

  # Stricter threshold should catch more unsystematic patterns
  expect_true(check_strict$n_unsystematic >= check_lenient$n_unsystematic)
})


test_that("check_systematic_cp works with id column", {
  # Create CP-like data with multiple subjects
  cp_data <- data.frame(
    id = rep(1:3, each = 5),
    x = rep(c(0.1, 1, 2, 5, 10), 3),
    y = c(10, 8, 6, 3, 1, 10, 9, 7, 4, 2, 10, 5, 8, 2, 0)
  )

  check <- check_systematic_cp(cp_data)

  expect_s3_class(check, "beezdemand_systematicity")
  expect_equal(check$type, "cp")
  expect_equal(check$n_total, 3)
})


test_that("check_systematic_cp works without id column", {
  # Create single pattern without id
  cp_single <- data.frame(
    x = c(0.1, 1, 2, 5, 10),
    y = c(10, 8, 6, 3, 1)
  )

  check <- check_systematic_cp(cp_single)

  expect_s3_class(check, "beezdemand_systematicity")
  expect_equal(check$type, "cp")
  expect_equal(check$n_total, 1)
})


test_that("check_systematic_cp results have correct columns", {
  cp_data <- data.frame(
    id = rep(1:3, each = 5),
    x = rep(c(0.1, 1, 2, 5, 10), 3),
    y = c(10, 8, 6, 3, 1, 10, 9, 7, 4, 2, 10, 5, 8, 2, 0)
  )

  check <- check_systematic_cp(cp_data)
  results <- check$results

  expected_cols <- c(
    "id", "type", "trend_stat", "trend_threshold", "trend_direction",
    "trend_pass", "bounce_stat", "bounce_threshold", "bounce_direction",
    "bounce_pass", "reversals", "reversals_pass", "returns", "n_positive",
    "systematic"
  )

  expect_true(all(expected_cols %in% names(results)))
  expect_true(all(results$type == "cp"))
})


test_that("print method for beezdemand_systematicity works", {
  data(apt, package = "beezdemand")

  check <- check_systematic_demand(apt)

  expect_output(print(check), "Systematicity Check")
  expect_output(print(check), "demand")
  expect_output(print(check), "Total patterns")
  expect_output(print(check), "Systematic")
  expect_output(print(check), "Unsystematic")
})


test_that("summary method for beezdemand_systematicity works", {
  data(apt, package = "beezdemand")

  check <- check_systematic_demand(apt)
  s <- summary(check)

  expect_s3_class(s, "summary.beezdemand_systematicity")
  expect_s3_class(s, "beezdemand_summary")
  expect_equal(s$model_class, "beezdemand_systematicity")
  expect_true("counts" %in% names(s))
  expect_s3_class(s$counts, "tbl_df")
  expect_true("problem_ids" %in% names(s))
})


test_that("tidy method for beezdemand_systematicity returns results", {
  data(apt, package = "beezdemand")

  check <- check_systematic_demand(apt)
  t <- tidy(check)

  expect_s3_class(t, "tbl_df")
  expect_true("id" %in% names(t))
  expect_true("type" %in% names(t))
  expect_true("systematic" %in% names(t))
  expect_true(all(t$type == "demand"))
})


test_that("glance method for beezdemand_systematicity works", {
  data(apt, package = "beezdemand")

  check <- check_systematic_demand(apt)
  g <- glance(check)

  expect_s3_class(g, "tbl_df")
  expect_equal(nrow(g), 1)
  expect_equal(g$model_class, "beezdemand_systematicity")
  expect_true("n_subjects" %in% names(g))
  expect_true("n_systematic" %in% names(g))
  expect_true("n_unsystematic" %in% names(g))
  expect_true("pct_systematic" %in% names(g))
  expect_equal(g$type, "demand")
})


test_that("check_systematic_demand handles column remapping", {
  data(apt, package = "beezdemand")

  # Rename columns
  apt_renamed <- apt
  names(apt_renamed) <- c("subject", "price", "consumption")

  check <- check_systematic_demand(
    apt_renamed,
    x_var = "price",
    y_var = "consumption",
    id_var = "subject"
  )

  expect_s3_class(check, "beezdemand_systematicity")
  expect_true(check$n_total > 0)
})


test_that("check_systematic_cp handles column remapping", {
  cp_data <- data.frame(
    subject = rep(1:3, each = 5),
    price = rep(c(0.1, 1, 2, 5, 10), 3),
    consumption = c(10, 8, 6, 3, 1, 10, 9, 7, 4, 2, 10, 5, 8, 2, 0)
  )

  check <- check_systematic_cp(
    cp_data,
    x_var = "price",
    y_var = "consumption",
    id_var = "subject"
  )

  expect_s3_class(check, "beezdemand_systematicity")
  expect_equal(check$n_total, 3)
})
