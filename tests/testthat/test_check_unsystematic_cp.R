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
