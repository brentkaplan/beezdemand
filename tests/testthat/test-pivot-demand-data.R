test_that("apt roundtrip: long -> wide -> long preserves data", {
  data(apt, package = "beezdemand")

  wide <- pivot_demand_data(apt, format = "wide")
  long <- pivot_demand_data(wide, format = "long")

  # Sort both for comparison
  apt_sorted <- apt[order(apt$id, apt$x), ]
  long_sorted <- long[order(long$id, long$x), ]

  expect_equal(nrow(apt_sorted), nrow(long_sorted))
  expect_equal(apt_sorted$id, long_sorted$id)
  expect_equal(apt_sorted$x, long_sorted$x)
  expect_equal(apt_sorted$y, long_sorted$y)
})

test_that("ko roundtrip: extra columns preserved through wide -> long", {
  data(ko, package = "beezdemand")

  # Drop y_ll4 (derived column that varies per observation, breaks wide pivot)
  ko_sub <- ko[, c("monkey", "x", "y", "drug", "dose")]

  wide <- pivot_demand_data(ko_sub, format = "wide",
                            id_var = "monkey", x_var = "x", y_var = "y")
  # monkey gets renamed to id on the way back
  long <- pivot_demand_data(wide, format = "long", id_var = "monkey")

  # Extra columns should be preserved
  expect_true("drug" %in% names(long))
  expect_true("dose" %in% names(long))

  # Sort both for comparison (monkey -> id in long output)
  ko_sorted <- ko_sub[order(ko_sub$monkey, ko_sub$drug, ko_sub$dose, ko_sub$x), ]
  long_sorted <- long[order(long$id, long$drug, long$dose, long$x), ]

  expect_equal(nrow(ko_sorted), nrow(long_sorted))
  expect_equal(ko_sorted$x, long_sorted$x)
  expect_equal(ko_sorted$y, long_sorted$y)
})

test_that("etm roundtrip: target and group columns preserved", {
  data(etm, package = "beezdemand")

  wide <- pivot_demand_data(etm, format = "wide")
  long <- pivot_demand_data(wide, format = "long")

  expect_true("target" %in% names(long))
  expect_true("group" %in% names(long))

  # Sort and compare
  etm_sorted <- etm[order(etm$id, etm$target, etm$group, etm$x), ]
  long_sorted <- long[order(long$id, long$target, long$group, long$x), ]

  expect_equal(nrow(etm_sorted), nrow(long_sorted))
  expect_equal(etm_sorted$x, long_sorted$x)
  expect_equal(etm_sorted$y, long_sorted$y)
})

test_that("numeric column names auto-parse correctly", {
  wide <- data.frame(
    id = 1:3,
    "0" = c(10, 8, 12),
    "0.5" = c(9, 7, 11),
    "1" = c(8, 6, 9),
    check.names = FALSE
  )

  long <- pivot_demand_data(wide, format = "long")

  expect_equal(sort(unique(long$x)), c(0, 0.5, 1))
  expect_equal(nrow(long), 9)
  expect_true(is.numeric(long$x))
  expect_true(is.numeric(long$y))
})

test_that("shinybeez-style wide: extra id column auto-detected", {
  wide <- data.frame(
    id = c(1, 1, 2, 2),
    group = c("A", "B", "A", "B"),
    "0" = c(10, 8, 12, 9),
    "0.25" = c(9, 7, 11, 8),
    "0.5" = c(8, 6, 9, 7),
    check.names = FALSE
  )

  long <- pivot_demand_data(wide, format = "long")

  expect_true("group" %in% names(long))
  expect_equal(sort(unique(long$x)), c(0, 0.25, 0.5))
  # 4 rows * 3 prices = 12

  expect_equal(nrow(long), 12)
})

test_that("non-parseable column names error without x_values", {
  wide <- data.frame(
    id = 1:2,
    price_1 = c(10, 8),
    price_2 = c(5, 4),
    price_3 = c(3, 2)
  )

  expect_error(
    pivot_demand_data(wide, format = "long"),
    "Cannot parse prices from column names"
  )
})

test_that("x_values override works with non-numeric column names", {
  wide <- data.frame(
    id = 1:2,
    price_1 = c(10, 8),
    price_2 = c(5, 4),
    price_3 = c(3, 2)
  )

  long <- pivot_demand_data(wide, format = "long",
                            x_values = c(0, 0.5, 1))

  expect_equal(sort(unique(long$x)), c(0, 0.5, 1))
  expect_equal(nrow(long), 6)
})

test_that("NA handling: drop_na = TRUE warns and drops", {
  wide <- data.frame(
    id = 1:2,
    "0" = c(10, NA),
    "1" = c(5, 4),
    check.names = FALSE
  )

  expect_warning(
    long <- pivot_demand_data(wide, format = "long", drop_na = TRUE),
    "Dropped 1 rows with NA"
  )
  expect_equal(nrow(long), 3)
  expect_false(any(is.na(long$y)))
})

test_that("NA handling: drop_na = FALSE warns but retains", {
  wide <- data.frame(
    id = 1:2,
    "0" = c(10, NA),
    "1" = c(5, 4),
    check.names = FALSE
  )

  expect_warning(
    long <- pivot_demand_data(wide, format = "long", drop_na = FALSE),
    "1 rows have NA"
  )
  expect_equal(nrow(long), 4)
  expect_true(any(is.na(long$y)))
})

test_that("mismatched x_values length errors", {
  wide <- data.frame(
    id = 1:2,
    price_1 = c(10, 8),
    price_2 = c(5, 4)
  )

  expect_error(
    pivot_demand_data(wide, format = "long",
                      x_values = c(0, 0.5, 1)),
    "Length of x_values"
  )
})

test_that("format = 'wide' works with custom column names", {
  long <- data.frame(
    subject = c(1, 1, 2, 2),
    price = c(0, 1, 0, 1),
    consumption = c(10, 5, 8, 4)
  )

  wide <- pivot_demand_data(long, format = "wide",
                            id_var = "subject",
                            x_var = "price",
                            y_var = "consumption")

  expect_equal(nrow(wide), 2)
  expect_true("0" %in% names(wide))
  expect_true("1" %in% names(wide))
})

test_that("missing id_var column gives clear error", {
  wide <- data.frame(
    subject = 1:2,
    "0" = c(10, 8),
    "1" = c(5, 4),
    check.names = FALSE
  )

  expect_error(
    pivot_demand_data(wide, format = "long"),
    "Column 'id' not found"
  )
})

test_that("missing columns in format = 'wide' gives clear error", {
  long <- data.frame(id = 1:4, x = c(0, 1, 0, 1), y = c(10, 5, 8, 4))

  expect_error(
    pivot_demand_data(long, format = "wide", y_var = "consumption"),
    "not found in data"
  )
})

test_that("explicit price_cols works correctly", {
  wide <- data.frame(
    id = 1:2,
    group = c("A", "B"),
    p1 = c(10, 8),
    p2 = c(5, 4)
  )

  long <- pivot_demand_data(wide, format = "long",
                            price_cols = c("p1", "p2"),
                            x_values = c(0, 0.5))

  expect_true("group" %in% names(long))
  expect_equal(sort(unique(long$x)), c(0, 0.5))
  expect_equal(nrow(long), 4)
})

test_that("invalid price_cols gives clear error", {
  wide <- data.frame(id = 1:2, a = c(10, 8))

  expect_error(
    pivot_demand_data(wide, format = "long",
                      price_cols = c("a", "b")),
    "not found in data"
  )
})

test_that("non-numeric x_values errors", {
  wide <- data.frame(id = 1:2, p1 = c(10, 8))

  expect_error(
    pivot_demand_data(wide, format = "long",
                      price_cols = "p1",
                      x_values = "zero"),
    "x_values must be a numeric"
  )
})
