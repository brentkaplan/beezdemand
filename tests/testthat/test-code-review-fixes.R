# Tests for code review fixes
# Verifies critical fixes from engineering/CODE_REVIEW.md

# R-07: exp^(predict()) bug fix in FitMeanCurves with equation="linear"
# Before fix, this would error with "non-numeric argument to binary operator"
# because exp^(predict(...)) tried to use ^ on the exp function object.
test_that("FitMeanCurves does not error with equation='linear'", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  # Should not error (before fix, exp^() caused "non-numeric argument")
  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "linear", method = "Mean")
  ))

  expect_s3_class(result, "data.frame")
  expect_true("L" %in% names(result))
  expect_true("b" %in% names(result))
  expect_true("a" %in% names(result))
})

# C-01: && in if() for FitCurves input validation
test_that("FitCurves rejects non-numeric constrainq0", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id == 19, ]

  expect_error(
    FitCurves(apt_test, equation = "hs", k = 2, constrainq0 = "abc"),
    "Q0 constraint must be a number"
  )
})

# C-04: CheckCols emits message (not print) for tibbles
test_that("CheckCols emits message when given a tibble", {
  data(apt, package = "beezdemand")
  apt_tbl <- tibble::as_tibble(apt[apt$id == 19, ])

  expect_message(
    CheckCols(apt_tbl, xcol = "x", ycol = "y", idcol = "id"),
    "Data casted as data.frame"
  )
})

# C-04: PlotCurve returns invisible(NULL) with warning for all-zero data
test_that("PlotCurve warns and returns NULL for all-zero consumption", {
  adf <- data.frame(x = c(0, 1, 2, 5, 10), y = c(0, 0, 0, 0, 0))
  dfrow <- data.frame(
    Equation = "hs", Q0d = NA, Alpha = NA, K = NA,
    Pmaxd = NA, Omaxd = NA, Pmaxa = NA, Omaxa = NA,
    stringsAsFactors = FALSE
  )
  newdats <- data.frame(x = c(0.1, 1, 5, 10), y = rep(NA, 4))

  expect_warning(
    result <- PlotCurve(adf, dfrow, newdats),
    "No positive consumption values"
  )
  expect_null(result)
})
