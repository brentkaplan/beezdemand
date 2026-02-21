# Tests for code review fixes
# Verifies critical fixes from engineering/CODE_REVIEW.md

# R-07: exp^(predict()) bug fix in FitMeanCurves with equation="linear"
test_that("FitMeanCurves works with equation='linear'", {
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30), ]

  result <- suppressMessages(suppressWarnings(
    FitMeanCurves(apt_test, equation = "linear", method = "Mean")
  ))

  expect_type(result, "list")
  expect_true("dfres" %in% names(result))
  expect_true(all(is.finite(result$dfres[["L"]]) | is.na(result$dfres[["L"]])))
  expect_true(all(is.finite(result$dfres[["b"]]) | is.na(result$dfres[["b"]])))
  expect_true(all(is.finite(result$dfres[["a"]]) | is.na(result$dfres[["a"]])))
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
