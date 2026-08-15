# TICKET-059: ExtraF() unguarded per-group dereference (F3) and
# GetSharedK()'s unreachable try-error sentinel / unguarded start-grid
# construction (F4). Neither function previously had any test coverage.

# =============================================================================
# F3: ExtraF() names the failing group instead of an opaque dispatch error
# =============================================================================

test_that("TICKET-059 (F3): ExtraF names the failing group on a degenerate fit", {
  skip_on_cran()
  gA <- data.frame(
    id = "A1", x = c(0.1, 0.5, 1, 3, 6, 12, 24),
    y = c(9.977, 9.885, 9.771, 9.32, 8.66, 7.42, 5.4), group = "A"
  )
  # after hs's zero-drop, group B is reduced to a single row -- not enough
  # data to fit q0 and alpha
  gB <- data.frame(id = "B1", x = c(0.1, 0.5, 1, 3), y = c(4, 0, 0, 0), group = "B")

  err <- tryCatch(
    suppressWarnings(ExtraF(
      rbind(gA, gB), equation = "hs", groups = c("A", "B"),
      groupcol = "group", k = 2
    )),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_true(grepl("group 'B'", conditionMessage(err)))
  expect_false(grepl("no applicable method", conditionMessage(err)))
})

test_that("TICKET-059 (F3): ExtraF completes normally when both groups fit", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  gA <- apt[apt$id == 19, ]
  gA$group <- "A"
  gB <- apt[apt$id == 30, ]
  gB$group <- "B"
  result <- suppressWarnings(suppressMessages(
    ExtraF(rbind(gA, gB), equation = "hs", groups = c("A", "B"), groupcol = "group", k = 2)
  ))
  expect_type(result, "list")
  expect_true(all(c("simpmodel", "compmodels") %in% names(result)))
})

# =============================================================================
# F4: GetSharedK() always returns (numeric k or character sentinel), never
# raises -- so both ExtraF()'s try()-wrapped caller and FitCurves(k =
# "share")'s bare caller can rely on is.character(k) to detect failure.
# =============================================================================

test_that("TICKET-059 (F4): GetSharedK returns a sentinel instead of crashing on low-consumption data", {
  skip_on_cran()
  tiny <- data.frame(
    id = rep(c("t1", "t2"), each = 4),
    x = rep(c(0.1, 0.5, 1, 3), 2),
    y = c(0.35, 0.2, 0.1, 0.04, 0.3, 0.15, 0.08, 0.03)
  )
  result <- suppressWarnings(GetSharedK(tiny, equation = "hs", sharecol = "id"))
  expect_type(result, "character")
  expect_true(grepl("Unable to find a shared k", result))
})

test_that("TICKET-059 (F4): FitCurves(k = 'share') falls back to GetK() with a warning", {
  skip_on_cran()
  tiny <- data.frame(
    id = rep(c("t1", "t2"), each = 4),
    x = rep(c(0.1, 0.5, 1, 3), 2),
    y = c(0.35, 0.2, 0.1, 0.04, 0.3, 0.15, 0.08, 0.03)
  )
  expect_warning(
    result <- suppressMessages(FitCurves(tiny, equation = "hs", k = "share")),
    "Unable to find a shared k"
  )
  expect_equal(nrow(result), 2L)
  expect_false(any(is.na(result$K)))
})

test_that("TICKET-059 (F4): dropping to <2 usable groups returns an informative sentinel", {
  skip_on_cran()
  d <- data.frame(
    id = rep(c("g1", "g2", "g3"), c(6, 2, 2)),
    x = c(0.1, 0.5, 1, 3, 6, 12, 0.1, 0.5, 0.1, 0.5),
    y = c(9, 8, 7, 6, 5, 4, 3, 2, 3, 2)
  )
  result <- GetSharedK(d, equation = "hs", sharecol = "id")
  expect_type(result, "character")
  expect_true(grepl("fewer than 2 groups", result))
})

test_that("TICKET-059 (F4): GetSharedK finds a numeric shared k for healthy data", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]
  result <- suppressMessages(GetSharedK(apt_test, equation = "hs", sharecol = "id"))
  expect_true(is.numeric(result))
  expect_true(is.finite(result))
})
