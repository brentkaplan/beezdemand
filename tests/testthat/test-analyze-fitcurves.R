# Golden tests for FitCurves (legacy analyze.R functions)
# These tests capture the expected behavior of FitCurves to prevent regressions
# during future modernization efforts (API-008)

# Helper function to compare numeric values with tolerance
expect_equal_numeric <- function(actual, expected, tolerance = 1e-4) {
  expect_equal(actual, expected, tolerance = tolerance)
}

# Test data setup
test_that("FitCurves test data is available", {
  data(apt, package = "beezdemand")
  expect_true(exists("apt"))
  expect_true(is.data.frame(apt))
  expect_true(all(c("id", "x", "y") %in% names(apt)))
})

# =============================================================================
# Golden Tests for FitCurves with HS equation
# =============================================================================

test_that("FitCurves with HS equation and fixed k produces correct results", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  # This should produce a warning about zeros being dropped
  result <- suppressWarnings(FitCurves(test_data, "hs", k = 2))

  # Check structure

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("id", "Equation", "Q0d", "K", "Alpha", "R2", "Pmaxd", "Omaxd") %in% names(result)))
  expect_true(all(c("alpha_star", "alpha_star_se") %in% names(result)))

  # Golden values for ID 19
  row_19 <- result[result$id == 19, ]
  expect_equal_numeric(row_19$Q0d, 10.158664, tolerance = 0.01)
  expect_equal(row_19$K, 2)
  expect_equal_numeric(row_19$Alpha, 0.002047574, tolerance = 1e-5)
  expect_equal_numeric(
    row_19$alpha_star,
    -row_19$Alpha / log(1 - 1 / (row_19$K * log(10))),
    tolerance = 1e-8
  )
  expect_true(is.finite(row_19$alpha_star_se) && row_19$alpha_star_se >= 0)
  expect_equal_numeric(row_19$R2, 0.9804182, tolerance = 0.01)
  expect_equal_numeric(row_19$Pmaxd, 13.86976, tolerance = 0.1)
  expect_equal_numeric(row_19$Omaxd, 44.43035, tolerance = 0.1)

  # Golden values for ID 30
  row_30 <- result[result$id == 30, ]
  expect_equal_numeric(row_30$Q0d, 2.807366, tolerance = 0.01)
  expect_equal_numeric(row_30$Alpha, 0.005865523, tolerance = 1e-5)
  expect_equal_numeric(row_30$R2, 0.7723159, tolerance = 0.01)

  # Golden values for ID 38
  row_38 <- result[result$id == 38, ]
  expect_equal_numeric(row_38$Q0d, 4.497456, tolerance = 0.01)
  expect_equal_numeric(row_38$Alpha, 0.004203441, tolerance = 1e-5)
})

test_that("FitCurves with HS equation warns about zeros", {
  data(apt, package = "beezdemand")
  # ID 38 has zeros in the data
  test_data <- apt[apt$id == 38, ]

  expect_warning(
    FitCurves(test_data, "hs", k = 2),
    "Zeros found in data"
  )
})

# =============================================================================
# Golden Tests for FitCurves with Koffarnus equation
# =============================================================================

test_that("FitCurves with Koff equation and fixed k produces correct results", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  result <- FitCurves(test_data, "koff", k = 2)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # Golden values for ID 19
  row_19 <- result[result$id == 19, ]
  expect_equal(row_19$Equation, "koff")
  expect_equal_numeric(row_19$Q0d, 10.072114, tolerance = 0.01)
  expect_equal_numeric(row_19$Alpha, 0.002003155, tolerance = 1e-5)
  expect_equal_numeric(
    row_19$alpha_star,
    -row_19$Alpha / log(1 - 1 / (row_19$K * log(10))),
    tolerance = 1e-8
  )
  expect_true(is.finite(row_19$alpha_star_se) && row_19$alpha_star_se >= 0)
  expect_equal_numeric(row_19$R2, 0.9676372, tolerance = 0.01)
  expect_equal_numeric(row_19$Pmaxd, 14.29914, tolerance = 0.1)

  # Golden values for ID 30
  row_30 <- result[result$id == 30, ]
  expect_equal_numeric(row_30$Q0d, 2.967428, tolerance = 0.01)
  expect_equal_numeric(row_30$Alpha, 0.006381213, tolerance = 1e-5)

  # Golden values for ID 38
  row_38 <- result[result$id == 38, ]
  expect_equal_numeric(row_38$Q0d, 4.605634, tolerance = 0.01)
  expect_equal_numeric(row_38$Alpha, 0.004874198, tolerance = 1e-5)
})

# =============================================================================
# Golden Tests for FitCurves with aggregation
# =============================================================================

test_that("FitCurves with Mean aggregation produces correct results", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  result <- FitCurves(test_data, "hs", k = 2, agg = "Mean")

  # Check structure - should be single row
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$id), "mean")

  # Golden values
  expect_equal_numeric(result$Q0d, 6.170368, tolerance = 0.01)
  expect_equal_numeric(result$Alpha, 0.003859777, tolerance = 1e-5)
  expect_equal_numeric(result$R2, 0.9729868, tolerance = 0.01)
})

test_that("FitCurves with Pooled aggregation produces correct results", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  result <- suppressWarnings(FitCurves(test_data, "hs", k = 2, agg = "Pooled"))

  # Check structure - should be single row
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$id), "pooled")

  # Golden values (lower R2 expected for pooled data)
  expect_equal_numeric(result$Q0d, 4.993143, tolerance = 0.01)
  expect_equal_numeric(result$Alpha, 0.003598877, tolerance = 1e-5)
  expect_equal_numeric(result$R2, 0.3060571, tolerance = 0.01)
})

# =============================================================================
# Golden Tests for FitCurves with k="range"
# =============================================================================

test_that("FitCurves with k='range' produces correct results", {
  data(apt, package = "beezdemand")
  test_ids <- c(19, 30, 38)
  test_data <- apt[apt$id %in% test_ids, ]

  result <- suppressWarnings(FitCurves(test_data, "hs", k = "range"))

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # All should have the same K value (calculated from range)
  expect_equal_numeric(result$K[1], 1.077236, tolerance = 0.01)
  expect_equal(result$K[1], result$K[2])
  expect_equal(result$K[1], result$K[3])

  # Golden values for ID 19
  row_19 <- result[result$id == 19, ]
  expect_equal_numeric(row_19$Q0d, 10.454333, tolerance = 0.01)
  expect_equal_numeric(row_19$Alpha, 0.004395264, tolerance = 1e-5)
})

# =============================================================================
# Input validation tests
# =============================================================================

test_that("FitCurves validates required inputs", {
  data(apt, package = "beezdemand")

  # Missing data
  expect_error(FitCurves(), "Need to provide a dataframe")

  # Missing equation
  expect_error(FitCurves(apt), "Need to specify an equation")

  # Invalid aggregation
  expect_error(
    FitCurves(apt[apt$id == 19, ], "hs", k = 2, agg = "invalid"),
    "No correct agg specified"
  )
})

test_that("FitCurves handles constrainq0 validation", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  # constrainq0 must be numeric
  expect_error(
    FitCurves(test_data, "hs", k = 2, constrainq0 = "abc"),
    "Q0 constraint must be a number"
  )
})

# =============================================================================
# Detailed output tests
# =============================================================================

test_that("FitCurves detailed=TRUE returns list with model objects", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  result <- suppressWarnings(FitCurves(test_data, "hs", k = 2, detailed = TRUE))

  # Should be a list with 4 elements
  expect_type(result, "list")
  expect_length(result, 4)

  # First element is the results dataframe
  expect_s3_class(result[[1]], "data.frame")
  expect_equal(nrow(result[[1]]), 1)

  # Second element contains model fits
  expect_type(result[[2]], "list")

  # Third element contains individual data
  expect_type(result[[3]], "list")

  # Fourth element contains new data
  expect_type(result[[4]], "list")
})

# =============================================================================
# Column specification tests
# =============================================================================

test_that("FitCurves works with custom column names", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  # Rename columns
  names(test_data) <- c("subject", "price", "consumption")

  result <- suppressWarnings(
    FitCurves(test_data, "hs", k = 2,
              xcol = "price", ycol = "consumption", idcol = "subject")
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

# =============================================================================
# Edge case tests
# =============================================================================

test_that("FitCurves handles single subject", {
  data(apt, package = "beezdemand")
  test_data <- apt[apt$id == 19, ]

  result <- suppressWarnings(FitCurves(test_data, "hs", k = 2))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  # Note: id is returned as character
  expect_equal(as.character(result$id), "19")
})

test_that("FitCurves handles data with no zeros (koff equation)", {
  data(apt, package = "beezdemand")
  # Filter to data without zeros
  test_data <- apt[apt$id == 19 & apt$y > 0, ]

  # Should not produce warning for koff equation
  result <- FitCurves(test_data, "koff", k = 2)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

# =============================================================================
# TICKET-055: sticky start values + unguarded fit$m$Rmat() in nls fallback
# =============================================================================

test_that("TICKET-055: batch fit survives an unfittable subject", {
  skip_on_cran()
  d <- data.frame(
    id = rep(c("s1", "s2"), each = 6),
    x  = rep(c(0, 0.5, 1, 2, 4, 8), 2),
    y  = c(10, 8, 6, 4, 2, 1, 4e8, 1e7, 5e5, 1e13, 2e27, 60)
  )
  f <- suppressWarnings(FitCurves(
    d, equation = "simplified", xcol = "x", ycol = "y", idcol = "id"
  ))
  expect_equal(nrow(f), 2L)
})

test_that("TICKET-055: batch start values are per-subject (order-invariant)", {
  skip_on_cran()
  # two well-behaved subjects at very different consumption scales
  d <- data.frame(
    id = rep(c("a", "b"), each = 6),
    x  = rep(c(0, 0.5, 1, 2, 4, 8), 2),
    y  = c(10, 8, 6, 4, 2, 1, 1000, 800, 600, 400, 200, 100)
  )
  d_rev <- d[order(match(d$id, c("b", "a"))), ]
  f1 <- suppressWarnings(FitCurves(d,     equation = "simplified",
    xcol = "x", ycol = "y", idcol = "id"))
  f2 <- suppressWarnings(FitCurves(d_rev, equation = "simplified",
    xcol = "x", ycol = "y", idcol = "id"))
  r1 <- f1[order(f1$id), ]
  r2 <- f2[order(f2$id), ]
  expect_equal(r1$Alpha, r2$Alpha, tolerance = 1e-8)
  expect_equal(r1$Q0d,   r2$Q0d,   tolerance = 1e-8)
})

test_that("TICKET-055: all-zero and single-positive subjects yield rows, not errors", {
  skip_on_cran()
  d <- data.frame(
    id = rep(c("normal", "allzero", "onepos"), each = 6),
    x  = rep(c(0, 0.5, 1, 2, 4, 8), 3),
    y  = c(10, 8, 6, 4, 2, 1,      # normal
           0, 0, 0, 0, 0, 0,       # all-zero
           0, 0, 0, 0, 0, 5)       # one positive
  )
  f <- suppressWarnings(FitCurves(
    d, equation = "simplified", xcol = "x", ycol = "y", idcol = "id"
  ))
  expect_equal(nrow(f), 3L)
})

# =============================================================================
# TICKET-057: k="fit" + param_space="log10" re-applies log10() to kstart
# each iteration, compounding across subjects
# =============================================================================

test_that("TICKET-057: k='fit' + param_space='log10' batch matches subject-alone fits", {
  skip_on_cran()
  mk_subj <- function(id, q0 = 10, alpha = 0.005, k = 2,
                       x = c(0.1, 0.5, 1, 3, 6, 12, 24)) {
    y <- q0 * 10^(k * (exp(-alpha * q0 * x) - 1))
    data.frame(id = id, x = x, y = round(y, 3))
  }
  d3 <- rbind(mk_subj("s1"), mk_subj("s2", q0 = 12), mk_subj("s3", q0 = 8))

  batch <- suppressWarnings(suppressMessages(
    FitCurves(d3, "hs", k = "fit", param_space = "log10")
  ))
  expect_equal(nrow(batch), 3L)
  for (s in unique(d3$id)) {
    alone <- suppressWarnings(suppressMessages(
      FitCurves(d3[d3$id == s, ], "hs", k = "fit", param_space = "log10")
    ))
    expect_equal(batch$K[batch$id == s], alone$K, tolerance = 1e-4)
  }
})

# =============================================================================
# TICKET-069: FitCurves() reports unverified optimizer endpoints as
# estimates -- no convergence gate on the fallback chain
# =============================================================================

test_that("TICKET-069: flat data warns on the non-positive parameter but Notes/converged_strict stay byte-identical to develop", {
  skip_on_cran()
  # Taboo 4 / decision Q5a: domain validity is signalled ONLY by a warning
  # naming the subject and the offending parameter. converged_strict is the
  # literal isConv && finite_ok && !at_bound verdict -- NOT demoted for
  # domain-invalid estimates -- and Notes is NEVER modified by the domain
  # check. The flat subject's exact Notes text (verified against develop,
  # pre-TICKET-069) is NOT bare "converged" -- alpha_star computation
  # already fails (and appends its own note) on this subject's negative
  # alpha, independent of and pre-dating the domain-invalid warning added
  # here. That pre-existing text must be untouched by this ticket's fix.
  flat <- data.frame(id = "flat", x = c(0.1, 0.5, 1, 3, 6, 12, 24), y = rep(7, 7))
  expect_warning(
    result <- FitCurves(flat, equation = "koff", k = 2),
    "Alpha"
  )
  expect_true(all(c("converged", "converged_strict") %in% names(result)))
  expect_true(isTRUE(result$converged))
  expect_true(isTRUE(result$converged_strict))
  expect_identical(
    result$Notes,
    "converged; alpha_star undefined: alpha and k must be finite and positive"
  )
})

test_that("TICKET-069: a fully unverifiable fallback endpoint is not reported as an estimate", {
  skip_on_cran()
  d <- data.frame(
    id = rep(c("s1", "s2"), each = 6),
    x  = rep(c(0, 0.5, 1, 2, 4, 8), 2),
    y  = c(10, 8, 6, 4, 2, 1, 4e8, 1e7, 5e5, 1e13, 2e27, 60)
  )
  result <- suppressWarnings(FitCurves(
    d, equation = "simplified", xcol = "x", ycol = "y", idcol = "id"
  ))
  s2 <- result[result$id == "s2", ]
  expect_true(is.na(s2$Q0d))
  expect_true(is.na(s2$Alpha))
  expect_false(isTRUE(s2$converged))
  expect_false(isTRUE(s2$converged_strict))
  expect_true(grepl("unverified", s2$Notes, ignore.case = TRUE))
})

test_that("TICKET-069: healthy fits are converged and converged_strict", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt_test <- apt[apt$id %in% c(19, 30, 38), ]
  result <- suppressMessages(suppressWarnings(
    FitCurves(apt_test, equation = "hs", k = 2)
  ))
  expect_true(all(result$converged))
  expect_true(all(result$converged_strict))
  expect_true(all(result$Notes == "converged"))
})

test_that("TICKET-069: fit_demand_fixed()$results$converged derives from converged_strict", {
  skip_on_cran()
  # A subject with a full fallback-verification failure IS flagged.
  # TICKET-047 (item 4): the multi-start rescue protocol
  # only accepts DOMAIN-VALID (Q0 > 0, Alpha > 0) strict-converged sampled
  # starts as rescue candidates, so s2's pathological (non-monotonic,
  # 26-orders-of-magnitude) data stays non-converged under the DEFAULT
  # multistart budget too -- no multistart = FALSE workaround needed. See
  # test-fixed-multistart.R for dedicated multistart/domain-validity
  # coverage.
  d <- data.frame(
    id = rep(c("s1", "s2"), each = 6),
    x  = rep(c(0, 0.5, 1, 2, 4, 8), 2),
    y  = c(10, 8, 6, 4, 2, 1, 4e8, 1e7, 5e5, 1e13, 2e27, 60)
  )
  f <- suppressWarnings(fit_demand_fixed(
    d, equation = "simplified", x_var = "x", y_var = "y", id_var = "id"
  ))
  expect_false(isTRUE(f$results$converged[f$results$id == "s2"]))
})

test_that("TICKET-069: fit_demand_fixed()$results$converged is TRUE for a numerically-converged domain-invalid fit (warning-only signal, decision Q5a)", {
  skip_on_cran()
  flat <- data.frame(id = "flat", x = c(0.1, 0.5, 1, 3, 6, 12, 24), y = rep(7, 7))
  f <- suppressWarnings(fit_demand_fixed(flat, equation = "koff", k = 2))
  expect_true(isTRUE(f$results$converged))
  expect_warning(
    fit_demand_fixed(flat, equation = "koff", k = 2),
    "Alpha"
  )
})

# =============================================================================
# Additional TICKET-069 convergence-verdict coverage
# =============================================================================

test_that("TICKET-069: a verified fallback rescue is converged and converged_strict", {
  skip_on_cran()
  # Deterministic start values that make wrapnlsr fail (singular gradient)
  # but nlxb reach a good endpoint that the port refit verifies and
  # converges from -- a genuine rescue, not a stalled snapshot.
  q0_true <- 10
  alpha_true <- 0.05
  x <- c(0, 0.5, 1, 2, 4, 8, 16)
  y <- round(q0_true * exp(-alpha_true * q0_true * x), 4)
  d <- data.frame(id = "s1", x = x, y = y)

  result <- suppressWarnings(FitCurves(
    d, equation = "simplified", startq0 = 50.20276, startalpha = -0.08881612
  ))
  expect_equal(result$Q0d, q0_true, tolerance = 1e-3)
  expect_equal(result$Alpha, alpha_true, tolerance = 1e-3)
  expect_true(isTRUE(result$converged))
  expect_true(isTRUE(result$converged_strict))
  expect_true(grepl("verified", result$Notes, ignore.case = TRUE))
})

test_that("TICKET-069: a fit sitting at a supplied bound is converged but not converged_strict", {
  skip_on_cran()
  q0_true <- 10
  alpha_true <- 0.05
  x <- c(0, 0.5, 1, 2, 4, 8, 16)
  y <- round(q0_true * exp(-alpha_true * q0_true * x), 4)
  d <- data.frame(id = "s1", x = x, y = y)

  # alpha's true optimum (0.05) is above the supplied upper bound (0.03),
  # so the optimizer clamps to the bound.
  result <- suppressWarnings(FitCurves(
    d, equation = "simplified",
    lobound = c(q0 = -Inf, alpha = -Inf),
    hibound = c(q0 = Inf, alpha = 0.03)
  ))
  expect_equal(result$Alpha, 0.03, tolerance = 1e-8)
  expect_true(isTRUE(result$converged))
  expect_false(isTRUE(result$converged_strict))
})

test_that("TICKET-069: a batch with two domain-invalid subjects raises exactly two domain warnings", {
  skip_on_cran()
  spike1 <- data.frame(id = "spike1", x = c(0.1, 0.5, 1, 3, 6, 12, 24), y = c(1, 1, 1, 1, 1, 1, 10))
  spike2 <- data.frame(id = "spike2", x = c(0.1, 0.5, 1, 3, 6, 12, 24), y = c(2, 2, 2, 2, 2, 2, 15))
  good   <- data.frame(
    id = "good", x = c(0.1, 0.5, 1, 3, 6, 12, 24),
    y = c(9.977, 9.885, 9.771, 9.32, 8.66, 7.42, 5.4)
  )
  d <- rbind(spike1, spike2, good)

  domain_warnings <- character(0)
  withCallingHandlers(
    suppressMessages(FitCurves(d, equation = "koff", k = 2)),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("reported as converged with a", msg)) {
        domain_warnings <<- c(domain_warnings, msg)
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(length(domain_warnings), 2L)
  expect_true(any(grepl("spike1", domain_warnings)))
  expect_true(any(grepl("spike2", domain_warnings)))
})

# =============================================================================
# Frozen-contract byte-identity (alone vs. in a batch)
# =============================================================================

test_that("a healthy subject's FitCurves() row is byte-identical alone vs. inside a batch", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  alone <- suppressMessages(suppressWarnings(
    FitCurves(apt[apt$id == 19, ], equation = "hs", k = 2)
  ))
  batch <- suppressMessages(suppressWarnings(
    FitCurves(apt[apt$id %in% c(19, 30, 38), ], equation = "hs", k = 2)
  ))
  batch_19 <- batch[batch$id == "19", ]
  rownames(alone) <- NULL
  rownames(batch_19) <- NULL
  num_cols <- names(alone)[vapply(alone, is.numeric, logical(1))]
  expect_identical(alone[, num_cols], batch_19[, num_cols])
  expect_identical(alone$Notes, batch_19$Notes)
})

test_that("a healthy subject's fit_demand_fixed() row is byte-identical alone vs. inside a batch", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  alone <- suppressWarnings(fit_demand_fixed(apt[apt$id == 19, ], equation = "hs", k = 2))
  batch <- suppressWarnings(fit_demand_fixed(apt[apt$id %in% c(19, 30, 38), ], equation = "hs", k = 2))
  a_res <- alone$results
  b_res <- batch$results[batch$results$id == "19", ]
  rownames(a_res) <- NULL
  rownames(b_res) <- NULL
  num_cols <- names(a_res)[vapply(a_res, is.numeric, logical(1))]
  expect_identical(a_res[, num_cols], b_res[, num_cols])
  expect_identical(a_res$Notes, b_res$Notes)
})

# =============================================================================
# TICKET-058: ExtractCoefs.linear() lacks the try-error guard its sibling has
# =============================================================================

test_that("TICKET-058: linear batch degrades per-subject on fit failure", {
  skip_on_cran()
  good <- data.frame(
    id = "good", x = c(0.1, 0.5, 1, 3, 6, 12, 24),
    y = c(9.977, 9.885, 9.771, 9.32, 8.66, 7.42, 5.4)
  )
  bad <- data.frame(id = "bad", x = c(1, 10), y = c(5, 3))  # too few rows to fit
  res <- suppressWarnings(FitCurves(rbind(good, bad), equation = "linear"))
  expect_equal(nrow(res), 2L)
  expect_true(is.na(res$L[res$id == "bad"]))
  expect_false(is.na(res$L[res$id == "good"]))
})

test_that("converged linear fit reports the optimizer's own stopMessage as Notes (dead nls2 branch removed)", {
  skip_on_cran()
  # ExtractCoefs.linear() has no nlxb/nls2 fallback chain (FitCurves.linear()
  # only ever calls wrapnlsr() once), so `inherits(fit, "nls2")` was always
  # FALSE and the branch was dead code; Notes must always come from the
  # fit's own convInfo$stopMessage.
  good <- data.frame(
    id = "good", x = c(0.1, 0.5, 1, 3, 6, 12, 24),
    y = c(9.977, 9.885, 9.771, 9.32, 8.66, 7.42, 5.4)
  )
  res <- suppressWarnings(FitCurves(good, equation = "linear"))
  expect_false(grepl("reverted to nlxb", res$Notes))
  expect_true(nzchar(res$Notes))
})

test_that("TICKET-058: linear fit failure alone returns a 1-row NA/Notes result, no error", {
  skip_on_cran()
  bad <- data.frame(id = "bad", x = c(1, 10), y = c(5, 3))
  res <- suppressWarnings(FitCurves(bad, equation = "linear"))
  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$L))
  expect_true(is.character(res$Notes) && !is.na(res$Notes) && nzchar(res$Notes))
})
