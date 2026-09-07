# Tests for the fixed-k default in fit_demand_tmb() and the free-k
# identification screen that backs it (F-BD4-1).
#
# Background: for the k-bearing equations the response depends on
# k * (exp(-alpha * Q0 * price) - 1). While alpha * Q0 * price stays small the
# curve is a straight line with slope k * alpha, so only that product is
# identified and a free k can drift arbitrarily far. `apt` fitted with
# `equation = "exponentiated"` is the worked example: free k walks to ~1e13.

# ---------------------------------------------------------------------------
# 1. The default
# ---------------------------------------------------------------------------

test_that("fit_demand_tmb() fixes k at 2 by default", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")

  fit <- fit_demand_tmb(apt, equation = "exponentiated", verbose = 0)

  expect_false(fit$param_info$estimate_k)
  expect_true(fit$param_info$has_k)
  expect_equal(fit$param_info$k_fixed, 2)
  expect_equal(unname(.tmb_get_k(fit)), 2)
  expect_false("log_k" %in% names(fit$opt$par))
  expect_false("log_k" %in% names(fit$model$coefficients))

  # The point of the change: the flagship example now converges.
  expect_true(fit$converged)
  expect_true(fit$hessian_pd)
})

test_that("the defaulted k is announced once, and only when verbose >= 1", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")

  # Count only the k announcement; verbose >= 1 emits other informational
  # messages that are not this test's business.
  k_messages <- function(expr) {
    msgs <- character(0)
    withCallingHandlers(
      force(expr),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    sum(grepl("fixed k = 2", msgs))
  }

  expect_equal(
    k_messages(fit_demand_tmb(apt, equation = "exponentiated", verbose = 1)),
    1L
  )
  expect_equal(
    k_messages(fit_demand_tmb(apt, equation = "exponentiated", verbose = 0)),
    0L
  )
  # An explicitly supplied k needs no announcement.
  expect_equal(
    k_messages(fit_demand_tmb(apt, equation = "exponentiated", k = 2,
                              verbose = 1)),
    0L
  )
  # Neither does an equation without k.
  expect_equal(
    k_messages(fit_demand_tmb(apt, equation = "simplified", verbose = 1)),
    0L
  )
})

# ---------------------------------------------------------------------------
# 2. The screen on real fits
# ---------------------------------------------------------------------------

test_that(".tmb_k_identification() returns NULL when k was not estimated", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")

  fit_fixed <- fit_demand_tmb(apt, equation = "exponentiated", verbose = 0)
  expect_null(.tmb_k_identification(fit_fixed))

  fit_nok <- fit_demand_tmb(apt, equation = "simplified", verbose = 0)
  expect_null(.tmb_k_identification(fit_nok))
})

test_that("a free-k fit on data that never reaches its floor is flagged", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")

  fit <- suppressWarnings(suppressMessages(
    fit_demand_tmb(apt, equation = "exponentiated", estimate_k = TRUE,
                   verbose = 0)
  ))

  ki <- .tmb_k_identification(fit)
  expect_type(ki, "list")
  expect_identical(ki$severity, "warn")
  expect_gt(ki$k, 1e3)
  expect_lt(ki$decay_exponent_max, 0.05)
  expect_gte(length(ki$reasons), 2L)

  diag <- suppressWarnings(check_demand_model(fit))
  expect_true(any(grepl("k is not identified", diag$issues)))
  expect_true(any(grepl("estimate_k = FALSE", diag$recommendations)))
  expect_identical(diag$boundary$k_identification$severity, "warn")

  out <- paste(utils::capture.output(print(diag)), collapse = "\n")
  expect_match(out, "k is not identified")

  notes <- suppressWarnings(summary(fit))$notes
  expect_true(any(grepl("k is not identified", notes)))
})

test_that("a free-k fit whose data do identify k is not flagged", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")

  # Same data, exponential form: the decay exponent reaches ~0.8 over the
  # observed prices, the fit converges with a PD Hessian, and k lands near 2.4.
  fit <- fit_demand_tmb(apt, equation = "exponential", estimate_k = TRUE,
                        verbose = 0)

  ki <- .tmb_k_identification(fit)
  expect_identical(ki$severity, "none")
  expect_length(ki$reasons, 0L)
  expect_length(ki$at_boundary, 0L)

  diag <- check_demand_model(fit)
  expect_false(any(grepl("identified", diag$issues)))
  expect_length(diag$boundary$at_boundary, 0L)
})

# ---------------------------------------------------------------------------
# 3. The triggers, deterministically
# ---------------------------------------------------------------------------

# A minimal stand-in carrying the fields the screen reads. It goes through the
# real design-matrix path (`.tmb_row_decay()`), so the decay exponent is
# computed per row exactly as it is for a fitted object.
.ki_stub <- function(log_k = log(2), alpha = 0.01, q0 = 10, prices = c(0, 20),
                     hessian_pd = TRUE, log_k_bounds = NULL, x_var = "x") {
  n <- length(prices)
  d <- data.frame(x = prices)
  names(d) <- x_var
  structure(
    list(
      model = list(coefficients = c(beta_q0 = log(q0),
                                    beta_alpha = log(alpha),
                                    log_k = log_k)),
      subject_pars = data.frame(Q0 = q0, alpha = alpha),
      data = d,
      formula_details = list(X_q0 = matrix(1, n, 1), X_alpha = matrix(1, n, 1)),
      hessian_pd = hessian_pd,
      param_info = list(has_k = TRUE, estimate_k = TRUE, x_var = x_var,
                        log_k_bounds = log_k_bounds)
    ),
    class = "beezdemand_tmb"
  )
}

# Two subjects x two conditions, with the fixed-effect design carrying the
# condition contrast. Mirrors the shape of a factor fit, where a single
# reference-level alpha would misrepresent the model.
.ki_stub_factor <- function(q0 = 10, alpha_ref = 1e-4, alpha_other = 1e-2,
                            prices = c(0, 20)) {
  n <- length(prices)
  X_int <- cbind(1, rep(0, n))       # reference level
  X_oth <- cbind(1, rep(1, n))       # second level
  X_alpha <- rbind(X_int, X_oth)
  X_q0 <- cbind(rep(1, 2 * n))
  structure(
    list(
      model = list(coefficients = c(beta_q0 = log(q0),
                                    beta_alpha = log(alpha_ref),
                                    beta_alpha = log(alpha_other / alpha_ref),
                                    log_k = log(2))),
      subject_pars = data.frame(Q0 = q0, alpha = alpha_ref),
      data = data.frame(x = rep(prices, 2)),
      formula_details = list(X_q0 = X_q0, X_alpha = X_alpha),
      hessian_pd = TRUE,
      param_info = list(has_k = TRUE, estimate_k = TRUE, x_var = "x",
                        log_k_bounds = NULL)
    ),
    class = "beezdemand_tmb"
  )
}

test_that("a healthy stub is not flagged", {
  ki <- .tmb_k_identification(.ki_stub())
  expect_identical(ki$severity, "none")
  expect_length(ki$reasons, 0L)
  expect_equal(ki$decay_exponent_max, 2)
  expect_equal(ki$alpha_min, 0.01)
})

test_that("an implausible k is flagged as 'warn'", {
  expect_identical(.tmb_k_identification(.ki_stub(log_k = log(5e3)))$severity,
                   "warn")
  expect_identical(.tmb_k_identification(.ki_stub(log_k = log(1e-5)))$severity,
                   "warn")
  # Just inside the range is not flagged.
  expect_identical(.tmb_k_identification(.ki_stub(log_k = log(500)))$severity,
                   "none")
  expect_identical(.tmb_k_identification(.ki_stub(log_k = log(2e-3)))$severity,
                   "none")
})

test_that("a flat decay exponent is flagged as 'warn'", {
  # alpha * Q0 * max(price) = 1e-4 * 10 * 20 = 0.02 < 0.05
  ki <- .tmb_k_identification(.ki_stub(alpha = 1e-4))
  expect_identical(ki$severity, "warn")
  expect_equal(ki$decay_exponent_max, 0.02)
  expect_true(any(grepl("decay exponent", ki$reasons)))

  # 3e-4 * 10 * 20 = 0.06 is above the threshold.
  expect_identical(.tmb_k_identification(.ki_stub(alpha = 3e-4))$severity,
                   "none")
})

test_that("the decay exponent uses each row's own price, not the global maximum", {
  # One row at price 200 with a tiny alpha, one at price 2 with a larger one:
  # every row's exponent is 0.02, though alpha_max * price_max would be 2.
  stub <- .ki_stub(alpha = 1e-5, q0 = 10, prices = c(200))
  stub$model$coefficients <- c(beta_q0 = log(10), beta_q0 = 0,
                               beta_alpha = log(1e-5), beta_alpha = log(100),
                               log_k = log(2))
  stub$data <- data.frame(x = c(200, 2))
  stub$formula_details <- list(
    X_q0 = cbind(1, c(0, 1)),
    X_alpha = cbind(1, c(0, 1))
  )
  ki <- .tmb_k_identification(stub)
  expect_equal(ki$decay_exponent_max, 0.02)
  expect_identical(ki$severity, "warn")
})

test_that("the decay exponent sees non-reference factor levels", {
  # Reference alpha alone gives 0.02 (flagged); the second level reaches 2.
  ki <- .tmb_k_identification(.ki_stub_factor())
  expect_equal(ki$decay_exponent_max, 2)
  expect_identical(ki$severity, "none")
})

test_that("a custom price column is honoured", {
  ki <- .tmb_k_identification(.ki_stub(alpha = 1e-4, x_var = "cost"))
  expect_equal(ki$decay_exponent_max, 0.02)
  expect_identical(ki$severity, "warn")
})

test_that("a numerically degenerate alpha alone is graded 'suspect'", {
  # Q0 * price is large enough that the decay check stays clear, isolating the
  # alpha trigger: 1e-9 * 1e6 * 100 = 100.
  ki <- .tmb_k_identification(.ki_stub(alpha = 1e-9, q0 = 1e6,
                                       prices = c(0, 100)))
  expect_identical(ki$severity, "suspect")
  expect_length(ki$reasons, 1L)
  expect_true(any(grepl("smallest fitted alpha", ki$reasons)))

  # Just above the threshold, nothing fires.
  ki2 <- .tmb_k_identification(.ki_stub(alpha = 1e-7, q0 = 1e6,
                                        prices = c(0, 100)))
  expect_identical(ki2$severity, "none")
})

test_that("log_k resting on a user-supplied bound is graded 'suspect'", {
  ki <- .tmb_k_identification(
    .ki_stub(log_k = 4, log_k_bounds = c(lower = -2, upper = 4))
  )
  expect_identical(ki$severity, "suspect")
  expect_identical(ki$at_boundary, "log_k")
  expect_true(any(grepl("bound", ki$reasons)))

  # Away from both bounds, nothing fires.
  ki2 <- .tmb_k_identification(
    .ki_stub(log_k = 1, log_k_bounds = c(lower = -2, upper = 4))
  )
  expect_identical(ki2$severity, "none")
  expect_length(ki2$at_boundary, 0L)

  # Just outside the relative tolerance is not "on" the bound.
  ki3 <- .tmb_k_identification(
    .ki_stub(log_k = 4 - 1e-3, log_k_bounds = c(lower = -2, upper = 4))
  )
  expect_length(ki3$at_boundary, 0L)

  # A zero bound is handled by the relative-tolerance comparison.
  ki4 <- .tmb_k_identification(
    .ki_stub(log_k = 0, log_k_bounds = c(lower = 0, upper = Inf))
  )
  expect_identical(ki4$at_boundary, "log_k")

  # Infinite endpoints never count as a bound.
  ki5 <- .tmb_k_identification(
    .ki_stub(log_k = 1, log_k_bounds = c(lower = -Inf, upper = Inf))
  )
  expect_length(ki5$at_boundary, 0L)
})

test_that("a non-PD Hessian alone is graded 'suspect', not 'warn'", {
  ki <- .tmb_k_identification(.ki_stub(hessian_pd = FALSE))
  expect_identical(ki$severity, "suspect")
  expect_true(any(grepl("positive definite", ki$reasons)))

  # A failed sdreport leaves hessian_pd as NA; that is not a finding.
  ki_na <- .tmb_k_identification(.ki_stub(hessian_pd = NA))
  expect_identical(ki_na$severity, "none")

  # Combined with a strong trigger it does not soften the grade.
  ki2 <- .tmb_k_identification(.ki_stub(log_k = log(5e3), hessian_pd = FALSE))
  expect_identical(ki2$severity, "warn")
  expect_length(ki2$reasons, 2L)
})

test_that("the decay check is skipped, not faked, when the design is unusable", {
  stub <- .ki_stub()
  stub$formula_details <- NULL
  stub$data <- NULL
  ki <- .tmb_k_identification(stub)
  expect_true(is.na(ki$decay_exponent_max))
  expect_true(is.na(ki$alpha_min))
  expect_identical(ki$severity, "none")

  # A design whose width does not match the coefficient vector is refused
  # rather than silently recycled.
  stub2 <- .ki_stub()
  stub2$formula_details$X_alpha <- matrix(1, nrow(stub2$formula_details$X_q0), 2)
  expect_null(.tmb_row_decay(stub2))
  expect_true(is.na(.tmb_k_identification(stub2)$decay_exponent_max))
})
