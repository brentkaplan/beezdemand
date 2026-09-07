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

# A minimal stand-in carrying only the fields the screen reads. Building one
# per trigger keeps the threshold tests exact and fast.
.ki_stub <- function(log_k = log(2), alpha = 0.01, q0 = 10, price = 20,
                     hessian_pd = TRUE, log_k_bounds = NULL) {
  structure(
    list(
      model = list(coefficients = c(beta_q0 = log(q0),
                                    beta_alpha = log(alpha),
                                    log_k = log_k)),
      subject_pars = data.frame(Q0 = q0, alpha = alpha),
      data = data.frame(x = c(0, price)),
      hessian_pd = hessian_pd,
      param_info = list(has_k = TRUE, estimate_k = TRUE, x_var = "x",
                        log_k_bounds = log_k_bounds)
    ),
    class = "beezdemand_tmb"
  )
}

test_that("a healthy stub is not flagged", {
  ki <- .tmb_k_identification(.ki_stub())
  expect_identical(ki$severity, "none")
  expect_length(ki$reasons, 0L)
})

test_that("an implausible k is flagged", {
  expect_identical(.tmb_k_identification(.ki_stub(log_k = log(5e3)))$severity,
                   "warn")
  expect_identical(.tmb_k_identification(.ki_stub(log_k = log(1e-5)))$severity,
                   "warn")
  # Just inside the range is not flagged.
  expect_identical(.tmb_k_identification(.ki_stub(log_k = log(500)))$severity,
                   "none")
})

test_that("a flat decay exponent is flagged", {
  # alpha * Q0 * max(price) = 1e-4 * 10 * 20 = 0.02 < 0.05
  ki <- .tmb_k_identification(.ki_stub(alpha = 1e-4))
  expect_identical(ki$severity, "warn")
  expect_equal(ki$decay_exponent_max, 0.02)
  expect_true(any(grepl("decay exponent", ki$reasons)))

  # 3e-4 * 10 * 20 = 0.06 is above the threshold.
  expect_identical(.tmb_k_identification(.ki_stub(alpha = 3e-4))$severity,
                   "none")
})

test_that("a numerically degenerate alpha is flagged", {
  ki <- .tmb_k_identification(.ki_stub(alpha = 1e-16))
  expect_identical(ki$severity, "warn")
  expect_true(any(grepl("alpha", ki$reasons)))
})

test_that("log_k resting on a user-supplied bound is flagged", {
  ki <- .tmb_k_identification(
    .ki_stub(log_k = 4, log_k_bounds = c(lower = -2, upper = 4))
  )
  expect_identical(ki$severity, "warn")
  expect_identical(ki$at_boundary, "log_k")
  expect_true(any(grepl("bound", ki$reasons)))

  # Away from both bounds, nothing fires.
  ki2 <- .tmb_k_identification(
    .ki_stub(log_k = 1, log_k_bounds = c(lower = -2, upper = 4))
  )
  expect_identical(ki2$severity, "none")
  expect_length(ki2$at_boundary, 0L)

  # A zero bound is handled by the relative-tolerance comparison.
  ki3 <- .tmb_k_identification(
    .ki_stub(log_k = 0, alpha = 1, q0 = 1, log_k_bounds = c(lower = 0,
                                                            upper = Inf))
  )
  expect_identical(ki3$at_boundary, "log_k")
})

test_that("a non-PD Hessian alone is graded 'suspect', not 'warn'", {
  ki <- .tmb_k_identification(.ki_stub(hessian_pd = FALSE))
  expect_identical(ki$severity, "suspect")
  expect_true(any(grepl("positive definite", ki$reasons)))

  # Combined with a real trigger it does not soften the grade.
  ki2 <- .tmb_k_identification(.ki_stub(log_k = log(5e3), hessian_pd = FALSE))
  expect_identical(ki2$severity, "warn")
})

test_that("the screen falls back to the reference parameters when subject_pars is unusable", {
  # Continuous random-slope fits cache an all-NA subject_pars table; the screen
  # then evaluates the decay exponent at the reference Q0 and alpha
  # (0.01 * 10 * 20 = 2), rather than giving up.
  stub <- .ki_stub()
  stub$subject_pars <- data.frame(Q0 = NA_real_, alpha = NA_real_)
  ki <- .tmb_k_identification(stub)
  expect_equal(ki$decay_exponent_max, 2)
  expect_identical(ki$severity, "none")

  stub2 <- .ki_stub()
  stub2$subject_pars <- NULL
  expect_silent(ki2 <- .tmb_k_identification(stub2))
  expect_equal(ki2$decay_exponent_max, 2)
  expect_identical(ki2$severity, "none")

  # With no usable price column either, the decay check is skipped, not faked.
  stub3 <- .ki_stub()
  stub3$subject_pars <- NULL
  stub3$data <- data.frame(x = NA_real_)
  ki3 <- .tmb_k_identification(stub3)
  expect_true(is.na(ki3$decay_exponent_max))
  expect_identical(ki3$severity, "none")
})
