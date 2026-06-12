# TICKET-046: classed, deduplicated sdreport warnings ----------------------------
#
# TMB's summary.sdreport() takes sqrt() of (possibly negative) variance
# estimates, leaking raw "NaNs produced" warnings on weakly identified fits.
# The extraction paths wrap the whole sdreport + summary region in
# .tmb_quiet_sdreport(): sqrt-call warnings are muffled and replaced by AT
# MOST ONE classed warning per fit (beezdemand_sdreport_warning /
# beezdemand_warning); anything else propagates untouched.

test_that(".tmb_quiet_sdreport muffles sqrt warnings into one classed warning", {
  # multiple sqrt warnings inside the region -> exactly one classed warning,
  # value passes through
  warns <- list()
  out <- withCallingHandlers(
    beezdemand:::.tmb_quiet_sdreport({
      sqrt(-1)
      sqrt(-2)
      42
    }),
    warning = function(w) {
      warns[[length(warns) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(out, 42)
  classed <- vapply(warns, inherits, logical(1), "beezdemand_sdreport_warning")
  expect_identical(sum(classed), 1L)
  expect_true(all(vapply(warns[classed], inherits, logical(1), "beezdemand_warning")))
  expect_false(any(grepl(
    "NaNs produced",
    vapply(warns, conditionMessage, character(1)),
    fixed = TRUE
  )))
})

test_that(".tmb_quiet_sdreport dedupes across a warn-error-retry fallback chain", {
  # mimics the sdreport tryCatch fallback: first attempt warns then errors,
  # the retry warns then succeeds -- still exactly one classed warning
  warns <- list()
  out <- withCallingHandlers(
    beezdemand:::.tmb_quiet_sdreport({
      tryCatch(
        {
          sqrt(-1)
          stop("boom")
        },
        error = function(e) NULL
      )
      sqrt(-1)
      "ok"
    }),
    warning = function(w) {
      warns[[length(warns) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(out, "ok")
  classed <- vapply(warns, inherits, logical(1), "beezdemand_sdreport_warning")
  expect_identical(sum(classed), 1L)
})

test_that("two regions sharing one guard emit exactly one classed warning", {
  # the hurdle fit path wraps two separate regions (the sdreport fallback
  # chain and the summary extraction) with one shared guard env
  # (Codex 042-diff R2)
  guard <- new.env(parent = emptyenv())
  warns <- list()
  withCallingHandlers(
    {
      beezdemand:::.tmb_quiet_sdreport(sqrt(-1), guard = guard)
      beezdemand:::.tmb_quiet_sdreport(sqrt(-1), guard = guard)
    },
    warning = function(w) {
      warns[[length(warns) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  classed <- vapply(warns, inherits, logical(1), "beezdemand_sdreport_warning")
  expect_identical(sum(classed), 1L)

  # independent guards (separate fits) each emit their own warning
  warns2 <- list()
  withCallingHandlers(
    {
      beezdemand:::.tmb_quiet_sdreport(sqrt(-1))
      beezdemand:::.tmb_quiet_sdreport(sqrt(-1))
    },
    warning = function(w) {
      warns2[[length(warns2) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  classed2 <- vapply(warns2, inherits, logical(1), "beezdemand_sdreport_warning")
  expect_identical(sum(classed2), 2L)
})

test_that(".tmb_quiet_sdreport passes unrelated warnings through untouched", {
  warns <- list()
  out <- withCallingHandlers(
    beezdemand:::.tmb_quiet_sdreport({
      warning("unrelated diagnostic")
      7
    }),
    warning = function(w) {
      warns[[length(warns) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(out, 7)
  expect_identical(length(warns), 1L)
  expect_match(conditionMessage(warns[[1]]), "unrelated diagnostic")
  expect_false(inherits(warns[[1]], "beezdemand_sdreport_warning"))
})

test_that("weakly identified TMB fits emit one classed warning, no raw NaNs text", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  # 2 subjects x 4 prices with a 2-RE model: produces negative variance
  # estimates (sqrt-NaN warnings in summary.sdreport) on most platforms.
  # Whether the pathology materializes depends on platform numerics
  # (BLAS/compiler); when the fit happens to converge cleanly there is
  # nothing to classify, so the assertions are conditional. The helper's
  # unit tests above pin the muffle/classify/dedupe contract everywhere.
  d <- expand.grid(id = factor(1:2), x = c(0.1, 1, 5, 20))
  d$y <- c(10, 9, 0.5, 0.1, 10.2, 8.8, 0.6, 0.05)

  warns <- list()
  fit <- withCallingHandlers(
    fit_demand_tmb(
      d,
      equation = "exponential",
      random_effects = c("q0", "alpha"),
      verbose = 0
    ),
    warning = function(w) {
      warns[[length(warns) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  msgs <- vapply(warns, conditionMessage, character(1))
  classed <- vapply(warns, inherits, logical(1), "beezdemand_sdreport_warning")
  raw_nan <- grepl("NaNs produced", msgs, fixed = TRUE)

  # raw NaN warnings must NEVER reach the user, pathology or not
  expect_false(any(raw_nan))

  skip_if(
    sum(classed) == 0L,
    "platform numerics did not produce the negative-variance pathology"
  )
  expect_identical(sum(classed), 1L)
})

test_that("healthy fits emit zero sdreport warnings (no false positives)", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  set.seed(5)
  d <- expand.grid(id = factor(1:3), x = c(0.1, 0.5, 2, 10))
  d$y <- pmax(0.1, 10 * exp(-0.4 * d$x) + rnorm(12, 0, 3))

  expect_no_warning(
    fit <- fit_demand_tmb(d, equation = "exponential", verbose = 0)
  )
  expect_true(fit$hessian_pd)
})
