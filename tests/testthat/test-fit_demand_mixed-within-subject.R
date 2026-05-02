# =============================================================================
# TICKET-011 Phase 2.1: NLME parity baseline for within-subject random-effects
# specifications. Once Phase 2.5 lands, fit_demand_tmb() must match these
# loglik / coefficient values to optimizer tolerance on the same data.
#
# Phase 2 acceptance specs:
#   pdDiag(Q0+alpha~1)         -- 2 RE intercepts, diagonal cov
#   pdSymm(Q0+alpha~1)         -- 2 RE intercepts, free correlation
#   pdDiag(Q0+alpha~condition) -- per-condition slopes, diagonal cov
#   pdSymm(Q0+alpha~condition) -- per-condition slopes, free correlation
#
# This file establishes the NLME side; Phase 2.6 adds the TMB-side
# equivalence tests against the same simulated dataset.
# =============================================================================

test_that(".simulate_within_subject_demand returns expected long-format shape", {
  skip_on_cran()
  dat <- .simulate_within_subject_demand(
    n_subjects = 10,
    n_conditions = 3,
    prices = c(0.1, 1, 10),
    seed = 42
  )

  expect_s3_class(dat, "tbl_df")
  expect_named(dat, c("id", "condition", "x", "y"))
  expect_equal(nrow(dat), 10 * 3 * 3)  # n_subjects * n_conditions * n_prices

  # condition varies within id: every subject sees all conditions.
  conds_per_subject <- tapply(
    as.character(dat$condition), dat$id,
    function(x) length(unique(x))
  )
  expect_true(all(conds_per_subject == 3L))

  # All consumption values positive (multiplicative log-normal noise).
  expect_true(all(dat$y > 0))
})

test_that("fit_demand_mixed converges on pdDiag(Q0+alpha~1) within-subject data", {
  skip_on_cran()
  dat <- .simulate_within_subject_demand(
    n_subjects = 25, n_conditions = 3, seed = 101
  )
  dat$y_ll4 <- ll4(dat$y)

  fit <- suppressMessages(fit_demand_mixed(
    data = dat,
    y_var = "y_ll4",
    x_var = "x",
    id_var = "id",
    equation_form = "zben",
    random_effects = nlme::pdDiag(Q0 + alpha ~ 1),
    verbose = FALSE
  ))
  expect_s3_class(fit, "beezdemand_nlme")
  expect_true(is.finite(stats::logLik(fit$model)))
})

test_that("fit_demand_mixed converges on pdSymm(Q0+alpha~1) within-subject data", {
  skip_on_cran()
  dat <- .simulate_within_subject_demand(
    n_subjects = 25, n_conditions = 3, seed = 102
  )
  dat$y_ll4 <- ll4(dat$y)

  fit <- suppressMessages(fit_demand_mixed(
    data = dat,
    y_var = "y_ll4",
    x_var = "x",
    id_var = "id",
    equation_form = "zben",
    random_effects = nlme::pdSymm(Q0 + alpha ~ 1),
    verbose = FALSE
  ))
  expect_s3_class(fit, "beezdemand_nlme")
  expect_true(is.finite(stats::logLik(fit$model)))
})

test_that("fit_demand_mixed converges on pdDiag(Q0+alpha~condition) within-subject data", {
  skip_on_cran()
  dat <- .simulate_within_subject_demand(
    n_subjects = 25, n_conditions = 3,
    delta_q0 = c(0, 0.3, -0.2),
    delta_alpha = c(0, 0.1, -0.1),
    seed = 103
  )
  dat$y_ll4 <- ll4(dat$y)

  fit <- suppressMessages(fit_demand_mixed(
    data = dat,
    y_var = "y_ll4",
    x_var = "x",
    id_var = "id",
    equation_form = "zben",
    factors = "condition",
    random_effects = nlme::pdDiag(Q0 + alpha ~ condition),
    verbose = FALSE
  ))
  expect_s3_class(fit, "beezdemand_nlme")
  expect_true(is.finite(stats::logLik(fit$model)))
})

test_that("fit_demand_mixed converges on pdSymm(Q0+alpha~condition) within-subject data", {
  skip_on_cran()
  dat <- .simulate_within_subject_demand(
    n_subjects = 25, n_conditions = 3,
    delta_q0 = c(0, 0.3, -0.2),
    delta_alpha = c(0, 0.1, -0.1),
    seed = 104
  )
  dat$y_ll4 <- ll4(dat$y)

  fit <- suppressMessages(fit_demand_mixed(
    data = dat,
    y_var = "y_ll4",
    x_var = "x",
    id_var = "id",
    equation_form = "zben",
    factors = "condition",
    random_effects = nlme::pdSymm(Q0 + alpha ~ condition),
    verbose = FALSE
  ))
  expect_s3_class(fit, "beezdemand_nlme")
  expect_true(is.finite(stats::logLik(fit$model)))
})
