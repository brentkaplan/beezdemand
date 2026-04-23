# Phase 0.1 of TICKET-011: guard subject_pars against within-id varying
# design columns. Row-order dependence in .tmb_compute_subject_pars() was
# flagged by adversarial review.

test_that("subject_pars warns and NAs Q0/alpha when a covariate varies within id", {
  data(apt, package = "beezdemand")

  set.seed(42)
  apt_within <- apt
  apt_within$within_cov <- stats::rnorm(nrow(apt_within))

  expect_warning(
    fit <- fit_demand_tmb(
      apt_within,
      equation = "simplified",
      continuous_covariates = "within_cov",
      verbose = 0
    ),
    regexp = "varies within"
  )

  spars <- get_subject_pars(fit)
  expect_true(all(is.na(spars$Q0)))
  expect_true(all(is.na(spars$alpha)))
  expect_true(all(is.na(spars$Pmax)))
  expect_true(all(is.na(spars$Omax)))
  expect_true(all(!is.na(spars$b_i)))
})

test_that("subject_pars is populated when all covariates are between-subjects", {
  data(apt, package = "beezdemand")

  set.seed(43)
  apt_between <- apt
  per_id_val <- stats::setNames(
    stats::rnorm(length(unique(apt_between$id))),
    unique(apt_between$id)
  )
  apt_between$between_cov <- per_id_val[as.character(apt_between$id)]

  fit <- fit_demand_tmb(
    apt_between,
    equation = "simplified",
    continuous_covariates = "between_cov",
    verbose = 0
  )

  spars <- get_subject_pars(fit)
  expect_true(all(is.finite(spars$Q0)))
  expect_true(all(is.finite(spars$alpha)))
})

test_that("validate_subject_pars = FALSE disables the within-id check", {
  data(apt, package = "beezdemand")

  set.seed(44)
  apt_within <- apt
  apt_within$within_cov <- stats::rnorm(nrow(apt_within))

  fit <- suppressWarnings(fit_demand_tmb(
    apt_within,
    equation = "simplified",
    continuous_covariates = "within_cov",
    validate_subject_pars = FALSE,
    verbose = 0
  ))

  spars <- get_subject_pars(fit)
  expect_true(all(is.finite(spars$Q0)))
})
