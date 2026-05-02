# =============================================================================
# Tests for calc_group_metrics.beezdemand_tmb()
#
# TICKET-011 Phase 0.5: For covariate-adjusted fits, the function returns
# Q0/alpha derived from the *intercept-only* coefficients (i.e., the
# covariate=0 reference subject), and `summary.beezdemand_tmb()` consumes
# this unconditionally. Codex adversarial review (round 3, promoted to
# HIGH severity in round 4) flagged this as silent misreporting of core
# derived outcomes (Pmax/Omax/Qmax) on the standard summary path.
#
# Phase 0.5 fix: warn + label (matches the predict(type='demand') warning
# convention, commit b61aeec). Phase 5 will replace warn-and-label with
# explicit `at` conditioning or proper marginalization, reusing the
# .tmb_build_emm_ref_grid() helper from Phase 0.4.
# =============================================================================

helper_subsample_apt_full <- function(n_per_group = 25) {
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ids_g <- unique(d$id[d$gender == g])
    head(ids_g[order(ids_g)], n_per_group)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  d
}

test_that("calc_group_metrics warns on covariate-adjusted TMB fits", {
  skip_on_cran()
  d <- helper_subsample_apt_full()
  fit <- fit_demand_tmb(
    d,
    equation = "exponential",
    continuous_covariates = "age",
    verbose = 0
  )

  expect_warning(
    metrics <- calc_group_metrics(fit),
    regexp = "covariates? held at 0|covariate=0|conditioned"
  )
  expect_true("conditioned_on" %in% names(metrics))
  expect_equal(metrics$conditioned_on$covariates[["age"]], 0)
})

test_that("calc_group_metrics is silent for no-covariate TMB fits", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt,
    equation = "exponential",
    verbose = 0
  )

  expect_no_warning(metrics <- calc_group_metrics(fit))
  # No covariates -> no conditioning to label.
  expect_true(
    is.null(metrics$conditioned_on) ||
      length(metrics$conditioned_on$covariates) == 0L
  )
})

test_that("summary.beezdemand_tmb propagates calc_group_metrics warning for covariate fits", {
  skip_on_cran()
  d <- helper_subsample_apt_full()
  fit <- fit_demand_tmb(
    d,
    equation = "exponential",
    continuous_covariates = "age",
    verbose = 0
  )

  # Codex round 3 specifically flagged this propagation path: summary()
  # consumes calc_group_metrics() unconditionally so the same silent
  # misreporting lands in the standard summary output.
  expect_warning(
    capture.output(summary(fit)),
    regexp = "covariates? held at 0|covariate=0|conditioned"
  )
})
