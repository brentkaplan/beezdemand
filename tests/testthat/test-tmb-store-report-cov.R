# Tests for `store_report_cov`: by default fit_demand_tmb() no longer
# materializes the full ADREPORT covariance ($sdr$cov), which is read by NO
# method (~88% of object size on large fits). `store_report_cov = TRUE`
# restores it. A lean fit and its store_report_cov = TRUE twin must therefore
# be identical on every public surface except the $sdr$cov slot.
#
# Fixtures use multi_start = FALSE so the optimization is deterministic: the
# lean and full fits share a byte-identical `opt`, differing only in the
# post-opt sdreport() call. That guarantees exact coef/vcov/etc. parity by
# construction (no RNG, no convergence-path divergence).

.src_cache <- new.env(parent = emptyenv())

.src_lean <- function() {
  if (is.null(.src_cache$lean)) {
    data(apt, package = "beezdemand")
    .src_cache$lean <- fit_demand_tmb(
      apt, y_var = "y", x_var = "x", id_var = "id",
      equation = "exponential", multi_start = FALSE, verbose = 0)
  }
  .src_cache$lean
}

.src_full <- function() {
  if (is.null(.src_cache$full)) {
    data(apt, package = "beezdemand")
    .src_cache$full <- fit_demand_tmb(
      apt, y_var = "y", x_var = "x", id_var = "id",
      equation = "exponential", multi_start = FALSE, verbose = 0,
      store_report_cov = TRUE)
  }
  .src_cache$full
}

# Factor-stratified cap-5 apt_full fixture (mirrors test-fit_demand_tmb.R):
# Female/Male capped at 5 subjects, the rare "Would rather not say" level kept
# whole, so the 3-level gender factor stays populated on ~200 rows. Used for
# the object-size guard (more ADREPORT'd quantities => a non-trivial $sdr$cov)
# and for the boot_demand / comparisons smokes (need >= 2 factor levels).
.src_gender_data <- function() {
  if (is.null(.src_cache$gdata)) {
    data(apt_full, package = "beezdemand")
    g <- as.factor(apt_full$gender)
    keep <- unlist(lapply(levels(g), function(lv) {
      ids <- unique(apt_full$id[g == lv])
      head(ids[order(ids)], 5L)
    }))
    d <- apt_full[apt_full$id %in% keep, , drop = FALSE]
    d$gender <- droplevels(as.factor(d$gender))
    .src_cache$gdata <- d
  }
  .src_cache$gdata
}

.src_lean_gender <- function() {
  if (is.null(.src_cache$lean_g)) {
    .src_cache$lean_g <- fit_demand_tmb(
      .src_gender_data(), y_var = "y", x_var = "x", id_var = "id",
      equation = "exponential", factors = "gender",
      multi_start = FALSE, verbose = 0)
  }
  .src_cache$lean_g
}

.src_full_gender <- function() {
  if (is.null(.src_cache$full_g)) {
    .src_cache$full_g <- fit_demand_tmb(
      .src_gender_data(), y_var = "y", x_var = "x", id_var = "id",
      equation = "exponential", factors = "gender",
      multi_start = FALSE, verbose = 0, store_report_cov = TRUE)
  }
  .src_cache$full_g
}


test_that("default fit does not materialize the full ADREPORT covariance", {
  # $sdr$cov is a scalar NA (TMB's getReportCovariance = FALSE), not a matrix.
  expect_false(is.matrix(.src_lean()$sdr$cov))
})

test_that("store_report_cov = TRUE materializes $sdr$cov as the full matrix", {
  full <- .src_full()
  expect_true(is.matrix(full$sdr$cov))
  # It is the full report covariance: square, dimension = number of
  # ADREPORT'd values.
  expect_identical(nrow(full$sdr$cov), ncol(full$sdr$cov))
  expect_identical(nrow(full$sdr$cov), length(full$sdr$value))
})

test_that("lean and store_report_cov fits agree on all inference surfaces", {
  lean <- .src_lean()
  full <- .src_full()
  # Same optimization (multi_start = FALSE) => bit-identical estimates.
  expect_identical(coef(lean), coef(full))
  expect_identical(as.numeric(logLik(lean)), as.numeric(logLik(full)))
  expect_identical(AIC(lean), AIC(full))
  expect_identical(BIC(lean), BIC(full))
  # cov.fixed (the kept slot) and everything derived from it.
  expect_equal(vcov(lean), vcov(full))
  expect_equal(tidy(lean), tidy(full))
  expect_equal(glance(lean), glance(full))
  expect_equal(predict(lean), predict(full), tolerance = 1e-8)
})

test_that("variance components match within tolerance", {
  # summary(sdr, 'report') marginal SDs: full-cov diagonal vs FALSE path may
  # differ at floating precision, so compare with tolerance (not identity).
  expect_equal(
    .src_lean()$model$variance_components,
    .src_full()$model$variance_components,
    tolerance = 1e-6
  )
})

test_that("confint(method='simulate') is identical for lean and full fits", {
  # Parametric draws use vcov() = cov.fixed (kept), so a fixed seed yields
  # identical intervals regardless of store_report_cov.
  ci_lean <- confint(.src_lean(), method = "simulate", R = 100, seed = 42)
  ci_full <- confint(.src_full(), method = "simulate", R = 100, seed = 42)
  expect_equal(ci_lean, ci_full)
})

test_that("a lean fit retains every sdreport piece any method needs", {
  # Version hedge (TMB >= 1.9.0): prove getReportCovariance = FALSE still
  # returns cov.fixed, marginal SDs, pdHess, and working report/random
  # summaries on whatever TMB the CI matrix runs.
  lean <- .src_lean()
  expect_true(is.matrix(lean$sdr$cov.fixed))
  expect_true(is.numeric(lean$sdr$sd) && length(lean$sdr$sd) > 0)
  expect_true(is.logical(lean$sdr$pdHess))
  expect_no_error(summary(lean$sdr, "report"))
  expect_no_error(summary(lean$sdr, "random"))
})

test_that("store_report_cov round-trips through the stored call and update()", {
  data(apt, package = "beezdemand")
  # Not passed -> absent from the stored call (default FALSE).
  expect_false("store_report_cov" %in% names(.src_lean()$call))
  # Passed explicitly -> retained by match.call().
  expect_true("store_report_cov" %in% names(.src_full()$call))
  # update() can re-materialize the covariance on demand.
  upgraded <- update(.src_lean(), store_report_cov = TRUE)
  expect_true(is.matrix(upgraded$sdr$cov))
})

test_that("public methods work on a lean (store_report_cov = FALSE) fit", {
  lean <- .src_lean()
  expect_no_error(VarCorr(lean))
  expect_no_error(logLik(lean))
  expect_no_error(AIC(lean))
  expect_no_error(BIC(lean))
  expect_no_error(summary(lean))
  # group_by = "parameter": the apt fit is intercept-only, so "auto" has no
  # non-intercept term to test; the point here is that anova() (which reads
  # vcov() = cov.fixed) runs on a lean fit, not the grouping default.
  expect_no_error(anova(lean, group_by = "parameter"))
  expect_no_error(vcov(lean))
})

test_that("boot_demand and get_demand_comparisons work on a lean fit", {
  expect_no_error(suppressWarnings(
    boot_demand(.src_lean_gender(), R = 100, seed = 1)
  ))
  expect_no_error(suppressMessages(
    get_demand_comparisons(.src_lean_gender(), param = "alpha")
  ))
})

test_that("dropping $sdr$cov yields a smaller stored object", {
  lean <- .src_lean_gender()
  full <- .src_full_gender()
  expect_true(is.matrix(full$sdr$cov))
  expect_false(is.matrix(lean$sdr$cov))
  # The only difference between the two fits is the $sdr$cov slot, so the
  # byte savings are essentially the size of that dropped matrix. This is
  # robust on small fixtures (no dependence on cov dominating the whole
  # object). On large real datasets the matrix is ~88% of the object.
  sz_lean <- as.numeric(object.size(lean))
  sz_full <- as.numeric(object.size(full))
  sz_cov  <- as.numeric(object.size(full$sdr$cov))
  expect_lt(sz_lean, sz_full)
  expect_gt(sz_full - sz_lean, 0.8 * sz_cov)
})
