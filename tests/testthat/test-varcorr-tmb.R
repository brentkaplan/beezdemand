# TICKET-021: VarCorr.beezdemand_tmb() shim for nlme/lme4 familiarity.
#
# VarCorr(fit_tmb) surfaces the random-effect variance components already
# computed by summary(fit)$variance_components / $correlations in the
# matrix-like shape produced by nlme::VarCorr.lme() (columns Variance,
# StdDev, optionally Corr; final row "Residual").
#
# The cross-backend tests use the matched pdDiag pair on `apt` proven by
# test-variance-component-scale.R. The ticket draft's apt_full + factors =
# "id_group" recipe was invalid: apt_full has no id_group column, a
# between-subjects factor (e.g. gender) expands fixed effects rather than the
# random effects, and a pdSymm NLME fit on the small `apt` data is singular.

test_that("VarCorr.beezdemand_tmb returns a VarCorr.lme-shaped object", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  vc <- VarCorr(fit)

  expect_true(inherits(vc, "VarCorr.lme") || is.matrix(vc))
  expect_true(all(c("Variance", "StdDev") %in% colnames(vc)))
  expect_true("Residual" %in% rownames(vc))
})

test_that("VarCorr.beezdemand_tmb matches nlme VarCorr shape on a matched fit", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y, lambda = 4)

  fit_tmb <- fit_demand_tmb(
    apt, equation = "zben", y_var = "y_ll4",
    covariance_structure = "pdDiag", verbose = 0
  )
  fit_nlme <- suppressWarnings(suppressMessages(fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben", param_space = "log10",
    covariance_structure = "pdDiag"
  )))
  skip_if(is.null(fit_nlme$model), "NLME comparison fit did not converge")

  vc_tmb  <- VarCorr(fit_tmb)
  vc_nlme <- nlme::VarCorr(fit_nlme$model)

  expect_equal(nrow(vc_tmb), nrow(vc_nlme))
  expect_true(all(c("Variance", "StdDev") %in% colnames(vc_tmb)))
  expect_true("Residual" %in% rownames(vc_tmb))
})

test_that("VarCorr.beezdemand_tmb StdDev agrees with nlme within 5%", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y, lambda = 4)

  fit_tmb <- fit_demand_tmb(
    apt, equation = "zben", y_var = "y_ll4",
    covariance_structure = "pdDiag", verbose = 0
  )
  fit_nlme <- suppressWarnings(suppressMessages(fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben", param_space = "log10",
    covariance_structure = "pdDiag"
  )))
  skip_if(is.null(fit_nlme$model), "NLME comparison fit did not converge")

  vc_tmb  <- VarCorr(fit_tmb)
  vc_nlme <- nlme::VarCorr(fit_nlme$model)

  # RE rows are every row except the final Residual row; compare positionally
  # (both backends order the rows Q0, alpha, Residual).
  sd_tmb  <- suppressWarnings(as.numeric(vc_tmb[, "StdDev"]))
  sd_nlme <- suppressWarnings(as.numeric(vc_nlme[, "StdDev"]))
  re_tmb  <- sd_tmb[seq_len(length(sd_tmb) - 1L)]
  re_nlme <- sd_nlme[seq_len(length(sd_nlme) - 1L)]

  expect_equal(length(re_tmb), length(re_nlme))
  expect_equal(re_tmb, re_nlme, tolerance = 0.05)
})

test_that("VarCorr.beezdemand_tmb exposes a Corr column on a 2-RE pdSymm fit", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, equation = "exponential",
    covariance_structure = "pdSymm", verbose = 0
  )

  vc <- VarCorr(fit)

  expect_true("Corr" %in% colnames(vc))
  # The alpha row must exist, else the correlation assertion below is vacuous.
  alpha_rows <- grep("^alpha", rownames(vc))
  expect_length(alpha_rows, 1L)
  corr_val <- suppressWarnings(as.numeric(vc[alpha_rows[1], "Corr"]))
  expect_true(is.finite(corr_val))
  expect_gte(corr_val, -1)
  expect_lte(corr_val, 1)
})

test_that("print() works on VarCorr.beezdemand_tmb output", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  vc <- VarCorr(fit)
  expect_no_error(print(vc))
})

test_that("VarCorr.beezdemand_tmb rejects a non-default sigma", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  # sigma cannot be honored: the TMB summary reports absolute SDs, not
  # components relative to a residual scale (as nlme's reStruct does).
  expect_error(VarCorr(fit, sigma = 2), "sigma")
  # The default sigma = 1 still works.
  expect_no_error(VarCorr(fit))
})
