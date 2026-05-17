# TICKET-015: TMB variance-component scale convention.
#
# fit_demand_tmb() estimates random-effect SDs on the natural-log scale
# internally (src/MixedDemand.h evaluates Q0_i = exp(log_q0_i); the random
# effect perturbs log_q0_i). summary()$variance_components reports the Q0 and
# alpha RE SDs on the log10 scale -- exp(logsigma) / log(10) -- so they are
# directly comparable with nlme::VarCorr() on a param_space = "log10" NLME fit
# (the NLME default). The residual SD is on the model's likelihood scale and
# the RE correlations are scale-invariant; both are reported without rescaling.

test_that("summary() reports Q0 and alpha RE SDs on the log10 scale", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y, lambda = 4)
  fit <- fit_demand_tmb(
    apt, equation = "zben", y_var = "y_ll4",
    covariance_structure = "pdDiag", verbose = 0
  )

  vc <- summary(fit)$variance_components
  re_rows <- vc[!grepl("Residual", vc$Component), , drop = FALSE]

  coefs <- fit$model$coefficients
  logsigma <- unname(coefs[names(coefs) == "logsigma"])

  expect_equal(nrow(re_rows), length(logsigma))
  expect_equal(re_rows$Estimate, exp(logsigma) / log(10), tolerance = 1e-10)
})

test_that("summary() residual SD is reported on the likelihood scale, not rescaled", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y, lambda = 4)
  fit <- fit_demand_tmb(
    apt, equation = "zben", y_var = "y_ll4",
    covariance_structure = "pdDiag", verbose = 0
  )

  vc <- summary(fit)$variance_components
  resid_row <- vc[grepl("Residual", vc$Component), , drop = FALSE]

  coefs <- fit$model$coefficients
  expect_equal(nrow(resid_row), 1L)
  expect_equal(
    resid_row$Estimate,
    unname(exp(coefs[["logsigma_e"]])),
    tolerance = 1e-10
  )
})

test_that("TMB RE SDs agree with NLME VarCorr on a matched log10 fit", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y, lambda = 4)

  fit_tmb <- fit_demand_tmb(
    apt, equation = "zben", y_var = "y_ll4",
    covariance_structure = "pdDiag", verbose = 0
  )
  fit_nlme <- suppressWarnings(suppressMessages(
    fit_demand_mixed(
      apt, y_var = "y_ll4", x_var = "x", id_var = "id",
      equation_form = "zben", param_space = "log10",
      covariance_structure = "pdDiag"
    )
  ))
  skip_if(is.null(fit_nlme$model), "NLME comparison fit did not converge")

  vc_tmb <- summary(fit_tmb)$variance_components
  re_tmb <- vc_tmb$Estimate[!grepl("Residual", vc_tmb$Component)]

  vc_nlme <- nlme::VarCorr(fit_nlme$model)
  sd_nlme <- suppressWarnings(as.numeric(vc_nlme[, "StdDev"]))
  sd_nlme <- sd_nlme[!is.na(sd_nlme)]
  re_nlme <- sd_nlme[seq_len(length(sd_nlme) - 1L)]  # drop the residual row

  expect_equal(length(re_tmb), length(re_nlme))
  expect_equal(re_tmb, re_nlme, tolerance = 0.05)
})

test_that("summary() RE correlations are scale-invariant (not rescaled)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y, lambda = 4)
  fit <- fit_demand_tmb(
    apt, equation = "zben", y_var = "y_ll4",
    covariance_structure = "pdSymm", verbose = 0
  )

  corr <- summary(fit)$correlations
  skip_if(is.null(corr), "fit has no RE correlation component")

  expect_true(all(abs(corr$Estimate) <= 1))

  coefs <- fit$model$coefficients
  rho_raw <- unname(coefs[names(coefs) == "rho_raw"])
  # Default 2-RE pdSymm block: the single marginal correlation is
  # tanh(rho_raw); summary() must report it untouched.
  if (length(rho_raw) == 1L) {
    expect_equal(corr$Estimate[1], tanh(rho_raw), tolerance = 1e-8)
  }
})

test_that("log10 rescaling applies to every row of a factor-expanded RE fit", {
  skip_on_cran()
  # Within-subject factor -> factor-expanded REs: one Q0 and one alpha RE SD
  # per condition level. Exercises the per-column loop in
  # .tmb_format_variance_components(), not just the single-RE-per-parameter
  # case the other tests cover.
  sim <- .simulate_within_subject_demand(
    n_subjects = 25, n_conditions = 3,
    delta_q0 = c(0, 0.3, -0.2), delta_alpha = c(0, 0.1, -0.1),
    seed = 515
  )
  sim$y_ll4 <- ll4(sim$y)
  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", factors = "condition",
    random_effects = nlme::pdDiag(Q0 + alpha ~ condition),
    verbose = 0
  )))
  skip_if_not(isTRUE(fit$converged), "factor-expanded TMB fit did not converge")

  vc <- summary(fit)$variance_components
  re_rows <- vc[!grepl("Residual", vc$Component), , drop = FALSE]
  coefs <- fit$model$coefficients
  logsigma <- unname(coefs[names(coefs) == "logsigma"])

  expect_gt(length(logsigma), 2L)  # confirm the fit really is factor-expanded
  expect_equal(nrow(re_rows), length(logsigma))
  expect_equal(re_rows$Estimate, exp(logsigma) / log(10), tolerance = 1e-10)
})

test_that("log10 conversion arithmetic is correct on a synthetic fit (no model fit)", {
  # Deliberately NOT skip_on_cran(): a pure-arithmetic check on a synthetic
  # beezdemand_tmb object with no optimizer/TMB fitting, so it runs under
  # CRAN-style R CMD check and guards the exp(logsigma) / log(10) conversion
  # against regression -- the model-fitting tests above are skipped on CRAN.
  object <- list(
    model = list(coefficients = c(
      logsigma   = log(0.30),  # Q0 (Intercept) RE SD, natural-log scale
      logsigma   = log(0.45),  # Q0 slope RE SD, natural-log scale
      logsigma   = log(0.20),  # alpha (Intercept) RE SD, natural-log scale
      logsigma_e = log(0.08)   # residual SD
    )),
    param_info = list(random_effects_parsed = list(blocks = list(
      list(
        terms_q0    = c("(Intercept)", "groupB"),
        terms_alpha = "(Intercept)",
        pdmat_class = "pdDiag"
      )
    )))
  )

  vc <- beezdemand:::.tmb_format_variance_components(object)
  re_rows <- vc$table[!grepl("Residual", vc$table$Component), , drop = FALSE]
  resid_row <- vc$table[grepl("Residual", vc$table$Component), , drop = FALSE]

  # Q0/alpha RE SDs: natural-log estimates rescaled to log10.
  expect_equal(nrow(re_rows), 3L)
  expect_equal(re_rows$Estimate, c(0.30, 0.45, 0.20) / log(10), tolerance = 1e-12)
  # Residual SD: likelihood scale, NOT rescaled.
  expect_equal(resid_row$Estimate, 0.08, tolerance = 1e-12)
})
