# TICKET-017: cross-backend harmonization of summary()/tidy()/glance().
#
# These tests assert that the broom-style introspection methods expose the
# same column names, default arguments, and component labels on both the
# NLME and TMB backends, so backend-agnostic code needs no dispatch glue.
#
# Fit setup note: the TICKET-017 draft fitted with `factors = "id_group"`
# (no such column in `apt_full`) and omitted the required `x_var` / `id_var`.
# The cross-backend pair below uses the known-good recipe from
# test-variance-component-scale.R instead: `apt` + LL4-transformed `y` + the
# "zben" equation + pdDiag covariance, with the NLME fit on
# `param_space = "log10"` so the two backends are directly comparable. No
# within-subject factor is needed -- harmonization is about column shape and
# component labels, not factor expansion.

.harmonize_fit_pair <- function() {
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
  list(tmb = fit_tmb, nlme = fit_nlme)
}

test_that("glance() canonical column shape on both backends", {
  skip_on_cran()
  fits <- .harmonize_fit_pair()
  skip_if(is.null(fits$nlme$model), "NLME comparison fit did not converge")

  canonical <- c("model_class", "backend", "equation_form", "nobs",
                 "n_subjects", "n_random_effects", "converged",
                 "logLik", "AIC", "BIC")
  g_nlme <- glance(fits$nlme)
  g_tmb  <- glance(fits$tmb)
  expect_true(all(canonical %in% names(g_nlme)))
  expect_true(all(canonical %in% names(g_tmb)))
})

test_that("tidy(effects='fixed') returns fixed rows on both backends", {
  skip_on_cran()
  fits <- .harmonize_fit_pair()
  skip_if(is.null(fits$nlme$model), "NLME comparison fit did not converge")

  t_nlme <- tidy(fits$nlme, effects = "fixed")
  t_tmb  <- tidy(fits$tmb,  effects = "fixed")
  # Both backends use the canonical "fixed" label (no shim -- TMB API unreleased):
  expect_true(all(t_nlme$component == "fixed"))
  expect_true(all(t_tmb$component  == "fixed"))
  # No variance rows pass through:
  expect_false(any(t_nlme$component == "variance"))
  expect_false(any(t_tmb$component  == "variance"))
  # No "consumption" rows on TMB (renamed outright):
  expect_false(any(t_tmb$component == "consumption"))
  # Acceptance criterion: identical column shape across backends.
  expect_setequal(names(t_nlme), names(t_tmb))
})

test_that("tidy(effects='ran_pars') returns variance rows only on both backends", {
  skip_on_cran()
  fits <- .harmonize_fit_pair()
  skip_if(is.null(fits$nlme$model), "NLME comparison fit did not converge")

  t_nlme <- tidy(fits$nlme, effects = "ran_pars")
  t_tmb  <- tidy(fits$tmb,  effects = "ran_pars")
  expect_true(all(t_nlme$component == "variance"))
  expect_true(all(t_tmb$component  == "variance"))
  # Scale contract: TMB tidy(ran_pars) reports the same values as
  # summary()$variance_components -- Q0/alpha RE SDs on the log10 scale and
  # the residual SD on the likelihood scale (the TICKET-015 convention) --
  # rather than the raw internal `logsigma` optimizer coefficients.
  vc_tmb <- summary(fits$tmb)$variance_components
  expect_equal(t_tmb$estimate, vc_tmb$Estimate, tolerance = 1e-8)
  expect_true(all(t_tmb$estimate_scale %in% c("natural", "log10")))
  # TICKET-030: NLME tidy(ran_pars)$estimate is now an SD (matching the
  # broom.mixed::tidy.lme convention and the TMB sibling), not a variance.
  vc_nlme <- nlme::VarCorr(fits$nlme$model)
  expect_equal(t_nlme$estimate,
               as.numeric(vc_nlme[, "StdDev"]),
               tolerance = 1e-10)
  # Cross-backend SD agreement on a matched fit pair (drop residual rows --
  # TMB residual is on the likelihood scale, NLME residual is on the data
  # scale, so they are not directly comparable; the RE SDs are).
  re_nlme <- t_nlme$estimate[!grepl("Residual", t_nlme$term)]
  re_tmb  <- t_tmb$estimate[!grepl("Residual",  t_tmb$term)]
  expect_equal(length(re_nlme), length(re_tmb))
  expect_equal(re_tmb, re_nlme, tolerance = 0.05)
})

test_that("TMB glance()$equation renamed to equation_form outright (no alias)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  g <- glance(fit)
  expect_true("equation_form" %in% names(g))
  expect_false("equation" %in% names(g))
  td <- tidy(fit)
  # "fixed" is the canonical label; "consumption" is gone outright (no shim):
  expect_true(any(td$component == "fixed"))
  expect_false(any(td$component == "consumption"))
})

test_that("summary() and tidy() default to report_space='natural' on both backends (no flip)", {
  skip_on_cran()
  fits <- .harmonize_fit_pair()
  skip_if(is.null(fits$nlme$model), "NLME comparison fit did not converge")

  # Default-call tidy() returns natural-scale on both backends:
  td_nlme_default <- tidy(fits$nlme)
  td_nlme_natural <- tidy(fits$nlme, report_space = "natural")
  td_tmb_default  <- tidy(fits$tmb)
  td_tmb_natural  <- tidy(fits$tmb,  report_space = "natural")
  # Default == explicit "natural" on both:
  expect_equal(td_nlme_default$estimate, td_nlme_natural$estimate, tolerance = 1e-12)
  expect_equal(td_tmb_default$estimate,  td_tmb_natural$estimate,  tolerance = 1e-12)
})

test_that("tidy(effects='bogus') errors with match.arg-style message", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  expect_error(tidy(fit, effects = "bogus"), "should be one of")
})

test_that("tidy(effects='ran_pars') excludes RE correlations on pdSymm TMB fits", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y, lambda = 4)
  fit <- fit_demand_tmb(
    apt, equation = "zben", y_var = "y_ll4",
    covariance_structure = "pdSymm", verbose = 0
  )
  t_ran <- tidy(fit, effects = "ran_pars")
  vc <- summary(fit)$variance_components
  # tidy(ran_pars) reports the variance-component SD rows only -- exactly
  # summary()$variance_components. RE correlations live in
  # summary()$correlations (and VarCorr()), not in tidy() rows, matching the
  # row structure of tidy.beezdemand_nlme().
  expect_false(any(grepl("correlation|rho", t_ran$term)))
  expect_equal(t_ran$term, vc$Component)
  expect_equal(t_ran$estimate, vc$Estimate, tolerance = 1e-8)
})
