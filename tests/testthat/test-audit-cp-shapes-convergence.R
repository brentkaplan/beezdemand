# =============================================================================
# F-BD11-4 (audit 2026-09-06): cross-price method contracts.
#  * tidy()/glance()/augment() on a NULL-model object return the SAME columns
#    as the success path (typed empties), so downstream bind_rows()/select()
#    code does not branch on failure.
#  * lmer fits store convergence metadata; print()/summary() show it and the
#    fitter warns once when lme4 reports a problem.
#  * print.cp_model_nls() shows a non-converged winning fit.
#  * `qalone` is the infinite-alternative-price asymptote, not the zero-price
#    value.
# =============================================================================

.f114_data <- function() {
  data(etm, package = "beezdemand")
  etm
}

.f114_null <- function(cls, data = NULL) {
  structure(
    list(model = NULL, data = data, equation = "exponentiated", method = "none"),
    class = cls
  )
}

test_that("cp tidy() typed empties match the success-path columns (F-BD11-4)", {
  skip_on_cran()
  skip_if_not_installed("broom.mixed")
  d <- .f114_data()
  nls_fit <- suppressWarnings(fit_cp_nls(d, equation = "exponentiated"))
  lm_fit <- suppressWarnings(fit_cp_linear(d, type = "fixed"))
  lmer_fit <- suppressWarnings(fit_cp_linear(d, type = "mixed"))

  chk <- function(empty, full) {
    expect_identical(names(empty), names(full))
    expect_identical(nrow(empty), 0L)
    expect_s3_class(empty, "tbl_df")
  }
  chk(tidy(.f114_null("cp_model_nls")), tidy(nls_fit))
  chk(tidy(.f114_null("cp_model_lm")), tidy(lm_fit))
  for (e in c("fixed", "ran_vals", "ran_pars")) {
    chk(tidy(.f114_null("cp_model_lmer"), effects = e),
        tidy(lmer_fit, effects = e))
  }
  # "random" is kept as an alias for broom.mixed's "ran_vals" (it used to be
  # accepted by match.arg() and then rejected by broom.mixed).
  expect_identical(names(tidy(lmer_fit, effects = "random")),
                   names(tidy(lmer_fit, effects = "ran_vals")))
})

test_that("cp glance() typed empties are one all-NA row with the success-path columns (F-BD11-4)", {
  skip_on_cran()
  skip_if_not_installed("broom.mixed")
  d <- .f114_data()
  nls_fit <- suppressWarnings(fit_cp_nls(d, equation = "exponentiated"))
  lm_fit <- suppressWarnings(fit_cp_linear(d, type = "fixed"))
  lmer_fit <- suppressWarnings(fit_cp_linear(d, type = "mixed"))

  chk <- function(empty, full) {
    expect_identical(names(empty), names(full))
    expect_identical(nrow(empty), 1L)
    num <- vapply(empty, is.numeric, logical(1))
    expect_true(all(is.na(unlist(empty[num]))))
  }
  chk(glance(.f114_null("cp_model_nls")), glance(nls_fit))
  chk(glance(.f114_null("cp_model_lm")), glance(lm_fit))
  chk(glance(.f114_null("cp_model_lmer")), glance(lmer_fit))
})

test_that("cp augment() typed empties carry the modelling columns plus the fitted columns (F-BD11-4)", {
  skip_on_cran()
  d <- .f114_data()
  nls_fit <- suppressWarnings(fit_cp_nls(d, equation = "exponentiated"))
  lm_fit <- suppressWarnings(fit_cp_linear(d, type = "fixed"))
  lmer_fit <- suppressWarnings(fit_cp_linear(d, type = "mixed"))

  chk <- function(empty, full) {
    expect_identical(names(empty), names(full))
    expect_identical(nrow(empty), 0L)
  }
  chk(augment(.f114_null("cp_model_nls", data = nls_fit$data)), augment(nls_fit))
  chk(augment(.f114_null("cp_model_lm", data = lm_fit$data)), augment(lm_fit))
  chk(augment(.f114_null("cp_model_lmer", data = lmer_fit$data)), augment(lmer_fit))
  # No data either: still a typed 0-row tibble, never a 0 x 0 frame.
  e <- augment(.f114_null("cp_model_lmer"))
  expect_identical(names(e), c(".fitted", ".resid", ".fixed"))
  expect_identical(nrow(e), 0L)
})

test_that("fit_cp_linear(type = 'mixed') stores lme4 convergence metadata and warns when it is bad (F-BD11-4)", {
  skip_on_cran()
  d <- .f114_data()
  ok <- suppressWarnings(fit_cp_linear(d, type = "mixed"))
  expect_true(isTRUE(ok$converged))
  expect_identical(ok$convergence_messages, character(0))

  # Starve the optimiser so lme4 reports a convergence problem.
  expect_warning(
    bad <- fit_cp_linear(
      d, type = "mixed",
      control = lme4::lmerControl(optimizer = "Nelder_Mead",
                                  optCtrl = list(maxfun = 5))
    ),
    class = "beezdemand_cp_lmer_nonconverged_warning"
  )
  expect_false(isTRUE(bad$converged))
  expect_true(length(bad$convergence_messages) > 0)
  out <- capture.output(print(bad))
  expect_true(any(grepl("Converged:\\s+no", out)))
  s <- summary(bad)
  expect_false(isTRUE(s$converged))
})

test_that("print.cp_model_nls() reports a non-converged winning fit (F-BD11-4)", {
  skip_on_cran()
  d <- .f114_data()
  fit <- suppressWarnings(fit_cp_nls(d, equation = "exponentiated"))
  fit$convergence$isConv <- FALSE
  fit$convergence$stopMessage <- "forced for test"
  out <- capture.output(suppressWarnings(print(fit)))
  expect_true(any(grepl("Converged:\\s+no", out)))
  fit$convergence$isConv <- TRUE
  out2 <- capture.output(print(fit))
  expect_false(any(grepl("Converged:\\s+no", out2)))
})

test_that("summary.cp_model_nls print describes qalone as the infinite-price asymptote (F-BD11-4)", {
  skip_on_cran()
  d <- .f114_data()
  fit <- suppressWarnings(fit_cp_nls(d, equation = "exponentiated"))
  out <- paste(capture.output(print(summary(fit))), collapse = "\n")
  expect_false(grepl("zero alternative price", out))
  expect_true(grepl("alternative price", out))
})

test_that("glance.cp_model_lmer NULL schema follows the REML flag (F-BD11-4, end-pass)", {
  skip_on_cran()
  skip_if_not_installed("broom.mixed")
  d <- .f114_data()
  ml_fit <- suppressWarnings(fit_cp_linear(d, type = "mixed", REML = FALSE))
  expect_false(isTRUE(ml_fit$REML))
  empty_ml <- .f114_null("cp_model_lmer"); empty_ml$REML <- FALSE
  expect_identical(names(glance(empty_ml)), names(glance(ml_fit)))
  reml_fit <- suppressWarnings(fit_cp_linear(d, type = "mixed"))
  expect_true(isTRUE(reml_fit$REML))
  expect_identical(names(glance(.f114_null("cp_model_lmer"))), names(glance(reml_fit)))
})

test_that("glance.cp_model_lmer() carries a `converged` column (F-BD11-4, batch 3)", {
  skip_on_cran()
  skip_if_not_installed("broom.mixed")
  d <- .f114_data()
  ok <- suppressWarnings(fit_cp_linear(d, type = "mixed"))
  g_ok <- glance(ok)
  expect_true("converged" %in% names(g_ok))
  expect_identical(names(g_ok)[length(names(g_ok))], "converged")
  expect_identical(g_ok$converged, TRUE)

  bad <- suppressWarnings(fit_cp_linear(
    d, type = "mixed",
    control = lme4::lmerControl(optimizer = "Nelder_Mead",
                                optCtrl = list(maxfun = 5))
  ))
  expect_identical(glance(bad)$converged, FALSE)

  # Legacy object without the field: NA, not FALSE, and still a valid glance.
  legacy <- ok
  legacy$converged <- NULL
  g_legacy <- glance(legacy)
  expect_identical(g_legacy$converged, NA)
  expect_identical(names(g_legacy), names(g_ok))

  # Typed empty keeps the same schema (converged included).
  expect_identical(names(glance(.f114_null("cp_model_lmer"))), names(g_ok))
  expect_identical(glance(.f114_null("cp_model_lmer"))$converged, NA)
})
