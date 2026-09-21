# Test model diagnostics suite

test_that("check_demand_model.beezdemand_hurdle returns expected structure", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_s3_class(diag, "beezdemand_diagnostics")
  expect_true("convergence" %in% names(diag))
  expect_true("boundary" %in% names(diag))
  expect_true("residuals" %in% names(diag))
  expect_true("random_effects" %in% names(diag))
  expect_true("issues" %in% names(diag))
  expect_true("recommendations" %in% names(diag))
  expect_equal(diag$model_class, "beezdemand_hurdle")
})

test_that("check_demand_model.beezdemand_hurdle detects convergence", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  # Model should converge on this data
  expect_true(diag$convergence$converged)
})

test_that("check_demand_model.beezdemand_hurdle provides residual stats", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_true(!is.na(diag$residuals$mean))
  expect_true(!is.na(diag$residuals$sd))
  expect_true(is.numeric(diag$residuals$n_outliers))
})

test_that("check_demand_model.beezdemand_nlme returns expected structure", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  skip_if(is.null(fit$model), "Model fitting failed")

  diag <- check_demand_model(fit)

  expect_s3_class(diag, "beezdemand_diagnostics")
  expect_equal(diag$model_class, "beezdemand_nlme")
  expect_true("convergence" %in% names(diag))
  expect_true("random_effects" %in% names(diag))
})

test_that("check_demand_model.beezdemand_fixed returns expected structure", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_fixed(
    apt, y_var = "y", x_var = "x", id_var = "id"
  )

  diag <- check_demand_model(fit)

  expect_s3_class(diag, "beezdemand_diagnostics")
  expect_equal(diag$model_class, "beezdemand_fixed")
  expect_true("convergence" %in% names(diag))
  expect_true(is.numeric(diag$convergence$n_total))
  expect_true(is.numeric(diag$convergence$n_failed))
})

test_that("print.beezdemand_diagnostics works without error", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_output(print(diag), "Model Diagnostics")
  expect_output(print(diag), "Convergence")
  expect_output(print(diag), "Residuals")
})

test_that("plot_residuals works for hurdle models", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Test single plot types
  p_fitted <- plot_residuals(fit, type = "fitted")
  expect_s3_class(p_fitted, "ggplot")

  p_hist <- plot_residuals(fit, type = "histogram")
  expect_s3_class(p_hist, "ggplot")

  p_qq <- plot_residuals(fit, type = "qq")
  expect_s3_class(p_qq, "ggplot")
})

test_that("plot_residuals type='all' returns ggplot when patchwork available", {
  skip_if_not_installed("patchwork")

  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  plots <- plot_residuals(fit, type = "all")

  # patchwork objects inherit from ggplot
  expect_s3_class(plots, "ggplot")
})

test_that("plot_residuals type='all' returns object compatible with ggsave", {
  skip_if_not_installed("patchwork")

  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  plots <- plot_residuals(fit, type = "all")
  # patchwork objects are ggplot objects, which ggsave accepts
  expect_s3_class(plots, "ggplot")
})

test_that("plot_qq.beezdemand_hurdle works", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  p <- plot_qq(fit)
  expect_s3_class(p, "ggplot")
})

test_that("plot_qq.beezdemand_hurdle with specific effects", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Should work with specific effect
  p <- plot_qq(fit, which = "Q0")
  expect_s3_class(p, "ggplot")
})

test_that("plot_qq.beezdemand_nlme works", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  skip_if(is.null(fit$model), "Model fitting failed")

  p <- plot_qq(fit)
  expect_s3_class(p, "ggplot")
})

test_that("check_demand_model reports n_issues correctly", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_equal(diag$n_issues, length(diag$issues))
})

test_that("plot_residuals works for nlme models", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)

  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )

  skip_if(is.null(fit$model), "Model fitting failed")

  p <- plot_residuals(fit, type = "fitted")
  expect_s3_class(p, "ggplot")
})

test_that("plot_residuals works for fixed models", {
  data(apt, package = "beezdemand")

  fit <- fit_demand_fixed(
    apt, y_var = "y", x_var = "x", id_var = "id"
  )

  p <- plot_residuals(fit, type = "fitted")
  expect_s3_class(p, "ggplot")
})

# --- Strengthen existing tier tests: residuals$sd > 0 ---

test_that("check_demand_model.beezdemand_hurdle residuals sd > 0", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"), verbose = 0
  )
  diag <- check_demand_model(fit)
  expect_gt(diag$residuals$sd, 0)
})

test_that("check_demand_model.beezdemand_nlme residuals sd > 0", {
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- fit_demand_mixed(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben"
  )
  skip_if(is.null(fit$model), "Model fitting failed")
  diag <- check_demand_model(fit)
  expect_gt(diag$residuals$sd, 0)
})

test_that("check_demand_model.beezdemand_fixed residuals sd > 0", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_fixed(
    apt, y_var = "y", x_var = "x", id_var = "id"
  )
  diag <- check_demand_model(fit)
  expect_gt(diag$residuals$sd, 0)
})

# --- TMB diagnostics tests ---

test_that("check_demand_model.beezdemand_tmb returns expected structure", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_s3_class(diag, "beezdemand_diagnostics")
  expect_equal(diag$model_class, "beezdemand_tmb")
  expect_true("convergence" %in% names(diag))
  expect_true("boundary" %in% names(diag))
  expect_true("residuals" %in% names(diag))
  expect_true("random_effects" %in% names(diag))
  expect_true("issues" %in% names(diag))
  expect_true("recommendations" %in% names(diag))
  expect_true(diag$convergence$converged)
})

test_that("check_demand_model.beezdemand_tmb residuals have all fields with sd > 0", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_true(!is.na(diag$residuals$mean))
  expect_true(!is.na(diag$residuals$sd))
  expect_true(!is.na(diag$residuals$min))
  expect_true(!is.na(diag$residuals$max))
  expect_gt(diag$residuals$sd, 0)
  expect_true(is.numeric(diag$residuals$n_outliers))
})

test_that("print.beezdemand_diagnostics works for TMB without error", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_output(print(diag), "Model Diagnostics")
  expect_output(print(diag), "Convergence")
  expect_output(print(diag), "Residuals")
  expect_output(print(diag), "Random Effects")
})

test_that("check_demand_model.beezdemand_tmb 1-RE: named near_zero, full residual fields, print works", {
  # Regression for TICKET-002. The 1-RE path produced a single-element near_zero
  # vector and a residuals_info list missing mean/sd/min/max, which crashed the
  # print method. Both were fixed in 719c0ed; this test guards against return.
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential",
    random_effects = "q0",
    verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_s3_class(diag, "beezdemand_diagnostics")
  expect_true(is.logical(diag$random_effects$near_zero))
  expect_named(diag$random_effects$near_zero, "sigma_b")
  expect_false(any(is.na(diag$random_effects$near_zero)))

  expect_true(!is.na(diag$residuals$mean))
  expect_true(!is.na(diag$residuals$sd))
  expect_true(!is.na(diag$residuals$min))
  expect_true(!is.na(diag$residuals$max))

  expect_output(print(diag), "Model Diagnostics")
})

test_that("check_demand_model.beezdemand_tmb 2-RE: named near_zero contains both REs", {
  # Regression for TICKET-002. Confirms the 2-RE branch produces a named two-
  # element near_zero vector indexable by sigma_b and sigma_c (the print method
  # accesses entries by name).
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential",
    random_effects = c("q0", "alpha"),
    verbose = 0
  )

  diag <- check_demand_model(fit)

  expect_named(diag$random_effects$near_zero, c("sigma_b", "sigma_c"))
  expect_false(any(is.na(diag$random_effects$near_zero)))
  expect_output(print(diag), "Model Diagnostics")
})

test_that("check_demand_model TMB: RE variances, SDs and internal SDs (TICKET-002 addendum; F-BD13-1)", {
  # TICKET-002 addendum pinned $variances to the public
  # summary()$variance_components convention -- but those are Q0/alpha RE *SDs*
  # on the log10 scale (TICKET-015), so the field name and the print label both
  # lied. F-BD13-1 (audit 2026-09-06): $variances now holds true variances on
  # the log10 scale, the SDs move to the explicitly named $sd_log10 (which is
  # what still matches summary()), and $sd_internal_log keeps the raw
  # natural-log-scale SDs used by the near-zero degeneracy heuristic.
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  diag <- check_demand_model(fit)
  vc <- summary(fit)$variance_components
  re_summary <- vc$Estimate[!grepl("Residual", vc$Component)]

  # $sd_log10 == summary() Q0/alpha rows (log10-scale SDs).
  expect_equal(unname(diag$random_effects$sd_log10), re_summary,
               tolerance = 1e-8)
  # $variances is the square of those -- the field name is now true.
  expect_equal(unname(diag$random_effects$variances), re_summary^2,
               tolerance = 1e-8)
  # $sd_internal_log is the raw natural-log-scale SD (a factor log(10) larger).
  expect_equal(unname(diag$random_effects$sd_internal_log),
               re_summary * log(10), tolerance = 1e-8)
  # Names stay aligned across the three vectors.
  expect_identical(names(diag$random_effects$variances),
                   names(diag$random_effects$sd_log10))

  # The printed line reports the variance, matching its label.
  out <- paste(capture.output(print(diag)), collapse = "\n")
  expect_match(out, "variance", fixed = TRUE)
  expect_match(out, sprintf("%.4g", diag$random_effects$variances[[1]]),
               fixed = TRUE)
})

test_that("check_demand_model NLME: $variances holds VarCorr variances (F-BD13-1)", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit <- tryCatch(
    fit_demand_mixed(apt, y_var = "y_ll4", x_var = "x", id_var = "id",
                     equation_form = "zben"),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "NLME fixture did not fit here")

  re <- suppressWarnings(check_demand_model(fit))$random_effects
  skip_if(is.null(re$variances) || length(re$variances) == 0,
          "no random-effect variances available")

  vc <- nlme::VarCorr(fit$model)
  skip_if(!("Variance" %in% colnames(vc)), "VarCorr has no Variance column")
  nm <- names(re$variances)
  expect_equal(unname(re$variances),
               as.numeric(vc[nm, "Variance"]), tolerance = 1e-8)
})

test_that("plot_residuals works for TMB models", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  p_fitted <- plot_residuals(fit, type = "fitted")
  expect_s3_class(p_fitted, "ggplot")

  p_hist <- plot_residuals(fit, type = "histogram")
  expect_s3_class(p_hist, "ggplot")

  p_qq <- plot_residuals(fit, type = "qq")
  expect_s3_class(p_qq, "ggplot")
})

test_that("plot_qq.beezdemand_tmb works", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  p <- plot_qq(fit)
  expect_s3_class(p, "ggplot")
})

test_that("plot_qq.beezdemand_tmb with specific effect", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  p <- plot_qq(fit, which = "Q0")
  expect_s3_class(p, "ggplot")
})

test_that("plot_qq.beezdemand_tmb errors on invalid effect", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )

  expect_error(plot_qq(fit, which = "nonexistent"),
               "Specified random effects not found")
})


# --- TICKET-066: check_demand_model() must not report a failed internal --
# --- check as passing (fixed/hurdle residuals, NLME random effects) ------

test_that(".check_fixed_residuals: augment() error -> computation_failed + classed warning, not a clean pass", {
  fake <- structure(list(), class = "beezdemand_fixed")
  warns <- testthat::capture_warnings(res <- beezdemand:::.check_fixed_residuals(fake))
  expect_true(any(grepl("could not be computed", warns)))
  expect_true(isTRUE(res$computation_failed))
  expect_false(res$has_outliers)
  expect_identical(res$n_outliers, 0)
  expect_true(is.na(res$mean))
})

test_that(".check_hurdle_residuals: augment() error -> computation_failed + classed warning, not a clean pass", {
  fake <- structure(list(), class = "beezdemand_hurdle")
  warns <- testthat::capture_warnings(res <- beezdemand:::.check_hurdle_residuals(fake))
  expect_true(any(grepl("could not be computed", warns)))
  expect_true(isTRUE(res$computation_failed))
  expect_false(res$has_outliers)
  expect_identical(res$n_outliers, 0)
})

test_that(".check_nlme_random_effects: VarCorr() error -> computation_failed + classed warning, not a silently-skipped check", {
  fake <- structure(list(model = list()), class = "beezdemand_nlme")
  warns <- testthat::capture_warnings(res <- beezdemand:::.check_nlme_random_effects(fake))
  expect_true(any(grepl("could not be computed", warns)))
  expect_true(isTRUE(res$computation_failed))
  expect_null(res$variances)
  expect_null(res$near_zero)
})

test_that(".check_nlme_random_effects: no model -> computation_failed is FALSE (nothing to check, not a failure)", {
  fake <- structure(list(model = NULL), class = "beezdemand_nlme")
  expect_no_warning(res <- beezdemand:::.check_nlme_random_effects(fake))
  expect_false(res$computation_failed)
})

test_that("check_demand_model.beezdemand_fixed surfaces a could-not-compute issue instead of a silent pass", {
  fake <- structure(list(), class = "beezdemand_fixed")
  # convergence/parameter sub-checks on this degenerate object may also
  # raise; only the residuals-related issue text is asserted here.
  diag <- suppressWarnings(check_demand_model(fake))
  expect_true(diag$n_issues > 0)
  expect_true(any(grepl("Residual diagnostics could not be computed", diag$issues)))
  expect_true(isTRUE(diag$residuals$computation_failed))
})

test_that("check_demand_model.beezdemand_hurdle surfaces a could-not-compute issue instead of a silent pass", {
  fake <- structure(
    list(hessian_pd = TRUE, param_info = list()),
    class = "beezdemand_hurdle"
  )
  diag <- suppressWarnings(check_demand_model(fake))
  expect_true(any(grepl("Residual diagnostics could not be computed", diag$issues)))
  expect_true(isTRUE(diag$residuals$computation_failed))
})

test_that("check_demand_model.beezdemand_nlme surfaces a could-not-compute issue for a broken random-effects check", {
  fake <- structure(
    list(model = list(), param_info = list()),
    class = "beezdemand_nlme"
  )
  diag <- suppressWarnings(check_demand_model(fake))
  expect_true(any(grepl("Random-effects diagnostics could not be computed", diag$issues)))
  expect_true(isTRUE(diag$random_effects$computation_failed))
})

test_that("check_demand_model: healthy fits are unaffected (computation_failed FALSE, byte-identical issue text)", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id", verbose = 0)
  expect_no_warning(diag <- check_demand_model(fit))
  expect_false(isTRUE(diag$residuals$computation_failed))
  expect_false(any(grepl("could not be computed", diag$issues)))
})

# --- F-BD13-2: nested-grouping VarCorr extraction -----------------------------
# nlme::VarCorr() on a nested lme returns group-header rows ("site =" with a
# pdMat class string in the Variance cell) and REPEATED parameter rownames per
# level. Indexing by rowname turned the headers into NA and returned the first
# level's variance for every level, so the inner level was never reported.

.f132_nested_lme <- function(slope = FALSE) {
  set.seed(1)
  d <- expand.grid(site = factor(1:4), subj = factor(1:6), x = 1:5)
  d$y <- 2 + c(-.5, .2, .4, -.1)[d$site] +
    rnorm(24, sd = .3)[as.integer(interaction(d$site, d$subj))] +
    0.5 * d$x + rnorm(nrow(d), sd = .2)
  if (slope) {
    nlme::lme(y ~ x, random = ~ x | site/subj, data = d,
              control = nlme::lmeControl(opt = "optim"))
  } else {
    nlme::lme(y ~ x, random = ~ 1 | site/subj, data = d)
  }
}

test_that(".check_nlme_random_effects: nested grouping yields one finite variance per level, no NA flags (F-BD13-2)", {
  fit <- .f132_nested_lme()
  vc <- nlme::VarCorr(fit)
  fake <- structure(list(model = fit), class = "beezdemand_nlme")
  expect_no_warning(res <- beezdemand:::.check_nlme_random_effects(fake))
  expect_false(isTRUE(res$computation_failed))
  expect_length(res$variances, 2L)
  expect_true(all(is.finite(res$variances)))
  expect_false(anyNA(res$near_zero))
  expect_false(anyDuplicated(names(res$variances)) > 0)
  expect_setequal(names(res$variances), c("site:(Intercept)", "subj:(Intercept)"))
  # Values are the two levels' own variances, in VarCorr order.
  expect_equal(unname(res$variances["site:(Intercept)"]), as.numeric(vc[2, "Variance"]))
  expect_equal(unname(res$variances["subj:(Intercept)"]), as.numeric(vc[4, "Variance"]))
})

test_that(".check_nlme_random_effects: nested random slope keeps every (level, term) pair and the Corr check (F-BD13-2)", {
  fit <- .f132_nested_lme(slope = TRUE)
  fake <- structure(list(model = fit), class = "beezdemand_nlme")
  expect_no_warning(res <- beezdemand:::.check_nlme_random_effects(fake))
  expect_setequal(
    names(res$variances),
    c("site:(Intercept)", "site:x", "subj:(Intercept)", "subj:x")
  )
  expect_true(all(is.finite(res$variances)))
  expect_false(anyNA(res$near_zero))
  expect_true(is.logical(res$near_singular) && !is.na(res$near_singular))
})

test_that(".check_nlme_random_effects: single-level fits keep bare parameter names (no regression)", {
  set.seed(2)
  d <- data.frame(id = factor(rep(1:8, each = 5)), x = rep(1:5, 8))
  d$y <- 1 + rnorm(8)[d$id] + 0.3 * d$x + rnorm(40, sd = .2)
  fit <- nlme::lme(y ~ x, random = ~ 1 | id, data = d)
  fake <- structure(list(model = fit), class = "beezdemand_nlme")
  res <- beezdemand:::.check_nlme_random_effects(fake)
  expect_identical(names(res$variances), "(Intercept)")
})

# --- F-BD13-1 sub-items: NLME residual check is guarded and flags emptiness ---

test_that(".check_nlme_residuals: residuals() failing both ways -> computation_failed + classed warning", {
  fake <- structure(list(model = structure(list(), class = "f131_noresid")),
                    class = "beezdemand_nlme")
  warns <- testthat::capture_warnings(res <- beezdemand:::.check_nlme_residuals(fake))
  expect_true(any(grepl("could not be computed", warns)))
  expect_true(isTRUE(res$computation_failed))
  expect_false(res$has_outliers)
  expect_true(is.na(res$mean))
})

test_that(".check_nlme_residuals: zero residuals is a failed computation, not a clean pass", {
  registerS3method("residuals", "f131_empty", function(object, ...) numeric(0),
                   envir = asNamespace("stats"))
  fake <- structure(list(model = structure(list(), class = "f131_empty")),
                    class = "beezdemand_nlme")
  warns <- testthat::capture_warnings(res <- beezdemand:::.check_nlme_residuals(fake))
  expect_true(any(grepl("could not be computed", warns)))
  expect_true(isTRUE(res$computation_failed))
})

test_that("check_demand_model.beezdemand_nlme surfaces a could-not-compute residual issue", {
  registerS3method("residuals", "f131_empty2", function(object, ...) numeric(0),
                   envir = asNamespace("stats"))
  fake <- structure(
    list(model = structure(list(), class = "f131_empty2"), param_info = list()),
    class = "beezdemand_nlme"
  )
  diag <- suppressWarnings(check_demand_model(fake))
  expect_true(any(grepl("Residual diagnostics could not be computed", diag$issues)))
  expect_true(isTRUE(diag$residuals$computation_failed))
})

test_that(".check_fixed_residuals / .check_hurdle_residuals: non-numeric .resid is a failed computation", {
  registerS3method("augment", "f131_chr",
                   function(x, ...) data.frame(.resid = c("a", "b")),
                   envir = asNamespace("generics"))
  for (cls in c("beezdemand_fixed", "beezdemand_hurdle")) {
    fake <- structure(list(), class = c("f131_chr", cls))
    fn <- if (cls == "beezdemand_fixed") beezdemand:::.check_fixed_residuals else
      beezdemand:::.check_hurdle_residuals
    warns <- testthat::capture_warnings(res <- fn(fake))
    expect_true(any(grepl("could not be computed", warns)), info = cls)
    expect_true(isTRUE(res$computation_failed), info = cls)
  }
})
