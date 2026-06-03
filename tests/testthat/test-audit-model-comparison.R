# tests/testthat/test-audit-model-comparison.R
#
# Pre-CRAN audit: non-circular verification of the model-comparison machinery
# (R/model-comparison.R) against textbook definitions, not its own output.
#
#   AIC = -2*logLik + 2*k
#   BIC = -2*logLik + log(n)*k
#   LRT: LR = 2*(logLik_full - logLik_reduced),
#        df = k_full - k_reduced,
#        p  = pchisq(LR, df, lower.tail = FALSE)   [central chi-square reference]
#
# The LRT check confirms the *arithmetic* the function performs. The separate
# (documented) caveat that a central chi-square is conservative for
# variance-component boundary tests (Stram & Lee, 1994) is a statistical
# interpretation issue, not an arithmetic one, and is out of scope here.

test_that("hurdle AIC/BIC equal the -2logLik + penalty identity", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"), verbose = 0
  )
  k <- length(fit$model$coefficients)
  n <- nrow(fit$data)
  expect_equal(fit$AIC, -2 * fit$loglik + 2 * k, tolerance = 1e-8)
  expect_equal(fit$BIC, -2 * fit$loglik + log(n) * k, tolerance = 1e-8)
})

test_that("TMB AIC/BIC equal the identity with df = length(opt$par)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "simplified", random_effects = "q0",
    multi_start = FALSE, verbose = 0
  )
  k <- length(fit$opt$par)
  n <- fit$param_info$n_obs
  expect_equal(fit$AIC, -2 * fit$loglik + 2 * k, tolerance = 1e-8)
  expect_equal(fit$BIC, -2 * fit$loglik + log(n) * k, tolerance = 1e-8)
})

test_that("nested hurdle LRT uses LR=2*dlogLik, df=#added params, chi-square ref", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit2 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"), verbose = 0
  )
  fit3 <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"), verbose = 0
  )

  k2 <- length(fit2$model$coefficients)
  k3 <- length(fit3$model$coefficients)
  expect_gt(k3, k2) # the 3-RE model genuinely adds parameters

  LR <- 2 * (fit3$loglik - fit2$loglik)
  df <- k3 - k2
  expect_gte(LR, 0) # nested: the fuller model cannot fit worse

  p <- stats::pchisq(LR, df = df, lower.tail = FALSE)

  # anova.beezdemand_hurdle
  av <- anova(fit2, fit3)
  expect_equal(av$lrt$LR_stat[1], LR, tolerance = 1e-8)
  expect_equal(av$lrt$df[1], df)
  expect_equal(av$lrt$Pr_Chisq[1], p, tolerance = 1e-8)

  # compare_models() (independent path; emits nesting-not-verified message)
  cm <- suppressWarnings(suppressMessages(
    compare_models(fit2, fit3, test = "lrt")
  ))
  expect_equal(cm$lrt_results$LR_stat[1], LR, tolerance = 1e-8)
  expect_equal(cm$lrt_results$df[1], df)
  expect_equal(cm$lrt_results$p_value[1], p, tolerance = 1e-8)

  # The reported df equals the #added parameters, and the chi-square reference
  # is the central pchisq on that df (not df+/-1).
  expect_equal(av$lrt$df[1], k3 - k2)
})
