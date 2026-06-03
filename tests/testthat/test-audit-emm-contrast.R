# tests/testthat/test-audit-emm-contrast.R
#
# Pre-CRAN audit: non-circular checks of the estimated-marginal-means and
# contrast back-transforms and the multiplicity adjustment. The data are
# simulated with a KNOWN group effect on alpha, so the checks anchor to external
# truth (the simulated alphas and their ratios) and to cross-function agreement
# (the contrast function vs the independent EMM function), not merely to a
# function's own output:
#
#   - EMMs recover the simulated group alphas (external truth).
#   - Each contrast ratio recovers the true alpha ratio (external truth).
#   - The log10 contrast equals the difference of the two EMMs (ties
#     get_demand_comparisons() to the independent get_demand_param_emms()).
#   - The natural ratio (and bounds) are 10^(log10 contrast) (scale lock).
#   - Default Holm-adjusted p-values equal stats::p.adjust(unadjusted, "holm")
#     over the family (anchored to base R, not the function under test).
#   - TMB EMM natural estimate is exp() of the log-scale estimate (scale lock).

# Self-contained 3-group demand data with a known group effect on alpha.
.AUDIT_TRUE_ALPHA <- c(A = 0.004, B = 0.006, C = 0.009)

.make_factor_demand <- function(seed = 11) {
  set.seed(seed)
  prices <- c(0, 1, 2, 4, 8, 16)
  rows <- list()
  for (g in names(.AUDIT_TRUE_ALPHA)) {
    for (s in 1:6) {
      q0_i <- 20 * exp(stats::rnorm(1, 0, 0.2))
      mu <- q0_i * exp(-.AUDIT_TRUE_ALPHA[[g]] * q0_i * prices)
      y <- mu * exp(stats::rnorm(length(prices), 0, 0.05))
      rows[[length(rows) + 1L]] <- data.frame(
        id = paste0(g, s), x = prices, y = y, grp = g
      )
    }
  }
  do.call(rbind, rows)
}

test_that("NLME EMMs/contrasts recover known alphas; ratio = 10^(log10 contrast)", {
  skip_on_cran()
  dat <- .make_factor_demand()
  fit <- suppressWarnings(suppressMessages(fit_demand_mixed(
    data = dat, y_var = "y", x_var = "x", id_var = "id",
    factors = "grp", equation_form = "simplified"
  )))
  skip_if(is.null(fit$model), "NLME factor fit did not converge")

  emms <- as.data.frame(suppressWarnings(suppressMessages(
    get_demand_param_emms(fit, param = "alpha", factors_in_emm = "grp")
  )))
  emm_nat <- stats::setNames(emms$alpha_natural, emms$grp)
  emm_log10 <- stats::setNames(emms$alpha_param_log10, emms$grp)
  # EXTERNAL: natural EMMs recover the simulated group alphas.
  expect_equal(unname(emm_nat[names(.AUDIT_TRUE_ALPHA)]),
               unname(.AUDIT_TRUE_ALPHA), tolerance = 0.15)

  comps <- suppressWarnings(suppressMessages(
    get_demand_comparisons(fit, param = "alpha")
  ))
  log10_tab <- as.data.frame(comps$alpha$contrasts_log10)
  ratio_tab <- as.data.frame(comps$alpha$contrasts_ratio)
  ridx <- match(log10_tab$contrast_definition, ratio_tab$contrast_definition)
  ratio_est <- ratio_tab$ratio_estimate[ridx]
  pair <- strsplit(log10_tab$contrast_definition, " - ", fixed = TRUE)
  L1 <- vapply(pair, `[`, character(1), 1L)
  L2 <- vapply(pair, `[`, character(1), 2L)

  # EXTERNAL: each contrast ratio recovers the true alpha ratio.
  true_ratio <- .AUDIT_TRUE_ALPHA[L1] / .AUDIT_TRUE_ALPHA[L2]
  expect_equal(ratio_est, unname(true_ratio), tolerance = 0.20)
  # CROSS-FUNCTION: the log10 contrast equals the difference of the two EMMs.
  expect_equal(log10_tab$estimate, unname(emm_log10[L1] - emm_log10[L2]),
               tolerance = 1e-6)
  # SCALE LOCK: natural ratio and bounds are 10^(log10 contrast).
  expect_equal(ratio_est, 10^log10_tab$estimate, tolerance = 1e-9)
  expect_equal(ratio_tab$LCL_ratio[ridx], 10^log10_tab$lower.CL, tolerance = 1e-9)
  expect_equal(ratio_tab$UCL_ratio[ridx], 10^log10_tab$upper.CL, tolerance = 1e-9)
})

test_that("NLME comparisons default to Holm, matching p.adjust over the family", {
  skip_on_cran()
  dat <- .make_factor_demand()
  fit <- suppressWarnings(suppressMessages(fit_demand_mixed(
    data = dat, y_var = "y", x_var = "x", id_var = "id",
    factors = "grp", equation_form = "simplified"
  )))
  skip_if(is.null(fit$model), "NLME factor fit did not converge")

  none <- suppressWarnings(suppressMessages(
    get_demand_comparisons(fit, param = "alpha", adjust = "none")
  ))$alpha$contrasts_log10
  holm <- suppressWarnings(suppressMessages(
    get_demand_comparisons(fit, param = "alpha", adjust = "holm")
  ))$alpha$contrasts_log10
  deflt <- suppressWarnings(suppressMessages(
    get_demand_comparisons(fit, param = "alpha")
  ))$alpha$contrasts_log10

  ord <- match(holm$contrast_definition, none$contrast_definition)
  # External anchor: base-R p.adjust, not the function under test.
  expect_equal(holm$p.value, stats::p.adjust(none$p.value[ord], "holm"),
               tolerance = 1e-10)
  # Default adjust is Holm.
  ord2 <- match(deflt$contrast_definition, holm$contrast_definition)
  expect_equal(deflt$p.value, holm$p.value[ord2], tolerance = 1e-12)
})

test_that("TMB EMMs recover known alphas; estimate = exp(estimate_log)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  dat <- .make_factor_demand()
  fit <- fit_demand_tmb(
    data = dat, y_var = "y", x_var = "x", id_var = "id",
    factors = "grp", equation = "simplified", random_effects = "q0",
    multi_start = FALSE, verbose = 0
  )
  emms <- as.data.frame(
    get_demand_param_emms(fit, param = "alpha", factors_in_emm = "grp")
  )
  expect_true(all(c("level", "estimate", "estimate_log") %in% names(emms)))
  lev <- sub("^grp=", "", emms$level)
  emm_nat <- stats::setNames(emms$estimate, lev)
  # EXTERNAL: natural EMMs recover the simulated group alphas.
  expect_equal(unname(emm_nat[names(.AUDIT_TRUE_ALPHA)]),
               unname(.AUDIT_TRUE_ALPHA), tolerance = 0.15)
  # SCALE LOCK: natural estimate is exp() of the log-scale estimate.
  expect_equal(emms$estimate, exp(emms$estimate_log), tolerance = 1e-9)
})
