# tests/testthat/test-audit-emm-contrast.R
#
# Pre-CRAN audit: non-circular checks of the estimated-marginal-means and
# contrast back-transforms and the multiplicity adjustment.
#
#   - NLME contrasts: the natural-scale ratio is the base-10 exponent of the
#     log10-scale contrast (ratio_estimate = 10^estimate, and likewise for the
#     interval bounds). This locks the back-transform so a future edit cannot
#     silently swap 10^() for exp() or read a wrong column.
#   - Multiplicity: the default Holm-adjusted p-values equal
#     stats::p.adjust(unadjusted, "holm") over the contrast family, and the
#     default adjust is "holm".
#   - TMB EMMs: the natural-scale estimate is exp() of the log-scale estimate.

# Small self-contained 3-group demand data with a group effect on alpha.
.make_factor_demand <- function(seed = 11) {
  set.seed(seed)
  prices <- c(0, 1, 2, 4, 8, 16)
  groups <- c("A", "B", "C")
  alphas <- c(A = 0.004, B = 0.006, C = 0.009)
  rows <- list()
  for (g in groups) {
    for (s in 1:6) {
      q0_i <- 20 * exp(stats::rnorm(1, 0, 0.2))
      mu <- q0_i * exp(-alphas[[g]] * q0_i * prices)
      y <- mu * exp(stats::rnorm(length(prices), 0, 0.05))
      rows[[length(rows) + 1L]] <- data.frame(
        id = paste0(g, s), x = prices, y = y, grp = g
      )
    }
  }
  do.call(rbind, rows)
}

test_that("NLME contrast ratio is 10^(log10 contrast) end-to-end", {
  skip_on_cran()
  dat <- .make_factor_demand()
  fit <- suppressWarnings(suppressMessages(fit_demand_mixed(
    data = dat, y_var = "y", x_var = "x", id_var = "id",
    factors = "grp", equation_form = "simplified"
  )))
  skip_if(is.null(fit$model), "NLME factor fit did not converge")

  comps <- suppressWarnings(suppressMessages(
    get_demand_comparisons(fit, param = "alpha")
  ))
  log10_tab <- comps$alpha$contrasts_log10
  ratio_tab <- comps$alpha$contrasts_ratio
  expect_gt(nrow(log10_tab), 0)
  # Align by contrast_definition to be robust to row order.
  ord <- match(ratio_tab$contrast_definition, log10_tab$contrast_definition)
  expect_equal(ratio_tab$ratio_estimate, 10^log10_tab$estimate[ord],
               tolerance = 1e-9)
  expect_equal(ratio_tab$LCL_ratio, 10^log10_tab$lower.CL[ord],
               tolerance = 1e-9)
  expect_equal(ratio_tab$UCL_ratio, 10^log10_tab$upper.CL[ord],
               tolerance = 1e-9)
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
  expect_equal(holm$p.value, stats::p.adjust(none$p.value[ord], "holm"),
               tolerance = 1e-10)
  # Default adjust is Holm.
  ord2 <- match(deflt$contrast_definition, holm$contrast_definition)
  expect_equal(deflt$p.value, holm$p.value[ord2], tolerance = 1e-12)
})

test_that("TMB EMM natural estimate is exp() of the log-scale estimate", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  dat <- .make_factor_demand()
  fit <- fit_demand_tmb(
    data = dat, y_var = "y", x_var = "x", id_var = "id",
    factors = "grp", equation = "simplified", random_effects = "q0",
    multi_start = FALSE, verbose = 0
  )
  emms <- get_demand_param_emms(fit, param = "alpha", factors_in_emm = "grp")
  expect_true(all(c("estimate", "estimate_log") %in% names(emms)))
  expect_equal(emms$estimate, exp(emms$estimate_log), tolerance = 1e-9)
})
