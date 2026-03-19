# Tests for Advanced Visualization Functions (plot-viz.R)

skip_on_cran()

# Shared fixture: fit a small hurdle model once for all tests
data(apt, package = "beezdemand")
fit_hurdle <- tryCatch(
  fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id"),
  error = function(e) NULL
)

skip_if(is.null(fit_hurdle), "Hurdle model fitting failed — skipping viz tests")


# =============================================================================
# plot_loss_surface()
# =============================================================================

test_that("plot_loss_surface returns ggplot for hurdle model", {
  p <- plot_loss_surface(fit_hurdle, resolution = 20)
  expect_s3_class(p, "gg")
})

test_that("plot_loss_surface respects show_mle and show_contours", {
  p <- plot_loss_surface(fit_hurdle, resolution = 20,
                         show_mle = TRUE, show_contours = TRUE)
  expect_s3_class(p, "gg")
  # Should have at least 3 layers: raster + contour + mle point
  expect_gte(length(p$layers), 3)
})

test_that("plot_loss_surface accepts custom ranges", {
  mle <- beezdemand:::.extract_hurdle_mle(fit_hurdle)
  p <- plot_loss_surface(
    fit_hurdle,
    resolution = 15,
    q0_range = c(mle$Q0 / 10, mle$Q0 * 10),
    alpha_range = c(mle$alpha / 10, mle$alpha * 10)
  )
  expect_s3_class(p, "gg")
})

test_that("plot_loss_surface dispatches on class", {
  expect_error(plot_loss_surface(lm(y ~ x, data.frame(x = 1:5, y = 1:5))),
               "no applicable method")
})


# =============================================================================
# plot_loss_profile()
# =============================================================================

test_that("plot_loss_profile returns ggplot for single parameter", {
  p <- plot_loss_profile(fit_hurdle, parameter = "q0", resolution = 50)
  expect_s3_class(p, "gg")

  p2 <- plot_loss_profile(fit_hurdle, parameter = "alpha", resolution = 50)
  expect_s3_class(p2, "gg")
})

test_that("plot_loss_profile returns combined plot for 'both'", {
  skip_if_not_installed("patchwork")
  p <- plot_loss_profile(fit_hurdle, parameter = "both", resolution = 50)
  # patchwork objects inherit from gg
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})


# =============================================================================
# plot_demand_overlay()
# =============================================================================

test_that("plot_demand_overlay works with single model", {
  p <- plot_demand_overlay(fit_hurdle, labels = "Hurdle 2-RE")
  expect_s3_class(p, "gg")
})

test_that("plot_demand_overlay works with model_list", {
  p <- plot_demand_overlay(
    model_list = list(M1 = fit_hurdle),
    x_trans = "log10"
  )
  expect_s3_class(p, "gg")
})

test_that("plot_demand_overlay errors with no models", {
  expect_error(plot_demand_overlay(), "At least one model")
})


# =============================================================================
# plot_model_comparison()
# =============================================================================

test_that("plot_model_comparison returns ggplot", {
  p <- plot_model_comparison(fit_hurdle, labels = "Hurdle")
  expect_s3_class(p, "gg")
})

test_that("plot_model_comparison errors on missing parameters", {
  expect_error(
    plot_model_comparison(fit_hurdle,
                          labels = "Hurdle",
                          parameters = "nonexistent_param"),
    "None of the requested parameters"
  )
})


# =============================================================================
# plot_re_diagnostics()
# =============================================================================

test_that("plot_re_diagnostics returns plots for hurdle model", {
  p <- plot_re_diagnostics(fit_hurdle, which = "zeros")
  # Should return patchwork or list

  expect_true(
    inherits(p, "gg") || inherits(p, "patchwork") ||
      inherits(p, "beezdemand_diagnostic_plots")
  )
})

test_that("plot_re_diagnostics handles 'all' selection", {
  p <- plot_re_diagnostics(fit_hurdle, which = "all")
  expect_true(
    inherits(p, "gg") || inherits(p, "patchwork") ||
      inherits(p, "beezdemand_diagnostic_plots")
  )
})

test_that("plot_re_diagnostics errors for unavailable RE", {
  # 2-RE model doesn't have c_i (alpha RE)
  n_re <- fit_hurdle$param_info$n_random_effects
  if (n_re < 3) {
    expect_error(
      plot_re_diagnostics(fit_hurdle, which = "alpha"),
      "not found in model"
    )
  } else {
    # 3-RE model has alpha RE — should work without error
    p <- plot_re_diagnostics(fit_hurdle, which = "alpha")
    expect_true(
      inherits(p, "gg") || inherits(p, "patchwork") ||
        inherits(p, "beezdemand_diagnostic_plots")
    )
  }
})


# =============================================================================
# plot_expenditure()
# =============================================================================

test_that("plot_expenditure returns ggplot", {
  p <- plot_expenditure(fit_hurdle)
  expect_s3_class(p, "gg")
})

test_that("plot_expenditure works with custom prices", {
  p <- plot_expenditure(fit_hurdle, prices = c(0.5, 1, 2, 5, 10))
  expect_s3_class(p, "gg")
})

test_that("plot_expenditure respects show_pmax/show_omax", {
  p <- plot_expenditure(fit_hurdle, show_pmax = FALSE, show_omax = FALSE)
  expect_s3_class(p, "gg")
})


# =============================================================================
# plot_elasticity()
# =============================================================================

test_that("plot_elasticity returns ggplot", {
  p <- plot_elasticity(fit_hurdle)
  expect_s3_class(p, "gg")
})

test_that("plot_elasticity works with custom prices", {
  p <- plot_elasticity(fit_hurdle, prices = c(0.5, 1, 2, 5, 10))
  expect_s3_class(p, "gg")
})

test_that("plot_elasticity handles show_unit toggle", {
  p <- plot_elasticity(fit_hurdle, show_unit = FALSE)
  expect_s3_class(p, "gg")
})


# =============================================================================
# plot_alpha_distribution()
# =============================================================================

test_that("plot_alpha_distribution returns ggplot", {
  p <- plot_alpha_distribution(fit_hurdle)
  expect_s3_class(p, "gg")
})

test_that("plot_alpha_distribution supports histogram type", {
  p <- plot_alpha_distribution(fit_hurdle, type = "histogram")
  expect_s3_class(p, "gg")
})

test_that("plot_alpha_distribution supports linear scale", {
  p <- plot_alpha_distribution(fit_hurdle, log_scale = FALSE)
  expect_s3_class(p, "gg")
})


# =============================================================================
# Internal helper tests
# =============================================================================

test_that(".map_hurdle_equation handles all Part II variants", {
  expect_equal(
    beezdemand:::.map_hurdle_equation("zhao_exponential"),
    "zhao_exponential"
  )
  expect_equal(
    beezdemand:::.map_hurdle_equation("exponential"),
    "exponential"
  )
  expect_equal(
    beezdemand:::.map_hurdle_equation("simplified_exponential"),
    "simplified_exponential"
  )
  expect_error(
    beezdemand:::.map_hurdle_equation("unknown_equation"),
    "Unrecognized hurdle Part II equation"
  )
})

test_that(".extract_hurdle_mle returns expected structure", {
  mle <- beezdemand:::.extract_hurdle_mle(fit_hurdle)
  expect_true(is.list(mle))
  expect_true(mle$Q0 > 0)
  expect_true(mle$alpha > 0)
  expect_true(is.numeric(mle$ln_q0))
  expect_true(is.numeric(mle$ln_alpha))
})

test_that(".aggregate_positive_log_means returns valid data", {
  agg <- beezdemand:::.aggregate_positive_log_means(fit_hurdle)
  expect_true(is.data.frame(agg))
  expect_true(all(c("price", "mean_log_y", "n") %in% names(agg)))
  expect_true(nrow(agg) > 0)
  expect_true(all(agg$n > 0))
})

test_that(".compute_ssr_grid is vectorized correctly", {
  agg <- beezdemand:::.aggregate_positive_log_means(fit_hurdle)
  mle <- beezdemand:::.extract_hurdle_mle(fit_hurdle)

  # Single point at MLE should have minimal SSR
  ssr_at_mle <- beezdemand:::.compute_ssr_grid(
    ln_q0_vec = mle$ln_q0,
    alpha_vec = mle$alpha,
    k = mle$k,
    prices = agg$price,
    observed = agg$mean_log_y,
    equation = "zhao_exponential"
  )
  expect_true(is.finite(ssr_at_mle))

  # Point far from MLE should have larger SSR
  ssr_far <- beezdemand:::.compute_ssr_grid(
    ln_q0_vec = mle$ln_q0 + 5,
    alpha_vec = mle$alpha * 100,
    k = mle$k,
    prices = agg$price,
    observed = agg$mean_log_y,
    equation = "zhao_exponential"
  )
  expect_true(ssr_far > ssr_at_mle)
})


# =============================================================================
# TMB Model Fixture
# =============================================================================

fit_tmb <- tryCatch(
  fit_demand_tmb(apt, y_var = "y", x_var = "x", id_var = "id",
                 equation = "exponentiated"),
  error = function(e) NULL
)


# =============================================================================
# TMB: Internal helper tests
# =============================================================================

test_that(".map_tmb_equation handles all TMB equation forms", {
  expect_equal(beezdemand:::.map_tmb_equation("exponential"), "tmb_exponential")
  expect_equal(beezdemand:::.map_tmb_equation("exponentiated"), "tmb_exponentiated")
  expect_equal(beezdemand:::.map_tmb_equation("simplified"), "tmb_simplified")
  expect_equal(beezdemand:::.map_tmb_equation("zben"), "tmb_zben")
  expect_error(
    beezdemand:::.map_tmb_equation("unknown"),
    "Unrecognized TMB equation"
  )
})

test_that(".extract_tmb_mle returns expected structure", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  mle <- beezdemand:::.extract_tmb_mle(fit_tmb)
  expect_true(is.list(mle))
  expect_true(mle$Q0 > 0)
  expect_true(mle$alpha > 0)
  expect_true(is.numeric(mle$ln_q0))
  expect_true(is.numeric(mle$ln_alpha))
})

test_that(".aggregate_tmb_means returns valid data", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  agg <- beezdemand:::.aggregate_tmb_means(fit_tmb)
  expect_true(is.data.frame(agg))
  expect_true(all(c("price", "mean_response", "n") %in% names(agg)))
  expect_true(nrow(agg) > 0)
  expect_true(all(agg$n > 0))
})

test_that(".compute_ssr_grid works for TMB equations", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  agg <- beezdemand:::.aggregate_tmb_means(fit_tmb)
  mle <- beezdemand:::.extract_tmb_mle(fit_tmb)

  ssr_at_mle <- beezdemand:::.compute_ssr_grid(
    ln_q0_vec = mle$ln_q0,
    alpha_vec = mle$alpha,
    k = mle$k,
    prices = agg$price,
    observed = agg$mean_response,
    equation = "tmb_exponentiated"
  )
  expect_true(is.finite(ssr_at_mle))

  ssr_far <- beezdemand:::.compute_ssr_grid(
    ln_q0_vec = mle$ln_q0 + 5,
    alpha_vec = mle$alpha * 100,
    k = mle$k,
    prices = agg$price,
    observed = agg$mean_response,
    equation = "tmb_exponentiated"
  )
  expect_true(ssr_far > ssr_at_mle)
})

test_that(".check_tmb_intercept_only passes for intercept-only model", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  expect_silent(beezdemand:::.check_tmb_intercept_only(fit_tmb))
})


# =============================================================================
# TMB: plot_loss_surface()
# =============================================================================

test_that("plot_loss_surface returns ggplot for TMB model", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_loss_surface(fit_tmb, resolution = 20)
  expect_s3_class(p, "gg")
})

test_that("plot_loss_surface.beezdemand_tmb respects show_mle and show_contours", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_loss_surface(fit_tmb, resolution = 20,
                         show_mle = TRUE, show_contours = TRUE)
  expect_s3_class(p, "gg")
  expect_gte(length(p$layers), 3)
})


# =============================================================================
# TMB: plot_loss_profile()
# =============================================================================

test_that("plot_loss_profile returns ggplot for TMB model", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_loss_profile(fit_tmb, parameter = "q0", resolution = 50)
  expect_s3_class(p, "gg")

  p2 <- plot_loss_profile(fit_tmb, parameter = "alpha", resolution = 50)
  expect_s3_class(p2, "gg")
})

test_that("plot_loss_profile returns combined plot for TMB 'both'", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  skip_if_not_installed("patchwork")
  p <- plot_loss_profile(fit_tmb, parameter = "both", resolution = 50)
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})


# =============================================================================
# TMB: plot_demand_overlay()
# =============================================================================

test_that("plot_demand_overlay works with TMB model", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_demand_overlay(fit_tmb, labels = "TMB exponentiated")
  expect_s3_class(p, "gg")
})

test_that("plot_demand_overlay works with mixed model types", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_demand_overlay(
    model_list = list(Hurdle = fit_hurdle, TMB = fit_tmb)
  )
  expect_s3_class(p, "gg")
})


# =============================================================================
# TMB: plot_model_comparison()
# =============================================================================

test_that("plot_model_comparison works with TMB model", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_model_comparison(fit_tmb, labels = "TMB")
  expect_s3_class(p, "gg")
})


# =============================================================================
# TMB: plot_re_diagnostics()
# =============================================================================

test_that("plot_re_diagnostics returns plots for TMB model (q0)", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_re_diagnostics(fit_tmb, which = "q0")
  expect_true(
    inherits(p, "gg") || inherits(p, "patchwork") ||
      inherits(p, "beezdemand_diagnostic_plots")
  )
})

test_that("plot_re_diagnostics handles 'all' for TMB model", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_re_diagnostics(fit_tmb, which = "all")
  expect_true(
    inherits(p, "gg") || inherits(p, "patchwork") ||
      inherits(p, "beezdemand_diagnostic_plots")
  )
})

test_that("plot_re_diagnostics TMB errors for unavailable RE", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  n_re <- fit_tmb$param_info$n_random_effects
  if (n_re < 2) {
    expect_error(
      plot_re_diagnostics(fit_tmb, which = "alpha"),
      "not found in model"
    )
  }
})

test_that("plot_re_diagnostics TMB does not accept 'zeros'", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  expect_error(
    plot_re_diagnostics(fit_tmb, which = "zeros"),
    "should be one of"
  )
})


# =============================================================================
# TMB: plot_expenditure()
# =============================================================================

test_that("plot_expenditure returns ggplot for TMB model", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_expenditure(fit_tmb)
  expect_s3_class(p, "gg")
})

test_that("plot_expenditure TMB works with custom prices", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_expenditure(fit_tmb, prices = c(0.5, 1, 2, 5, 10))
  expect_s3_class(p, "gg")
})

test_that("plot_expenditure TMB respects show_pmax/show_omax", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_expenditure(fit_tmb, show_pmax = FALSE, show_omax = FALSE)
  expect_s3_class(p, "gg")
})


# =============================================================================
# TMB: plot_elasticity()
# =============================================================================

test_that("plot_elasticity returns ggplot for TMB model", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_elasticity(fit_tmb)
  expect_s3_class(p, "gg")
})

test_that("plot_elasticity TMB works with custom prices", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_elasticity(fit_tmb, prices = c(0.5, 1, 2, 5, 10))
  expect_s3_class(p, "gg")
})

test_that("plot_elasticity TMB handles show_unit toggle", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_elasticity(fit_tmb, show_unit = FALSE)
  expect_s3_class(p, "gg")
})


# =============================================================================
# TMB: plot_alpha_distribution()
# =============================================================================

test_that("plot_alpha_distribution returns ggplot for TMB model", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_alpha_distribution(fit_tmb)
  expect_s3_class(p, "gg")
})

test_that("plot_alpha_distribution TMB supports histogram type", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_alpha_distribution(fit_tmb, type = "histogram")
  expect_s3_class(p, "gg")
})

test_that("plot_alpha_distribution TMB supports linear scale", {
  skip_if(is.null(fit_tmb), "TMB model fitting failed")
  p <- plot_alpha_distribution(fit_tmb, log_scale = FALSE)
  expect_s3_class(p, "gg")
})
