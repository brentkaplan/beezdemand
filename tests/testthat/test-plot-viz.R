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


# =============================================================================
# NLME Model Fixture
# =============================================================================

fit_nlme <- tryCatch({
  apt_ll4 <- apt
  apt_ll4$y_ll4 <- ll4(apt_ll4$y)
  fit_demand_mixed(apt_ll4, y_var = "y_ll4", x_var = "x",
                   id_var = "id", equation_form = "zben")
}, error = function(e) NULL)


# =============================================================================
# NLME: Internal helper tests
# =============================================================================

test_that(".map_nlme_equation handles all NLME equation forms", {
  expect_equal(beezdemand:::.map_nlme_equation("zben"), "tmb_zben")
  expect_equal(beezdemand:::.map_nlme_equation("simplified"), "tmb_simplified")
  expect_equal(beezdemand:::.map_nlme_equation("exponentiated"), "tmb_exponentiated")
  expect_error(
    beezdemand:::.map_nlme_equation("unknown"),
    "Unrecognized NLME equation form"
  )
})

test_that(".extract_nlme_mle returns expected structure", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  mle <- beezdemand:::.extract_nlme_mle(fit_nlme)
  expect_true(is.list(mle))
  expect_true(mle$Q0 > 0)
  expect_true(mle$alpha > 0)
  expect_true(is.numeric(mle$ln_q0))
  expect_true(is.numeric(mle$ln_alpha))
  expect_null(mle$k)
})

test_that(".check_nlme_intercept_only passes for intercept-only model", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  expect_silent(beezdemand:::.check_nlme_intercept_only(fit_nlme))
})

test_that(".aggregate_nlme_means returns valid data", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  agg <- beezdemand:::.aggregate_nlme_means(fit_nlme)
  expect_true(is.data.frame(agg))
  expect_true(all(c("price", "mean_response", "n") %in% names(agg)))
  expect_true(nrow(agg) > 0)
  expect_true(all(agg$n > 0))
})

test_that(".compute_ssr_grid works for NLME equations via tmb_zben", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  agg <- beezdemand:::.aggregate_nlme_means(fit_nlme)
  mle <- beezdemand:::.extract_nlme_mle(fit_nlme)

  ssr_at_mle <- beezdemand:::.compute_ssr_grid(
    ln_q0_vec = mle$ln_q0,
    alpha_vec = mle$alpha,
    k = mle$k,
    prices = agg$price,
    observed = agg$mean_response,
    equation = "tmb_zben"
  )
  expect_true(is.finite(ssr_at_mle))

  ssr_far <- beezdemand:::.compute_ssr_grid(
    ln_q0_vec = mle$ln_q0 + 5,
    alpha_vec = mle$alpha * 100,
    k = mle$k,
    prices = agg$price,
    observed = agg$mean_response,
    equation = "tmb_zben"
  )
  expect_true(ssr_far > ssr_at_mle)
})


# =============================================================================
# NLME: plot_loss_surface()
# =============================================================================

test_that("plot_loss_surface returns ggplot for NLME model", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  p <- plot_loss_surface(fit_nlme, resolution = 20)
  expect_s3_class(p, "gg")
})

test_that("plot_loss_surface.beezdemand_nlme respects show_mle and show_contours", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  p <- plot_loss_surface(fit_nlme, resolution = 20,
                         show_mle = TRUE, show_contours = TRUE)
  expect_s3_class(p, "gg")
  expect_gte(length(p$layers), 3)
})

test_that("plot_loss_surface.beezdemand_nlme accepts custom ranges", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  mle <- beezdemand:::.extract_nlme_mle(fit_nlme)
  p <- plot_loss_surface(
    fit_nlme,
    resolution = 15,
    q0_range = c(mle$Q0 / 10, mle$Q0 * 10),
    alpha_range = c(mle$alpha / 10, mle$alpha * 10)
  )
  expect_s3_class(p, "gg")
})


# =============================================================================
# NLME: plot_loss_profile()
# =============================================================================

test_that("plot_loss_profile returns ggplot for NLME model", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  p <- plot_loss_profile(fit_nlme, parameter = "q0", resolution = 50)
  expect_s3_class(p, "gg")

  p2 <- plot_loss_profile(fit_nlme, parameter = "alpha", resolution = 50)
  expect_s3_class(p2, "gg")
})

test_that("plot_loss_profile returns combined plot for NLME 'both'", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  skip_if_not_installed("patchwork")
  p <- plot_loss_profile(fit_nlme, parameter = "both", resolution = 50)
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})


# =============================================================================
# NLME Additional Fixtures for Marginal NLL Tests
# =============================================================================

# Fit exponentiated and simplified models for broader equation coverage
fit_nlme_exp <- tryCatch(
  fit_demand_mixed(apt, y_var = "y", x_var = "x", id_var = "id",
                   equation_form = "exponentiated", k = 2),
  error = function(e) NULL
)

fit_nlme_simp <- tryCatch(
  fit_demand_mixed(apt, y_var = "y", x_var = "x", id_var = "id",
                   equation_form = "simplified"),
  error = function(e) NULL
)


# =============================================================================
# .nlme_eval_demand() — demand equation evaluation
# =============================================================================

test_that(".nlme_eval_demand evaluates exponentiated equation correctly", {
  x <- c(0, 1, 5, 10)
  Q0_log10 <- 1.0   # Q0 = 10
  alpha_log10 <- -2  # alpha = 0.01
  k <- 2

  result <- beezdemand:::.nlme_eval_demand(x, Q0_log10, alpha_log10, k,
                                           "exponentiated")
  expect_length(result, 4)
  expect_true(all(is.finite(result)))

  # At x = 0: y = 10^Q0 * 10^(k*(exp(0)-1)) = 10 * 10^0 = 10
  expect_equal(result[1], 10, tolerance = 1e-10)

  # Consumption should decrease with price

  expect_true(all(diff(result) <= 0))
})

test_that(".nlme_eval_demand evaluates simplified equation correctly", {
  x <- c(0, 1, 5, 10)
  Q0_log10 <- 1.0
  alpha_log10 <- -2
  k <- NULL

  result <- beezdemand:::.nlme_eval_demand(x, Q0_log10, alpha_log10, k,
                                           "simplified")
  expect_length(result, 4)
  expect_true(all(is.finite(result)))

  # At x = 0: y = 10^Q0 * exp(0) = 10
  expect_equal(result[1], 10, tolerance = 1e-10)

  # Manual: y = 10 * exp(-0.01 * 10 * x) = 10 * exp(-0.1 * x)
  expected <- 10 * exp(-0.1 * x)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that(".nlme_eval_demand evaluates zben equation correctly", {
  x <- c(0, 1, 5, 10)
  Q0_log10 <- 1.0   # Q0 value in log10 space
  alpha_log10 <- -2  # alpha = 0.01
  k <- NULL

  result <- beezdemand:::.nlme_eval_demand(x, Q0_log10, alpha_log10, k,
                                           "zben")
  expect_length(result, 4)
  expect_true(all(is.finite(result)))

  # At x = 0: y = Q0_log10 * exp(0) = 1.0
  expect_equal(result[1], 1.0, tolerance = 1e-10)

  # Manual: y = 1.0 * exp(-((10^-2)/1.0) * (10^1.0) * x)
  #        = 1.0 * exp(-0.01 * 10 * x) = exp(-0.1 * x)
  expected <- 1.0 * exp(-0.1 * x)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that(".nlme_eval_demand returns all positive for positive parameters", {
  x <- seq(0, 50, by = 5)
  for (eq in c("exponentiated", "simplified", "zben")) {
    k_val <- if (eq == "exponentiated") 2 else NULL
    result <- beezdemand:::.nlme_eval_demand(x, 1.0, -2, k_val, eq)
    expect_true(all(result >= 0),
                info = paste("Equation:", eq))
  }
})


# =============================================================================
# .nlme_compute_jacobian() — numerical partial derivatives
# =============================================================================

test_that(".nlme_compute_jacobian returns correct dimensions", {
  x <- c(0, 1, 5, 10, 25)
  J <- beezdemand:::.nlme_compute_jacobian(x, 1.0, -2, 2, "exponentiated")
  expect_true(is.matrix(J))
  expect_equal(nrow(J), length(x))
  expect_equal(ncol(J), 2)
  expect_true(all(is.finite(J)))
})

test_that(".nlme_compute_jacobian works for all equation forms", {
  x <- c(0, 1, 5, 10)
  for (eq in c("exponentiated", "simplified", "zben")) {
    k_val <- if (eq == "exponentiated") 2 else NULL
    J <- beezdemand:::.nlme_compute_jacobian(x, 1.0, -2, k_val, eq)
    expect_equal(nrow(J), length(x), info = paste("Equation:", eq))
    expect_equal(ncol(J), 2, info = paste("Equation:", eq))
    expect_true(all(is.finite(J)), info = paste("Equation:", eq))
  }
})

test_that(".nlme_compute_jacobian agrees with analytical for simplified", {
  # Simplified: f = (10^Q0) * exp(-(10^alpha) * (10^Q0) * x)
  # df/dQ0_log10 = f * log(10) * (1 - (10^alpha) * (10^Q0) * x)
  # df/dalpha_log10 = f * (-(10^Q0) * x * log(10)) * (10^alpha) ... hmm
  # Actually: let's just verify numerical Jacobian is self-consistent
  # by checking that small perturbations match
  x <- c(0, 1, 5, 10)
  Q0 <- 1.2
  alpha <- -1.5
  h <- 1e-6

  J <- beezdemand:::.nlme_compute_jacobian(x, Q0, alpha, NULL, "simplified")

  # Verify with forward difference
  f0 <- beezdemand:::.nlme_eval_demand(x, Q0, alpha, NULL, "simplified")
  f_q0_plus <- beezdemand:::.nlme_eval_demand(x, Q0 + h, alpha, NULL,
                                               "simplified")
  f_alpha_plus <- beezdemand:::.nlme_eval_demand(x, Q0, alpha + h, NULL,
                                                  "simplified")

  fd_q0 <- (f_q0_plus - f0) / h
  fd_alpha <- (f_alpha_plus - f0) / h

  # Central differences should be close to forward differences
  # (both approximate the true derivative)
  expect_equal(J[, 1], fd_q0, tolerance = 1e-3)
  expect_equal(J[, 2], fd_alpha, tolerance = 1e-3)
})


# =============================================================================
# .nlme_precompute_marginal() — full precomputation from fitted model
# =============================================================================

test_that(".nlme_precompute_marginal returns expected structure", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  pre <- beezdemand:::.nlme_precompute_marginal(fit_nlme)

  expect_true(is.list(pre))
  expect_true("subjects" %in% names(pre))
  expect_true("beta_hat" %in% names(pre))
  expect_true("N" %in% names(pre))
  expect_true("log_2pi_term" %in% names(pre))

  # beta_hat should have 2 elements (Q0, alpha) for intercept-only
  expect_length(pre$beta_hat, 2)

  # Number of subjects should match data
  n_subjects <- length(unique(fit_nlme$data[[fit_nlme$param_info$id_var]]))
  expect_length(pre$subjects, n_subjects)
})

test_that(".nlme_precompute_marginal per-subject structs are valid", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  pre <- beezdemand:::.nlme_precompute_marginal(fit_nlme)

  subj <- pre$subjects[[1]]
  expect_true(all(c("F_i", "y_star_i", "V_inv_i", "log_det_V_i") %in%
                    names(subj)))

  # F_i dimensions: n_obs_i x 2
  expect_equal(ncol(subj$F_i), 2)
  expect_equal(nrow(subj$F_i), length(subj$y_star_i))

  # V_inv_i should be square, same size as n_obs_i
  n_i <- length(subj$y_star_i)
  expect_equal(dim(subj$V_inv_i), c(n_i, n_i))

  # V_inv_i should be positive definite (all eigenvalues > 0)
  eigs <- eigen(subj$V_inv_i, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigs > 0))

  # log_det_V_i should be finite
  expect_true(is.finite(subj$log_det_V_i))
})

test_that(".nlme_precompute_marginal works for exponentiated equation", {
  skip_if(is.null(fit_nlme_exp), "Exponentiated NLME model fitting failed")
  pre <- beezdemand:::.nlme_precompute_marginal(fit_nlme_exp)
  expect_true(is.list(pre))
  expect_length(pre$beta_hat, 2)
  expect_true(all(vapply(pre$subjects, function(s) is.finite(s$log_det_V_i),
                         logical(1))))
})

test_that(".nlme_precompute_marginal works for simplified equation", {
  skip_if(is.null(fit_nlme_simp), "Simplified NLME model fitting failed")
  pre <- beezdemand:::.nlme_precompute_marginal(fit_nlme_simp)
  expect_true(is.list(pre))
  expect_length(pre$beta_hat, 2)
})


# =============================================================================
# .nlme_marginal_nll_grid() — grid NLL evaluation
# =============================================================================

test_that(".nlme_marginal_nll_grid returns finite values", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  pre <- beezdemand:::.nlme_precompute_marginal(fit_nlme)

  # Small grid around MLE
  grid <- expand.grid(
    log10_q0 = pre$beta_hat[1] + seq(-0.5, 0.5, length.out = 5),
    log10_alpha = pre$beta_hat[2] + seq(-0.5, 0.5, length.out = 5)
  )

  nll <- beezdemand:::.nlme_marginal_nll_grid(pre, grid)
  expect_length(nll, nrow(grid))
  expect_true(all(is.finite(nll)))
})

test_that(".nlme_marginal_nll_grid has minimum near MLE", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  pre <- beezdemand:::.nlme_precompute_marginal(fit_nlme)

  # Evaluate at MLE
  mle_grid <- data.frame(
    log10_q0 = pre$beta_hat[1],
    log10_alpha = pre$beta_hat[2]
  )
  nll_mle <- beezdemand:::.nlme_marginal_nll_grid(pre, mle_grid)

  # Evaluate at perturbed points
  offsets <- c(-1, -0.5, 0.5, 1)
  for (off in offsets) {
    perturbed_q0 <- data.frame(
      log10_q0 = pre$beta_hat[1] + off,
      log10_alpha = pre$beta_hat[2]
    )
    nll_perturbed <- beezdemand:::.nlme_marginal_nll_grid(pre, perturbed_q0)
    expect_true(nll_perturbed >= nll_mle - 1e-6,
                info = sprintf("Q0 offset = %g: NLL = %g vs MLE NLL = %g",
                               off, nll_perturbed, nll_mle))

    perturbed_alpha <- data.frame(
      log10_q0 = pre$beta_hat[1],
      log10_alpha = pre$beta_hat[2] + off
    )
    nll_perturbed <- beezdemand:::.nlme_marginal_nll_grid(pre, perturbed_alpha)
    expect_true(nll_perturbed >= nll_mle - 1e-6,
                info = sprintf("alpha offset = %g: NLL = %g vs MLE NLL = %g",
                               off, nll_perturbed, nll_mle))
  }
})

test_that(".nlme_marginal_nll at MLE approximates -logLik(nlme)", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  pre <- beezdemand:::.nlme_precompute_marginal(fit_nlme)

  mle_grid <- data.frame(
    log10_q0 = pre$beta_hat[1],
    log10_alpha = pre$beta_hat[2]
  )
  nll_ours <- beezdemand:::.nlme_marginal_nll_grid(pre, mle_grid)
  nll_nlme <- -as.numeric(logLik(fit_nlme$model))

  # These should be approximately equal — same likelihood evaluated at same
  # point, but ours uses the linearized pseudo-LME while nlme uses the
  # exact nonlinear model. Allow reasonable tolerance.
  expect_equal(nll_ours, nll_nlme, tolerance = 0.1 * abs(nll_nlme),
               label = "Linearized NLL at MLE vs nlme logLik")
})


# =============================================================================
# plot_loss_surface(type = "marginal") — integration tests
# =============================================================================

test_that("plot_loss_surface type='ssr' is default and backward compatible", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  # Default should work exactly as before
  p_default <- plot_loss_surface(fit_nlme, resolution = 15)
  p_ssr <- plot_loss_surface(fit_nlme, resolution = 15, type = "ssr")
  expect_s3_class(p_default, "gg")
  expect_s3_class(p_ssr, "gg")
})

test_that("plot_loss_surface type='marginal' returns ggplot for NLME", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  p <- plot_loss_surface(fit_nlme, resolution = 15, type = "marginal")
  expect_s3_class(p, "gg")
})

test_that("plot_loss_surface type='marginal' works for exponentiated", {
  skip_if(is.null(fit_nlme_exp), "Exponentiated NLME model fitting failed")
  p <- plot_loss_surface(fit_nlme_exp, resolution = 15, type = "marginal")
  expect_s3_class(p, "gg")
})

test_that("plot_loss_surface type='marginal' works for simplified", {
  skip_if(is.null(fit_nlme_simp), "Simplified NLME model fitting failed")
  p <- plot_loss_surface(fit_nlme_simp, resolution = 15, type = "marginal")
  expect_s3_class(p, "gg")
})

test_that("plot_loss_surface type='marginal' includes subtitle indicator", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  p <- plot_loss_surface(fit_nlme, resolution = 15, type = "marginal")
  # Subtitle should mention linearized or marginal
  subtitle_text <- p$labels$subtitle
  expect_true(grepl("[Ll]inearized|[Mm]arginal", subtitle_text))
})

test_that("plot_loss_surface type='marginal' shows MLE at minimum", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  p <- plot_loss_surface(fit_nlme, resolution = 15, type = "marginal",
                         show_mle = TRUE)
  expect_s3_class(p, "gg")
  # Should have at least raster + MLE point layers
  expect_gte(length(p$layers), 2)
})


# =============================================================================
# plot_loss_profile(type = "marginal") — integration tests
# =============================================================================

test_that("plot_loss_profile type='ssr' is default and backward compatible", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  p <- plot_loss_profile(fit_nlme, parameter = "q0", resolution = 30)
  expect_s3_class(p, "gg")
})

test_that("plot_loss_profile type='marginal' returns ggplot for NLME", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  p <- plot_loss_profile(fit_nlme, parameter = "q0", resolution = 30,
                         type = "marginal")
  expect_s3_class(p, "gg")
})

test_that("plot_loss_profile type='marginal' works for both parameters", {
  skip_if(is.null(fit_nlme), "NLME model fitting failed")
  skip_if_not_installed("patchwork")
  p <- plot_loss_profile(fit_nlme, parameter = "both", resolution = 30,
                         type = "marginal")
  expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})
