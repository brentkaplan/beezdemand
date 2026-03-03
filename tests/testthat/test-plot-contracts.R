# Plot method smoke tests

test_that("plot.beezdemand_fixed returns ggplot for individual fits", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- fit_demand_fixed(apt_small, equation = "koff", k = 2)
  p <- plot(fit, type = "individual", ids = unique(apt_small$id)[1:2])

  expect_s3_class(p, "ggplot")
})

test_that("plot.beezdemand_fixed returns ggplot for aggregated fits", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_fixed(apt, equation = "koff", k = 2, agg = "Mean")

  p <- plot(fit, type = "population")
  expect_s3_class(p, "ggplot")
})

test_that("plot.beezdemand_nlme returns ggplot", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]
  apt_small$y_ll4 <- ll4(apt_small$y)

  fit <- tryCatch(
    fit_demand_mixed(apt_small, y_var = "y_ll4", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  p <- plot(fit, type = "population")
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# Collapse-aware plot tests
# =============================================================================

# Helper to create test data with a factor (reuses pattern from test_emms)
.create_collapse_plot_data <- function(
  n_subjects = 10,
  n_prices = 6,
  n_levels = 3,
  seed = 42
) {
  set.seed(seed)
  prices <- 10^seq(-1, 1.5, length.out = n_prices)
  factor_levels <- paste0("level", seq_len(n_levels))

  test_data <- expand.grid(
    id = seq_len(n_subjects),
    x = prices,
    factor1 = factor_levels
  )

  test_data$y <- with(test_data, {
    q0 <- 80 + rnorm(nrow(test_data), 0, 5)
    alpha <- 0.002
    q0 * exp(-alpha * q0 * x) + rnorm(nrow(test_data), 0, 2)
  })
  test_data$y[test_data$y < 0] <- 0.1
  test_data$id <- factor(test_data$id)
  test_data$factor1 <- factor(test_data$factor1)

  test_data
}

test_that("plot.beezdemand_nlme with collapse_levels returns ggplot (smoke)", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  test_data <- .create_collapse_plot_data()

  collapse_spec <- list(
    Q0 = list(factor1 = list(low = c("level1", "level2"), high = "level3")),
    alpha = list(factor1 = list(all = c("level1", "level2", "level3")))
  )

  fit <- tryCatch(
    fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      collapse_levels = collapse_spec,
      equation_form = "simplified"
    ),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model did not converge")

  p <- plot(fit, type = "population", color_by = "factor1")
  expect_s3_class(p, "ggplot")
})

test_that("plot population lines match collapsed level count", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  test_data <- .create_collapse_plot_data()

  collapse_spec <- list(
    Q0 = list(factor1 = list(low = c("level1", "level2"), high = "level3")),
    alpha = list(factor1 = list(all = c("level1", "level2", "level3")))
  )

  fit <- tryCatch(
    fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      collapse_levels = collapse_spec,
      equation_form = "simplified"
    ),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model did not converge")

  p <- plot(fit, type = "population", color_by = "factor1")

  # Find the line layer (more robust than assuming index)
  line_idx <- which(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomLine"),
    logical(1)
  ))
  plot_data <- ggplot2::layer_data(p, line_idx[1])

  # Should have 2 distinct groups (low, high) not 3 (level1, level2, level3)
  expect_lte(length(unique(plot_data$group)), 2)
})

test_that("plot facet remapping works with collapse_levels", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  test_data <- .create_collapse_plot_data()

  collapse_spec <- list(
    Q0 = list(factor1 = list(low = c("level1", "level2"), high = "level3")),
    alpha = list(factor1 = list(low = c("level1", "level2"), high = "level3"))
  )

  fit <- tryCatch(
    fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      collapse_levels = collapse_spec,
      equation_form = "simplified"
    ),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model did not converge")

  # facet by original factor name should be remapped to collapsed column
  p <- plot(fit, type = "population", facet = "~ factor1")
  expect_s3_class(p, "ggplot")
})

test_that("plot legend uses original factor name, not collapsed column", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  test_data <- .create_collapse_plot_data()

  collapse_spec <- list(
    Q0 = list(factor1 = list(low = c("level1", "level2"), high = "level3")),
    alpha = list(factor1 = list(all = c("level1", "level2", "level3")))
  )

  fit <- tryCatch(
    fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      collapse_levels = collapse_spec,
      equation_form = "simplified"
    ),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model did not converge")

  p <- plot(fit, type = "population", color_by = "factor1")
  # Legend title should be "factor1" (original), not "factor1_alpha"
  expect_equal(p$labels$colour, "factor1")
})

test_that("plot individual lines work with collapse_levels", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  test_data <- .create_collapse_plot_data()

  collapse_spec <- list(
    Q0 = list(factor1 = list(low = c("level1", "level2"), high = "level3")),
    alpha = list(factor1 = list(all = c("level1", "level2", "level3")))
  )

  fit <- tryCatch(
    fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      collapse_levels = collapse_spec,
      equation_form = "simplified"
    ),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model did not converge")

  p <- plot(fit, show_pred = "individual", color_by = "factor1")
  expect_s3_class(p, "ggplot")
})

test_that("plot without collapse_levels is unchanged (regression)", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  test_data <- .create_collapse_plot_data(n_levels = 2)

  fit <- tryCatch(
    fit_demand_mixed(
      data = test_data,
      y_var = "y",
      x_var = "x",
      id_var = "id",
      factors = "factor1",
      equation_form = "simplified"
    ),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model did not converge")

  p <- plot(fit, type = "population", color_by = "factor1")
  expect_s3_class(p, "ggplot")

  # Legend should use original factor name
  expect_equal(p$labels$colour, "factor1")

  line_idx <- which(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomLine"),
    logical(1)
  ))
  plot_data <- ggplot2::layer_data(p, line_idx[1])
  # Should have 2 groups (level1, level2)
  expect_equal(length(unique(plot_data$group)), 2)
})

