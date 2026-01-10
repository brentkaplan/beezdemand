# Tests for plot methods for beezdemand_hurdle class

test_that("plot type='demand' returns ggplot", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  skip_if_not_installed("ggplot2")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  p <- plot(fit, type = "demand")

  expect_s3_class(p, "ggplot")
})

test_that("plot type='probability' returns ggplot", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  skip_if_not_installed("ggplot2")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  p <- plot(fit, type = "probability")

  expect_s3_class(p, "ggplot")
})

test_that("plot type='probability' supports ids and faceting", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  skip_if_not_installed("ggplot2")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  p_ids <- plot(fit, type = "probability", ids = c("1", "2", "3"))
  expect_s3_class(p_ids, "ggplot")
  expect_true(inherits(p_ids$facet, "FacetNull"))
  expect_gte(length(p_ids$layers), 2)

  p_facet <- plot(fit, type = "probability", ids = c("1", "2", "3"), facet = TRUE)
  expect_s3_class(p_facet, "ggplot")
  expect_false(inherits(p_facet$facet, "FacetNull"))
})

test_that("plot type='parameters' works with parameter selection", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  skip_if_not_installed("ggplot2")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # All parameters
  p1 <- plot(fit, type = "parameters")
  expect_s3_class(p1, "ggplot")

  # Selected parameters
  p2 <- plot(fit, type = "parameters", parameters = c("Q0", "alpha"))
  expect_s3_class(p2, "ggplot")
})

test_that("plot type='individual' shows correct subjects", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  skip_if_not_installed("ggplot2")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # Default (first 9 subjects)
  p1 <- plot(fit, type = "individual")
  expect_s3_class(p1, "ggplot")

  # Specific subjects
  p2 <- plot(fit, type = "individual", ids = c("1", "2", "3"))
  expect_s3_class(p2, "ggplot")
})

test_that("plot_subject shows single subject curve", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  skip_if_not_installed("ggplot2")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  p <- plot_subject(fit, subject_id = "1")

  expect_s3_class(p, "ggplot")
})

test_that("plot_subject errors for invalid subject", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  expect_error(
    plot_subject(fit, subject_id = "nonexistent"),
    "not found in model"
  )
})

test_that("plot with x_trans/y_trans works", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  skip_if_not_installed("ggplot2")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  p <- plot(fit, type = "demand", x_trans = "log10", y_trans = "log10")

  expect_s3_class(p, "ggplot")
})
