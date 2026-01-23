# Test confint() methods for all model classes

test_that("confint.beezdemand_fixed returns tibble with correct structure", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  # Suppress deprecation warning from FitCurves
  fit <- suppressMessages(
    fit_demand_fixed(apt_small, equation = "hs", k = 2)
  )

  ci <- confint(fit)

  expect_s3_class(ci, "tbl_df")
  expect_true(all(c("id", "term", "estimate", "conf.low", "conf.high", "level") %in%
                    names(ci)))
  expect_equal(unique(ci$level), 0.95)
  expect_true(nrow(ci) > 0)
})

test_that("confint.beezdemand_fixed respects level parameter", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- suppressMessages(
    fit_demand_fixed(apt_small, equation = "hs", k = 2)
  )

  ci_90 <- confint(fit, level = 0.90)
  ci_95 <- confint(fit, level = 0.95)

  expect_equal(unique(ci_90$level), 0.90)
  expect_equal(unique(ci_95$level), 0.95)

  # 90% CI should be narrower than 95% CI
  ci_90_q0 <- ci_90[ci_90$term == "Q0", ]
  ci_95_q0 <- ci_95[ci_95$term == "Q0", ]

  if (nrow(ci_90_q0) > 0 && nrow(ci_95_q0) > 0) {
    width_90 <- ci_90_q0$conf.high[1] - ci_90_q0$conf.low[1]
    width_95 <- ci_95_q0$conf.high[1] - ci_95_q0$conf.low[1]
    if (!is.na(width_90) && !is.na(width_95)) {
      expect_lt(width_90, width_95)
    }
  }
})

test_that("confint.beezdemand_fixed filters parameters with parm argument", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- suppressMessages(
    fit_demand_fixed(apt_small, equation = "hs", k = 2)
  )

  ci_all <- confint(fit)
  ci_q0 <- confint(fit, parm = "Q0")

  expect_true(nrow(ci_q0) < nrow(ci_all))
  expect_true(all(ci_q0$term == "Q0"))
})

test_that("confint.beezdemand_fixed handles empty results gracefully", {
  fit <- structure(
    list(results = data.frame()),
    class = c("beezdemand_fixed", "list")
  )

  ci <- confint(fit)

  expect_s3_class(ci, "tbl_df")
  expect_equal(nrow(ci), 0)
})


test_that("confint.beezdemand_hurdle returns tibble with correct structure", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  fit <- tryCatch(
    fit_demand_hurdle(apt_small, y_var = "y", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  ci <- confint(fit)

  expect_s3_class(ci, "tbl_df")
  expect_true(all(c("term", "estimate", "conf.low", "conf.high", "level",
                    "component", "estimate_scale") %in% names(ci)))
  expect_equal(unique(ci$level), 0.95)
  expect_true(nrow(ci) > 0)
})

test_that("confint.beezdemand_hurdle supports report_space argument", {
  skip_if_not_installed("TMB")
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:5], ]

  fit <- tryCatch(
    fit_demand_hurdle(apt_small, y_var = "y", x_var = "x", id_var = "id"),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "Model fitting failed")

  ci_internal <- confint(fit, report_space = "internal")
  ci_natural <- confint(fit, report_space = "natural")

  # Internal scale should have log terms, natural should not
  expect_true(any(grepl("log", ci_internal$term)))

  # Natural scale Q0 should be exponentiated (larger than log scale)
  q0_internal <- ci_internal[grepl("Q0|log_q0", ci_internal$term), "estimate"]
  q0_natural <- ci_natural[grepl("Q0", ci_natural$term), "estimate"]

  if (nrow(q0_internal) > 0 && nrow(q0_natural) > 0) {
    expect_true(q0_natural$estimate[1] > q0_internal$estimate[1])
  }
})


test_that("confint.beezdemand_nlme returns tibble with correct structure", {
  skip_on_cran()

  # Create simple test data with non-negative log10 values
  set.seed(42)
  n_subj <- 8
  n_obs <- 8
  prices <- c(0.1, 0.5, 1, 2, 5, 10, 20, 50)
  base_consumption <- c(100, 95, 85, 65, 30, 12, 4, 1)
  test_data <- do.call(rbind, lapply(1:n_subj, function(i) {
    y_vals <- base_consumption * (0.8 + 0.4 * runif(1)) + rnorm(n_obs, 0, 3)
    y_vals <- pmax(y_vals, 0.1)
    data.frame(
      id = i,
      x = prices,
      y = log10(y_vals)
    )
  }))

  fit <- tryCatch(
    suppressWarnings(suppressMessages(
      fit_demand_mixed(
        test_data,
        y_var = "y",
        x_var = "x",
        id_var = "id",
        equation_form = "zben"
      )
    )),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model fitting failed")

  ci <- confint(fit)

  expect_s3_class(ci, "tbl_df")
  expect_true(all(c("term", "estimate", "conf.low", "conf.high", "level",
                    "component") %in% names(ci)))
  if (nrow(ci) > 0) {
    expect_equal(unique(ci$level), 0.95)
  }
})

test_that("confint.beezdemand_nlme supports method argument", {
  skip_on_cran()

  # Create simple test data with non-negative log10 values
  set.seed(42)
  n_subj <- 8
  n_obs <- 8
  prices <- c(0.1, 0.5, 1, 2, 5, 10, 20, 50)
  base_consumption <- c(100, 95, 85, 65, 30, 12, 4, 1)
  test_data <- do.call(rbind, lapply(1:n_subj, function(i) {
    y_vals <- base_consumption * (0.8 + 0.4 * runif(1)) + rnorm(n_obs, 0, 3)
    y_vals <- pmax(y_vals, 0.1)
    data.frame(
      id = i,
      x = prices,
      y = log10(y_vals)
    )
  }))

  fit <- tryCatch(
    suppressWarnings(suppressMessages(
      fit_demand_mixed(
        test_data,
        y_var = "y",
        x_var = "x",
        id_var = "id",
        equation_form = "zben"
      )
    )),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model fitting failed")

  ci_wald <- confint(fit, method = "wald")

  expect_s3_class(ci_wald, "tbl_df")
  if (nrow(ci_wald) > 0) {
    expect_equal(unique(ci_wald$level), 0.95)
  }
})


test_that("confint.cp_model_nls returns tibble with correct structure", {
  skip_on_cran()

  # Create simple cross-price test data
  set.seed(123)
  n_obs <- 20
  test_data <- data.frame(
    x = seq(0.1, 10, length.out = n_obs),
    y = 100 * 10^(-0.5 * exp(-0.3 * seq(0.1, 10, length.out = n_obs))) +
        rnorm(n_obs, 0, 5)
  )
  test_data$y <- pmax(test_data$y, 0.1)

  fit <- tryCatch(
    fit_cp_nls(test_data, equation = "exponentiated", return_all = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model fitting failed")

  ci <- confint(fit)

  expect_s3_class(ci, "tbl_df")
  expect_true(all(c("term", "estimate", "conf.low", "conf.high", "level",
                    "method") %in% names(ci)))
  expect_equal(unique(ci$level), 0.95)
})

test_that("confint.cp_model_nls respects method argument", {
  skip_on_cran()

  set.seed(123)
  n_obs <- 20
  test_data <- data.frame(
    x = seq(0.1, 10, length.out = n_obs),
    y = 100 * 10^(-0.5 * exp(-0.3 * seq(0.1, 10, length.out = n_obs))) +
        rnorm(n_obs, 0, 5)
  )
  test_data$y <- pmax(test_data$y, 0.1)

  fit <- tryCatch(
    fit_cp_nls(test_data, equation = "exponentiated", return_all = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(fit) || is.null(fit$model), "Model fitting failed")

  ci_asymp <- confint(fit, method = "asymptotic")

  expect_s3_class(ci_asymp, "tbl_df")
  expect_equal(unique(ci_asymp$method), "asymptotic")
})


test_that("confint methods reject invalid level arguments", {
  data(apt, package = "beezdemand")
  apt_small <- apt[apt$id %in% unique(apt$id)[1:3], ]

  fit <- suppressMessages(
    fit_demand_fixed(apt_small, equation = "hs", k = 2)
  )

  expect_error(confint(fit, level = 0), "between 0 and 1")
  expect_error(confint(fit, level = 1), "between 0 and 1")
  expect_error(confint(fit, level = -0.5), "between 0 and 1")
  expect_error(confint(fit, level = "0.95"), "between 0 and 1")
})
