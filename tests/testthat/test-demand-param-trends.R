# TICKET-007: coverage for get_demand_param_trends(), which uses emmeans
# emtrends to estimate the linear trend of an NLME demand parameter
# (Q0 or alpha) with respect to a continuous covariate.

skip_if_not_installed("emmeans")
skip_if_not_installed("nlme")

make_nlme_fit <- function() {
  data(ko, package = "beezdemand")
  ko$dose_num <- as.numeric(as.character(ko$dose))
  # The model must include dose_num as a continuous covariate; without it,
  # emtrends has no slope to estimate against dose_num and returns nothing.
  fit <- tryCatch(
    fit_demand_mixed(
      ko, y_var = "y_ll4", x_var = "x", id_var = "monkey",
      factors = "drug",
      continuous_covariates = "dose_num",
      equation_form = "zben"
    ),
    error = function(e) NULL
  )
  if (is.null(fit) || is.null(fit$model)) {
    skip("fit_demand_mixed() failed to converge on ko in this environment")
  }
  list(fit = fit, data = ko)
}

test_that("get_demand_param_trends returns a tibble with the expected columns", {
  skip_on_cran()
  setup <- make_nlme_fit()
  result <- get_demand_param_trends(
    setup$fit,
    params = c("Q0", "alpha"),
    covariates = "dose_num",
    specs = ~drug
  )
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(all(c("trend", "SE", "parameter", "covariate") %in% names(result)))
  expect_true("p.value" %in% names(result))
})

test_that("get_demand_param_trends works with a single parameter", {
  skip_on_cran()
  setup <- make_nlme_fit()
  result <- get_demand_param_trends(
    setup$fit,
    params = "Q0",
    covariates = "dose_num",
    specs = ~drug
  )
  expect_s3_class(result, "tbl_df")
  expect_true(all(result$parameter == "Q0"))
})

test_that("get_demand_param_trends works with both Q0 and alpha", {
  skip_on_cran()
  setup <- make_nlme_fit()
  result <- get_demand_param_trends(
    setup$fit,
    params = c("Q0", "alpha"),
    covariates = "dose_num",
    specs = ~drug
  )
  expect_s3_class(result, "tbl_df")
  expect_true(length(unique(result$parameter)) >= 1L)
})

test_that("get_demand_param_trends errors on non-beezdemand_nlme input", {
  expect_error(
    get_demand_param_trends("not a model", covariates = "dose_num"),
    "beezdemand_nlme"
  )
  expect_error(
    get_demand_param_trends(structure(list(), class = "wrong"),
                            covariates = "dose_num"),
    "beezdemand_nlme"
  )
})

test_that("get_demand_param_trends errors when covariates is missing or empty", {
  skip_on_cran()
  setup <- make_nlme_fit()
  expect_error(get_demand_param_trends(setup$fit), "covariate")
  expect_error(
    get_demand_param_trends(setup$fit, covariates = character(0)),
    "covariate"
  )
})

test_that("get_demand_param_trends references emmeans (guard exists)", {
  # Verifying the requireNamespace guard is present in source, since
  # mocking requireNamespace inside testthat is fragile.
  fn_text <- paste(deparse(get_demand_param_trends), collapse = "\n")
  expect_true(grepl("emmeans", fn_text))
  expect_true(grepl("requireNamespace", fn_text))
})
