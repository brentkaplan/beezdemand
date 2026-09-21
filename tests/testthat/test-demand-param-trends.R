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

# --- TICKET-064 (F13): dropped (param, covariate) combinations warn --------

test_that("get_demand_param_trends warns naming a dropped (param, covariate) combination", {
  skip_on_cran()
  setup <- make_nlme_fit()

  warns <- testthat::capture_warnings(
    result <- get_demand_param_trends(
      setup$fit,
      params = c("Q0", "alpha"),
      covariates = c("dose_num", "not_a_real_covariate"),
      specs = ~drug
    )
  )
  drop_warns <- grepl("not_a_real_covariate", warns, fixed = TRUE)
  expect_true(any(drop_warns))
  # the valid covariate's rows are still returned
  expect_true(all(result$covariate == "dose_num"))
  expect_true(nrow(result) > 0)
})

test_that("get_demand_param_trends: all-valid covariates raise no dropped-combo warning", {
  skip_on_cran()
  setup <- make_nlme_fit()
  # The ko zben fit sits near the apVar PD boundary: on some platforms (Windows,
  # covr-instrumented Linux) the convergence gate fires. That warning is
  # correct and out of scope here; only a dropped-combo warning should fail.
  expect_no_warning(
    withCallingHandlers(
      get_demand_param_trends(
        setup$fit, params = c("Q0", "alpha"), covariates = "dose_num", specs = ~drug
      ),
      beezdemand_nlme_convergence_warning = function(w) invokeRestart("muffleWarning")
    )
  )
})

# --- TICKET-064 F13 (recommended follow-up) ---------------------------------

test_that("get_demand_param_trends: all-bogus covariates still name every dropped combination (not just the generic message)", {
  skip_on_cran()
  setup <- make_nlme_fit()

  warns <- testthat::capture_warnings(
    result <- get_demand_param_trends(
      setup$fit, params = "Q0",
      covariates = c("not_a_real_covariate1", "not_a_real_covariate2"),
      specs = ~drug
    )
  )
  expect_true(any(grepl("not_a_real_covariate1", warns, fixed = TRUE)))
  expect_true(any(grepl("not_a_real_covariate2", warns, fixed = TRUE)))
  expect_equal(nrow(result), 0)
})

# --- F-BD6-1 (release-correctness audit 2026-09-06) --------------------------
# get_demand_param_trends() was the only NLME inference surface that did not
# call .nlme_warn_if_not_converged(), so trends from a fit whose apVar could
# not be inverted were returned with no condition at all. The gate keys on
# apVar + error_message (R/diagnostics.R .check_nlme_convergence), NOT on a
# $converged field, so the fixture below installs nlme's own failure sentinel.

test_that("get_demand_param_trends() gates on NLME convergence (F-BD6-1)", {
  skip_on_cran()
  setup <- make_nlme_fit()

  bad <- setup$fit
  # nlme sets apVar to a character message when the approximate
  # variance-covariance of the variance parameters could not be inverted.
  bad$model$apVar <- "Non-positive definite approximate variance-covariance"

  conds <- list()
  res <- withCallingHandlers(
    get_demand_param_trends(
      bad, params = "Q0", covariates = "dose_num", specs = ~drug
    ),
    warning = function(w) {
      conds[[length(conds) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  n_gate <- sum(vapply(conds, inherits, logical(1),
                       "beezdemand_nlme_convergence_warning"))
  expect_identical(n_gate, 1L)
  # The gate warns; it does not suppress the table.
  expect_s3_class(res, "tbl_df")
})

test_that("get_demand_param_trends() does not warn on a usable fit (F-BD6-1)", {
  skip_on_cran()
  setup <- make_nlme_fit()
  skip_if(!is.matrix(setup$fit$model$apVar),
          "fixture fit has a non-finite apVar in this environment")

  conds <- list()
  withCallingHandlers(
    get_demand_param_trends(
      setup$fit, params = "Q0", covariates = "dose_num", specs = ~drug
    ),
    warning = function(w) {
      conds[[length(conds) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  n_gate <- sum(vapply(conds, inherits, logical(1),
                       "beezdemand_nlme_convergence_warning"))
  expect_identical(n_gate, 0L)
})
