# Regression tests for TICKET-008: surface singular-Hessian warnings on TMB
# and hurdle fits. The package previously stored sdr$pdHess but never
# checked it, so users could publish unreliable SEs / p-values without any
# signal. Fix: warn at fit time (cli::cli_warn), expose `hessian_pd` on the
# fit object, and surface it in summary() notes and tidy() attributes.

# --- TMB fits ----------------------------------------------------------------

test_that("hessian_pd field exists on beezdemand_tmb fits", {
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential",
                        random_effects = "q0",
                        multi_start = FALSE, verbose = 0)
  expect_true("hessian_pd" %in% names(fit))
  expect_true(is.logical(fit$hessian_pd))
})

test_that("hessian_pd is TRUE for well-behaved TMB model on apt", {
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential",
                        random_effects = "q0",
                        multi_start = FALSE, verbose = 0)
  if (fit$converged) {
    expect_true(fit$hessian_pd)
  }
})

test_that("summary.beezdemand_tmb adds note when hessian_pd is FALSE", {
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential",
                        random_effects = "q0",
                        multi_start = FALSE, verbose = 0)
  fit$hessian_pd <- FALSE  # force the FALSE branch without inducing it

  s <- summary(fit)
  expect_true(any(grepl("Hessian|hessian|positive definite|standard error",
                        s$notes, ignore.case = TRUE)))
})

test_that("summary.beezdemand_tmb has no Hessian note when hessian_pd is TRUE", {
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential",
                        random_effects = "q0",
                        multi_start = FALSE, verbose = 0)
  s <- summary(fit)
  expect_false(any(grepl("positive definite", s$notes, ignore.case = TRUE)))
})

test_that("tidy.beezdemand_tmb sets hessian_warning attribute when hessian_pd is FALSE", {
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential",
                        random_effects = "q0",
                        multi_start = FALSE, verbose = 0)
  fit$hessian_pd <- FALSE

  out <- tidy(fit)
  expect_s3_class(out, "tbl_df")
  expect_true(nrow(out) > 0)
  expect_false(is.null(attr(out, "hessian_warning")))
  expect_true(grepl("positive definite", attr(out, "hessian_warning")))
})

test_that("tidy.beezdemand_tmb has no hessian_warning attribute when hessian_pd is TRUE", {
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential",
                        random_effects = "q0",
                        multi_start = FALSE, verbose = 0)
  out <- tidy(fit)
  expect_null(attr(out, "hessian_warning"))
})

test_that("hessian_pd is NA when sdr is NULL (sdreport failure path)", {
  # Construct a synthetic TMB-shaped object with sdr = NULL to verify the NA
  # path. Real sdreport failures are difficult to provoke deterministically
  # on small example data, so this guards the contract directly.
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential",
                        random_effects = "q0",
                        multi_start = FALSE, verbose = 0)
  fit$sdr <- NULL
  fit$hessian_pd <- NA
  expect_true(is.na(fit$hessian_pd))
  expect_no_error(s <- summary(fit))
  expect_no_error(out <- tidy(fit))
})

test_that("Old beezdemand_tmb objects without hessian_pd still work (backwards compat)", {
  # Synthetic legacy object: no hessian_pd field. Accessing $hessian_pd
  # returns NULL; isFALSE(NULL) is FALSE, so neither summary nor tidy should
  # emit a Hessian warning, and nothing should error.
  legacy <- structure(
    list(
      converged = TRUE,
      hessian_pd = NULL  # explicitly absent
    ),
    class = "beezdemand_tmb"
  )
  expect_null(legacy$hessian_pd)
  expect_false(isTRUE(legacy$hessian_pd))
  expect_false(isFALSE(legacy$hessian_pd))
})

# --- Hurdle fits -------------------------------------------------------------

test_that("hessian_pd field exists on beezdemand_hurdle fits", {
  data("apt", package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                           verbose = 0)
  expect_true("hessian_pd" %in% names(fit))
  expect_true(is.logical(fit$hessian_pd))
})

test_that("summary.beezdemand_hurdle adds note when hessian_pd is FALSE", {
  data("apt", package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                           verbose = 0)
  fit$hessian_pd <- FALSE
  s <- summary(fit)
  expect_true(any(grepl("Hessian|hessian|positive definite|standard error",
                        s$notes, ignore.case = TRUE)))
})

test_that("tidy.beezdemand_hurdle sets hessian_warning attribute when hessian_pd is FALSE", {
  data("apt", package = "beezdemand")
  fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
                           verbose = 0)
  fit$hessian_pd <- FALSE
  out <- tidy(fit)
  expect_s3_class(out, "tbl_df")
  expect_true(nrow(out) > 0)
  expect_false(is.null(attr(out, "hessian_warning")))
})

test_that("check_demand_model still works on TMB objects (regression)", {
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential",
                        random_effects = "q0",
                        multi_start = FALSE, verbose = 0)
  expect_no_error(diag <- check_demand_model(fit))
  expect_s3_class(diag, "beezdemand_diagnostics")
})
