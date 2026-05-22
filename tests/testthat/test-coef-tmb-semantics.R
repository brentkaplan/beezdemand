# TICKET-019: coef.beezdemand_tmb(type=) dispatch.
# Default stays "internal" (no flip). "subject"/"combined" alias ->
# get_subject_pars(expanded = NULL) tibble. "fixed" -> 1-row tibble of the
# fixed-effect block only (no log_k / logsigma* / rho*). report_space via
# ... is rejected (no scale conversion in this ticket).

# Within-subject factor fit (genuine expansion). apt_full has no
# within-subject factor (memory: project_apt_full_between_subjects), so the
# expansion case must be simulated.
.coef_fit_within <- function(seed = 7, n_subjects = 12) {
  sim <- .simulate_within_subject_demand(
    n_subjects = n_subjects, n_conditions = 3,
    prices = c(0.25, 0.5, 1, 2, 4, 8, 16, 32),
    log_q0_pop = log(15), log_alpha_pop = log(0.0015),
    delta_q0 = c(0, -0.4, -0.9), delta_alpha = c(0, 0.2, 0.5),
    sigma_b = 0.4, sigma_d = 0.4, seed = seed
  )
  sim$id <- factor(sim$id)
  sim$condition <- factor(sim$condition)
  suppressWarnings(fit_demand_tmb(
    sim, equation = "simplified",
    random_effects = nlme::pdBlocked(list(
      nlme::pdSymm(Q0 + alpha ~ 1),
      nlme::pdDiag(Q0 + alpha ~ condition - 1)
    )),
    multi_start = FALSE, verbose = 0
  ))
}

test_that("coef() default equals type='internal' and the raw optimizer vector (no flip)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  expect_identical(coef(fit), coef(fit, type = "internal"))
  expect_identical(coef(fit, type = "internal"), fit$model$coefficients)
  expect_type(coef(fit), "double")
})

test_that("coef(type='subject') on intercept-only fit is the get_subject_pars tibble", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  cs <- coef(fit, type = "subject")
  expect_s3_class(cs, "tbl_df")
  expect_true("id" %in% names(cs))
  expect_equal(nrow(cs), length(unique(apt$id)))
  expect_true(all(is.finite(cs$Q0)))
  expect_true(all(is.finite(cs$alpha)))
  expect_equal(cs, tibble::as_tibble(get_subject_pars(fit, expanded = NULL)))
})

test_that("coef(type='combined') aliases type='subject'", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  expect_equal(coef(fit, type = "combined"), coef(fit, type = "subject"))
})

test_that("coef(type='subject') on within-subject factor fit matches get_subject_pars(expanded=NULL)", {
  skip_on_cran()
  fit <- .coef_fit_within()

  cs <- coef(fit, type = "subject")
  sp <- tibble::as_tibble(get_subject_pars(fit, expanded = NULL))
  # Auto-detect expands across the within-id factor: more than one row per subject.
  expect_gt(nrow(cs), length(unique(fit$subject_pars$id)))
  expect_equal(cs, sp)
})

test_that("coef(type='fixed') returns exactly the fixed-effect columns, k estimated", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  tn <- beezdemand:::.tmb_build_term_names(fit)
  expected_cols <- tn$term[c(tn$q0_idx, tn$alpha_idx)]

  cf <- coef(fit, type = "fixed")
  expect_s3_class(cf, "tbl_df")
  expect_equal(nrow(cf), 1L)
  expect_false("id" %in% names(cf))
  expect_equal(names(cf), expected_cols)
  # Never leak optimizer hyperparameters into the fixed-effect table.
  expect_false(any(grepl("^log_k$|^logsigma|^rho", names(cf))))
  # k is estimated here, so log_k IS in the raw vector but excluded from "fixed".
  expect_true("log_k" %in% names(fit$model$coefficients))
  # Values are the beta block of the raw coefficients.
  co <- fit$model$coefficients
  expect_equal(unname(unlist(cf)), unname(co[c(tn$q0_idx, tn$alpha_idx)]))
})

test_that("coef(type='fixed') excludes hyperparameters with k fixed", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential",
                        estimate_k = FALSE, verbose = 0)

  tn <- beezdemand:::.tmb_build_term_names(fit)
  expected_cols <- tn$term[c(tn$q0_idx, tn$alpha_idx)]

  cf <- coef(fit, type = "fixed")
  expect_s3_class(cf, "tbl_df")
  expect_equal(nrow(cf), 1L)
  expect_equal(names(cf), expected_cols)
  expect_false(any(grepl("^log_k$|^logsigma|^rho", names(cf))))
})

test_that("coef() rejects report_space supplied through ...", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  expect_error(coef(fit, type = "fixed", report_space = "natural"),
               "report_space")
  expect_error(coef(fit, type = "subject", report_space = "natural"),
               "report_space")
})

test_that("coef(type='bogus') errors via match.arg", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  expect_error(coef(fit, type = "bogus"), "should be one of")
})

test_that("coef(type='subject') on a between-subjects factor fit is one row per subject", {
  skip_on_cran()
  data(apt_full, package = "beezdemand")
  # gender is between-subjects (constant within id) -> no within-id expansion.
  fit <- suppressWarnings(fit_demand_tmb(
    apt_full, equation = "exponential", factors = "gender", verbose = 0
  ))

  cs <- coef(fit, type = "subject")
  expect_s3_class(cs, "tbl_df")
  expect_equal(nrow(cs), length(unique(fit$subject_pars$id)))
  expect_equal(cs, tibble::as_tibble(get_subject_pars(fit, expanded = NULL)))
})

test_that("fixef() returns the internal coefficient vector (invariant vs a future coef() default)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  expect_identical(nlme::fixef(fit), coef(fit, type = "internal"))
})
