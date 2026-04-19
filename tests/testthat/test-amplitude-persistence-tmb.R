# Regression tests for TICKET-004: calculate_amplitude_persistence method on
# beezdemand_tmb objects. Mirrors the hurdle method's contract: extract
# fit$subject_pars and delegate to calculate_amplitude_persistence.default.

test_that("calculate_amplitude_persistence works for beezdemand_tmb (2-RE)", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, equation = "exponential",
    random_effects = c("q0", "alpha"),
    multi_start = FALSE, verbose = 0
  )

  result <- calculate_amplitude_persistence(fit)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("id", "Amplitude", "Persistence") %in% names(result)))
})

test_that("calculate_amplitude_persistence.beezdemand_tmb output matches manual delegation", {
  # The TMB method must be a thin wrapper around the .default method on
  # fit$subject_pars; otherwise the new method has subtly different behavior
  # from a manual workaround. This pins them as identical.
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, equation = "exponential",
    random_effects = c("q0", "alpha"),
    multi_start = FALSE, verbose = 0
  )

  via_method <- calculate_amplitude_persistence(fit)
  via_manual <- calculate_amplitude_persistence(
    fit$subject_pars,
    amplitude = c("Q0"),
    persistence = c("Pmax", "Omax", "alpha")
  )

  expect_equal(via_method$Amplitude, via_manual$Amplitude)
  expect_equal(via_method$Persistence, via_manual$Persistence)
  expect_equal(via_method$id, via_manual$id)
})

test_that("calculate_amplitude_persistence works for beezdemand_tmb (1-RE)", {
  # 1-RE models still produce subject_pars rows for all subjects (alpha is a
  # fixed effect applied to each). The factor decomposition should compute
  # without error; persistence components include Pmax, Omax, alpha (all
  # available even in 1-RE).
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, equation = "exponential",
    random_effects = "q0",
    multi_start = FALSE, verbose = 0
  )

  result <- calculate_amplitude_persistence(fit)

  expect_s3_class(result, "data.frame")
  expect_true("Amplitude" %in% names(result))
  expect_true("Persistence" %in% names(result))
})

test_that("custom amplitude/persistence vars work for beezdemand_tmb", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, equation = "exponential",
    random_effects = c("q0", "alpha"),
    multi_start = FALSE, verbose = 0
  )

  result <- calculate_amplitude_persistence(
    fit,
    amplitude = c("Q0"),
    persistence = c("Pmax", "alpha"),
    min_persistence_components = 1L
  )

  expect_s3_class(result, "data.frame")
  expect_true("Amplitude" %in% names(result))
  expect_true("Persistence" %in% names(result))
})

test_that("basis_means / basis_sds pass-through works for beezdemand_tmb", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, equation = "exponential",
    random_effects = c("q0", "alpha"),
    multi_start = FALSE, verbose = 0
  )

  pars <- fit$subject_pars
  custom_means <- c(Q0 = mean(pars$Q0, na.rm = TRUE),
                    alpha = mean(pars$alpha, na.rm = TRUE))
  custom_sds <- c(Q0 = sd(pars$Q0, na.rm = TRUE),
                  alpha = sd(pars$alpha, na.rm = TRUE))

  result <- calculate_amplitude_persistence(
    fit,
    amplitude = "Q0",
    persistence = "alpha",
    min_persistence_components = 1L,
    basis_means = custom_means,
    basis_sds = custom_sds
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("calculate_amplitude_persistence errors when subject_pars is NULL or empty", {
  fake_null <- structure(
    list(subject_pars = NULL, converged = FALSE),
    class = "beezdemand_tmb"
  )
  expect_error(
    calculate_amplitude_persistence(fake_null),
    "subject-level parameters"
  )

  fake_empty <- structure(
    list(subject_pars = data.frame(), converged = TRUE),
    class = "beezdemand_tmb"
  )
  expect_error(
    calculate_amplitude_persistence(fake_empty),
    "subject-level parameters"
  )
})

test_that("calculate_amplitude_persistence dispatches to TMB method (not .default)", {
  # Sanity check that S3 dispatch routes a beezdemand_tmb object to the new
  # method rather than falling through to .default (which would error because
  # the fit object isn't a data.frame).
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt, equation = "exponential",
    random_effects = c("q0", "alpha"),
    multi_start = FALSE, verbose = 0
  )

  expect_s3_class(fit, "beezdemand_tmb")
  expect_no_error(calculate_amplitude_persistence(fit))
})
