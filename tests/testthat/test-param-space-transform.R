# Independent verification of the delta-method back-transform in
# beezdemand_transform_est_se() (R/param-space.R). The standard error on the
# target scale must equal |d(target)/d(source)| * SE_source. Each case is
# checked against a finite difference of the back-transform function, never
# against the code's own SE formula (non-circular).

test_that("log10 -> natural SE matches the delta method ln10*10^est*se", {
  est <- 0.7
  se <- 0.05
  res <- beezdemand:::beezdemand_transform_est_se(est, se, "log10", "natural")
  g <- function(z) 10^z
  fd <- (g(est + 1e-6) - g(est - 1e-6)) / (2e-6)
  expect_equal(res$estimate, 10^est, tolerance = 1e-12)
  expect_equal(res$se, abs(fd) * se, tolerance = 1e-6)
  expect_equal(res$se, log(10) * 10^est * se, tolerance = 1e-10)
})

test_that("log -> natural SE matches the delta method exp(est)*se", {
  est <- -1.2
  se <- 0.1
  res <- beezdemand:::beezdemand_transform_est_se(est, se, "log", "natural")
  g <- function(z) exp(z)
  fd <- (g(est + 1e-6) - g(est - 1e-6)) / (2e-6)
  expect_equal(res$estimate, exp(est), tolerance = 1e-12)
  expect_equal(res$se, abs(fd) * se, tolerance = 1e-6)
})

test_that("natural -> log10 SE matches the delta method se/(est*ln10)", {
  est <- 5
  se <- 0.4
  res <- beezdemand:::beezdemand_transform_est_se(est, se, "natural", "log10")
  g <- function(z) log10(z)
  fd <- (g(est + 1e-6) - g(est - 1e-6)) / (2e-6)
  expect_equal(res$estimate, log10(est), tolerance = 1e-12)
  expect_equal(res$se, abs(fd) * se, tolerance = 1e-6)
})

test_that("round-trip natural -> log10 -> natural preserves estimate and se", {
  est <- 12
  se <- 0.9
  to_log <- beezdemand:::beezdemand_transform_est_se(est, se, "natural", "log10")
  back <- beezdemand:::beezdemand_transform_est_se(
    to_log$estimate, to_log$se, "log10", "natural"
  )
  expect_equal(back$estimate, est, tolerance = 1e-9)
  expect_equal(back$se, se, tolerance = 1e-9)
})
