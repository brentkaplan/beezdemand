# Fast tests split out of the BEEZ_FULL_TESTS-gated test-boot-demand.R so they
# still run in R CMD check / CI / CRAN.

test_that(".boot_demand_ci excludes non-finite draws, counts them, aborts if all fail", {
  probs <- c(0.025, 0.975)

  ci <- beezdemand:::.boot_demand_ci(as.numeric(1:20), probs, "Pmax", NA_character_)
  expect_identical(ci$n_failed, 0L)
  expect_true(is.finite(ci$conf.low) && is.finite(ci$conf.high))

  # Partial non-finite draws are excluded and counted; CI from finite draws only.
  ci2 <- beezdemand:::.boot_demand_ci(
    c(1, 2, NA, Inf, 3, 4, NaN, 5), probs, "Omax", "gender=Male"
  )
  expect_identical(ci2$n_failed, 3L)
  expect_true(is.finite(ci2$conf.low) && is.finite(ci2$conf.high))

  # All non-finite -> abort (a CI cannot be formed).
  expect_error(
    beezdemand:::.boot_demand_ci(c(NA, NaN, Inf, -Inf), probs, "Pmax", NA_character_),
    "non-finite"
  )
})

