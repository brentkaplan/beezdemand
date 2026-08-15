# TICKET-042: predict.beezdemand_hurdle() defaults to the marginal
# expectation (type = "demand") as of 0.3.0 -------------------------------------
#
# The 0.2.0 default ("response", the conditional positive mean E[Y | Y > 0])
# is a documented CV-scoring footgun: scoring zero-inclusive consumption with
# it systematically overstates hurdle error where P(zero) is large. The flip
# is a deliberate, NEWS-documented breaking change with a one-time-per-session
# transition message when `type` is omitted. STRICT MODE: no type branch's
# math changes -- these tests pin both the new default and the old semantics.

test_that("predict default is type = 'demand' with a one-time transition message", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  # force frequency-managed messages to always emit so once-per-session
  # cannot hide the transition message from this test
  withr::local_options(rlib_message_verbosity = "verbose")

  expect_message(p_default <- predict(fit), "demand")

  # the default IS the marginal expectation
  p_demand <- predict(fit, type = "demand")
  expect_identical(p_default$.fitted, p_demand$.fitted)
  expect_identical(p_demand$.fitted, p_demand$expected_consumption)

  # the price column travels with the predictions -- the frame is unusable for
  # plotting or scoring without it (harvested from arg-matrix, TICKET-071)
  expect_true(all(
    c("id", "x", ".fitted", "prob_zero", "expected_consumption") %in%
      names(p_demand)
  ))
  expect_true(all(is.finite(p_demand$x)))

  # "response" keeps the conditional positive mean (math unchanged), and the
  # within-call marginal identity holds: demand = (1 - p0) * response
  p_resp <- predict(fit, type = "response")
  expect_identical(p_resp$.fitted, p_resp$predicted_consumption)
  expect_equal(
    p_demand$expected_consumption,
    (1 - p_demand$prob_zero) * p_demand$predicted_consumption
  )

  # the default differs from the old one where zeros carry mass -- the
  # footgun this ticket kills
  expect_false(isTRUE(all.equal(p_default$.fitted, p_resp$.fitted)))

  # explicit type never triggers the transition message
  expect_no_message(predict(fit, type = "demand"))
  expect_no_message(predict(fit, type = "response"))

  # POSITIONAL type is not "missing": no message, old semantics intact
  # (Codex 042-diff R1)
  expect_no_message(p_pos <- predict(fit, NULL, "response"))
  expect_identical(p_pos$.fitted, p_pos$predicted_consumption)

  # the prices path follows the new default too: omitted type == explicit
  # "demand" at the same prices
  pr <- c(0.5, 1, 5)
  p_prices_default <- predict(fit, prices = pr)
  p_prices_demand <- predict(fit, type = "demand", prices = pr)
  expect_identical(p_prices_default$.fitted, p_prices_demand$.fitted)
})

test_that("downstream methods pass type explicitly and are unaffected by the flip", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  sim_data <- simulate_hurdle_data(n_subjects = 30, seed = 123)
  fit <- fit_demand_hurdle(
    sim_data,
    y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0"),
    verbose = 0
  )

  withr::local_options(rlib_message_verbosity = "verbose")
  expect_no_message(f_marg <- fitted(fit))
  expect_no_message(residuals(fit))
  expect_no_message(a <- augment(fit))

  # fitted(marginal = TRUE) and augment both report the marginal expectation
  expect_equal(unname(f_marg), unname(a$.fitted))
})
