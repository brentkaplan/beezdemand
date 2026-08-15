# Two-factor NLME fits with factor_interaction = TRUE, and factor-aware
# predict() newdata.
#
# Before TICKET-071 the only place factor_interaction = TRUE met the NLME
# backend was the gitignored arg-matrix harness -- and it proved nothing:
# every one of its five fit_demand_mixed cases failed with "Singularity in
# backsolve at level 0, block 1", returned a beezdemand_nlme wrapper with
# $model = NULL, and still reported ok = TRUE because the case check was
# inherits(value, "beezdemand_nlme"). Tracked coverage of factor_interaction
# is formula-builder only (test_fit_demand_mixed.R) or TMB-backend
# (test-tmb-contrast-by.R). Tracked NLME predict tests are intercept-only.
#
# The fixture below uses real apt_full data and converges in well under a
# second, so every assertion here is made against a fit that actually happened.
#
# No skip_on_cran(): these fits are fast enough to run everywhere, CRAN
# included (CI sets NOT_CRAN=true since TICKET-070; cran-everything does not).

nlme_factor_data <- function(n_ids = 60, seed = 1) {
  data(apt_full, package = "beezdemand", envir = environment())
  apt_full <- get("apt_full", envir = environment())

  # sort() before sampling so the fixture depends on the id VALUES, not on
  # apt_full's row order; local_seed() so building the fixture cannot perturb
  # the RNG stream of whatever test runs next in the same process.
  withr::local_seed(seed)
  keep <- sample(sort(unique(apt_full$id)), n_ids)
  dat <- apt_full[apt_full$id %in% keep, ]

  # Drop the sparse third gender level so both factors are balanced 2-level
  # between-subject factors (apt_full has no within-subject factor).
  dat <- dat[dat$gender %in% c("Female", "Male"), ]
  dat$gender <- droplevels(factor(dat$gender))
  dat$age_cut <- factor(
    ifelse(dat$age >= stats::median(dat$age, na.rm = TRUE), "older", "younger")
  )
  dat$y_ll4 <- ll4(dat$y)
  dat
}

test_that("fit_demand_mixed converges with two factors and an interaction", {
  skip_if_not_installed("nlme")

  dat <- nlme_factor_data()
  fit <- suppressMessages(fit_demand_mixed(
    dat,
    x_var = "x",
    y_var = "y_ll4",
    id_var = "id",
    factors = c("gender", "age_cut"),
    factor_interaction = TRUE
  ))

  expect_s3_class(fit, "beezdemand_nlme")

  # The wrapper is returned with $model = NULL on failure, so a class check
  # alone cannot distinguish a fit from a crash. Assert the fit happened.
  expect_false(is.null(fit$model))
  expect_true(is.finite(as.numeric(stats::logLik(fit$model))))

  # The interaction must reach the fixed effects for both demand parameters.
  fe <- names(nlme::fixef(fit$model))
  expect_true("Q0.genderMale:age_cutyounger" %in% fe)
  expect_true("alpha.genderMale:age_cutyounger" %in% fe)
  expect_true(all(is.finite(nlme::fixef(fit$model))))
})

test_that("factor_interaction = TRUE adds terms over the additive NLME fit", {
  skip_if_not_installed("nlme")

  dat <- nlme_factor_data()
  common <- list(
    data = dat, x_var = "x", y_var = "y_ll4", id_var = "id",
    factors = c("gender", "age_cut")
  )

  additive <- suppressMessages(do.call(
    fit_demand_mixed, c(common, list(factor_interaction = FALSE))
  ))
  inter <- suppressMessages(do.call(
    fit_demand_mixed, c(common, list(factor_interaction = TRUE))
  ))

  expect_false(is.null(additive$model))
  expect_false(is.null(inter$model))

  # 2 params x (intercept + 2 main effects) = 6 additive;
  # + one interaction per param = 8.
  expect_equal(length(nlme::fixef(additive$model)), 6)
  expect_equal(length(nlme::fixef(inter$model)), 8)

  add_fe <- names(nlme::fixef(additive$model))
  expect_false(any(grepl(":", add_fe, fixed = TRUE)))
})

test_that("predict.beezdemand_nlme accepts newdata carrying the factors", {
  skip_if_not_installed("nlme")

  dat <- nlme_factor_data()
  fit <- suppressMessages(fit_demand_mixed(
    dat,
    x_var = "x",
    y_var = "y_ll4",
    id_var = "id",
    factors = c("gender", "age_cut"),
    factor_interaction = TRUE
  ))

  newdata <- expand.grid(
    x = c(0, 1, 5),
    gender = factor(c("Female", "Male"), levels = levels(dat$gender)),
    age_cut = factor(c("older", "younger"), levels = levels(dat$age_cut))
  )
  preds <- predict(fit, newdata = newdata, level = 0)

  expect_true(is.data.frame(preds))
  expect_true(".fitted" %in% names(preds))
  expect_equal(nrow(preds), nrow(newdata))
  expect_true(all(is.finite(preds$.fitted)))

  # The factor columns must survive into the output, and the predictions must
  # actually vary by factor level -- otherwise the factors are being ignored.
  expect_true(all(c("gender", "age_cut") %in% names(preds)))
  at_x0 <- preds[preds$x == 0, ]
  expect_gt(length(unique(round(at_x0$.fitted, 8))), 1)
})

test_that("predict.beezdemand_nlme without newdata returns one row per observation", {
  skip_if_not_installed("nlme")

  dat <- nlme_factor_data()
  fit <- suppressMessages(fit_demand_mixed(
    dat,
    x_var = "x",
    y_var = "y_ll4",
    id_var = "id",
    factors = c("gender", "age_cut"),
    factor_interaction = TRUE
  ))

  preds <- predict(fit)

  expect_true(".fitted" %in% names(preds))
  expect_equal(nrow(preds), nrow(stats::na.omit(dat[, c("x", "y_ll4", "id")])))
  expect_true(all(is.finite(preds$.fitted)))
})
