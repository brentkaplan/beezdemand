# =============================================================================
# Tests for predict.beezdemand_tmb newdata handling (codex review Bug 2)
#
# Bug: predict() ignored fixed-effect structure in newdata, silently returning
# biased values for models with factors or continuous covariates. For known
# subjects it reused training-time Q0/alpha; for unknown subjects it fell back
# to exp(beta_q0[1]) / exp(beta_alpha[1]) (reference intercepts). The fix
# rebuilds X_q0_new / X_alpha_new from the stored formula RHS.
# =============================================================================

# Small balanced subsample of apt_full with gender + age for fast tests.
.predict_tmb_subset <- function(n_per_gender = 25) {
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ids_g <- unique(d$id[d$gender == g])
    head(ids_g[order(ids_g)], n_per_gender)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  d
}

test_that("predict.beezdemand_tmb default reproduces training-time fitted", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  p1 <- predict(fit)
  p2 <- predict(fit, newdata = fit$data)
  expect_equal(p1$.fitted, p2$.fitted, tolerance = 1e-10)
})

test_that("predict.beezdemand_tmb respects newdata factor for known subjects", {
  skip_on_cran()
  d <- .predict_tmb_subset()
  fit <- fit_demand_tmb(d, equation = "exponential",
                        factors = "gender", verbose = 0)
  base <- d[d$gender == "Male", ][1:5, ]
  alt  <- base
  alt$gender <- factor("Female", levels = levels(d$gender))
  p_base <- predict(fit, newdata = base)$.fitted
  p_alt  <- predict(fit, newdata = alt)$.fitted
  expect_false(isTRUE(all.equal(p_base, p_alt)))
})

test_that("predict.beezdemand_tmb respects continuous covariate values in newdata", {
  skip_on_cran()
  d <- .predict_tmb_subset()
  fit <- fit_demand_tmb(d, equation = "exponential",
                        continuous_covariates = "age", verbose = 0)
  base <- d[1:5, ]
  hi   <- base; hi$age <- hi$age + 50
  expect_false(isTRUE(all.equal(predict(fit, newdata = base)$.fitted,
                                 predict(fit, newdata = hi)$.fitted)))
})

test_that("predict.beezdemand_tmb errors on factor levels not seen in training", {
  skip_on_cran()
  d <- .predict_tmb_subset()
  fit <- fit_demand_tmb(d, equation = "exponential",
                        factors = "gender", verbose = 0)
  bad <- d[1:5, ]
  bad$gender <- factor("ZZ_unseen",
                       levels = c(levels(d$gender), "ZZ_unseen"))
  expect_error(predict(fit, newdata = bad),
               "levels not seen in training")
})

test_that("predict.beezdemand_tmb warns and uses newdata FE for unknown subjects", {
  skip_on_cran()
  d <- .predict_tmb_subset()
  fit <- fit_demand_tmb(d, equation = "exponential",
                        factors = "gender", verbose = 0)
  newd <- d[1:3, ]
  newd$id <- factor("NEW_ID")
  expect_warning(p <- predict(fit, newdata = newd),
                 "unknown subject")
  expect_equal(nrow(p), 3)
  expect_true(all(is.finite(p$.fitted)))
})

test_that("predict.beezdemand_tmb errors on missing required columns", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  bad <- apt[1:5, c("id", "y")]  # drop x
  expect_error(predict(fit, newdata = bad),
               "missing required column")
})

# Codex review P2: predict(type='demand') builds the population curve
# from beta_q0[1] and beta_alpha[1] (the intercepts), which corresponds
# to all covariates = 0 rather than the training means. A warning already
# fires when factors are present; this extends the same guard to
# continuous_covariates so covariate-only fits do not silently render
# a curve at covariate = 0. Proper mean-centered population curves land
# in TICKET-011 Phase 5.
test_that("predict(type='demand') warns for covariate-only TMB fits", {
  skip_on_cran()
  d <- .predict_tmb_subset(n_per_gender = 15)
  fit <- fit_demand_tmb(
    d,
    equation = "exponential",
    continuous_covariates = "age",
    verbose = 0
  )
  expect_warning(
    predict(fit, type = "demand"),
    regexp = "covariate|reference"
  )
})

test_that("predict(type='demand') still warns for factor-only TMB fits", {
  skip_on_cran()
  d <- .predict_tmb_subset(n_per_gender = 15)
  fit <- fit_demand_tmb(
    d,
    equation = "exponential",
    factors = "gender",
    verbose = 0
  )
  expect_warning(
    predict(fit, type = "demand"),
    regexp = "reference level"
  )
})

test_that("predict(type='demand') does not warn for intercept-only TMB fits", {
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  expect_warning(predict(fit, type = "demand"), regexp = NA)
})
