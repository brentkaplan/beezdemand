# =============================================================================
# Tests for predict.beezdemand_tmb(level=) -- TICKET-014
#
# `level = "subject"` (default) conditions on each subject's random effects and
# requires an `id` column in `newdata` (the pre-ticket behavior). `level =
# "population"` evaluates at the fixed-effect coefficients with random effects
# set to zero and does NOT require `id`. `level = c("population", "subject")`
# returns both predictions in one call (`predict.fixed` + `predict.id`).
#
# Adaptation note: the ticket drafted these tests against an `id_group` factor
# on `apt_full`; `apt_full` has no such column (its factors are gender, age,
# binges, ...), so the tests below use `gender` with population level "Male".
# =============================================================================

# Balanced Male/Female subsample of apt_full -- kept small so the TMB fits
# below stay fast.
.ptl_subset <- function(n_per_gender = 20) {
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ig <- unique(d$id[d$gender == g])
    head(ig[order(ig)], n_per_gender)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  d
}

# Memoized fits: each runs at most once, and only when first reached past a
# skip_on_cran() guard inside a test (so CRAN never pays the fitting cost).
.ptl_fit_exp <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- fit_demand_tmb(.ptl_subset(), equation = "exponential",
                               factors = "gender", verbose = 0)
    }
    cache
  }
})

.ptl_fit_simplified <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- fit_demand_tmb(.ptl_subset(), equation = "simplified",
                               y_var = "y", factors = "gender", verbose = 0)
    }
    cache
  }
})

test_that("predict.beezdemand_tmb level='subject' default preserves current behavior", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  id1 <- as.character(unique(fit$data$id))[1]
  nd <- data.frame(gender = "Male", x = c(0.01, 1, 10), id = id1)
  pred_default <- predict(fit, newdata = nd)
  pred_subject <- predict(fit, newdata = nd, level = "subject")
  expect_identical(pred_default, pred_subject)
  # Backward-compatible column name retained for the single subject path.
  expect_true(".fitted" %in% names(pred_subject))
})

test_that("predict.beezdemand_tmb level='population' does not require id column", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  nd_no_id <- data.frame(gender = "Male", x = c(0.01, 1, 10))
  pred <- predict(fit, newdata = nd_no_id, level = "population")
  expect_equal(nrow(pred), 3)
  expect_true("predict.fixed" %in% names(pred))
  expect_false("id" %in% names(pred))
})

test_that("predict.beezdemand_tmb level=c('population','subject') returns both columns", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  id1 <- as.character(unique(fit$data$id))[1]
  nd <- data.frame(gender = "Male", x = c(0.01, 1, 10), id = id1)
  pred <- predict(fit, newdata = nd, level = c("population", "subject"))
  expect_true(all(c("predict.fixed", "predict.id") %in% names(pred)))
  # predict.fixed precedes predict.id regardless of the requested order.
  expect_lt(match("predict.fixed", names(pred)),
            match("predict.id", names(pred)))
  pred_rev <- predict(fit, newdata = nd, level = c("subject", "population"))
  expect_identical(names(pred), names(pred_rev))
})

test_that("predict.beezdemand_tmb level='population' equals manual emmeans evaluation", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  nd <- data.frame(gender = "Male", x = 0.01)
  # Population mean on the natural scale, no lognormal bias correction:
  # exp(model-scale prediction) == Q0 * 10^(k*(exp(-alpha*Q0*x) - 1)).
  pred <- predict(fit, newdata = nd, level = "population",
                  scale = "natural", correction = FALSE)
  emms_q <- get_demand_param_emms(fit, param = "Q0")
  emms_a <- get_demand_param_emms(fit, param = "alpha")
  q0_m <- emms_q$estimate[emms_q$level == "gender=Male"]
  a_m  <- emms_a$estimate[emms_a$level == "gender=Male"]
  k    <- beezdemand:::.tmb_get_k(fit)
  expected <- q0_m * 10^(k * (exp(-a_m * q0_m * 0.01) - 1))
  expect_equal(unname(pred$predict.fixed[1]), expected, tolerance = 1e-6)
})

test_that("predict.beezdemand_tmb errors helpfully on missing id (subject)", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  nd <- data.frame(gender = "Male", x = 0.01)
  expect_error(
    predict(fit, newdata = nd, level = "subject"),
    "id"
  )
})

test_that("predict.beezdemand_tmb level='subject' errors on an unknown id value", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  # `id` column present but the value is not a subject in the fit. TICKET-014
  # specifies an error here, not a silent population-mean (RE = 0) fallback.
  nd <- data.frame(gender = "Male", x = c(0.01, 1), id = "not_a_real_subject")
  expect_error(
    predict(fit, newdata = nd, level = "subject"),
    "not found in the fitted model"
  )
})

test_that("predict.beezdemand_tmb errors helpfully on numeric level", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  nd <- data.frame(gender = "Male", x = 0.01)
  expect_error(
    predict(fit, newdata = nd, level = 0),
    "should be one of|character"
  )
})

test_that("predict.beezdemand_tmb level='population' dispatches the simplified equation", {
  skip_on_cran()
  fit <- .ptl_fit_simplified()
  nd <- data.frame(gender = "Male", x = 0.01)
  pred <- predict(fit, newdata = nd, level = "population")
  emms_q <- get_demand_param_emms(fit, param = "Q0")
  emms_a <- get_demand_param_emms(fit, param = "alpha")
  q0_m <- emms_q$estimate[emms_q$level == "gender=Male"]
  a_m  <- emms_a$estimate[emms_a$level == "gender=Male"]
  # SND / simplified: Q = Q0 * exp(-alpha * Q0 * x), already on natural scale.
  expected <- q0_m * exp(-a_m * q0_m * 0.01)
  expect_equal(unname(pred$predict.fixed[1]), expected, tolerance = 1e-6)
})

test_that("predict.beezdemand_tmb level='population' errors on missing factor column", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  nd <- data.frame(x = c(0.01, 1))   # `gender` absent
  expect_error(
    predict(fit, newdata = nd, level = "population"),
    "gender|required column"
  )
})

test_that("predict.beezdemand_tmb level='population' returns a 0-row tibble for empty newdata", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  nd <- data.frame(gender = character(0), x = numeric(0))
  pred <- predict(fit, newdata = nd, level = "population")
  expect_s3_class(pred, "tbl_df")
  expect_equal(nrow(pred), 0)
  expect_true("predict.fixed" %in% names(pred))
})

# `fitted()` / `residuals()` already expose `level = c("subject",
# "population")`; verify they honor "population" rather than silently
# returning subject-level values.

test_that("fitted.beezdemand_tmb level='population' returns population-level values", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  pred_pop <- predict(fit, newdata = fit$data, type = "response",
                      level = "population")
  f_pop  <- fitted(fit, level = "population")
  f_subj <- fitted(fit, level = "subject")
  expect_equal(unname(f_pop), unname(pred_pop$predict.fixed))
  expect_false(isTRUE(all.equal(f_subj, f_pop)))
})

test_that("residuals.beezdemand_tmb level='population' uses population fitted", {
  skip_on_cran()
  fit <- .ptl_fit_exp()
  r_pop  <- residuals(fit, level = "population")
  r_subj <- residuals(fit, level = "subject")
  f_pop  <- fitted(fit, level = "population")
  f_subj <- fitted(fit, level = "subject")
  expect_false(isTRUE(all.equal(r_subj, r_pop)))
  # A response residual plus its fitted value reconstructs the on-scale
  # observation, so the subject and population decompositions sum alike.
  expect_equal(r_pop + f_pop, r_subj + f_subj)
})
