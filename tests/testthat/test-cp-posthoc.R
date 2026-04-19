# TICKET-007: coverage for cp_posthoc_slopes() and cp_posthoc_intercepts().
# Both functions wrap emmeans on cp_model_lmer objects, return a `cp_posthoc`
# class object with $emmeans and $significant_interaction, and conditionally
# include $contrasts when an interaction is detected at level alpha.

skip_if_not_installed("emmeans")
skip_if_not_installed("lme4")

make_lmer_fit <- function(log10x = FALSE) {
  data(etm, package = "beezdemand")
  fit_cp_linear(etm, type = "mixed", group_effects = TRUE, log10x = log10x,
                return_all = TRUE)
}

# --- cp_posthoc_slopes -----------------------------------------------------

test_that("cp_posthoc_slopes returns a cp_posthoc object tagged 'slopes'", {
  skip_on_cran()
  result <- cp_posthoc_slopes(make_lmer_fit())
  expect_s3_class(result, "cp_posthoc")
  expect_equal(attr(result, "type"), "slopes")
})

test_that("cp_posthoc_slopes always includes a non-empty emmeans data frame", {
  skip_on_cran()
  result <- cp_posthoc_slopes(make_lmer_fit())
  expect_true("emmeans" %in% names(result))
  expect_s3_class(result$emmeans, "data.frame")
  expect_true(nrow(result$emmeans) > 0)
})

test_that("cp_posthoc_slopes records significant_interaction as a single logical", {
  skip_on_cran()
  result <- cp_posthoc_slopes(make_lmer_fit())
  expect_true("significant_interaction" %in% names(result))
  expect_type(result$significant_interaction, "logical")
  expect_length(result$significant_interaction, 1L)
})

test_that("cp_posthoc_slopes returns $contrasts when interaction is significant", {
  skip_on_cran()
  # alpha = 1 forces "everything significant" so the contrasts branch runs
  result <- cp_posthoc_slopes(make_lmer_fit(), alpha = 1)
  expect_true(result$significant_interaction)
  expect_true("contrasts" %in% names(result))
  expect_s3_class(result$contrasts, "data.frame")
  expect_true("p.value" %in% names(result$contrasts))
  expect_true("significance" %in% names(result$contrasts))
})

test_that("cp_posthoc_slopes returns $message when no significant interaction", {
  skip_on_cran()
  # alpha = 1e-30 forces "nothing significant" so the message branch runs
  result <- cp_posthoc_slopes(make_lmer_fit(), alpha = 1e-30)
  expect_false(result$significant_interaction)
  expect_true("message" %in% names(result))
  expect_type(result$message, "character")
  expect_false("contrasts" %in% names(result))
})

test_that("cp_posthoc_slopes errors on non-cp_model_lmer input", {
  expect_error(cp_posthoc_slopes("not a model"), "cp_model_lmer")
  expect_error(cp_posthoc_slopes(list()), "cp_model_lmer")
})

test_that("cp_posthoc_slopes alpha threshold affects detection direction", {
  skip_on_cran()
  fit <- make_lmer_fit()
  lenient <- cp_posthoc_slopes(fit, alpha = 1)
  strict  <- cp_posthoc_slopes(fit, alpha = 1e-30)
  # Lenient must be at least as permissive: if strict says significant, lenient
  # must also; if lenient says not, strict must also say not.
  if (!lenient$significant_interaction) {
    expect_false(strict$significant_interaction)
  }
})

test_that("cp_posthoc_slopes records the adjustment method on the result", {
  skip_on_cran()
  fit <- make_lmer_fit()
  res_tukey <- cp_posthoc_slopes(fit, adjust = "tukey")
  res_bonf  <- cp_posthoc_slopes(fit, adjust = "bonferroni")
  expect_equal(attr(res_tukey, "adjust"), "tukey")
  expect_equal(attr(res_bonf, "adjust"), "bonferroni")
})

test_that("print.cp_posthoc renders without error for slopes", {
  skip_on_cran()
  result <- cp_posthoc_slopes(make_lmer_fit())
  expect_output(print(result))
})

# --- cp_posthoc_intercepts -------------------------------------------------

test_that("cp_posthoc_intercepts returns a cp_posthoc object tagged 'intercepts'", {
  skip_on_cran()
  result <- cp_posthoc_intercepts(make_lmer_fit())
  expect_s3_class(result, "cp_posthoc")
  expect_equal(attr(result, "type"), "intercepts")
})

test_that("cp_posthoc_intercepts always includes a non-empty emmeans data frame", {
  skip_on_cran()
  result <- cp_posthoc_intercepts(make_lmer_fit())
  expect_true("emmeans" %in% names(result))
  expect_s3_class(result$emmeans, "data.frame")
  expect_true(nrow(result$emmeans) > 0)
})

test_that("cp_posthoc_intercepts handles log10x models (evaluates at x = 1)", {
  skip_on_cran()
  fit_log10 <- make_lmer_fit(log10x = TRUE)
  result <- cp_posthoc_intercepts(fit_log10)
  expect_s3_class(result, "cp_posthoc")
  expect_true("emmeans" %in% names(result))
  expect_true(nrow(result$emmeans) > 0)
})

test_that("cp_posthoc_intercepts errors on non-cp_model_lmer input", {
  expect_error(cp_posthoc_intercepts("not a model"), "cp_model_lmer")
  expect_error(cp_posthoc_intercepts(list()), "cp_model_lmer")
})
