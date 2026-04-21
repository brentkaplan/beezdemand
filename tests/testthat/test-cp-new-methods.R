# TICKET-005: tests for the print / augment / confint / nobs methods on
# cross-price model classes (cp_model_nls, cp_model_lm, cp_model_lmer).

skip_if_not_installed("emmeans")
skip_if_not_installed("lme4")

# Lazily build fixtures inside the helpers so tests stay self-sufficient.
# We deliberately avoid hard-coding "did this fit succeed?" because it depends
# on the underlying NLS backend's behaviour on the etm dataset.
make_fits <- function() {
  data(etm, package = "beezdemand")
  list(
    nls = fit_cp_nls(etm, equation = "exponentiated", return_all = TRUE),
    lm  = fit_cp_linear(etm, type = "fixed", group_effects = TRUE,
                        return_all = TRUE),
    lmer = fit_cp_linear(etm, type = "mixed", group_effects = TRUE,
                         return_all = TRUE)
  )
}

# --- print methods ----------------------------------------------------------

test_that("print.cp_model_nls produces informative output", {
  skip_on_cran()
  fits <- make_fits()
  expect_output(print(fits$nls), "Cross-Price Demand Model \\(NLS\\)")
  expect_output(print(fits$nls), "Equation")
  expect_output(print(fits$nls), "Observations")
})

test_that("print.cp_model_lm produces informative output", {
  skip_on_cran()
  fits <- make_fits()
  expect_output(print(fits$lm), "Cross-Price Demand Model \\(LM\\)")
  expect_output(print(fits$lm), "Formula")
})

test_that("print.cp_model_lmer produces informative output", {
  skip_on_cran()
  fits <- make_fits()
  expect_output(print(fits$lmer), "Cross-Price Demand Model \\(LMER\\)")
  expect_output(print(fits$lmer), "Random effects")
})

test_that("print methods handle NULL model gracefully", {
  fake_nls <- structure(list(model = NULL, equation = "x", method = "y"),
                        class = "cp_model_nls")
  fake_lm <- structure(list(model = NULL, equation = "x"), class = "cp_model_lm")
  fake_lmer <- structure(list(model = NULL, equation = "x"), class = "cp_model_lmer")

  expect_output(print(fake_nls), "failed|NULL")
  expect_output(print(fake_lm), "failed|NULL")
  expect_output(print(fake_lmer), "failed|NULL")
})

# --- augment methods --------------------------------------------------------

test_that("augment.cp_model_nls returns tibble with .fitted and .resid", {
  skip_on_cran()
  fits <- make_fits()
  aug <- augment(fits$nls)
  expect_s3_class(aug, "tbl_df")
  expect_true(all(c(".fitted", ".resid") %in% names(aug)))
  expect_equal(nrow(aug), nobs(fits$nls))
})

test_that("augment.cp_model_lm returns tibble with .fitted and .resid", {
  skip_on_cran()
  fits <- make_fits()
  aug <- augment(fits$lm)
  expect_s3_class(aug, "tbl_df")
  expect_true(all(c(".fitted", ".resid") %in% names(aug)))
  expect_equal(nrow(aug), nobs(fits$lm))
})

test_that("augment.cp_model_lmer returns tibble with .fitted, .resid, .fixed", {
  skip_on_cran()
  fits <- make_fits()
  aug <- augment(fits$lmer)
  expect_s3_class(aug, "tbl_df")
  expect_true(all(c(".fitted", ".resid", ".fixed") %in% names(aug)))
  expect_equal(nrow(aug), nobs(fits$lmer))
})

test_that("augment methods preserve original data columns", {
  skip_on_cran()
  fits <- make_fits()
  aug <- augment(fits$nls)
  data_cols <- names(fits$nls$data)
  expect_true(all(data_cols %in% names(aug)))
})

test_that("augment methods on NULL model return empty tibble (no error)", {
  fake_lm <- structure(list(model = NULL, data = NULL), class = "cp_model_lm")
  aug <- augment(fake_lm)
  expect_s3_class(aug, "tbl_df")
  expect_equal(nrow(aug), 0)
})

# --- confint methods --------------------------------------------------------

test_that("confint.cp_model_lm returns the expected tibble shape", {
  skip_on_cran()
  fits <- make_fits()
  ci <- confint(fits$lm)
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, c("term", "estimate", "conf.low", "conf.high", "level", "method"))
  expect_true(nrow(ci) > 0)
  expect_true(all(ci$conf.low <= ci$estimate + 1e-8))
  expect_true(all(ci$estimate <= ci$conf.high + 1e-8))
  expect_true(all(ci$level == 0.95))
  expect_true(all(ci$method == "Wald"))
})

test_that("confint.cp_model_lmer returns the expected tibble shape", {
  skip_on_cran()
  fits <- make_fits()
  ci <- confint(fits$lmer)
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, c("term", "estimate", "conf.low", "conf.high", "level", "method"))
  expect_true(nrow(ci) > 0)
  expect_true(all(ci$method == "Wald"))
})

test_that("confint level controls interval width (lm)", {
  skip_on_cran()
  fits <- make_fits()
  ci_95 <- confint(fits$lm, level = 0.95)
  ci_99 <- confint(fits$lm, level = 0.99)
  expect_true(all(ci_99$level == 0.99))
  width_95 <- ci_95$conf.high - ci_95$conf.low
  width_99 <- ci_99$conf.high - ci_99$conf.low
  # 99% intervals must be at least as wide as 95% intervals (typically wider).
  expect_true(all(width_99 + 1e-12 >= width_95))
})

test_that("confint methods reject invalid level", {
  fake_lm <- structure(list(model = lm(y ~ x, data.frame(x = 1:5, y = 1:5))),
                       class = "cp_model_lm")
  expect_error(confint(fake_lm, level = 2), "between 0 and 1")
  expect_error(confint(fake_lm, level = -0.1), "between 0 and 1")
})

test_that("confint methods on NULL model return empty tibble (no error)", {
  fake_lm <- structure(list(model = NULL), class = "cp_model_lm")
  fake_lmer <- structure(list(model = NULL), class = "cp_model_lmer")
  expect_warning(ci_lm <- confint(fake_lm), "Model fitting may have failed")
  expect_warning(ci_lmer <- confint(fake_lmer), "Model fitting may have failed")
  expect_s3_class(ci_lm, "tbl_df")
  expect_s3_class(ci_lmer, "tbl_df")
  expect_equal(nrow(ci_lm), 0)
  expect_equal(nrow(ci_lmer), 0)
})

# --- nobs methods -----------------------------------------------------------

test_that("nobs returns correct integer counts for all three classes", {
  skip_on_cran()
  fits <- make_fits()
  for (nm in names(fits)) {
    n <- nobs(fits[[nm]])
    expect_true(is.integer(n) || is.numeric(n), info = nm)
    expect_true(n > 0, info = nm)
  }
})

test_that("nobs returns NA on NULL model", {
  fake_nls <- structure(list(model = NULL), class = "cp_model_nls")
  fake_lm <- structure(list(model = NULL), class = "cp_model_lm")
  fake_lmer <- structure(list(model = NULL), class = "cp_model_lmer")
  expect_true(is.na(nobs(fake_nls)))
  expect_true(is.na(nobs(fake_lm)))
  expect_true(is.na(nobs(fake_lmer)))
})
