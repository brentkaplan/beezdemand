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

# TICKET-068 (E5c): documented .fitted/.resid/.fixed columns must not
# disappear silently when fitted()/residuals()/predict() error or the
# result's length doesn't match the augmented data.

test_that("augment.cp_model_nls warns when .fitted/.resid are omitted (real fixture + failure)", {
  d <- data.frame(x = 1:6, y = c(2, 4, 5, 8, 11, 13))
  real_model <- stats::nls(y ~ a + b * x, data = d, start = list(a = 0, b = 2))

  # Length mismatch: real fitted() output, but `data` has an extra row.
  obj_mismatch <- structure(
    list(model = real_model, data = rbind(d, d[1, ])),
    class = "cp_model_nls"
  )
  expect_warning(
    out_mismatch <- augment(obj_mismatch),
    class = "beezdemand_cp_augment_omitted_warning",
    regexp = "fitted\\(\\).*length mismatch"
  )
  expect_false(".fitted" %in% names(out_mismatch))
  expect_false(".resid" %in% names(out_mismatch))

  # fitted() genuinely produces nothing usable (length-0 -> mismatch path).
  obj_fail <- structure(list(model = list(), data = d), class = "cp_model_nls")
  expect_warning(
    out_fail <- augment(obj_fail),
    class = "beezdemand_cp_augment_omitted_warning"
  )
  expect_false(".fitted" %in% names(out_fail))
})

# The warning must carry conditionMessage(e),
# not just "fitted()/residuals() failed". A class-specific S3 method defined
# at test-file scope is NOT visible to UseMethod() dispatch happening inside
# the package's own namespace (fitted.<class> here would never be found), so
# force the error at the generic itself via local_mocked_bindings(.package=
# "stats") -- this really does make stats::fitted() throw, giving a genuine
# caught condition, not a fabricated one.
test_that("augment.cp_model_nls warning includes conditionMessage(e) from a real fitted() error", {
  d <- data.frame(x = 1:6, y = c(2, 4, 5, 8, 11, 13))
  real_model <- stats::nls(y ~ a + b * x, data = d, start = list(a = 0, b = 2))
  obj <- structure(list(model = real_model, data = d), class = "cp_model_nls")

  testthat::local_mocked_bindings(
    fitted = function(...) stop("custom nls fitted boom"),
    .package = "stats"
  )

  expect_warning(
    out <- augment(obj),
    class = "beezdemand_cp_augment_omitted_warning",
    regexp = "fitted\\(\\) failed: custom nls fitted boom"
  )
  expect_false(".fitted" %in% names(out))
})

test_that("augment.cp_model_lm warns when .fitted/.resid are omitted (length mismatch)", {
  d <- data.frame(x = 1:6, y = c(2, 4, 5, 8, 11, 13))
  real_model <- stats::lm(y ~ x, data = d)

  obj_mismatch <- structure(
    list(model = real_model, data = rbind(d, d[1, ])),
    class = "cp_model_lm"
  )
  expect_warning(
    out <- augment(obj_mismatch),
    class = "beezdemand_cp_augment_omitted_warning",
    regexp = "\\.fitted: fitted\\(\\) length mismatch"
  )
  expect_false(".fitted" %in% names(out))
  expect_false(".resid" %in% names(out))
})

# Same conditionMessage requirement for lm.
test_that("augment.cp_model_lm warning includes conditionMessage(e) from a real fitted() error", {
  d <- data.frame(x = 1:6, y = c(2, 4, 5, 8, 11, 13))
  real_model <- stats::lm(y ~ x, data = d)
  obj <- structure(list(model = real_model, data = d), class = "cp_model_lm")

  testthat::local_mocked_bindings(
    fitted = function(...) stop("custom lm fitted boom"),
    .package = "stats"
  )

  expect_warning(
    out <- augment(obj),
    class = "beezdemand_cp_augment_omitted_warning",
    regexp = "\\.fitted: fitted\\(\\) failed: custom lm fitted boom"
  )
  expect_false(".fitted" %in% names(out))
})

test_that("augment.cp_model_lmer warns when .fitted/.resid are omitted (length mismatch)", {
  skip_if_not_installed("lme4")
  d <- data.frame(
    x = rep(1:6, 2),
    y = c(2, 4, 5, 8, 11, 13, 3, 5, 6, 9, 12, 14),
    id = factor(rep(c("a", "b"), each = 6))
  )
  real_model <- lme4::lmer(y ~ x + (1 | id), data = d)

  # Extra row: fitted()/residuals() (length 12, from the original model)
  # mismatch nrow(out) = 13; predict(newdata = x$data, ...) legitimately
  # produces 13 values, so .fixed is unaffected by THIS mismatch -- the
  # .fixed-omission path is exercised separately below.
  obj_mismatch <- structure(
    list(model = real_model, data = rbind(d, d[1, ])),
    class = "cp_model_lmer"
  )
  expect_warning(
    out <- augment(obj_mismatch),
    class = "beezdemand_cp_augment_omitted_warning"
  )
  expect_false(".fitted" %in% names(out))
  expect_false(".resid" %in% names(out))
})

test_that("augment.cp_model_lmer warns when .fixed is omitted (predict() failure)", {
  skip_if_not_installed("lme4")
  d <- data.frame(
    x = rep(1:6, 2),
    y = c(2, 4, 5, 8, 11, 13, 3, 5, 6, 9, 12, 14),
    id = factor(rep(c("a", "b"), each = 6))
  )
  real_model <- lme4::lmer(y ~ x + (1 | id), data = d)

  # `data` lacks the `x` predictor the fitted formula needs ->
  # predict(newdata=...) errors ("object 'x' not found").
  obj_bad <- structure(
    list(model = real_model, data = d[, "y", drop = FALSE]),
    class = "cp_model_lmer"
  )
  expect_warning(
    out <- augment(obj_bad),
    class = "beezdemand_cp_augment_omitted_warning",
    regexp = "\\.fixed: predict\\(\\) failed:.*object 'x' not found"
  )
  expect_false(".fixed" %in% names(out))
})

test_that("augment methods are silent when everything succeeds (real fixture)", {
  skip_on_cran()
  fits <- make_fits()
  expect_no_warning(
    augment(fits$nls),
    class = "beezdemand_cp_augment_omitted_warning"
  )
  expect_no_warning(
    augment(fits$lm),
    class = "beezdemand_cp_augment_omitted_warning"
  )
  expect_no_warning(
    augment(fits$lmer),
    class = "beezdemand_cp_augment_omitted_warning"
  )
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
