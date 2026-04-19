# TICKET-007: coverage for extract_coefficients(), the cross-price coefficient
# dispatcher. Returns coef() for nls/lm and a list(fixed, random, combined)
# for lmer; errors on unsupported classes.

skip_if_not_installed("emmeans")
skip_if_not_installed("lme4")

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

test_that("extract_coefficients on cp_model_nls matches coef()", {
  skip_on_cran()
  fits <- make_fits()
  result <- extract_coefficients(fits$nls)
  expect_equal(result, coef(fits$nls))
})

test_that("extract_coefficients on cp_model_lm matches coef()", {
  skip_on_cran()
  fits <- make_fits()
  result <- extract_coefficients(fits$lm)
  expect_equal(result, coef(fits$lm))
})

test_that("extract_coefficients on cp_model_lmer returns list(fixed, random, combined)", {
  skip_on_cran()
  fits <- make_fits()
  result <- extract_coefficients(fits$lmer)
  expect_type(result, "list")
  expect_true(all(c("fixed", "random", "combined") %in% names(result)))
  expect_true(length(result$fixed) > 0L)
  expect_true(length(result$random) > 0L)
  expect_true(length(result$combined) > 0L)
})

test_that("extract_coefficients errors on unsupported class", {
  expect_error(extract_coefficients("not a model"), "Unsupported model class")
  expect_error(
    extract_coefficients(lm(y ~ x, data.frame(x = 1:5, y = 1:5))),
    "Unsupported model class"
  )
})
