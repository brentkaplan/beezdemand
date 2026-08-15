# fit_cp_linear() argument combinations with no tracked coverage before
# TICKET-071. Each of these was reachable only through the gitignored
# arg-matrix harness.
#
# Two gaps are worth naming explicitly:
#
#   * random_slope = TRUE had zero references anywhere executable --
#     vignettes/cross-price-models.Rmd passes FALSE, and inst/llm/cheatsheet.md
#     is prose.
#   * every tracked fit_cp_linear() test uses `etm` or constructs a
#     target/tgt column, so the no-target path was never exercised.
#
# No skip_on_cran() here on purpose -- see TICKET-070.

cp_linear_data <- function(n_subjects = 20, n_groups = 2, seed = 11) {
  withr::local_seed(seed)
  ids <- paste0("id", seq_len(n_subjects))
  groups <- paste0("g", seq_len(n_groups))
  x_vals <- exp(seq(log(0.5), log(25), length.out = 8))

  subject_df <- data.frame(
    id = ids,
    group = factor(rep(groups, length.out = n_subjects), levels = groups),
    intercept = stats::rnorm(n_subjects, mean = 10, sd = 2),
    slope = stats::rnorm(n_subjects, mean = -0.6, sd = 0.2),
    stringsAsFactors = FALSE
  )

  grid <- expand.grid(id = ids, x = x_vals, stringsAsFactors = FALSE)
  dat <- merge(grid, subject_df, by = "id")
  dat$y <- dat$intercept + dat$slope * dat$x +
    stats::rnorm(nrow(dat), sd = 0.8)

  dat[, c("id", "group", "x", "y")]
}

test_that("fit_cp_linear works on data with no target column (fixed)", {
  dat <- cp_linear_data()
  expect_false("target" %in% names(dat))

  fit <- fit_cp_linear(
    dat,
    type = "fixed",
    group_effects = "intercept",
    return_all = TRUE
  )

  expect_s3_class(fit, "cp_model_lm")
  expect_s3_class(fit$model, "lm")
  expect_true(all(is.finite(stats::coef(fit$model))))
})

test_that("fit_cp_linear works on data with no target column (mixed)", {
  skip_if_not_installed("lme4")

  dat <- cp_linear_data()
  expect_false("target" %in% names(dat))

  fit <- fit_cp_linear(
    dat,
    type = "mixed",
    group_effects = "intercept",
    return_all = TRUE
  )

  expect_s3_class(fit, "cp_model_lmer")
  expect_true(all(is.finite(lme4::fixef(fit$model))))
})

test_that("fit_cp_linear(random_slope = TRUE) fits a random slope on x", {
  skip_if_not_installed("lme4")

  dat <- cp_linear_data()
  fit <- fit_cp_linear(
    dat,
    type = "mixed",
    group_effects = "intercept",
    random_slope = TRUE,
    return_all = TRUE
  )

  expect_s3_class(fit, "cp_model_lmer")
  expect_true(fit$random_slope)

  # The random part must actually gain the slope term, not just record the flag.
  rand_terms <- deparse(stats::formula(fit$model))
  expect_match(paste(rand_terms, collapse = " "), "\\(1 \\+ x \\| id\\)")

  # A random slope adds a per-subject x column to the conditional modes.
  expect_true("x" %in% names(lme4::ranef(fit$model)$id))
})

test_that("random_slope = TRUE differs from the random-intercept fit", {
  skip_if_not_installed("lme4")

  dat <- cp_linear_data()
  ri <- fit_cp_linear(dat, type = "mixed", group_effects = "intercept",
                      random_slope = FALSE, return_all = TRUE)
  rs <- fit_cp_linear(dat, type = "mixed", group_effects = "intercept",
                      random_slope = TRUE, return_all = TRUE)

  expect_false(ri$random_slope)
  expect_true(rs$random_slope)
  expect_false("x" %in% names(lme4::ranef(ri$model)$id))
  # More variance components means strictly more parameters.
  expect_gt(
    attr(stats::logLik(rs$model), "df"),
    attr(stats::logLik(ri$model), "df")
  )
})

test_that("fit_cp_linear supports fixed group interaction with log10x", {
  dat <- cp_linear_data()
  fit <- fit_cp_linear(
    dat,
    type = "fixed",
    group_effects = "interaction",
    log10x = TRUE,
    return_all = TRUE
  )

  expect_s3_class(fit, "cp_model_lm")
  expect_true(fit$log10x)

  # log10x must reach the model formula, and "interaction" must produce the
  # group-by-price product term -- not just an additive group offset.
  coef_names <- names(stats::coef(fit$model))
  expect_true(any(grepl("log10\\(x\\)", coef_names)))
  expect_true(any(grepl("log10\\(x\\):group", coef_names)))
})

test_that("fixed interaction has more terms than the additive fit", {
  dat <- cp_linear_data()
  additive <- fit_cp_linear(dat, type = "fixed", group_effects = "intercept",
                            log10x = TRUE, return_all = TRUE)
  inter <- fit_cp_linear(dat, type = "fixed", group_effects = "interaction",
                         log10x = TRUE, return_all = TRUE)

  expect_gt(
    length(stats::coef(inter$model)),
    length(stats::coef(additive$model))
  )
})

test_that("predict on a grouped cp_model_lm requires group in newdata", {
  dat <- cp_linear_data()
  fit <- fit_cp_linear(
    dat,
    type = "fixed",
    group_effects = "intercept",
    return_all = TRUE
  )

  newdata <- data.frame(
    x = c(1, 5, 10),
    group = factor(c("g1", "g1", "g2"), levels = c("g1", "g2"))
  )
  preds <- predict(fit, newdata = newdata)

  expect_true(is.data.frame(preds))
  expect_true(all(c("x", "y_pred") %in% names(preds)))
  expect_equal(nrow(preds), 3)
  expect_true(all(is.finite(preds$y_pred)))

  # Omitting the grouping column is an error, not a silent NA. The arg-matrix
  # harness hit exactly this and swallowed it, because extract_fn failures were
  # excluded from its pass/fail flag. Message text is not pinned -- only that it
  # refuses rather than fabricating predictions.
  expect_error(predict(fit, newdata = data.frame(x = c(1, 5, 10))))
})

test_that("predict on a grouped cp_model_lmer requires group in newdata", {
  skip_if_not_installed("lme4")

  dat <- cp_linear_data()
  fit <- fit_cp_linear(
    dat,
    type = "mixed",
    group_effects = "intercept",
    return_all = TRUE
  )

  newdata <- data.frame(
    x = c(1, 5, 10),
    group = factor(c("g1", "g1", "g2"), levels = c("g1", "g2"))
  )
  preds <- predict(fit, newdata = newdata)

  expect_true(is.data.frame(preds))
  # nrow/column assertions before the finiteness check -- an empty frame would
  # satisfy all(is.finite(numeric(0))) and pass vacuously.
  expect_true(all(c("x", "y_pred") %in% names(preds)))
  expect_equal(nrow(preds), 3)
  expect_true(all(is.finite(preds$y_pred)))
  expect_error(predict(fit, newdata = data.frame(x = c(1, 5, 10))))
})
