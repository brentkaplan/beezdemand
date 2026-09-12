# =============================================================================
# F-BD6-2 (audit 2026-09-06): every design matrix rebuilt from a fitted TMB
# model must be pinned to the contrasts in force at fit time. model.matrix()
# otherwise honours options("contrasts") at CALL time, and a changed global
# option keeps the column count while changing the basis, so beta multiplies
# the wrong columns silently. The EMM builder was pinned in TICKET-016 (F1);
# predict(), the RE (Z) design, and the `assign` fallback were not.
# =============================================================================

.f62_subset <- function(n_per_gender = 25) {
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

.f62_sum_contrasts <- list(contrasts = c("contr.sum", "contr.poly"))

test_that("predict.beezdemand_tmb is invariant to options(contrasts) (F-BD6-2)", {
  skip_on_cran()
  d <- .f62_subset()
  fit <- suppressWarnings(fit_demand_tmb(
    d, equation = "exponential", factors = "gender", verbose = 0
  ))
  nd <- d[d$gender == "Female", ][1:8, ]

  # Population-level output uses the nlme-style `predict.fixed` column;
  # subject-level keeps `.fitted`. Pull the level-appropriate column and
  # require numeric values so a missing column cannot pass as NULL == NULL.
  col_for <- c(population = "predict.fixed", subject = ".fitted")
  for (lev in c("population", "subject")) {
    expect_no_warning(p_def <- predict(fit, newdata = nd, level = lev)[[col_for[[lev]]]])
    p_sum <- withr::with_options(
      .f62_sum_contrasts,
      predict(fit, newdata = nd, level = lev)[[col_for[[lev]]]]
    )
    expect_true(is.numeric(p_def) && length(p_def) == nrow(nd), info = lev)
    expect_equal(p_sum, p_def, tolerance = 1e-10, info = lev)
  }
})

# F-BD6-4 (end-pass review of F-BD6-2): a collapse_levels fit predicts from
# newdata in the ORIGINAL shape (the original factor, not the internal
# `<factor>_Q0` / `<factor>_alpha` columns), under either contrasts setting.
test_that("predict.beezdemand_tmb accepts original-shaped newdata for a collapse_levels fit (F-BD6-4)", {
  skip_on_cran()
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  d$age_group <- factor(
    cut(d$age, c(0, 25, 35, Inf), labels = c("young", "mid", "old")),
    levels = c("young", "mid", "old")
  )
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ig <- unique(d$id[d$gender == g]); head(ig[order(ig)], 30)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    d, equation = "exponential", factors = c("gender", "age_group"),
    collapse_levels = list(
      Q0 = list(age_group = list(young_mid = c("young", "mid"), old = "old")),
      alpha = list(age_group = list(young = "young", mid_old = c("mid", "old")))
    ),
    verbose = 0
  )))
  nd <- d[, c("id", "x", "gender", "age_group")][1:12, ]
  expect_false(any(c("age_group_Q0", "age_group_alpha") %in% names(nd)))

  p_pop <- predict(fit, newdata = nd, level = "population")$predict.fixed
  p_sub <- predict(fit, newdata = nd, level = "subject")$.fitted
  expect_true(is.numeric(p_pop) && all(is.finite(p_pop)))
  expect_true(is.numeric(p_sub) && all(is.finite(p_sub)))
  # Same rows through the training-time (already collapsed) frame agree.
  nd_int <- fit$data[match(rownames(nd), rownames(fit$data)), ]
  expect_equal(predict(fit, newdata = nd_int, level = "population")$predict.fixed,
               p_pop, tolerance = 1e-10)

  p_pop_sum <- withr::with_options(
    .f62_sum_contrasts,
    predict(fit, newdata = nd, level = "population")$predict.fixed)
  p_sub_sum <- withr::with_options(
    .f62_sum_contrasts,
    predict(fit, newdata = nd, level = "subject")$.fitted)
  expect_equal(p_pop_sum, p_pop, tolerance = 1e-10)
  expect_equal(p_sub_sum, p_sub, tolerance = 1e-10)
})

test_that("subject-level predictions with a factor-expanded RE are invariant to options(contrasts) (F-BD6-2)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  apt_cond <- apt
  apt_cond$cond <- factor(rep_len(c("A", "B"), nrow(apt_cond)))
  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    apt_cond, equation = "simplified",
    random_effects = Q0 + alpha ~ cond, verbose = 0
  )))
  nd <- apt_cond[apt_cond$cond == "B", ][1:10, ]

  p_def <- predict(fit, newdata = nd, level = "subject")$.fitted
  p_sum <- withr::with_options(
    .f62_sum_contrasts,
    predict(fit, newdata = nd, level = "subject")$.fitted
  )
  expect_equal(p_sum, p_def, tolerance = 1e-10)
  # The stored RE block carries the fit-time contrasts explicitly.
  expect_equal(
    fit$param_info$random_effects_parsed$blocks[[1]]$contrasts,
    list(cond = "contr.treatment")
  )
})

test_that("the term-assign fallback rebuild is invariant to options(contrasts) (F-BD6-2)", {
  skip_on_cran()
  d <- .f62_subset()
  fit <- suppressWarnings(fit_demand_tmb(
    d, equation = "exponential", factors = "gender", verbose = 0
  ))
  # Strip `assign` so .tmb_term_assign_map() takes the rebuild path.
  attr(fit$formula_details$X_q0, "assign") <- NULL
  attr(fit$formula_details$X_alpha, "assign") <- NULL

  a_def <- suppressWarnings(anova(fit))
  a_sum <- withr::with_options(.f62_sum_contrasts, suppressWarnings(anova(fit)))
  expect_equal(a_sum, a_def, tolerance = 1e-10)
})
