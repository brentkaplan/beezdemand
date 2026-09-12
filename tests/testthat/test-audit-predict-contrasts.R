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

  for (lev in c("population", "subject")) {
    p_def <- predict(fit, newdata = nd, level = lev)$.fitted
    p_sum <- withr::with_options(
      .f62_sum_contrasts,
      predict(fit, newdata = nd, level = lev)$.fitted
    )
    expect_equal(p_sum, p_def, tolerance = 1e-10, info = lev)
  }
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
