# Batch 3 (2026-09-12), F-BD10-1: `df_method = c("containment", "between")` on
# summary()/tidy() for beezdemand_nlme. nlme's containment rule assigns the
# observation-level residual df to between-subject terms; "between" uses
# n_subjects - rank(subject-level design) for those terms only.

.dfm_gender_fit <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      data(apt_full, package = "beezdemand")
      d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
      d$gender <- droplevels(factor(d$gender))
      ids <- unique(d$id)
      keep <- unlist(lapply(split(ids, d$gender[match(ids, d$id)]), head, 25))
      d <- d[d$id %in% keep, ]
      d$id <- droplevels(factor(d$id))
      d$y_ll4 <- ll4(d$y, lambda = 4)
      cache <<- suppressMessages(fit_demand_mixed(
        d, equation_form = "zben", factors = "gender",
        y_var = "y_ll4", x_var = "x", id_var = "id"))
    }
    cache
  }
})

test_that("tidy/summary carry a df column; containment is bit-identical to nlme", {
  skip_on_cran()
  fit <- .dfm_gender_fit()
  skip_if(is.null(fit$model), "gender NLME fit failed")
  tt <- summary(fit$model)$tTable
  n_subj <- length(unique(fit$data$id))
  expect_identical(n_subj, 50L)

  td <- tidy(fit, effects = "fixed")
  expect_true("df" %in% names(td))
  expect_equal(unname(td$df), unname(tt[, "DF"]))
  expect_equal(unname(td$p.value), unname(tt[, "p-value"]))
  td_c <- tidy(fit, effects = "fixed", df_method = "containment")
  expect_identical(td_c, td)
  # Variance rows carry NA df.
  td_all <- tidy(fit)
  expect_true(all(is.na(td_all$df[td_all$component == "variance"])))

  s <- summary(fit)
  expect_true("df" %in% names(s$coefficients))
  expect_identical(s$df_method, "containment")
  expect_equal(unname(s$coefficients$df), unname(tt[, "DF"]))
  expect_error(tidy(fit, df_method = "satterthwaite"))
  expect_error(summary(fit, df_method = "satterthwaite"))
})

test_that("df_method = 'between' uses n_subjects - rank for between-subject terms only", {
  skip_on_cran()
  fit <- .dfm_gender_fit()
  skip_if(is.null(fit$model), "gender NLME fit failed")
  tt <- summary(fit$model)$tTable
  n_subj <- length(unique(fit$data$id))

  td <- tidy(fit, effects = "fixed", df_method = "between")
  is_gender <- grepl("gender", td$term)
  expect_identical(sum(is_gender), 2L)
  expect_equal(unname(td$df[is_gender]), rep(n_subj - 2, 2))  # intercept + binary gender
  expect_equal(unname(td$df[!is_gender]), unname(tt[!is_gender, "DF"]))
  expect_equal(unname(td$statistic), unname(tt[, "t-value"]))
  expect_equal(unname(td$p.value[is_gender]),
               2 * stats::pt(-abs(unname(td$statistic[is_gender])), n_subj - 2))
  expect_true(all(td$p.value[is_gender] >= unname(tt[is_gender, "p-value"])))
  expect_equal(unname(td$p.value[!is_gender]), unname(tt[!is_gender, "p-value"]))
  # Estimates untouched by the df choice.
  expect_equal(td$estimate, tidy(fit, effects = "fixed")$estimate)

  s <- summary(fit, df_method = "between")
  expect_identical(s$df_method, "between")
  expect_equal(s$coefficients$df, td$df)
  out <- capture.output(print(s))
  expect_true(any(grepl(paste0("\\b", n_subj - 2, "\\b"), out)))
  expect_true(any(grepl("between-subject", out)))
  out_default <- capture.output(print(summary(fit)))
  expect_false(any(grepl("between-subject", out_default)))
  expect_true(any(grepl("\\b797\\b", out_default)))
})

test_that("df_method = 'between' leaves within-subject terms on containment df", {
  skip_on_cran()
  dat <- .simulate_within_subject_demand(n_subjects = 25, n_conditions = 3, seed = 101)
  dat$y_ll4 <- ll4(dat$y)
  cond_var <- setdiff(names(dat), c("id", "x", "y", "y_ll4"))[1]
  fit <- suppressMessages(suppressWarnings(fit_demand_mixed(
    data = dat, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation_form = "zben", factors = cond_var,
    random_effects = nlme::pdDiag(Q0 + alpha ~ 1), verbose = FALSE)))
  skip_if(is.null(fit$model), "within-subject NLME fit failed")
  # Precondition: the factor varies within subject.
  expect_true(any(tapply(dat[[cond_var]], dat$id, function(v) length(unique(v))) > 1))
  td_c <- tidy(fit, effects = "fixed", df_method = "containment")
  td_b <- tidy(fit, effects = "fixed", df_method = "between")
  expect_equal(td_b$df, td_c$df)
  expect_equal(td_b$p.value, td_c$p.value)
})

test_that("tidy.beezdemand_tmb reports df = Inf for its asymptotic z tests (shape parity)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- suppressWarnings(fit_demand_tmb(apt, equation = "exponentiated", verbose = 0))
  td <- tidy(fit)
  expect_true("df" %in% names(td))
  expect_true(all(is.infinite(td$df[td$component == "fixed"])))
  expect_true(all(is.na(td$df[td$component == "variance"])))
})
