# Tests for TICKET-013: anova.beezdemand_tmb() — joint Wald-chi-square for
# fixed-effect term groups (single fit) and nested LRT (multiple fits).
#
# Fixture notes
# -------------
# * `apt_full` has NO `id_group` (the ticket draft's column does not exist).
#   It has `gender` — a character column with three distinct values
#   (`Female`, `Male`, `Would rather not say`); used as `factors = "gender"`
#   it becomes a 3-level factor, giving 2-df Wald groups directly.
# * All fit-based tests skip_on_cran(); the first block is structural and
#   runs everywhere.

test_that(".tmb_group_terms groups by term, parameter, and auto", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential",
                        factors = "gender", verbose = 0)

  g_term <- beezdemand:::.tmb_group_terms(fit, group_by = "term")
  expect_true(all(vapply(g_term, function(g) length(g$idx), integer(1)) == 1L))

  g_param <- beezdemand:::.tmb_group_terms(fit, group_by = "parameter")
  expect_setequal(vapply(g_param, `[[`, character(1), "label"), c("Q0", "alpha"))

  g_auto <- beezdemand:::.tmb_group_terms(fit, group_by = "auto")
  labs <- vapply(g_auto, `[[`, character(1), "label")
  expect_true(all(grepl("~ gender$", labs)))         # intercepts excluded
  # 3-level factor -> 2-df joint group per parameter:
  expect_true(all(vapply(g_auto, function(g) length(g$idx), integer(1)) == 2L))
})

test_that(".tmb_group_terms honors explicit named-list and errors on unknown", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  tn <- beezdemand:::.tmb_build_term_names(fit)
  q0_terms <- tn$term[tn$q0_idx]
  grp <- beezdemand:::.tmb_group_terms(
    fit, terms = stats::setNames(list(q0_terms), "all Q0")
  )
  expect_equal(grp[[1]]$label, "all Q0")
  expect_equal(length(grp[[1]]$idx), length(q0_terms))

  expect_error(
    beezdemand:::.tmb_group_terms(fit, terms = "Q0:nonexistent"),
    "Unknown term"
  )
})

test_that("anova S3 method is registered", {
  expect_false(is.null(getS3method("anova", "beezdemand_tmb", optional = TRUE)))
})

test_that("anova.beezdemand_tmb returns expected schema (single fit)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  res <- anova(fit)
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("Group", "Chisq", "df", "p.value"))
  expect_true(all(res$df >= 1))
  expect_true(all(res$Chisq >= 0))
  expect_true(all(res$p.value >= 0 & res$p.value <= 1))
})

test_that("anova.beezdemand_tmb single-coefficient group equals z^2", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  res <- anova(fit, group_by = "term")
  td  <- broom::tidy(fit, report_space = "internal")   # C5: internal scale
  joined <- merge(res, td, by.x = "Group", by.y = "term")
  expect_gt(nrow(res), 0)
  expect_equal(nrow(joined), nrow(res))   # every group row must join - no silent drop
  expect_equal(joined$Chisq, (joined$estimate / joined$std.error)^2,
               tolerance = 1e-6)
})

test_that("anova.beezdemand_tmb group_by='parameter' yields two groups", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  res <- anova(fit, group_by = "parameter")
  expect_setequal(res$Group, c("Q0", "alpha"))
})

test_that("anova.beezdemand_tmb auto-groups a multi-df factor", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential",
                        factors = "gender", verbose = 0)
  res <- anova(fit)                               # default auto
  expect_true(any(grepl("^Q0 ~ gender$", res$Group)))
  expect_equal(res$df[res$Group == "Q0 ~ gender"], 2L)   # 3-level -> 2 df
})

test_that("anova.beezdemand_tmb honors an explicit terms list", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  tn <- beezdemand:::.tmb_build_term_names(fit)
  q0_terms <- tn$term[tn$q0_idx]
  res <- anova(fit, terms = stats::setNames(list(q0_terms), "Q0 block"))
  expect_equal(res$Group, "Q0 block")
  expect_equal(res$df, length(q0_terms))
})

test_that("anova.beezdemand_tmb errors on unknown / empty terms", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  expect_error(anova(fit, terms = "Q0:bogus"), "Unknown term")
  expect_error(anova(fit, terms = character(0)), "no terms to test")
})

test_that("anova.beezdemand_tmb errors helpfully on unconverged fit", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  fit$sdr$cov.fixed <- NULL
  expect_error(anova(fit), "converge|cov\\.fixed")
})

test_that("anova.beezdemand_tmb errors on a singular variance block", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit <- fit_demand_tmb(apt_full, equation = "exponential", factors = "gender",
                        verbose = 0)
  # Force the first auto group's covariance submatrix to be exactly singular
  # (rank 1) by setting it to all ones.
  idx <- beezdemand:::.tmb_group_terms(fit, group_by = "auto")[[1]]$idx
  fit$sdr$cov.fixed[idx, idx] <- 1
  expect_error(anova(fit), "singular")
})

# Task 3: multi-fit LRT / AIC tests

test_that("anova.beezdemand_tmb nested LRT matches anova.lme shape", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit_null <- fit_demand_tmb(apt_full, equation = "exponential", verbose = 0)
  fit_full <- fit_demand_tmb(apt_full, equation = "exponential",
                             factors = "gender", verbose = 0)
  res <- anova(fit_null, fit_full, test = "LRT")
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("Model", "df", "AIC", "Chisq", "Pr(>Chisq)") %in% names(res)))
  expect_equal(nrow(res), 2)
  expect_true(is.na(res$Chisq[1]))            # first row is the baseline
  expect_false(is.na(res[["Pr(>Chisq)"]][2]))
  # Value anchors: Chisq is 2 * delta-logLik; df holds the sorted model dfs.
  expect_equal(res$Chisq[2],
               2 * (as.numeric(logLik(fit_full)) - as.numeric(logLik(fit_null))),
               tolerance = 1e-6)
  expect_equal(res$df,
               sort(c(length(fit_null$opt$par), length(fit_full$opt$par))))
})

test_that("anova.beezdemand_tmb errors on non-nested fits under LRT (logLik decreases)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit_null <- fit_demand_tmb(apt_full, equation = "exponential", verbose = 0)
  fit_full <- fit_demand_tmb(apt_full, equation = "exponential",
                             factors = "gender", verbose = 0)
  # Force a nestedness violation: higher-df model with lower loglik.
  # (Only $loglik is mutated; $AIC is left stale - the LRT guard reads $loglik.)
  fit_full$loglik <- as.numeric(fit_null$loglik) - 10
  expect_error(anova(fit_null, fit_full, test = "LRT"), "not nested")
})

test_that("anova.beezdemand_tmb errors on equal-df non-nested fits under LRT", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  # `age` and `binges` each add one continuous covariate -> identical df,
  # but neither fit is nested in the other. The ddf <= 0 guard must fire.
  # The covariate fits emit benign TMB sdreport convergence notices (NaN SEs
  # from an ill-conditioned Hessian); anova()'s nesting guard is what this
  # test exercises, so the fit-time warnings are suppressed here.
  fit_age <- suppressWarnings(fit_demand_tmb(apt_full, equation = "exponential",
                                             continuous_covariates = "age", verbose = 0))
  fit_bin <- suppressWarnings(fit_demand_tmb(apt_full, equation = "exponential",
                                             continuous_covariates = "binges", verbose = 0))
  expect_error(anova(fit_age, fit_bin, test = "LRT"), "not nested")
})

test_that("anova.beezdemand_tmb test='AIC' returns a table without LRT error", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit_null <- fit_demand_tmb(apt_full, equation = "exponential", verbose = 0)
  fit_full <- fit_demand_tmb(apt_full, equation = "exponential",
                             factors = "gender", verbose = 0)
  fit_full$loglik <- as.numeric(fit_null$loglik) - 10   # non-nested ($AIC left as the real value)
  res <- anova(fit_null, fit_full, test = "AIC")         # must NOT error
  expect_true(all(c("Model", "df", "AIC") %in% names(res)))
  expect_equal(nrow(res), 2)
})

test_that("anova.beezdemand_tmb rejects test='Wald' for a multi-fit comparison", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  fit_null <- fit_demand_tmb(apt_full, equation = "exponential", verbose = 0)
  fit_full <- fit_demand_tmb(apt_full, equation = "exponential",
                             factors = "gender", verbose = 0)
  # Wald is a single-model joint test; comparing multiple fits must use LRT
  # or AIC. Previously test = "Wald" silently returned the LRT-shaped table.
  expect_error(anova(fit_null, fit_full, test = "Wald"), "Wald")
})

# Task 4: cross-backend direction-parity test

test_that("anova.beezdemand_tmb cross-backend direction parity (Wald vs F)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  apt_full$y_ll4 <- ll4(apt_full$y, lambda = 4)
  fit_nlme <- fit_demand_mixed(apt_full, equation_form = "zben",
                               factors = "gender", y_var = "y_ll4",
                               x_var = "x", id_var = "id")
  fit_tmb  <- fit_demand_tmb(apt_full, equation = "zben", factors = "gender",
                             y_var = "y_ll4", verbose = 0)
  a_nlme <- anova(fit_nlme$model)                 # anova.lme per-term F table
  a_tmb  <- anova(fit_tmb)
  # Direction parity: same significance verdict on the gender effect.
  p_nlme_rows <- a_nlme[grepl("gender", rownames(a_nlme)), "p-value"]
  p_tmb_rows  <- a_tmb$p.value[grepl("gender", a_tmb$Group)]
  # Guard: grepl must match - otherwise min(numeric(0)) = Inf mis-signals.
  expect_gt(length(p_nlme_rows), 0)
  expect_gt(length(p_tmb_rows), 0)
  expect_equal(min(p_nlme_rows) < 0.05, min(p_tmb_rows) < 0.05)
})
