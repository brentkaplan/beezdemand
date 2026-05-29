# Tests for TICKET-016: get_demand_comparisons() backend harmonization (NLME + TMB).
# Spec: internal_docs/tickets/TICKET-016-REFINED-2026-05-27.md
#
# Fixtures are memoized per-file (testthat parallelizes files, not test_that
# blocks, so a file-level cache is valid and bounds peak CI memory; pattern
# mirrors test-calc_group_metrics_nlme.R / test-boot-demand.R). NOTE: apt_full
# has NO id_group; the between-subjects factor is `gender`.

.h16_cache <- new.env(parent = emptyenv())

# Two-level gender subsample of apt_full, LL4-transformed for zben fits.
.h16_gender_data <- function(n_per_group = 18) {
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ig <- unique(d$id[d$gender == g])
    head(ig[order(ig)], n_per_group)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  d$y_ll4 <- ll4(d$y, lambda = 4)
  d
}

# gender (2 levels) + age_group (3 levels) subsample for marginalization tests.
# `drop_cell` removes one gender x age_group combination so full-grid vs
# observed-only weighting genuinely differ (pins Decision 10 Option A).
.h16_two_factor_data <- function(n_per_group = 30, drop_cell = NULL) {
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  d$age_group <- factor(
    cut(d$age, c(0, 25, 35, Inf), labels = c("young", "mid", "old")),
    levels = c("young", "mid", "old")
  )
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ig <- unique(d$id[d$gender == g])
    head(ig[order(ig)], n_per_group)
  }))
  d <- d[d$id %in% ids_keep, ]
  if (!is.null(drop_cell)) {
    d <- d[!(d$gender == drop_cell[["gender"]] &
               d$age_group == drop_cell[["age_group"]]), ]
  }
  d <- d[d$id %in% d$id, ]
  d$id <- droplevels(as.factor(d$id))
  d
}

.h16_nlme_fit <- function() {
  if (is.null(.h16_cache$nlme)) {
    .h16_cache$nlme <- suppressMessages(fit_demand_mixed(
      .h16_gender_data(), y_var = "y_ll4", x_var = "x", id_var = "id",
      factors = "gender", equation_form = "zben"))
  }
  .h16_cache$nlme
}

.h16_tmb_fit <- function() {
  if (is.null(.h16_cache$tmb)) {
    .h16_cache$tmb <- suppressWarnings(fit_demand_tmb(
      .h16_gender_data(), equation = "exponential",
      factors = "gender", verbose = 0))
  }
  .h16_cache$tmb
}

.h16_tmb_2f <- function() {
  if (is.null(.h16_cache$tmb2f)) {
    .h16_cache$tmb2f <- suppressWarnings(fit_demand_tmb(
      .h16_two_factor_data(), equation = "exponential",
      factors = c("gender", "age_group"), verbose = 0))
  }
  .h16_cache$tmb2f
}

.h16_tmb_2f_unbal <- function() {
  if (is.null(.h16_cache$tmb2fu)) {
    .h16_cache$tmb2fu <- suppressWarnings(fit_demand_tmb(
      .h16_two_factor_data(drop_cell = c(gender = "Male", age_group = "old")),
      equation = "exponential",
      factors = c("gender", "age_group"), verbose = 0))
  }
  .h16_cache$tmb2fu
}

# Asymmetric-collapse TMB fit (F2 regression): Q0 collapses age_group to 2
# levels (junior/old), alpha keeps 3 (young/mid/old). So factors_q0 =
# "age_group_Q0", factors_alpha = "age_group_alpha", but param_info$factors =
# "age_group" (the original name users pass via compare_specs). Subsampled
# across all three age tiers so every level survives; CI-memory hygiene.
.h16_collapse_data <- function(per_tier = 18) {
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$age_group <- factor(cut(d$age, c(0, 25, 35, Inf),
                            labels = c("young", "mid", "old")))
  d <- d[!is.na(d$age_group), ]
  ids_keep <- unlist(lapply(levels(d$age_group), function(g) {
    ig <- unique(d$id[d$age_group == g]); head(ig[order(ig)], per_tier)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  d
}

.h16_collapse_fit <- function() {
  if (is.null(.h16_cache$collapse)) {
    .h16_cache$collapse <- suppressWarnings(fit_demand_tmb(
      .h16_collapse_data(), equation = "exponential", factors = "age_group",
      collapse_levels = list(
        Q0    = list(age_group = list(junior = c("young", "mid"), old = "old")),
        alpha = list(age_group = list(young = "young", mid = "mid", old = "old"))
      ),
      verbose = 0))
  }
  .h16_cache$collapse
}

# NLME analog of the asymmetric-collapse fit (F3 follow-up regression). Same
# structure: factors_Q0 = age_group_Q0 (2 lev), factors_alpha = age_group_alpha
# (3 lev), param_info$factors = age_group (original). Used to verify the NLME
# compare_specs validator accepts both the original factor name AND the
# per-parameter collapsed column, and aborts per-param on a cross-parameter
# alias (mirroring the TMB resolver).
.h16_nlme_collapse_fit <- function() {
  if (is.null(.h16_cache$nlme_collapse)) {
    d <- .h16_collapse_data(per_tier = 12)  # smaller -> faster nlme fit
    d$y_ll4 <- ll4(d$y, lambda = 4)
    .h16_cache$nlme_collapse <- suppressMessages(fit_demand_mixed(
      d, y_var = "y_ll4", x_var = "x", id_var = "id",
      factors = "age_group", equation_form = "zben",
      collapse_levels = list(
        Q0    = list(age_group = list(junior = c("young", "mid"), old = "old")),
        alpha = list(age_group = list(young = "young", mid = "mid", old = "old"))
      )))
  }
  .h16_cache$nlme_collapse
}

# Synthetic single-factor exponential demand with a CUSTOM level order, used
# to prove factor-level (not data-appearance) contrast ordering on TMB.
.h16_sim_one_factor <- function(level_order, n_per = 9, seed = 11) {
  set.seed(seed)
  prices <- 10^seq(-1, 1.4, length.out = 6)
  # distinct true Q0 per level so contrasts are non-zero & well-identified
  q0_truth <- stats::setNames(seq(40, 90, length.out = length(level_order)),
                              level_order)
  rows <- list()
  sid <- 0L
  for (lev in level_order) {
    for (s in seq_len(n_per)) {
      sid <- sid + 1L
      q0 <- q0_truth[[lev]] + stats::rnorm(1, 0, 3)
      alpha <- 0.003
      y <- q0 * exp(-alpha * q0 * prices) + stats::rnorm(length(prices), 0, 1.2)
      y[y < 0] <- 0.1
      rows[[length(rows) + 1L]] <- data.frame(
        id = sid, x = prices, y = y, grp = lev, stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  out$grp <- factor(out$grp, levels = level_order)
  out$id <- factor(out$id)
  out
}

# Manual full-grid (Option A) and observed-only marginalized log-Q0 contrast
# for a 2-factor additive TMB fit, retaining `gender` and averaging `age_group`.
# Returns natural-log estimates (divide by log(10) for log10).
.h16_marginal_oracle <- function(fit, retain = "gender", omit = "age_group") {
  coefs <- fit$model$coefficients
  beta <- unname(coefs[names(coefs) == "beta_q0"])
  xnames <- colnames(fit$formula_details$X_q0)
  glev <- levels(fit$data[[retain]])
  alev <- levels(fit$data[[omit]])
  full <- expand.grid(
    setNames(list(factor(glev, levels = glev), factor(alev, levels = alev)),
             c(retain, omit)),
    stringsAsFactors = FALSE
  )
  rhs <- stats::as.formula(paste("~", paste(c(retain, omit), collapse = " + ")))
  Xf <- stats::model.matrix(rhs, data = full)
  Xf <- Xf[, xnames, drop = FALSE]
  pred <- as.numeric(Xf %*% beta)
  # full-grid: average over ALL omit levels for each retain level
  marg_full <- tapply(pred, full[[retain]], mean)
  # observed-only: average over observed omit levels per retain level
  obs <- unique(fit$data[, c(retain, omit)])
  marg_obs <- vapply(glev, function(g) {
    ag <- as.character(obs[[omit]][obs[[retain]] == g])
    sel <- as.character(full[[retain]]) == g &
      as.character(full[[omit]]) %in% ag
    mean(pred[sel])
  }, numeric(1))
  list(
    full_log10 = as.numeric(marg_full[glev[1]] - marg_full[glev[2]]) / log(10),
    obs_log10 = as.numeric(marg_obs[glev[1]] - marg_obs[glev[2]]) / log(10),
    levels = glev
  )
}

# =============================================================================
# 1. tidy() cross-backend column contract (Decision 3/4)
# =============================================================================
test_that("tidy() yields identical column contract on NLME and TMB", {
  skip_on_cran()
  contract <- c("param", "contrast", "estimate", "std.error",
                "statistic", "df", "conf.low", "conf.high", "p.value")

  res_nlme <- suppressMessages(get_demand_comparisons(
    .h16_nlme_fit(), param = c("Q0", "alpha"), compare_specs = ~ gender))
  res_tmb <- suppressMessages(get_demand_comparisons(
    .h16_tmb_fit(), param = c("Q0", "alpha"), compare_specs = ~ gender))

  td_nlme <- broom::tidy(res_nlme)
  td_tmb <- broom::tidy(res_tmb)

  expect_identical(names(td_nlme), contract)
  expect_identical(names(td_tmb), contract)
  expect_contains(unique(td_nlme$param), c("Q0", "alpha"))
  expect_contains(unique(td_tmb$param), c("Q0", "alpha"))
})

# =============================================================================
# 2. param vector default returns BOTH params (Decision 2)
# =============================================================================
test_that("get_demand_comparisons() returns both Q0 and alpha by default", {
  skip_on_cran()
  res_tmb <- suppressMessages(get_demand_comparisons(.h16_tmb_fit()))
  expect_contains(names(res_tmb), c("Q0", "alpha"))
  td <- broom::tidy(res_tmb)
  expect_contains(unique(td$param), c("Q0", "alpha"))

  res_nlme <- suppressMessages(get_demand_comparisons(.h16_nlme_fit()))
  expect_contains(names(res_nlme), c("Q0", "alpha"))
})

# =============================================================================
# 3. log10 scale + exponentiate identity (Decision 5)
# =============================================================================
test_that("TMB contrasts are on log10 and exponentiate gives ratios", {
  skip_on_cran()
  fit <- .h16_tmb_fit()
  res <- suppressMessages(get_demand_comparisons(fit, param = "Q0"))
  td <- broom::tidy(res)
  td_exp <- broom::tidy(res, exponentiate = TRUE)

  # Manual log10 difference of get_demand_param_emms cell means (the oracle).
  emms <- get_demand_param_emms(fit, param = "Q0")
  manual_log10 <- log10(emms$estimate[1]) - log10(emms$estimate[2])
  expect_equal(td$estimate[1], manual_log10, tolerance = 1e-8)

  # exponentiate identity + ratio agreement with the nested $contrasts_ratio.
  expect_equal(td_exp$estimate, 10^td$estimate, tolerance = 1e-10)
  expect_equal(td_exp$estimate[1], res$Q0$contrasts_ratio$ratio[1],
               tolerance = 1e-8)
})

# =============================================================================
# 4. factor-level contrast ordering on TMB (Decision 7) + determinism
# =============================================================================
test_that("TMB contrasts follow factor-level order, not data appearance", {
  skip_on_cran()
  dat <- .h16_sim_one_factor(level_order = c("C", "A", "B"))
  fit <- suppressWarnings(fit_demand_tmb(
    dat, equation = "exponential", factors = "grp", verbose = 0))
  skip_if_not(isTRUE(fit$converged), "TMB fit did not converge")

  res <- suppressMessages(get_demand_comparisons(fit, param = "Q0"))
  td <- broom::tidy(res)
  # Factor-level order C(1) A(2) B(3): pairwise i<j -> C-A, C-B, A-B.
  expect_identical(td$contrast, c("C - A", "C - B", "A - B"))

  # Determinism: reshuffling input ROWS must not change labels or signs.
  set.seed(7)
  dat_shuf <- dat[sample(nrow(dat)), ]
  fit_shuf <- suppressWarnings(fit_demand_tmb(
    dat_shuf, equation = "exponential", factors = "grp", verbose = 0))
  skip_if_not(isTRUE(fit_shuf$converged), "TMB fit did not converge")
  td_shuf <- broom::tidy(suppressMessages(
    get_demand_comparisons(fit_shuf, param = "Q0")))
  expect_identical(td_shuf$contrast, td$contrast)
  expect_equal(sign(td_shuf$estimate), sign(td$estimate))

  # Sign matches emms[C] - emms[A] on the C-A contrast.
  emms <- get_demand_param_emms(fit, param = "Q0")
  expect_equal(sign(td$estimate[1]),
               sign(log10(emms$estimate[1]) - log10(emms$estimate[2])))
})

# =============================================================================
# 5. adjust uniform "holm" + validation against p.adjust.methods (Decision 6)
# =============================================================================
test_that("TMB adjust validates against stats::p.adjust.methods", {
  skip_on_cran()
  fit <- .h16_tmb_fit()

  expect_error(
    suppressMessages(get_demand_comparisons(fit, param = "Q0", adjust = "tukey")),
    regexp = "holm|p.adjust|valid"
  )

  res_holm <- suppressMessages(get_demand_comparisons(fit, param = "Q0"))
  res_none <- suppressMessages(
    get_demand_comparisons(fit, param = "Q0", adjust = "none"))
  # default is holm; "none" leaves raw p-values
  p_raw <- res_none$Q0$contrasts_log10$p.value
  expect_equal(res_holm$Q0$contrasts_log10$p.value,
               stats::p.adjust(p_raw, "holm"), tolerance = 1e-12)
})

# =============================================================================
# 6. NLME params_to_compare deprecation -> param (Decision 2)
# =============================================================================
test_that("NLME get_demand_comparisons() deprecates params_to_compare -> param", {
  skip_on_cran()
  fit <- .h16_nlme_fit()
  old <- options(lifecycle_verbosity = "warning")
  on.exit(options(old), add = TRUE)
  expect_warning(
    res_dep <- suppressMessages(
      get_demand_comparisons(fit, params_to_compare = "Q0", compare_specs = ~ gender)),
    "deprecat"
  )
  res_new <- suppressMessages(
    get_demand_comparisons(fit, param = "Q0", compare_specs = ~ gender))
  expect_named(res_dep, "Q0")
  expect_equal(broom::tidy(res_dep)$estimate, broom::tidy(res_new)$estimate)
})

# =============================================================================
# 7. TMB p_adjust removed outright (Decision 6)
# =============================================================================
test_that("TMB get_demand_comparisons() no longer accepts p_adjust", {
  skip_on_cran()
  fit <- .h16_tmb_fit()
  expect_error(
    suppressMessages(get_demand_comparisons(fit, param = "Q0", p_adjust = "holm")),
    regexp = "unused argument|p_adjust"
  )
})

# =============================================================================
# 8. compare_specs validation (Decision 8)
# =============================================================================
test_that("compare_specs naming a non-fitted factor errors", {
  skip_on_cran()
  fit <- .h16_tmb_fit()
  expect_error(
    suppressMessages(get_demand_comparisons(fit, param = "Q0", compare_specs = ~ nope)),
    regexp = "nope"
  )
})

# F3 (Codex post-commit review): the NLME backend must validate compare_specs at
# the boundary like TMB does, instead of letting a bogus formula fall through
# emmeans() into a silent degenerate result. Harmonized contract parity.
test_that("NLME compare_specs naming a non-fitted factor errors (contract parity)", {
  skip_on_cran()
  fit <- .h16_nlme_fit()
  expect_error(
    suppressMessages(get_demand_comparisons(fit, param = "Q0", compare_specs = ~ nope)),
    regexp = "nope"
  )
  # The abort lists the valid factor(s) so the user can correct the call.
  expect_error(
    suppressMessages(get_demand_comparisons(fit, param = "Q0", compare_specs = ~ nope)),
    regexp = "gender"
  )
})

# F3 follow-up (Codex re-review after the post-commit fixes): the boundary
# validator must accept BOTH the original factor name AND the per-parameter
# collapsed column under asymmetric collapse_levels (parity with TMB and with
# the prior NLME behavior via .get_actual_factors_for_param), and a cross-
# parameter alias (e.g. ~ age_group_alpha for param = "Q0") must abort in the
# per-param resolver instead of silently degenerating to an intercept-only EMM.
test_that("NLME compare_specs accepts original or per-param collapsed name; aborts on cross-param alias (F3 follow-up)", {
  skip_on_cran()
  fit <- .h16_nlme_collapse_fit()
  expect_setequal(fit$param_info$factors_Q0, "age_group_Q0")
  expect_setequal(fit$param_info$factors_alpha, "age_group_alpha")

  # (1) original name -> works (already did).
  r_orig <- suppressMessages(
    get_demand_comparisons(fit, param = "Q0", compare_specs = ~ age_group))
  expect_s3_class(r_orig, "beezdemand_comparison")
  expect_true(!is.null(r_orig$Q0$contrasts_log10) &&
              nrow(r_orig$Q0$contrasts_log10) >= 1L)

  # (2) explicit collapsed column -> must also work (regression: my F3 boundary
  # check rejected this; .get_actual_factors_for_param previously accepted it).
  r_col <- suppressMessages(
    get_demand_comparisons(fit, param = "Q0", compare_specs = ~ age_group_Q0))
  expect_s3_class(r_col, "beezdemand_comparison")
  expect_true(!is.null(r_col$Q0$contrasts_log10) &&
              nrow(r_col$Q0$contrasts_log10) >= 1L)

  # (3) cross-parameter alias for param = "Q0" -> abort. The abort must come
  # from the per-param resolver (mentions the param scope), not the boundary
  # union validation (which by design accepts the union and lets per-param
  # mismatches abort with a clearer message).
  expect_error(
    suppressMessages(
      get_demand_comparisons(fit, param = "Q0", compare_specs = ~ age_group_alpha)),
    regexp = "age_group_alpha"
  )
  expect_error(
    suppressMessages(
      get_demand_comparisons(fit, param = "Q0", compare_specs = ~ age_group_alpha)),
    regexp = "Q0"
  )

  # (4) genuinely-bogus name -> still aborts (unchanged from F3).
  expect_error(
    suppressMessages(
      get_demand_comparisons(fit, param = "Q0", compare_specs = ~ nope)),
    regexp = "nope"
  )
})

# F1 (Codex post-commit review): the marginalization grid is rebuilt with
# model.matrix(), which otherwise honors options("contrasts") at call time. The
# rebuilt basis must be pinned to the fitted design's contrasts so a changed
# global contrasts option cannot silently multiply the wrong basis by beta.
test_that("TMB EMMs/contrasts are invariant to options(contrasts) (F1)", {
  skip_on_cran()
  fit <- .h16_tmb_fit()

  e_def <- suppressMessages(get_demand_param_emms(fit, param = "Q0"))$estimate
  e_flip <- withr::with_options(
    list(contrasts = c("contr.sum", "contr.poly")),
    suppressMessages(get_demand_param_emms(fit, param = "Q0"))$estimate)
  expect_equal(e_flip, e_def, tolerance = 1e-10)

  c_def <- broom::tidy(suppressMessages(
    get_demand_comparisons(fit, param = "Q0", compare_specs = ~ gender)))$estimate
  c_flip <- withr::with_options(
    list(contrasts = c("contr.sum", "contr.poly")),
    broom::tidy(suppressMessages(
      get_demand_comparisons(fit, param = "Q0", compare_specs = ~ gender)))$estimate)
  expect_equal(c_flip, c_def, tolerance = 1e-10)
})

# F2 (Codex post-commit review): under asymmetric collapse_levels the fitted
# factor columns are renamed per parameter (age_group -> age_group_Q0 /
# age_group_alpha). compare_specs names the ORIGINAL factor (as on NLME); it
# must map to the per-parameter column, not silently intersect to nothing and
# return a grand-mean (0 contrasts).
test_that("TMB compare_specs maps the original factor under asymmetric collapse (F2)", {
  skip_on_cran()
  fit <- .h16_collapse_fit()
  # Sanity: the fixture is genuinely asymmetric.
  expect_setequal(fit$param_info$factors_q0, "age_group_Q0")
  expect_setequal(fit$param_info$factors_alpha, "age_group_alpha")

  # Q0: original name resolves to age_group_Q0 (junior/old) -> one contrast,
  # identical to passing the explicit collapsed column (was 0 rows pre-fix).
  o_q0 <- broom::tidy(suppressWarnings(suppressMessages(
    get_demand_comparisons(fit, param = "Q0", compare_specs = ~ age_group))))
  c_q0 <- broom::tidy(suppressWarnings(suppressMessages(
    get_demand_comparisons(fit, param = "Q0", compare_specs = ~ age_group_Q0))))
  expect_equal(nrow(o_q0), 1L)
  expect_equal(o_q0$estimate, c_q0$estimate)

  # alpha: original name resolves to age_group_alpha (young/mid/old) -> 3
  # pairwise contrasts.
  o_al <- broom::tidy(suppressWarnings(suppressMessages(
    get_demand_comparisons(fit, param = "alpha", compare_specs = ~ age_group))))
  expect_equal(nrow(o_al), 3L)
})

test_that("TMB compare_specs aborts on a cross-parameter collapsed name (F2 guard)", {
  skip_on_cran()
  fit <- .h16_collapse_fit()
  # age_group_alpha passes the union validation (it is a fitted alpha factor)
  # but is not in the Q0 design and has no Q0 alias -> must abort, not silently
  # marginalize the comparison away.
  expect_error(
    suppressWarnings(suppressMessages(
      get_demand_comparisons(fit, param = "Q0", compare_specs = ~ age_group_alpha))),
    regexp = "age_group_alpha"
  )
})

# =============================================================================
# 9. TMB marginalization, Decision 10 Option A (full-grid equal weights)
# =============================================================================
test_that("TMB marginalizes equal-weight over the full omitted-factor grid", {
  skip_on_cran()
  fit <- .h16_tmb_2f()
  skip_if_not(isTRUE(fit$converged), "TMB 2-factor fit did not converge")

  res <- suppressMessages(
    get_demand_comparisons(fit, param = "Q0", compare_specs = ~ gender))
  td <- broom::tidy(res)

  # gender has 2 observed levels -> exactly one pairwise contrast.
  expect_equal(nrow(td), 1L)

  oracle <- .h16_marginal_oracle(fit, retain = "gender", omit = "age_group")
  expect_equal(td$estimate[1], unname(oracle$full_log10), tolerance = 1e-8)
})

test_that("TMB full-grid marginalization differs from observed-only (Option A)", {
  skip_on_cran()
  fit <- .h16_tmb_2f_unbal()
  skip_if_not(isTRUE(fit$converged), "TMB unbalanced fit did not converge")

  res <- suppressMessages(
    get_demand_comparisons(fit, param = "Q0", compare_specs = ~ gender))
  td <- broom::tidy(res)

  oracle <- .h16_marginal_oracle(fit, retain = "gender", omit = "age_group")
  # Result tracks the FULL-grid average (Option A) ...
  expect_equal(td$estimate[1], unname(oracle$full_log10), tolerance = 1e-8)
  # ... and the unbalanced fixture makes full-grid != observed-only, so this
  # assertion genuinely discriminates Option A from the rejected variant.
  expect_gt(abs(unname(oracle$full_log10) - unname(oracle$obs_log10)), 1e-4)
})

# =============================================================================
# 10. nested native dialect + class (Decision 3/4)
# =============================================================================
test_that("nested list keeps each backend's native dialect", {
  skip_on_cran()
  res_nlme <- suppressMessages(get_demand_comparisons(
    .h16_nlme_fit(), param = "Q0", compare_specs = ~ gender))
  res_tmb <- suppressMessages(get_demand_comparisons(
    .h16_tmb_fit(), param = "Q0", compare_specs = ~ gender))

  expect_s3_class(res_nlme, "beezdemand_comparison")
  expect_s3_class(res_tmb, "beezdemand_comparison")

  expect_contains(names(res_nlme$Q0$contrasts_log10),
                  c("contrast_definition", "SE", "t.ratio"))
  expect_contains(names(res_tmb$Q0$contrasts_log10),
                  c("contrast", "std.error", "statistic"))

  # report_ratios default TRUE -> ratio block present on both
  expect_true(!is.null(res_nlme$Q0$contrasts_ratio))
  expect_true(!is.null(res_tmb$Q0$contrasts_ratio))

  res_noratio <- suppressMessages(get_demand_comparisons(
    .h16_tmb_fit(), param = "Q0", compare_specs = ~ gender,
    report_ratios = FALSE))
  expect_null(res_noratio$Q0$contrasts_ratio)
})

# =============================================================================
# 11. backend-agnostic enhanced print (Decision 9)
# =============================================================================
test_that("print.beezdemand_comparison is backend-agnostic and renders tables", {
  # Hand-built fixed objects keep the snapshot deterministic (no dependence on
  # TMB/NLME fit reproducibility across platforms/BLAS — spec risk 3).
  mk_tmb <- function() {
    cl <- tibble::tibble(
      contrast = "gender=Female - gender=Male",
      estimate = 0.123, std.error = 0.045, statistic = 2.1, df = Inf,
      conf.low = 0.035, conf.high = 0.211, p.value = 0.036
    )
    attr(cl, "std_labels") <- "Female - Male"
    obj <- list(Q0 = list(contrasts_log10 = cl))
    class(obj) <- "beezdemand_comparison"
    attr(obj, "backend") <- "tmb"
    attr(obj, "compare_specs_used") <- "~gender"
    attr(obj, "contrast_type_used") <- "pairwise"
    attr(obj, "contrast_by_used") <- "NULL"
    attr(obj, "adjustment_method") <- "holm"
    obj
  }
  mk_nlme <- function() {
    cl <- tibble::tibble(
      contrast_definition = "Female - Male",
      estimate = 0.123, SE = 0.045, df = 42, lower.CL = 0.03,
      upper.CL = 0.21, t.ratio = 2.7, p.value = 0.01
    )
    obj <- list(Q0 = list(contrasts_log10 = cl))
    class(obj) <- "beezdemand_comparison"
    attr(obj, "backend") <- "nlme"
    attr(obj, "compare_specs_used") <- "~gender"
    attr(obj, "contrast_type_used") <- "pairwise"
    attr(obj, "contrast_by_used") <- "NULL"
    attr(obj, "adjustment_method") <- "holm"
    obj
  }
  expect_snapshot(print(mk_tmb()))
  expect_snapshot(print(mk_nlme()))
})

# =============================================================================
# 12. contrast_by now works on TMB (TICKET-032 — was deferred)
# =============================================================================
test_that("contrast_by works on TMB (TICKET-032)", {
  skip_on_cran()
  fit <- .h16_tmb_2f()
  res <- suppressMessages(get_demand_comparisons(
    fit, param = "Q0",
    compare_specs = ~ gender * age_group, contrast_by = "age_group"
  ))
  expect_s3_class(res, "beezdemand_comparison")
  # by-column present (user-requested original name), inserted before contrast
  expect_true("age_group" %in% names(res$Q0$contrasts_log10))
  expect_identical(names(res$Q0$contrasts_log10)[1], "age_group")
  # one gender contrast per observed age_group level
  expect_setequal(
    unique(res$Q0$contrasts_log10$age_group),
    as.character(unique(.h16_two_factor_data()$age_group))
  )
})

# =============================================================================
# 13. statistic/df are backend-native (Decision 1 — documented divergence)
# =============================================================================
test_that("tidy() statistic/df reflect each backend's inference engine", {
  skip_on_cran()
  td_nlme <- broom::tidy(suppressMessages(get_demand_comparisons(
    .h16_nlme_fit(), param = "Q0", compare_specs = ~ gender)))
  td_tmb <- broom::tidy(suppressMessages(get_demand_comparisons(
    .h16_tmb_fit(), param = "Q0", compare_specs = ~ gender)))

  # NLME: finite df + t-statistic
  expect_true(all(is.finite(td_nlme$df)))
  # TMB: asymptotic z (df = Inf)
  expect_true(all(is.infinite(td_tmb$df)))

  expect_true(all(is.finite(td_nlme$conf.low)) &&
                all(is.finite(td_nlme$conf.high)))
  expect_true(all(td_nlme$conf.low <= td_nlme$conf.high))
  expect_true(all(td_tmb$conf.low <= td_tmb$conf.high))
})

# =============================================================================
# 14. NLME arg-guards + comparison attrs (Codex fold-in finding 1)
# =============================================================================
test_that("NLME errors on both-supplied / bogus param; attrs are set", {
  skip_on_cran()
  fit <- .h16_nlme_fit()

  expect_error(
    suppressMessages(get_demand_comparisons(
      fit, param = "Q0", params_to_compare = "Q0")),
    regexp = "only one|both|ambiguous"
  )
  expect_error(
    suppressMessages(get_demand_comparisons(fit, param = "bogus")),
    regexp = "bogus|should be one of|arg"
  )

  res_nlme <- suppressMessages(get_demand_comparisons(
    fit, param = "Q0", compare_specs = ~ gender))
  res_tmb <- suppressMessages(get_demand_comparisons(
    .h16_tmb_fit(), param = "Q0", compare_specs = ~ gender))

  expect_identical(attr(res_nlme, "backend"), "nlme")
  expect_identical(attr(res_tmb, "backend"), "tmb")
  expect_false(is.null(attr(res_nlme, "adjustment_method")))
  expect_false(is.null(attr(res_tmb, "adjustment_method")))
  expect_false(is.null(attr(res_nlme, "compare_specs_used")))
  expect_false(is.null(attr(res_tmb, "compare_specs_used")))
})

# =============================================================================
# 15. tidy() labels survive delimiter-containing factor levels (T3 / risk 5)
# =============================================================================
test_that("tidy() builds contrast labels from structured level values, not regex", {
  skip_on_cran()
  # Level values contain a space and a hyphen — a naive regex split of a native
  # "factor=level - factor=level" string would mangle these. The structured
  # builder must reproduce them verbatim (and match what emmeans would emit).
  dat <- .h16_sim_one_factor(level_order = c("low dose", "x-ray", "mid"))
  fit <- suppressWarnings(fit_demand_tmb(
    dat, equation = "exponential", factors = "grp", verbose = 0))
  skip_if_not(isTRUE(fit$converged), "TMB fit did not converge")

  td <- broom::tidy(suppressMessages(get_demand_comparisons(fit, param = "Q0")))
  expect_identical(
    td$contrast,
    c("low dose - x-ray", "low dose - mid", "x-ray - mid")
  )
})
