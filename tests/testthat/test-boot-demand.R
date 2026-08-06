# =============================================================================
# Tests for boot_demand() — TICKET-024
# Parametric bootstrap CIs on derived demand metrics (Pmax/Omax/Qmax/EV/
# elasticity_at_pmax). v1: TMB parametric only; per-condition rows; harmonized
# columns (statistic, condition, estimate, conf.low, conf.high, level).
# Spec: internal_docs/tickets/TICKET-024-REFINED-2026-05-22.md
# =============================================================================

# --- fixtures (memoized so each model is fit at most once per file run) ------

.bd_cache <- new.env(parent = emptyenv())

.bd_gender_data <- function(n_per_group = 20) {
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ids_g <- unique(d$id[d$gender == g])
    head(ids_g[order(ids_g)], n_per_group)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  d
}

.bd_gender_fit <- function() {
  if (is.null(.bd_cache$gender)) {
    .bd_cache$gender <- suppressWarnings(fit_demand_tmb(
      .bd_gender_data(),
      equation = "exponential", factors = "gender", verbose = 0
    ))
  }
  .bd_cache$gender
}

.bd_int_fit <- function() {
  if (is.null(.bd_cache$int)) {
    data(apt, package = "beezdemand")
    .bd_cache$int <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  }
  .bd_cache$int
}

# --- 1. schema ---------------------------------------------------------------

test_that("boot_demand returns the harmonized schema with finite, ordered intervals", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .bd_gender_fit()
  res <- boot_demand(fit, statistics = c("Pmax", "EV"), R = 200, seed = 42)

  expect_s3_class(res, "tbl_df")
  expect_named(
    res,
    c("statistic", "condition", "estimate", "conf.low", "conf.high", "level"),
    ignore.order = TRUE
  )
  expect_true(all(c("Pmax", "EV") %in% res$statistic))
  # Required 3: percentile intervals of a nonlinear transform need not bracket
  # the point estimate. Assert ordering + finiteness, not low <= est <= high.
  expect_true(all(res$conf.low <= res$conf.high))
  expect_true(all(is.finite(res$estimate)))
  expect_true(all(is.finite(res$conf.low)))
  expect_true(all(is.finite(res$conf.high)))
  # Per-row excluded-draw count is exposed for transparency (no noisy warning).
  expect_true("n_nonfinite" %in% names(attributes(res)))
  expect_length(attr(res, "n_nonfinite"), nrow(res))
})

# --- 2. per-condition rows ---------------------------------------------------

test_that("boot_demand emits one row per factor level (per-condition)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .bd_gender_fit()
  res <- boot_demand(fit, statistics = "Pmax", R = 200, seed = 42)

  lvls <- levels(droplevels(as.factor(.bd_gender_data()$gender)))
  expect_equal(nrow(res), length(lvls))
  expect_true(all(!is.na(res$condition)))
  expect_setequal(res$condition, paste0("gender=", lvls))
})

# --- 3. no-factor -> condition NA --------------------------------------------

test_that("boot_demand on an intercept-only fit returns one row per statistic, condition NA", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .bd_int_fit()
  res <- boot_demand(fit, statistics = c("Pmax", "Omax", "EV"), R = 200, seed = 42)

  expect_equal(nrow(res), 3L)
  expect_true(all(is.na(res$condition)))
  expect_setequal(res$statistic, c("Pmax", "Omax", "EV"))
})

# --- 4. point-estimate consistency with calc_group_metrics(at = cell) --------

test_that("boot_demand per-cell point estimate reproduces calc_group_metrics(at = cell)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .bd_gender_fit()
  res <- boot_demand(fit, statistics = c("Pmax", "Omax"), R = 200, seed = 42)

  lvls <- levels(droplevels(as.factor(.bd_gender_data()$gender)))
  for (g in lvls) {
    cgm <- calc_group_metrics(fit, at = list(gender = g))
    cond <- paste0("gender=", g)
    est_pmax <- res$estimate[res$statistic == "Pmax" & res$condition == cond]
    est_omax <- res$estimate[res$statistic == "Omax" & res$condition == cond]
    expect_equal(est_pmax, cgm$Pmax, tolerance = 1e-6)
    expect_equal(est_omax, cgm$Omax, tolerance = 1e-6)
  }
})

# --- 5. reproducibility ------------------------------------------------------

test_that("boot_demand is reproducible with seed; differs without", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .bd_int_fit()
  r1 <- boot_demand(fit, statistics = "Pmax", R = 200, seed = 42)
  r2 <- boot_demand(fit, statistics = "Pmax", R = 200, seed = 42)
  expect_identical(r1, r2)
  r3 <- boot_demand(fit, statistics = "Pmax", R = 200, seed = 7)
  expect_false(isTRUE(all.equal(r1$conf.low, r3$conf.low)))
})

# --- 6. EV canonical name + formula (intercept-only -> unambiguous alpha) -----
# EV mirrors analyze.R's two conventions: k-bearing forms use the literature
# (Hursh & Silberberg) 1/(100*alpha*k^1.5); k-free SND/"simplified" forms use
# analyze.R's own "simplified"-branch formula, 1/alpha (no k, no /100). The
# historical bug computed 1/(100*alpha) everywhere -- wrong for both cases.

test_that("boot_demand EV uses 1/(100*alpha*k^1.5) for k-bearing fits and rejects 'essential_value'", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .bd_int_fit()
  res <- boot_demand(fit, statistics = "EV", R = 200, seed = 42)

  alpha_pt <- exp(fit$model$coefficients[["beta_alpha"]])
  k_pt <- beezdemand:::.tmb_get_k(fit)
  expect_equal(
    res$estimate[res$statistic == "EV"], 1 / (100 * alpha_pt * (k_pt^1.5)),
    tolerance = 1e-6
  )
  # Guard against the historical dropped-k bug: 1/(100*alpha) (no k) must NOT
  # be what's produced now that k is available.
  expect_false(isTRUE(all.equal(
    res$estimate[res$statistic == "EV"], 1 / (100 * alpha_pt)
  )))
  expect_error(
    boot_demand(fit, statistics = "essential_value", R = 200),
    "should be one of"
  )
})

test_that("boot_demand EV uses 1/alpha (no k, no /100) for the k-free SND ('simplified') form", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit_snd <- fit_demand_tmb(apt, equation = "simplified", verbose = 0)
  res <- boot_demand(fit_snd, statistics = "EV", R = 200, seed = 42)

  alpha_pt <- exp(fit_snd$model$coefficients[["beta_alpha"]])
  expect_equal(
    res$estimate[res$statistic == "EV"], 1 / alpha_pt,
    tolerance = 1e-6
  )
  # Guard against the historical bug: a spurious /100 must not appear for the
  # k-free SND form.
  expect_false(isTRUE(all.equal(
    res$estimate[res$statistic == "EV"], 1 / (100 * alpha_pt)
  )))
})

# --- 7. validation -----------------------------------------------------------

test_that("boot_demand validates R and statistics", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .bd_int_fit()
  expect_error(boot_demand(fit, statistics = "Pmax", R = 50), "R")
  # Non-integer R must be rejected, not silently floored (Codex-probe finding).
  expect_error(boot_demand(fit, statistics = "Pmax", R = 150.5), "R")
  # Unknown args (not partial-matchable to a formal) must error, not be silently
  # ignored. `level` is a plausible confint-style slip for `ci_level`. (Note
  # `statistic` would partial-match `statistics`, so it is NOT a dots case.)
  expect_error(boot_demand(fit, statistics = "Pmax", level = 0.9, R = 200), "empty")
  expect_error(
    boot_demand(fit, statistics = "breakpoint", R = 200),
    "should be one of"
  )
})

# --- 12. internal CI helper: non-finite draw handling ------------------------

test_that(".boot_demand_ci excludes non-finite draws, counts them, aborts if all fail", {
  probs <- c(0.025, 0.975)

  ci <- beezdemand:::.boot_demand_ci(as.numeric(1:20), probs, "Pmax", NA_character_)
  expect_identical(ci$n_failed, 0L)
  expect_true(is.finite(ci$conf.low) && is.finite(ci$conf.high))

  # Partial non-finite draws are excluded and counted; CI from finite draws only.
  ci2 <- beezdemand:::.boot_demand_ci(
    c(1, 2, NA, Inf, 3, 4, NaN, 5), probs, "Omax", "gender=Male"
  )
  expect_identical(ci2$n_failed, 3L)
  expect_true(is.finite(ci2$conf.low) && is.finite(ci2$conf.high))

  # All non-finite -> abort (a CI cannot be formed).
  expect_error(
    beezdemand:::.boot_demand_ci(c(NA, NaN, Inf, -Inf), probs, "Pmax", NA_character_),
    "non-finite"
  )
})

# --- 8. NLME (non-TMB) unsupported in v1 -------------------------------------

test_that("boot_demand errors helpfully on a non-TMB (NLME) fit", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  apt_full$y_ll4 <- ll4(apt_full$y, lambda = 4)
  fit_nlme <- fit_demand_mixed(
    apt_full, equation_form = "zben", factors = "gender",
    y_var = "y_ll4", x_var = "x", id_var = "id"
  )
  expect_error(
    boot_demand(fit_nlme, statistics = "Pmax"),
    "beezdemand_tmb|NLME|supported"
  )
})

# --- 9. k-fixed and k-estimated ----------------------------------------------

test_that("boot_demand runs on k-fixed and k-estimated fits", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit_kest <- fit_demand_tmb(
    apt, equation = "exponential", estimate_k = TRUE, verbose = 0
  )
  fit_kfix <- fit_demand_tmb(
    apt, equation = "exponential", estimate_k = FALSE, k = 2, verbose = 0
  )
  r_kest <- boot_demand(fit_kest, statistics = c("Pmax", "Omax"), R = 200, seed = 1)
  r_kfix <- boot_demand(fit_kfix, statistics = c("Pmax", "Omax"), R = 200, seed = 1)

  expect_true(all(is.finite(r_kest$conf.low)))
  expect_true(all(is.finite(r_kest$conf.high)))
  expect_true(all(is.finite(r_kfix$conf.low)))
  expect_true(all(is.finite(r_kfix$conf.high)))
})

# --- 10. at = filters to a single condition ----------------------------------

test_that("boot_demand at= filters to a single condition matching calc_group_metrics", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .bd_gender_fit()
  g <- levels(droplevels(as.factor(.bd_gender_data()$gender)))[1]
  res <- boot_demand(
    fit, statistics = "Pmax", at = list(gender = g), R = 200, seed = 42
  )

  expect_equal(nrow(res), 1L)
  expect_equal(res$condition, paste0("gender=", g))
  cgm <- calc_group_metrics(fit, at = list(gender = g))
  expect_equal(res$estimate, cgm$Pmax, tolerance = 1e-6)
})

# --- 11. collapse_levels (divergent Q0/alpha factors) guarded ----------------

test_that("boot_demand errors on collapse_levels (divergent Q0/alpha factors) in v1", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  d$age_group <- factor(cut(
    d$age, c(0, 25, 35, Inf), labels = c("young", "mid", "old")
  ))
  d$id <- droplevels(as.factor(d$id))
  fit <- suppressWarnings(fit_demand_tmb(
    d, equation = "exponential", factors = "age_group",
    collapse_levels = list(
      Q0    = list(age_group = list(junior = c("young", "mid"), old = "old")),
      alpha = list(age_group = list(young = "young", mid = "mid", old = "old"))
    ),
    verbose = 0
  ))
  expect_error(
    boot_demand(fit, statistics = "Pmax", R = 200),
    "collapse_levels|not support|divergent"
  )
})
