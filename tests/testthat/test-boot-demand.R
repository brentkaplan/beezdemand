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
  r_kest <- boot_demand(fit_kest, statistics = c("Pmax", "Omax", "EV"), R = 200, seed = 1)
  r_kfix <- boot_demand(fit_kfix, statistics = c("Pmax", "Omax", "EV"), R = 200, seed = 1)

  expect_true(all(is.finite(r_kest$conf.low)))
  expect_true(all(is.finite(r_kest$conf.high)))
  expect_true(all(is.finite(r_kfix$conf.low)))
  expect_true(all(is.finite(r_kfix$conf.high)))

  # EV pinned INDEPENDENTLY of the production helpers: k-fixed uses the k that
  # was supplied (2), k-estimated uses exp(log_k) straight from the coefficient
  # vector; alpha is exp(beta_alpha) (intercept-only fits). Point estimates AND
  # the empirical-quantile bounds are recomputed from the same parametric draws
  # so the draw-level k_draw^1.5 term is checked, not just the point value.
  ev_of <- function(alpha, k) 1 / (100 * alpha * k^1.5)
  a_kfix <- exp(fit_kfix$model$coefficients[["beta_alpha"]])
  expect_equal(r_kfix$estimate[r_kfix$statistic == "EV"], ev_of(a_kfix, 2),
               tolerance = 1e-8)
  a_kest <- exp(fit_kest$model$coefficients[["beta_alpha"]])
  k_kest <- exp(fit_kest$model$coefficients[["log_k"]])
  expect_equal(r_kest$estimate[r_kest$statistic == "EV"], ev_of(a_kest, k_kest),
               tolerance = 1e-8)

  d_kfix <- beezdemand:::.tmb_parametric_draws(fit_kfix, R = 200, seed = 1)
  ev_draws_kfix <- ev_of(exp(d_kfix[, "beta_alpha"]), 2)
  q_kfix <- stats::quantile(ev_draws_kfix, c(0.025, 0.975), names = FALSE)
  expect_equal(r_kfix$conf.low[r_kfix$statistic == "EV"], q_kfix[1], tolerance = 1e-8)
  expect_equal(r_kfix$conf.high[r_kfix$statistic == "EV"], q_kfix[2], tolerance = 1e-8)

  d_kest <- beezdemand:::.tmb_parametric_draws(fit_kest, R = 200, seed = 1)
  ev_draws_kest <- ev_of(exp(d_kest[, "beta_alpha"]), exp(d_kest[, "log_k"]))
  q_kest <- stats::quantile(ev_draws_kest, c(0.025, 0.975), names = FALSE)
  expect_equal(r_kest$conf.low[r_kest$statistic == "EV"], q_kest[1], tolerance = 1e-8)
  expect_equal(r_kest$conf.high[r_kest$statistic == "EV"], q_kest[2], tolerance = 1e-8)
  # k-estimated and k-fixed must NOT coincide (they use different k)
  expect_false(isTRUE(all.equal(
    r_kest$estimate[r_kest$statistic == "EV"], r_kfix$estimate[r_kfix$statistic == "EV"]
  )))
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

# --- 13. zben Pmax/Omax route through the numerical engine (#19) -------------

test_that("boot_demand computes finite zben Pmax CIs via the numerical path, not SND", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit_zben <- fit_demand_tmb(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", verbose = 0
  )

  res <- boot_demand(fit_zben, statistics = c("Pmax", "Omax"), R = 200, seed = 7)

  pmax_row <- res[res$statistic == "Pmax", ]
  omax_row <- res[res$statistic == "Omax", ]
  expect_true(is.finite(pmax_row$estimate))
  expect_true(is.finite(pmax_row$conf.low))
  expect_true(is.finite(pmax_row$conf.high))
  expect_true(is.finite(omax_row$estimate))

  # Regression guard: the pre-fix bug's Pmax point estimate for a k-free fit
  # was the SND closed form 1/(alpha*Q0). Confirm the returned zben value is
  # NOT that.
  alpha_pt <- exp(fit_zben$model$coefficients[["beta_alpha"]])
  q0_pt <- exp(fit_zben$model$coefficients[["beta_q0"]])
  snd_pmax <- 1 / (alpha_pt * q0_pt)
  expect_false(isTRUE(all.equal(pmax_row$estimate, unname(snd_pmax), tolerance = 1e-2)))
})

test_that("boot_demand zben Pmax bootstrap draws match an independent per-draw computation (#19 Codex review Recommended #3)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit_zben <- fit_demand_tmb(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", verbose = 0
  )

  R <- 300L
  seed <- 11L

  res <- boot_demand(fit_zben, statistics = "Pmax", R = R, seed = seed)
  pmax_row <- res[res$statistic == "Pmax", ]

  # Recompute the same draws boot_demand() used internally: same fit, R,
  # and seed give bit-identical draws from .tmb_parametric_draws() (it
  # reseeds internally via set.seed(seed) and restores the caller's RNG
  # state on exit, so calling it again here is fully reproducible).
  draws <- beezdemand:::.tmb_parametric_draws(fit_zben, R = R, seed = seed)
  bq0 <- draws[, colnames(draws) == "beta_q0", drop = FALSE]
  ba  <- draws[, colnames(draws) == "beta_alpha", drop = FALSE]
  # Intercept-only fit: the cell design column is a constant 1, matching
  # .boot_demand_cells()'s intercept-only path.
  q0_draws <- exp(as.numeric(bq0[, 1]))
  alpha_draws <- exp(as.numeric(ba[, 1]))

  # Independent per-draw expenditure maximization (not calling
  # beezdemand_calc_pmax_omax()/_vec() or any package Pmax helper): the
  # same grid-then-refine ground-truth finder used by the other GH #19
  # tests, applied to every draw.
  pmax_draws_indep <- vapply(seq_len(R), function(j) {
    .zben_truth(q0_draws[j], alpha_draws[j], upper = 2000)$maximum
  }, numeric(1))

  expected_ci <- stats::quantile(pmax_draws_indep, probs = c(0.025, 0.975),
                                 names = FALSE)
  expect_equal(c(pmax_row$conf.low, pmax_row$conf.high), expected_ci,
              tolerance = 1e-2)
  expect_equal(
    pmax_row$estimate,
    .zben_truth(exp(fit_zben$model$coefficients[["beta_q0"]]),
               exp(fit_zben$model$coefficients[["beta_alpha"]]),
               upper = 2000)$maximum,
    tolerance = 1e-2
  )

  # Regression guard: the draws must not follow the SND closed form
  # Pmax = 1 / (alpha * Q0) -- neither pointwise nor in aggregate.
  snd_draws <- 1 / (alpha_draws * q0_draws)
  snd_ci <- stats::quantile(snd_draws, probs = c(0.025, 0.975), names = FALSE)
  expect_false(isTRUE(all.equal(expected_ci, snd_ci, tolerance = 1e-2)))
  expect_gt(mean(abs(pmax_draws_indep - snd_draws) / pmax_draws_indep), 0.05)
})

test_that("boot_demand warns naming the count of zben draws that hit the Pmax expansion cap", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  apt$y_ll4 <- ll4(apt$y)
  fit_zben <- fit_demand_tmb(
    apt, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", verbose = 0
  )

  # Mock the per-draw engine call so a controlled fraction of "draws"
  # report is_boundary_model = TRUE (the domain-expansion cap was hit),
  # without needing to engineer a real fit whose bootstrap draws happen to
  # land in that regime. The point-estimate call (beezdemand_calc_pmax_omax,
  # singular) is untouched, so `estimate` still reflects the real fit.
  testthat::local_mocked_bindings(
    beezdemand_calc_pmax_omax_vec = function(params_df, model_type,
                                             param_scales = NULL,
                                             price_list = NULL, ...) {
      n <- nrow(params_df)
      data.frame(
        pmax_model = rep(50, n), omax_model = rep(10, n),
        q_at_pmax_model = rep(1, n),
        method_model = rep("numerical_optimize_expanded", n),
        is_boundary_model = rep(c(TRUE, FALSE, FALSE), length.out = n),
        elasticity_at_pmax_model = rep(-1, n),
        unit_elasticity_pass_model = rep(TRUE, n),
        pmax_unconditional = NA_real_, omax_unconditional = NA_real_,
        q_at_pmax_unconditional = NA_real_, p_zero_at_pmax = NA_real_,
        method_unconditional = NA_character_, is_boundary_unconditional = NA,
        pmax_obs = NA_real_, omax_obs = NA_real_,
        has_duplicate_prices = NA, n_max_ties = NA_integer_,
        stringsAsFactors = FALSE
      )
    },
    .package = "beezdemand"
  )

  expect_warning(
    boot_demand(fit_zben, statistics = "Pmax", R = 300, seed = 5),
    "boundary|expansion cap|domain"
  )
})


# --- TICKET-063: hessian_pd gate on boot_demand -----------------------------

test_that("boot_demand warns exactly once when hessian_pd is FALSE (dedup through draws->vcov)", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .weak_pd_tmb_fit()
  skip_if(!isFALSE(fit$hessian_pd),
          "platform numerics did not produce a non-PD Hessian")
  skip_if(!all(is.finite(suppressWarnings(vcov(fit)))),
          "weak fixture's covariance is non-finite on this platform (draws unavailable by design)")

  conds <- .capture_warning_conditions(
    res <- boot_demand(fit, statistics = "Pmax", R = 100, seed = 1)
  )
  expect_identical(.n_hessian_pd_warnings(conds), 1L)
})
