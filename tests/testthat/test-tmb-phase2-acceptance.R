# =============================================================================
# TICKET-011 Phase 2.6: equivalence + parity acceptance tests
#
# Two classes of assertions:
#
# 1. Backward-compatibility (bit-identical to Phase 1): intercept-only fits
#    on `apt` must produce the same loglik / coefs / subject_pars under the
#    new vectorized parameterization (logsigma, rho_raw) as the previous
#    scalar (logsigma_b, logsigma_c, rho_bc_raw) parameterization. The
#    LKJ-Cholesky construction reduces to tanh(rho_raw[0]) at d == 2, so
#    the objective surface is identical.
#
# 2. NLME parity on simulated within-subject data: TMB Laplace approximation
#    vs NLME's iterative algorithm should agree to within ~1% relative on
#    the loglik, with TMB converging with PD Hessian on all four Phase 2
#    target specs:
#      pdDiag(Q0+alpha~1), pdSymm(Q0+alpha~1),
#      pdDiag(Q0+alpha~condition), pdSymm(Q0+alpha~condition)
#
#    Note: Laplace vs exact-marginal LL approximations are NOT expected to
#    be bit-identical -- a few loglik units of gap is a structural
#    consequence of the two algorithms, not a bug.
# =============================================================================

# -----------------------------------------------------------------------------
# Backward-compat: Phase-2 fits intercept-only specs identically to Phase 1
# -----------------------------------------------------------------------------

test_that("Phase 2 backward-compat: c('q0','alpha') on apt matches Phase-1 baseline", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  # The Phase-1 baseline was captured before Phase 2.4 landed; values
  # are inlined here so the test is self-contained.
  phase1_loglik_2re <- -171.183300

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    apt, equation = "simplified",
    random_effects = c("q0", "alpha"),
    verbose = 0
  )))

  expect_equal(fit$loglik, phase1_loglik_2re, tolerance = 1e-4)
})

test_that("Phase 2 backward-compat: c('q0') on apt matches Phase-1 baseline", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  phase1_loglik_1re <- -236.390400

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    apt, equation = "simplified",
    random_effects = c("q0"),
    verbose = 0
  )))

  expect_equal(fit$loglik, phase1_loglik_1re, tolerance = 1e-4)
})

test_that("Phase 2 backward-compat: formula Q0+alpha~1 matches character c('q0','alpha')", {
  skip_on_cran()
  data(apt, package = "beezdemand")

  fit_chr <- suppressWarnings(suppressMessages(fit_demand_tmb(
    apt, equation = "simplified",
    random_effects = c("q0", "alpha"),
    verbose = 0
  )))
  fit_frm <- suppressMessages(fit_demand_tmb(
    apt, equation = "simplified",
    random_effects = Q0 + alpha ~ 1,
    verbose = 0
  ))

  # Identical objective + identical optimizer trajectory -> bit-identical
  # loglik. The two paths share the same parameter vector ordering after
  # the parser normalization.
  expect_equal(fit_chr$loglik, fit_frm$loglik, tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# Phase 2 acceptance: factor-expanded RE specs converge with PD Hessian
# -----------------------------------------------------------------------------

helper_within_subject_data <- function(seed, deltas = TRUE) {
  delta_q0 <- if (deltas) c(0, 0.3, -0.2) else NULL
  delta_alpha <- if (deltas) c(0, 0.1, -0.1) else NULL
  sim <- .simulate_within_subject_demand(
    n_subjects = 25, n_conditions = 3,
    delta_q0 = delta_q0, delta_alpha = delta_alpha,
    seed = seed
  )
  sim$y_ll4 <- ll4(sim$y)
  sim
}

test_that("Phase 2 acceptance: pdDiag(Q0+alpha~1) on within-subject data converges", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 201, deltas = FALSE)

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben",
    random_effects = nlme::pdDiag(Q0 + alpha ~ 1),
    verbose = 0
  )))
  expect_true(fit$converged)
  expect_true(fit$hessian_pd)
  expect_true(is.finite(fit$loglik))
})

test_that("Phase 2 acceptance: pdSymm(Q0+alpha~1) on within-subject data converges", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 202, deltas = FALSE)

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben",
    random_effects = nlme::pdSymm(Q0 + alpha ~ 1),
    verbose = 0
  )))
  expect_true(fit$converged)
  expect_true(fit$hessian_pd)
})

test_that("Phase 2 acceptance: pdDiag(Q0+alpha~condition) on within-subject data converges", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 203)

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", factors = "condition",
    random_effects = nlme::pdDiag(Q0 + alpha ~ condition),
    verbose = 0
  )))
  expect_true(fit$converged)
  expect_true(fit$hessian_pd)

  # rho_raw is empty for pdDiag (no off-diagonal correlations).
  expect_equal(
    length(fit$opt$par[names(fit$opt$par) == "rho_raw"]),
    0L
  )
  # logsigma has 6 entries: 3 q0 conditions + 3 alpha conditions.
  expect_equal(
    length(fit$opt$par[names(fit$opt$par) == "logsigma"]),
    6L
  )
})

test_that("Phase 2 acceptance: pdSymm(Q0+alpha~condition) on within-subject data converges", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 204)

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", factors = "condition",
    random_effects = nlme::pdSymm(Q0 + alpha ~ condition),
    verbose = 0
  )))
  expect_true(fit$converged)
  expect_true(fit$hessian_pd)

  # 6x6 pdSymm -> 15 free off-diagonal correlations.
  expect_equal(
    length(fit$opt$par[names(fit$opt$par) == "rho_raw"]),
    15L
  )
})

# -----------------------------------------------------------------------------
# NLME parity (loose tolerance per Laplace vs nlme()'s iterative algorithm)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Codex round 5: Phase 2 vector-parameter reporting regressions
# -----------------------------------------------------------------------------

test_that("Phase 2 SEs are populated for every element of vector parameters", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 203)

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", factors = "condition",
    random_effects = nlme::pdSymm(Q0 + alpha ~ condition),
    verbose = 0
  )))
  skip_if_not(isTRUE(fit$converged), "TMB fit did not converge")
  skip_if(is.null(fit$sdr), "sdreport unavailable")

  se_logsigma <- fit$model$se[names(fit$model$coefficients) == "logsigma"]
  se_rho_raw  <- fit$model$se[names(fit$model$coefficients) == "rho_raw"]

  # Codex round 5: only the first element used to get a non-NA SE because
  # the duplicated-name match in .tmb_extract_estimates() short-circuited
  # at idx[1]. Vector handling now mirrors beta_q0/beta_alpha.
  expect_equal(length(se_logsigma), 6L)        # 3 q0 + 3 alpha
  expect_equal(length(se_rho_raw), 15L)        # 6x6 -> 15 free off-diagonals
  expect_true(all(is.finite(se_logsigma)))
  expect_true(all(is.finite(se_rho_raw)))
})

test_that("tidy() classifies bare logsigma as variance component", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- suppressMessages(fit_demand_tmb(
    apt, equation = "simplified",
    random_effects = Q0 + alpha ~ 1,
    verbose = 0
  ))

  td <- broom::tidy(fit)
  ls_rows <- td[grepl("^logsigma", td$term) & td$term != "logsigma_e", ]
  # Codex round 5: bare `logsigma` rows used to land in the empty
  # component bucket because the regex required an underscore suffix.
  expect_true(nrow(ls_rows) >= 1L)
  expect_true(all(ls_rows$component == "variance"))
})

test_that("predict() rejects newdata missing RE-only RHS column (Codex round 6)", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 731)
  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben",  # condition NOT in factors
    random_effects = nlme::pdDiag(Q0 + alpha ~ condition),
    verbose = 0
  )))
  skip_if_not(isTRUE(fit$converged))

  # Prior to the fix this fell through to model.matrix() with
  # "object 'condition' not found".
  nd_missing <- sim[, c("id", "x", "y_ll4")]
  expect_error(
    predict(fit, newdata = nd_missing),
    regexp = "missing required column"
  )
})

test_that("predict() rejects newdata with NA in RE-only RHS (Codex round 6)", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 732)
  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben",
    random_effects = nlme::pdDiag(Q0 + alpha ~ condition),
    verbose = 0
  )))
  skip_if_not(isTRUE(fit$converged))

  # Prior to the fix an NA in `condition` flowed into model.matrix(),
  # producing a Z with fewer rows than newdata and a downstream
  # subscript-out-of-bounds error.
  nd_na <- sim
  nd_na$condition[1] <- NA
  expect_error(
    predict(fit, newdata = nd_na),
    regexp = "missing values in column"
  )
})

test_that("character RE-only RHS variables are coerced to factor at fit time (Codex round 6)", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 733)
  # Pass `condition` as a CHARACTER, not a factor. Prior to the fix this
  # was stored as character in fit$data, and predict() on a one-level
  # subset failed with "contrasts can be applied only to factors with
  # 2 or more levels".
  sim$condition <- as.character(sim$condition)
  expect_true(is.character(sim$condition))

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben",
    random_effects = nlme::pdDiag(Q0 + alpha ~ condition),
    verbose = 0
  )))
  # After fit-time coercion, fit$data$condition is a factor with the
  # full level set frozen for predict().
  expect_true(is.factor(fit$data$condition))
  expect_equal(sort(levels(fit$data$condition)), c("C1", "C2", "C3"))

  # Predict on a one-condition subset should now work cleanly.
  nd_subset <- sim[sim$condition == "C1", ]
  expect_no_error(p <- predict(fit, newdata = nd_subset))
  expect_true(is.data.frame(p))
  expect_true(".fitted" %in% names(p))
})

test_that("predict() uses full re_q0_mat / re_alpha_mat for factor-expanded fits", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 612)

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", factors = "condition",
    random_effects = nlme::pdDiag(Q0 + alpha ~ condition),
    verbose = 0
  )))
  skip_if_not(isTRUE(fit$converged), "TMB fit did not converge")

  # Predict on the training data; should reproduce the model's own
  # internal predictions (which use the full per-(subject, condition)
  # RE matrices). Codex round 5 reproduced max_abs_diff = 0.571 against
  # a hand-computed full-Z reconstruction; the prior code added only
  # subject_pars$b_i (first RE column) and silently dropped conditions
  # 2 and 3.
  pred <- predict(fit, type = "response")

  # Hand-compute the expected log_q0 / log_alpha per row using the
  # exposed re_q0_mat / re_alpha_mat attributes, then compare.
  spars <- fit$subject_pars
  re_q0_mat <- attr(spars, "re_q0_mat")
  re_alpha_mat <- attr(spars, "re_alpha_mat")
  re_parsed <- fit$param_info$random_effects_parsed
  z_train <- beezdemand:::.tmb_build_z_matrices(
    re_parsed, fit$data, id_var = "id"
  )

  coefs <- fit$model$coefficients
  beta_q0 <- unname(coefs[names(coefs) == "beta_q0"])
  beta_alpha <- unname(coefs[names(coefs) == "beta_alpha"])
  X_q0 <- fit$formula_details$X_q0
  X_alpha <- fit$formula_details$X_alpha

  subj_id_int <- as.integer(factor(
    fit$data$id, levels = fit$param_info$subject_levels
  ))
  expected_log_q0 <- as.numeric(X_q0 %*% beta_q0)
  expected_log_alpha <- as.numeric(X_alpha %*% beta_alpha)
  for (i in seq_along(expected_log_q0)) {
    expected_log_q0[i] <- expected_log_q0[i] +
      sum(z_train$Z_q0[i, ] * re_q0_mat[subj_id_int[i], ])
    expected_log_alpha[i] <- expected_log_alpha[i] +
      sum(z_train$Z_alpha[i, ] * re_alpha_mat[subj_id_int[i], ])
  }

  # Re-derive expected predictions on the model (ll4) scale via zben.
  # `predict(type='response')` returns a tibble with `.fitted` on the
  # model scale; we compare to the per-row reconstruction.
  Q0 <- exp(expected_log_q0)
  alpha_v <- exp(expected_log_alpha)
  Q0_log10 <- pmax(expected_log_q0 / log(10), 1e-3)
  rate <- (alpha_v / Q0_log10) * Q0
  expected_pred <- Q0_log10 * exp(-rate * fit$data$x)

  expect_equal(pred$.fitted, expected_pred, tolerance = 1e-8)
})

test_that("complete-case filter drops rows with NA in RE-only RHS variables", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 511)
  # Inject an NA into `condition` for a single row. condition is in the
  # RE formula RHS but NOT in `factors` -- the prior complete-case
  # filter ignored it, so .tmb_build_z_matrices() would silently drop
  # the row from Z while prepared$y kept it. Mismatched arrays.
  sim$condition[5] <- NA
  expect_true(any(is.na(sim$condition)))

  # Codex round 5 reproduction: this used to either hand TMB
  # mismatched arrays (cryptic crash) or silently fit the wrong model.
  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben",  # condition NOT in factors
    random_effects = nlme::pdDiag(Q0 + alpha ~ condition),
    verbose = 0
  )))
  expect_s3_class(fit, "beezdemand_tmb")
  # The NA row was dropped before TMB saw the data; n_obs reflects
  # the cleaned subset.
  expect_equal(fit$param_info$n_obs, nrow(sim) - 1L)
})

test_that("pdSymm correlation reporting uses marginal (not partial) correlations for d > 2", {
  skip_on_cran()
  sim <- helper_within_subject_data(seed = 304)

  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    sim, y_var = "y_ll4", x_var = "x", id_var = "id",
    equation = "zben", factors = "condition",
    random_effects = nlme::pdSymm(Q0 + alpha ~ condition),
    verbose = 0
  )))
  skip_if_not(isTRUE(fit$converged), "TMB fit did not converge")

  vc <- beezdemand:::.tmb_format_variance_components(fit)
  expect_false(is.null(vc$correlations))
  expect_equal(nrow(vc$correlations), 15L)  # 6x6 -> 15 off-diagonals

  # Reconstruct the marginal correlation matrix R_corr = L_corr * t(L_corr)
  # from rho_raw and assert the reported values match it -- NOT the raw
  # tanh(rho_raw) partials. Codex round 5 caught the prior reporting
  # using partials directly, which is a silent wrong answer for d > 2.
  coefs <- fit$model$coefficients
  rho_raw <- unname(coefs[names(coefs) == "rho_raw"])
  expect_equal(length(rho_raw), 15L)

  d <- 6L
  L_corr <- matrix(0, d, d)
  L_corr[1, 1] <- 1
  idx <- 0L
  for (j in 2L:d) {
    sum_sq <- 0
    for (k in seq_len(j - 1L)) {
      idx <- idx + 1L
      r <- tanh(rho_raw[idx])
      if (k == 1L) L_corr[j, k] <- r
      else L_corr[j, k] <- r * sqrt(max(0, 1 - sum_sq))
      sum_sq <- sum_sq + L_corr[j, k]^2
    }
    L_corr[j, j] <- sqrt(max(0, 1 - sum_sq))
  }
  R_corr <- L_corr %*% t(L_corr)

  reported_idx <- 0L
  for (j in 2L:d) {
    for (k in seq_len(j - 1L)) {
      reported_idx <- reported_idx + 1L
      expected <- R_corr[j, k]
      reported <- vc$correlations$Estimate[reported_idx]
      expect_equal(reported, expected, tolerance = 1e-12,
                   label = sprintf("rho[%d,%d]", j, k))
    }
  }
})

test_that("pdSymm d=2 still reports tanh(rho_raw[0]) as rho_bc (backward compat)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- suppressMessages(fit_demand_tmb(
    apt, equation = "simplified",
    random_effects = nlme::pdSymm(Q0 + alpha ~ 1),
    verbose = 0
  ))

  vc <- beezdemand:::.tmb_format_variance_components(fit)
  expect_equal(nrow(vc$correlations), 1L)
  expect_match(vc$correlations$Component, "rho_bc")

  # For d == 2 the single partial correlation equals the marginal,
  # so the reported value must equal tanh(rho_raw[0]).
  rho_raw <- unname(fit$model$coefficients[names(fit$model$coefficients) == "rho_raw"])
  expect_equal(vc$correlations$Estimate, tanh(rho_raw[1]),
               tolerance = 1e-12)
})

test_that("Phase 2 NLME parity: TMB and NLME loglik agree within ~1% across all four specs", {
  skip_on_cran()
  # Loose tolerance: Laplace approximation in TMB and the iterative
  # algorithm in nlme() converge to slightly different optima even on
  # the same data + same Sigma parameterization. Empirical observation
  # (see docs/Phase 2 commit message): diff is ~0.3-3.3 loglik units
  # across the four target specs. Pin a 5-unit absolute tolerance.
  ll_tol <- 5.0

  parity_diff <- function(re_spec, with_factor, deltas, seed) {
    sim <- helper_within_subject_data(seed = seed, deltas = deltas)
    factors <- if (with_factor) "condition" else NULL

    nlme_fit <- suppressMessages(suppressWarnings(fit_demand_mixed(
      data = sim, y_var = "y_ll4", x_var = "x", id_var = "id",
      equation_form = "zben", factors = factors,
      random_effects = re_spec, verbose = FALSE
    )))
    tmb_fit <- suppressMessages(suppressWarnings(fit_demand_tmb(
      data = sim, y_var = "y_ll4", x_var = "x", id_var = "id",
      equation = "zben", factors = factors,
      random_effects = re_spec, verbose = 0
    )))
    abs(as.numeric(stats::logLik(nlme_fit$model)) - tmb_fit$loglik)
  }

  expect_lt(
    parity_diff(nlme::pdDiag(Q0+alpha~1), FALSE, FALSE, 201),
    ll_tol
  )
  expect_lt(
    parity_diff(nlme::pdSymm(Q0+alpha~1), FALSE, FALSE, 202),
    ll_tol
  )
  expect_lt(
    parity_diff(nlme::pdDiag(Q0+alpha~condition), TRUE, TRUE, 203),
    ll_tol
  )
  expect_lt(
    parity_diff(nlme::pdSymm(Q0+alpha~condition), TRUE, TRUE, 204),
    ll_tol
  )
})
