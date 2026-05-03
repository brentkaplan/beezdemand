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
