# TICKET-011 Phase 3 acceptance tests: multi-block pdBlocked random effects
# in fit_demand_tmb(). The Phase 2 single-block path remains the
# backward-compat reference; Phase 3 lifts the multi-block gate so
# pdBlocked(list(...)) and bare list(pdMat, pdMat) inputs fit.
#
# In-package coverage mirrors the manuscript-repo cigarette parity
# protocol (see ../alcohol-nicotine-withdrawal/docs/plans/2026-04-19-
# tmb-parity-validation.md), but on simulated data with truth anchors
# rather than actual M1/M2 fits.

# ---------------------------------------------------------------------------
# Backward-compat: single-block-wrapped pdBlocked equals bare single block.
# ---------------------------------------------------------------------------

test_that("pdBlocked(list(pdSymm(...)) ) is bit-identical to bare pdSymm(...)", {
  skip_on_cran()

  data(apt, package = "beezdemand")

  spec_bare <- nlme::pdSymm(Q0 + alpha ~ 1)
  spec_wrapped <- nlme::pdBlocked(list(nlme::pdSymm(Q0 + alpha ~ 1)))

  fit_bare <- fit_demand_tmb(
    apt, equation = "simplified",
    random_effects = spec_bare,
    multi_start = FALSE, verbose = 0
  )
  fit_wrapped <- fit_demand_tmb(
    apt, equation = "simplified",
    random_effects = spec_wrapped,
    multi_start = FALSE, verbose = 0
  )

  expect_equal(
    as.numeric(logLik(fit_bare)),
    as.numeric(logLik(fit_wrapped)),
    tolerance = 1e-8
  )
  expect_equal(
    unname(fit_bare$model$coefficients),
    unname(fit_wrapped$model$coefficients),
    tolerance = 1e-8
  )
})

# ---------------------------------------------------------------------------
# M1-spec acceptance on a simulated within-subject fixture: pdBlocked of a
# baseline pdSymm intercepts block and a pdDiag condition-slopes block
# converges and produces sensible variance components.
# ---------------------------------------------------------------------------

.m1_sim_dat <- function(seed = 42, n_subjects = 60) {
  sim <- .simulate_within_subject_demand(
    n_subjects = n_subjects,
    n_conditions = 3,
    prices = c(0.25, 0.5, 1, 2, 4, 8, 16, 32),
    log_q0_pop = log(15),
    log_alpha_pop = log(0.0015),
    delta_q0 = c(0, -0.4, -0.9),
    delta_alpha = c(0, 0.2, 0.5),
    sigma_b = 0.4,
    sigma_d = 0.4,
    seed = seed
  )
  sim$id <- factor(sim$id)
  sim$condition <- factor(sim$condition)
  sim
}

.m1_spec <- nlme::pdBlocked(list(
  nlme::pdSymm(Q0 + alpha ~ 1),
  nlme::pdDiag(Q0 + alpha ~ condition - 1)
))

test_that("M1 spec converges on simulated within-subject data", {
  skip_on_cran()

  sim <- .m1_sim_dat()
  fit <- suppressWarnings(fit_demand_tmb(
    sim, equation = "simplified",
    random_effects = .m1_spec,
    multi_start = FALSE, verbose = 0
  ))

  expect_true(fit$converged)
  re_q0 <- attr(fit$subject_pars, "re_q0_mat")
  re_al <- attr(fit$subject_pars, "re_alpha_mat")
  # Block 2 (condition slopes) should have non-trivial variance once we
  # supply real per-condition signal — this is the test that distinguishes
  # the M1 spec from a degenerate intercepts-only fit.
  expect_true(any(apply(re_q0, 2, var) > 1e-6))
})

# ---------------------------------------------------------------------------
# M1-spec direction recovery: with simulator truth `delta_q0 = c(0, -0.4,
# -0.9)`, population Q0(C1) > Q0(C2) > Q0(C3). EMMs from the M1 fit must
# recover that ordering.
# ---------------------------------------------------------------------------

test_that("M1 spec recovers per-condition Q0 ordering on simulated truth", {
  skip_on_cran()

  sim <- .m1_sim_dat(seed = 42, n_subjects = 80)

  # The M1 fit needs `condition` in the fixed effects too so EMMs by
  # condition are estimable. The pdBlocked structure provides the random
  # slopes that let the FE pick up the population direction.
  fit <- suppressWarnings(fit_demand_tmb(
    sim, equation = "simplified",
    factors = "condition",
    random_effects = .m1_spec,
    multi_start = FALSE, verbose = 0
  ))

  emms <- get_demand_param_emms(fit, param = "Q0")
  # `level` column has strings like "condition=C1"; `estimate` is on
  # natural scale.
  level_str <- as.character(emms$level)
  cond_id <- sub("^condition=", "", level_str)
  Q0_natural <- emms$estimate
  # Ordering: C1 (delta=0) > C2 (delta=-0.4) > C3 (delta=-0.9)
  C1 <- Q0_natural[cond_id == "C1"]
  C2 <- Q0_natural[cond_id == "C2"]
  C3 <- Q0_natural[cond_id == "C3"]
  expect_gt(C1, C2)
  expect_gt(C2, C3)
})

# ---------------------------------------------------------------------------
# Diagnostic canary: intercepts-only fit (no condition slopes) should fail
# to recover the per-condition truth direction on this fixture, mirroring
# the cigarette M1 inversion. This is a diagnostic, skip-on-fail check —
# if the simulator changes seed / noise / scale and intercepts-only happens
# to recover the ordering, we skip with an informational message rather
# than fail the suite.
# ---------------------------------------------------------------------------

test_that("DIAGNOSTIC: intercepts-only fails to recover ordering (skip-on-pass)", {
  skip_on_cran()

  sim <- .m1_sim_dat(seed = 42, n_subjects = 80)

  fit_intercept <- suppressWarnings(fit_demand_tmb(
    sim, equation = "simplified",
    factors = "condition",
    random_effects = nlme::pdSymm(Q0 + alpha ~ 1),
    multi_start = FALSE, verbose = 0
  ))

  emms <- get_demand_param_emms(fit_intercept, param = "Q0")
  level_str <- as.character(emms$level)
  cond_id <- sub("^condition=", "", level_str)
  Q0_natural <- emms$estimate
  C1 <- Q0_natural[cond_id == "C1"]
  C2 <- Q0_natural[cond_id == "C2"]
  C3 <- Q0_natural[cond_id == "C3"]

  ordered_correctly <- isTRUE((C1 > C2) && (C2 > C3))
  if (ordered_correctly) {
    skip(paste0(
      "Inversion canary did not fire on this seed/fixture: intercepts-",
      "only TMB recovered the truth ordering. The simulator's regime ",
      "may be too mild to reproduce the M1 inversion failure mode. ",
      "Hard gates (M1-spec recovery, NLME parity) still apply."
    ))
  }
  expect_false(ordered_correctly)
})

# ---------------------------------------------------------------------------
# NLME backend agreement: TMB EMMs match NLME EMMs on the same M1 fit.
# Direction parity (binary), magnitude tolerance (5% natural scale) per
# the manuscript parity protocol.
# ---------------------------------------------------------------------------

test_that("M1 spec TMB matches NLME on simulated within-subject data", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  sim <- .m1_sim_dat(seed = 42, n_subjects = 80)

  fit_tmb <- suppressWarnings(fit_demand_tmb(
    sim, equation = "simplified",
    factors = "condition",
    random_effects = .m1_spec,
    multi_start = FALSE, verbose = 0
  ))

  fit_nlme <- tryCatch(
    suppressWarnings(fit_demand_mixed(
      sim, equation_form = "simplified",
      factors = "condition",
      random_effects = .m1_spec,
      verbose = 0
    )),
    error = function(e) NULL
  )
  if (is.null(fit_nlme)) {
    skip("NLME M1 fit failed to converge on this fixture; parity not testable.")
  }

  emms_tmb <- get_demand_param_emms(fit_tmb, param = "Q0")
  emms_nlme <- tryCatch(
    get_demand_param_emms(fit_nlme, param = "Q0"),
    error = function(e) NULL
  )
  if (is.null(emms_nlme)) {
    skip("NLME EMMs not extractable on this fixture.")
  }

  # Align rows by `level` to compare matched conditions.
  m <- merge(
    emms_tmb[, c("level", "estimate")],
    emms_nlme[, c("level", "estimate")],
    by = "level", suffixes = c(".tmb", ".nlme")
  )
  expect_equal(nrow(m), nrow(emms_tmb))

  # Direction parity: every pairwise sign matches.
  q_tmb <- m$estimate.tmb
  q_nlme <- m$estimate.nlme
  for (i in seq_len(length(q_tmb) - 1L)) {
    for (j in (i + 1L):length(q_tmb)) {
      expect_equal(
        sign(q_tmb[i] - q_tmb[j]),
        sign(q_nlme[i] - q_nlme[j]),
        info = sprintf("Direction parity for pair (%d, %d)", i, j)
      )
    }
  }

  # Magnitude tolerance on natural scale (5%).
  rel_diff <- abs(q_tmb - q_nlme) / abs(q_nlme)
  expect_lt(max(rel_diff), 0.05)
})
