# Heavy file: runs only with BEEZ_FULL_TESTS=true (full-tests.yaml tri-OS job
# and the pre-push hook); skipped in R CMD check / R-CMD-check.yaml to keep the
# Linux CI test phase short. See tests/testthat/helper-full-tests.R.
.skip_unless_full_tests()

# =============================================================================
# Monte Carlo power analysis: power_demand() / find_n_demand()
#
# Validity battery (see vignette("power-analysis") "Validity and Limitations"):
#   1. Type I error calibration (the load-bearing test). n_sim is computed
#      from the tolerance, not guessed: to assert |rate - .05| <= .02 with a
#      >= 3-sigma acceptance band under correct calibration, n_sim >=
#      9 * .05 * .95 / .02^2 = 1069 -> n_sim = 1200 (band [.03, .07] is
#      3.18 binomial SDs, and excludes both 1.5x and 0.5x the nominal rate).
#      Seed and configuration are fixed here BEFORE the first run; tolerances
#      are not adjusted after observing results.
#      HISTORY: the first run of this battery used the asymptotic Wald
#      z-test and FAILED calibration (rate 0.089 at n = 15). The engine now
#      refers the Wald statistic to t(n_subjects - 1) -- an EMPIRICALLY
#      CALIBRATED small-sample correction, not a model-derived df (the
#      Laplace/plug-in fit has no exact t sampling theory). Because the
#      same configuration exposed the z failure, this test alone is
#      selection-adjacent evidence for t; the independent null checks below
#      (different N, different target parameter, larger sigma_e) provide
#      the out-of-sample validation. df = Inf reproduces the z behavior
#      deliberately.
#   2. Convergence handling: non-converged / unusable-SE replicates are
#      excluded from the denominator and surfaced, never counted as misses.
#   3. Closed-form benchmark: degenerate config vs pwr::pwr.t.test().
#   4. Monotonicity sweeps (one fixed direction of each factor).
#   5. Reproducibility: identical seed -> identical result.
# =============================================================================

# Fast shared config: small but well-behaved (measured ~0.4 s/fit).

# -----------------------------------------------------------------------------
# Input validation (cheap, no MC)
# -----------------------------------------------------------------------------


test_that("the RNG state is left exactly as found", {
  skip_on_cran()
  # .Random.seed absent beforehand: it must be absent afterwards too.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(power_demand(
    n_subjects = 8,
    effect = list(delta_q0 = 0.5),
    design = power_test_design(),
    n_sim = 1,
    seed = 5,
    verbose = FALSE
  ))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))

  # .Random.seed present beforehand: restored bit-identically.
  set.seed(999)
  before <- get(".Random.seed", envir = globalenv())
  invisible(power_demand(
    n_subjects = 8,
    effect = list(delta_q0 = 0.5),
    design = power_test_design(),
    n_sim = 1,
    seed = 5,
    verbose = FALSE
  ))
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})


# -----------------------------------------------------------------------------
# Wilson interval helper (pure, unit-level)
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Replicate classification (pure, unit-level): converged-but-unusable SEs are
# excluded from the denominator, not counted as "no effect detected"
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Return structure + p-value/CI verdict agreement
# -----------------------------------------------------------------------------

test_that("power_demand returns the documented structure", {
  skip_on_cran()
  res <- power_demand(
    n_subjects = 10,
    effect = list(delta_q0 = 0.6),
    design = power_test_design(),
    n_sim = 8,
    seed = 101,
    verbose = FALSE
  )

  expect_s3_class(res, "beezdemand_power")
  expect_true(all(
    c(
      "power",
      "power_mc_ci",
      "hit_rate_p",
      "hit_rate_ci",
      "n_sim",
      "n_converged",
      "n_hessian_pd",
      "n_used",
      "alpha",
      "effect",
      "target_term",
      "design",
      "n_subjects",
      "replicates"
    ) %in%
      names(res)
  ))
  expect_equal(res$n_sim, 8)
  expect_equal(nrow(res$replicates), 8)
  expect_equal(res$target_term, "Q0:conditionC2")
  expect_true(all(
    c(
      "sim",
      "status",
      "converged",
      "hessian_pd",
      "estimate",
      "se",
      "statistic",
      "p_value",
      "ci_lower",
      "ci_upper",
      "hit_p",
      "hit_ci"
    ) %in%
      names(res$replicates)
  ))
  expect_gte(res$power, 0)
  expect_lte(res$power, 1)
  expect_lte(res$power_mc_ci[1], res$power)
  expect_gte(res$power_mc_ci[2], res$power)

  # p < alpha and the (1 - alpha) CI excluding 0 use the same SE and t
  # reference, so they are the same decision; assert the implementation
  # preserves that invariant.
  used <- res$replicates[res$replicates$status == "ok", ]
  expect_equal(used$hit_p, used$hit_ci)

  # print method smoke
  expect_output(print(res), "Monte Carlo power")
})

test_that("power_demand targets alpha when delta_alpha is supplied", {
  skip_on_cran()
  res <- power_demand(
    n_subjects = 10,
    effect = list(delta_alpha = 0.6),
    design = power_test_design(),
    n_sim = 4,
    seed = 102,
    verbose = FALSE
  )
  expect_equal(res$target_term, "alpha:conditionC2")
})

# -----------------------------------------------------------------------------
# Estimate recovery: extracted estimates are on the natural-log scale of the
# simulator's delta (codex review, blocking finding 1)
# -----------------------------------------------------------------------------

test_that("power_demand extracts the delta on the simulator's log scale", {
  skip_on_cran()
  delta <- 0.5
  res <- power_demand(
    n_subjects = 25,
    effect = list(delta_q0 = delta),
    design = power_test_design(),
    n_sim = 12,
    seed = 103,
    verbose = FALSE
  )
  used <- res$replicates[res$replicates$status == "ok", ]
  expect_gt(nrow(used), 6)
  # Mean of replicate estimates recovers the injected log-scale delta.
  # (If extraction were on the exp scale this would sit near exp(0.5) = 1.65.)
  expect_equal(mean(used$estimate), delta, tolerance = 0.15)
})

# -----------------------------------------------------------------------------
# Reproducibility
# -----------------------------------------------------------------------------

test_that("power_demand is exactly reproducible under a seed", {
  skip_on_cran()
  res1 <- power_demand(
    n_subjects = 8,
    effect = list(delta_q0 = 0.5),
    design = power_test_design(),
    n_sim = 5,
    seed = 42,
    verbose = FALSE
  )
  res2 <- power_demand(
    n_subjects = 8,
    effect = list(delta_q0 = 0.5),
    design = power_test_design(),
    n_sim = 5,
    seed = 42,
    verbose = FALSE
  )
  expect_identical(res1$replicates, res2$replicates)
  expect_identical(res1$power, res2$power)

  res3 <- power_demand(
    n_subjects = 8,
    effect = list(delta_q0 = 0.5),
    design = power_test_design(),
    n_sim = 5,
    seed = 43,
    verbose = FALSE
  )
  expect_false(identical(res1$replicates, res3$replicates))
})

# -----------------------------------------------------------------------------
# Convergence handling, tested not assumed
# -----------------------------------------------------------------------------

test_that("all-nonconverged replicates yield NA power and n_converged = 0", {
  skip_on_cran()
  res <- suppressWarnings(power_demand(
    n_subjects = 8,
    effect = list(delta_q0 = 0.5),
    design = power_test_design(),
    n_sim = 4,
    seed = 44,
    verbose = FALSE,
    tmb_control = list(iter_max = 1, eval_max = 2)
  ))
  expect_equal(res$n_converged, 0)
  expect_equal(res$n_used, 0)
  expect_true(is.na(res$power))
  expect_true(all(is.na(res$replicates$hit_p)))
  # Nonconverged replicates surface the optimizer's message and keep the
  # "nonconverged" status (they are not execution errors).
  expect_equal(unique(res$replicates$status), "nonconverged")
  expect_true(all(!is.na(res$replicates$message)))
})

test_that("partially non-converged runs exclude failures from the denominator", {
  skip_on_cran()
  # Config found empirically to produce a stable mix of converged and
  # failed fits under this seed (tiny N, extreme effect, high noise).
  res <- suppressWarnings(power_demand(
    n_subjects = 4,
    effect = list(delta_alpha = 3),
    design = power_test_design(sigma_e = 0.8, sigma_b = 0.8, sigma_d = 0.8),
    n_sim = 30,
    seed = 20260720,
    verbose = FALSE
  ))
  expect_lt(res$n_used, res$n_sim)
  expect_gt(res$n_used, 0)
  # Power denominator is n_used, not n_sim: recompute independently.
  used <- res$replicates[res$replicates$status == "ok", ]
  expect_equal(nrow(used), res$n_used)
  expect_equal(res$power, mean(used$hit_ci))
  # Which implies the failed replicates were NOT counted as misses:
  expect_gt(res$power, sum(used$hit_ci, na.rm = TRUE) / res$n_sim - 1e-12)
})

# -----------------------------------------------------------------------------
# Type I error calibration -- THE load-bearing test.
# Preregistered: config + seed fixed before first run; band derivation in the
# file header. n_sim = 1200 >= 1069 required for the [.03, .07] band.
# -----------------------------------------------------------------------------

test_that("Type I error is calibrated at nominal alpha under the null", {
  skip_on_cran()
  res <- power_demand(
    n_subjects = 15,
    effect = list(delta_q0 = 0),
    design = power_test_design(),
    n_sim = 1200,
    seed = 20260717,
    verbose = FALSE
  )
  # Usable-fit fraction must be high enough that conditional power is
  # not badly selected.
  expect_gte(res$n_used / res$n_sim, 0.95)
  expect_gte(res$hit_rate_p, 0.03)
  expect_lte(res$hit_rate_p, 0.07)
  expect_gte(res$hit_rate_ci, 0.03)
  expect_lte(res$hit_rate_ci, 0.07)
})

test_that("Type I error holds at a realistic N on the alpha target", {
  skip_on_cran()
  # Independent null check: different N (40), different target parameter.
  # Band: .05 +/- 3.18 * sqrt(.05 * .95 / 400) = [.015, .085], fixed before
  # the run.
  res <- power_demand(
    n_subjects = 40,
    effect = list(delta_alpha = 0),
    design = power_test_design(),
    n_sim = 400,
    seed = 20260718,
    verbose = FALSE
  )
  expect_gte(res$n_used / res$n_sim, 0.95)
  expect_gte(res$hit_rate_p, 0.015)
  expect_lte(res$hit_rate_p, 0.085)
})

test_that("Type I error holds at a larger residual SD (working-model stress)", {
  skip_on_cran()
  # The refit is a working model for the lognormal DGP (Gaussian raw-Q
  # likelihood); the mismatch grows with sigma_e. This null check probes the
  # upper end of the plausible sigma_e range. Band as above ([.015, .085] at
  # n_sim = 400), fixed before the run; seed 20260724.
  res <- power_demand(
    n_subjects = 15,
    effect = list(delta_q0 = 0),
    design = power_test_design(sigma_e = 0.3),
    n_sim = 400,
    seed = 20260724,
    verbose = FALSE
  )
  expect_gte(res$n_used / res$n_sim, 0.95)
  expect_gte(res$hit_rate_p, 0.015)
  expect_lte(res$hit_rate_p, 0.085)
})

# -----------------------------------------------------------------------------
# Closed-form benchmark: degenerate config vs analytic power.
# With sigma_e and sigma_d tiny, per-(subject, condition) log-Q0 is observed
# nearly exactly, so the delta_q0 Wald test reduces to a one-sample test on
# paired condition differences with sd = sqrt(2) * sigma_b.
# Tolerance fixed before the MC run at 0.10 = 3 MC SDs at n_sim = 400
# (~.065) plus slack for the working-model approximation (tiny-but-nonzero
# sigma_d / sigma_e and plug-in variance components). The engine's t(n - 1)
# reference matches pwr.t.test's one-sample df exactly.
# -----------------------------------------------------------------------------

test_that("Monte Carlo power matches analytic power in a degenerate design", {
  skip_on_cran()
  skip_if_not_installed("pwr")

  n_subj <- 30
  sigma_b <- 0.35
  delta <- 0.25
  analytic <- pwr::pwr.t.test(
    n = n_subj,
    d = delta / (sqrt(2) * sigma_b),
    sig.level = 0.05,
    type = "one.sample"
  )$power

  res <- power_demand(
    n_subjects = n_subj,
    effect = list(delta_q0 = delta),
    design = power_test_design(
      sigma_b = sigma_b,
      sigma_d = 0.05,
      sigma_e = 0.05
    ),
    n_sim = 400,
    seed = 20260719,
    verbose = FALSE
  )
  expect_gte(res$n_used / res$n_sim, 0.95)
  expect_equal(res$power, analytic, tolerance = 0.10)
})

# -----------------------------------------------------------------------------
# Monotonicity sanity sweeps (one fixed direction per factor; slack = 0.10
# covers MC noise at n_sim = 100 for configs with well-separated true power)
# -----------------------------------------------------------------------------

test_that("power increases with n_subjects", {
  skip_on_cran()
  p_small <- power_demand(
    n_subjects = 8,
    effect = list(delta_q0 = 0.35),
    design = power_test_design(),
    n_sim = 100,
    seed = 301,
    verbose = FALSE
  )$power
  p_large <- power_demand(
    n_subjects = 30,
    effect = list(delta_q0 = 0.35),
    design = power_test_design(),
    n_sim = 100,
    seed = 302,
    verbose = FALSE
  )$power
  expect_gte(p_large, p_small - 0.10)
  expect_gt(p_large, p_small)
})

test_that("power increases with effect size", {
  skip_on_cran()
  p_small <- power_demand(
    n_subjects = 15,
    effect = list(delta_q0 = 0.15),
    design = power_test_design(),
    n_sim = 100,
    seed = 303,
    verbose = FALSE
  )$power
  p_large <- power_demand(
    n_subjects = 15,
    effect = list(delta_q0 = 0.6),
    design = power_test_design(),
    n_sim = 100,
    seed = 304,
    verbose = FALSE
  )$power
  expect_gt(p_large, p_small)
})

test_that("power decreases as random-effect SD grows", {
  skip_on_cran()
  p_low_var <- power_demand(
    n_subjects = 15,
    effect = list(delta_q0 = 0.35),
    design = power_test_design(sigma_b = 0.2),
    n_sim = 100,
    seed = 305,
    verbose = FALSE
  )$power
  p_high_var <- power_demand(
    n_subjects = 15,
    effect = list(delta_q0 = 0.35),
    design = power_test_design(sigma_b = 0.7),
    n_sim = 100,
    seed = 306,
    verbose = FALSE
  )$power
  expect_lt(p_high_var, p_low_var)
})

# -----------------------------------------------------------------------------
# Between-subject design (design_type = "between"): each subject is assigned
# to ONE condition (first ceiling(n/2) to C1, rest to C2); the simulator is
# the same DGP with n_conditions = 1 per arm, so per-(subject, condition)
# REs degenerate to per-subject REs and the intercept-only RE refit is
# correctly specified. Wald df default is n - 2 (two-sample design).
# Preregistered seeds/bands fixed before the first run, as for the
# within-subject battery.
# -----------------------------------------------------------------------------


test_that("design_type = 'between' returns the documented structure", {
  skip_on_cran()
  res <- power_demand(
    n_subjects = 12,
    effect = list(delta_q0 = 0.8),
    design = power_test_design(),
    design_type = "between",
    n_sim = 6,
    seed = 601,
    verbose = FALSE
  )
  expect_s3_class(res, "beezdemand_power")
  expect_equal(res$df, 10)
  expect_equal(res$target_term, "Q0:conditionC2")
  expect_equal(res$settings$design_type, "between")
  used <- res$replicates[res$replicates$status == "ok", ]
  expect_equal(used$hit_p, used$hit_ci)
})

test_that("design_type = 'between' recovers the group delta on the log scale", {
  skip_on_cran()
  delta <- 0.6
  res <- power_demand(
    n_subjects = 50,
    effect = list(delta_q0 = delta),
    design = power_test_design(),
    design_type = "between",
    n_sim = 12,
    seed = 602,
    verbose = FALSE
  )
  used <- res$replicates[res$replicates$status == "ok", ]
  expect_gt(nrow(used), 6)
  expect_equal(mean(used$estimate), delta, tolerance = 0.15)
})

test_that("design_type = 'between' is exactly reproducible under a seed", {
  skip_on_cran()
  res1 <- power_demand(
    n_subjects = 10,
    effect = list(delta_alpha = 0.8),
    design = power_test_design(),
    design_type = "between",
    n_sim = 4,
    seed = 603,
    verbose = FALSE
  )
  res2 <- power_demand(
    n_subjects = 10,
    effect = list(delta_alpha = 0.8),
    design = power_test_design(),
    design_type = "between",
    n_sim = 4,
    seed = 603,
    verbose = FALSE
  )
  expect_identical(res1$replicates, res2$replicates)
})


test_that("Type I error is calibrated for the between-subject design", {
  skip_on_cran()
  # Same band derivation as the within-subject calibration ([.03, .07] at
  # n_sim = 1200); n = 30 total (15 per condition); seed 20260725, fixed
  # before the first run.
  res <- power_demand(
    n_subjects = 30,
    effect = list(delta_q0 = 0),
    design = power_test_design(),
    design_type = "between",
    n_sim = 1200,
    seed = 20260725,
    verbose = FALSE
  )
  expect_gte(res$n_used / res$n_sim, 0.95)
  expect_gte(res$hit_rate_p, 0.03)
  expect_lte(res$hit_rate_p, 0.07)
})

test_that("between-design Type I error holds at a larger N on the alpha target", {
  skip_on_cran()
  # Band: .05 +/- 3.18 * sqrt(.05 * .95 / 400) = [.015, .085]; seed 20260726.
  res <- power_demand(
    n_subjects = 60,
    effect = list(delta_alpha = 0),
    design = power_test_design(),
    design_type = "between",
    n_sim = 400,
    seed = 20260726,
    verbose = FALSE
  )
  expect_gte(res$n_used / res$n_sim, 0.95)
  expect_gte(res$hit_rate_p, 0.015)
  expect_lte(res$hit_rate_p, 0.085)
})

test_that("between-design Monte Carlo power matches analytic two-sample power", {
  skip_on_cran()
  skip_if_not_installed("pwr")
  # With sigma_e and sigma_d tiny, per-subject log-Q0 is observed nearly
  # exactly and the design reduces to a two-sample comparison of log Q0
  # with sd = sigma_b and equal groups (even N). Tolerance fixed before the
  # run at 0.10 (3 MC SDs at n_sim = 400 plus working-model slack); the
  # engine's t(n - 2) reference matches pwr.t.test's two-sample df exactly.
  n_subj <- 30
  sigma_b <- 0.35
  delta <- 0.34
  analytic <- pwr::pwr.t.test(
    n = n_subj / 2,
    d = delta / sigma_b,
    sig.level = 0.05,
    type = "two.sample"
  )$power

  res <- power_demand(
    n_subjects = n_subj,
    effect = list(delta_q0 = delta),
    design = power_test_design(
      sigma_b = sigma_b,
      sigma_d = 0.05,
      sigma_e = 0.05
    ),
    design_type = "between",
    n_sim = 400,
    seed = 20260727,
    verbose = FALSE
  )
  expect_gte(res$n_used / res$n_sim, 0.95)
  expect_equal(res$power, analytic, tolerance = 0.10)
})

test_that("between-design power increases with n_subjects and effect size", {
  skip_on_cran()
  p_small_n <- power_demand(
    n_subjects = 10,
    effect = list(delta_q0 = 0.5),
    design = power_test_design(),
    design_type = "between",
    n_sim = 100,
    seed = 604,
    verbose = FALSE
  )$power
  p_large_n <- power_demand(
    n_subjects = 40,
    effect = list(delta_q0 = 0.5),
    design = power_test_design(),
    design_type = "between",
    n_sim = 100,
    seed = 605,
    verbose = FALSE
  )$power
  expect_gt(p_large_n, p_small_n)

  p_small_d <- power_demand(
    n_subjects = 20,
    effect = list(delta_q0 = 0.2),
    design = power_test_design(),
    design_type = "between",
    n_sim = 100,
    seed = 606,
    verbose = FALSE
  )$power
  p_large_d <- power_demand(
    n_subjects = 20,
    effect = list(delta_q0 = 0.8),
    design = power_test_design(),
    design_type = "between",
    n_sim = 100,
    seed = 607,
    verbose = FALSE
  )$power
  expect_gt(p_large_d, p_small_d)
})

test_that("find_n_demand supports the between-subject design", {
  skip_on_cran()
  res <- find_n_demand(
    target_power = 0.8,
    effect = list(delta_q0 = 1.2),
    design = power_test_design(),
    design_type = "between",
    n_range = c(6, 40),
    n_sim = 40,
    seed = 608,
    verbose = FALSE
  )
  expect_s3_class(res, "beezdemand_power_n")
  expect_gte(res$n, 6)
  expect_lte(res$n, 40)
  # Well-formed range whose lower bound is too small for df = n - 2: the
  # design-specific guard fires.
  expect_error(
    find_n_demand(
      target_power = 0.8,
      effect = list(delta_q0 = 0.5),
      design_type = "between",
      n_range = c(2, 10)
    ),
    "n_range"
  )
  # A malformed range with lower bound 2 must still error on n_range, but via
  # the search's own (more precise) validation rather than the design guard.
  expect_error(
    find_n_demand(
      target_power = 0.8,
      effect = list(delta_q0 = 0.5),
      design_type = "between",
      n_range = c(2, 1)
    ),
    "n_range"
  )
})

# -----------------------------------------------------------------------------
# find_n_demand
# -----------------------------------------------------------------------------

test_that("find_n_demand finds a plausible minimum N for a large effect", {
  skip_on_cran()
  res <- find_n_demand(
    target_power = 0.8,
    effect = list(delta_q0 = 0.8),
    design = power_test_design(),
    n_range = c(4, 40),
    n_sim = 60,
    seed = 401,
    verbose = FALSE
  )
  expect_s3_class(res, "beezdemand_power_n")
  expect_true(all(
    c(
      "n",
      "target_power",
      "status",
      "uncertain",
      "evaluations"
    ) %in%
      names(res)
  ))
  expect_gte(res$n, 4)
  expect_lte(res$n, 40)
  expect_true(all(
    c(
      "n_subjects",
      "n_sim_total",
      "power",
      "ci_lower",
      "ci_upper",
      "decision"
    ) %in%
      names(res$evaluations)
  ))
  # The power estimate recorded at the selected N clears the target within
  # MC noise.
  sel <- res$evaluations[res$evaluations$n_subjects == res$n, ]
  expect_gte(max(sel$power), 0.7)
  # The final (confirmation) look at the selected N must have cleared the
  # target -- decisively, or by point estimate when flagged uncertain.
  expect_true(sel$decision[nrow(sel)] %in% c("above", "ambiguous_above"))
  if (!res$uncertain) expect_equal(sel$decision[nrow(sel)], "above")
  expect_output(print(res), "Monte Carlo uncertainty|minimum")
})

test_that("find_n_demand reports an unreachable target instead of extrapolating", {
  skip_on_cran()
  expect_error(
    find_n_demand(
      target_power = 0.95,
      effect = list(delta_q0 = 0.05),
      design = power_test_design(),
      n_range = c(4, 6),
      n_sim = 40,
      seed = 402,
      verbose = FALSE
    ),
    "not reach|unreachable"
  )
})


test_that("target extraction is robust to a global contr.sum option", {
  skip_on_cran()
  old <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(old), add = TRUE)
  res <- power_demand(
    n_subjects = 8,
    effect = list(delta_q0 = 0.6),
    design = power_test_design(),
    n_sim = 2,
    seed = 7,
    verbose = FALSE
  )
  # The engine forces treatment coding via a scoped options override, so the
  # target term is found and the estimand is still C2 - C1 on the log scale.
  expect_equal(res$n_used, 2)
  expect_equal(sum(is.na(res$replicates$estimate)), 0)
})

# -----------------------------------------------------------------------------
# Search decision logic under forced disagreement (fake run_batch; no fitting)
# -----------------------------------------------------------------------------


