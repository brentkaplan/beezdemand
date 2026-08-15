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
power_test_design <- function(...) {
  utils::modifyList(
    list(prices = c(0.1, 0.5, 1, 2, 5, 10)),
    list(...)
  )
}

# -----------------------------------------------------------------------------
# Input validation (cheap, no MC)
# -----------------------------------------------------------------------------

test_that("power_demand validates the effect specification", {
  expect_error(
    power_demand(
      n_subjects = 10,
      effect = list(delta_q0 = 0.5, delta_alpha = 0.3)
    ),
    "exactly one"
  )
  expect_error(
    power_demand(
      n_subjects = 10,
      effect = list(delta_q0 = NULL, delta_alpha = NULL)
    ),
    "exactly one"
  )
  expect_error(
    power_demand(n_subjects = 10, effect = list(bad_name = 0.5)),
    "delta_q0"
  )
  expect_error(
    power_demand(n_subjects = 10, effect = list(delta_q0 = "big")),
    "single finite number"
  )
})

test_that("power_demand validates design and locks rho_bd at 0", {
  expect_error(
    power_demand(
      n_subjects = 10,
      effect = list(delta_q0 = 0.5),
      design = list(rho_bd = 0.3)
    ),
    "rho_bd"
  )
  expect_error(
    power_demand(
      n_subjects = 10,
      effect = list(delta_q0 = 0.5),
      design = list(not_a_design_arg = 1)
    ),
    "not_a_design_arg"
  )
  # rho_bd = 0 explicitly is allowed (it is the locked v1 value)
  res <- power_demand(
    n_subjects = 8,
    effect = list(delta_q0 = 0.5),
    design = list(rho_bd = 0),
    n_sim = 2,
    seed = 1,
    verbose = FALSE
  )
  expect_s3_class(res, "beezdemand_power")
})

test_that("power_demand rejects unnamed or duplicated list elements", {
  expect_error(
    power_demand(n_subjects = 10, effect = list(0.5)),
    "named"
  )
  expect_error(
    power_demand(
      n_subjects = 10,
      effect = list(delta_q0 = 0.5, delta_q0 = 9)
    ),
    "duplicated"
  )
  expect_error(
    power_demand(
      n_subjects = 10,
      effect = list(delta_q0 = 0.5),
      design = list(0.2)
    ),
    "named"
  )
})

test_that("power_demand validates the seed range", {
  expect_error(
    power_demand(n_subjects = 10, effect = list(delta_q0 = 0.5), seed = 2^31),
    "seed"
  )
})

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

test_that("power_demand validates scalar arguments", {
  expect_error(
    power_demand(n_subjects = 1, effect = list(delta_q0 = 0.5)),
    "n_subjects"
  )
  expect_error(
    power_demand(n_subjects = 10, effect = list(delta_q0 = 0.5), alpha = 1.2),
    "alpha"
  )
  expect_error(
    power_demand(n_subjects = 10, effect = list(delta_q0 = 0.5), n_sim = 0),
    "n_sim"
  )
})

# -----------------------------------------------------------------------------
# Wilson interval helper (pure, unit-level)
# -----------------------------------------------------------------------------

test_that(".power_wilson_ci matches known values and handles edges", {
  ci <- beezdemand:::.power_wilson_ci(5, 10)
  expect_equal(ci, c(0.2366, 0.7634), tolerance = 1e-3)

  ci0 <- beezdemand:::.power_wilson_ci(0, 10)
  expect_equal(ci0[1], 0)
  expect_equal(ci0[2], 0.2775, tolerance = 1e-3)

  ci_all <- beezdemand:::.power_wilson_ci(10, 10)
  expect_equal(ci_all[2], 1)
  expect_gt(ci_all[1], 0.7)

  ci_none <- beezdemand:::.power_wilson_ci(0, 0)
  expect_equal(ci_none, c(NA_real_, NA_real_))

  ci_any <- beezdemand:::.power_wilson_ci(37, 100)
  expect_gte(ci_any[1], 0)
  expect_lte(ci_any[2], 1)
  expect_lt(ci_any[1], 0.37)
  expect_gt(ci_any[2], 0.37)
})

# -----------------------------------------------------------------------------
# Replicate classification (pure, unit-level): converged-but-unusable SEs are
# excluded from the denominator, not counted as "no effect detected"
# -----------------------------------------------------------------------------

test_that(".power_rep_row classifies unusable fits and never counts them as misses", {
  ok <- beezdemand:::.power_rep_row(
    converged = TRUE,
    hessian_pd = TRUE,
    estimate = 0.5,
    se = 0.1,
    alpha = 0.05
  )
  expect_equal(ok$status, "ok")
  expect_equal(ok$hit_p, TRUE)
  expect_equal(ok$hit_ci, TRUE)

  nonconv <- beezdemand:::.power_rep_row(
    converged = FALSE,
    hessian_pd = FALSE,
    estimate = 0.5,
    se = 0.1,
    alpha = 0.05
  )
  expect_equal(nonconv$status, "nonconverged")
  expect_equal(nonconv$hit_p, NA)

  bad_hess <- beezdemand:::.power_rep_row(
    converged = TRUE,
    hessian_pd = FALSE,
    estimate = 0.5,
    se = 0.1,
    alpha = 0.05
  )
  expect_equal(bad_hess$status, "hessian_not_pd")
  expect_equal(bad_hess$hit_p, NA)

  bad_se <- beezdemand:::.power_rep_row(
    converged = TRUE,
    hessian_pd = TRUE,
    estimate = 0.5,
    se = NA_real_,
    alpha = 0.05
  )
  expect_equal(bad_se$status, "se_unusable")
  expect_equal(bad_se$hit_p, NA)

  null_est <- beezdemand:::.power_rep_row(
    converged = TRUE,
    hessian_pd = TRUE,
    estimate = 0.001,
    se = 0.5,
    alpha = 0.05
  )
  expect_equal(null_est$status, "ok")
  expect_equal(null_est$hit_p, FALSE)
  expect_equal(null_est$hit_ci, FALSE)
})

test_that(".power_rep_row refers the Wald statistic to a t distribution", {
  # |z| = 2.1: rejects under z (crit 1.96) but not under t with 5 df
  # (crit 2.571); p-value and CI verdicts must agree in both cases.
  z_row <- beezdemand:::.power_rep_row(
    converged = TRUE,
    hessian_pd = TRUE,
    estimate = 0.21,
    se = 0.1,
    alpha = 0.05,
    df = Inf
  )
  expect_equal(z_row$hit_p, TRUE)
  expect_equal(z_row$hit_ci, TRUE)

  t_row <- beezdemand:::.power_rep_row(
    converged = TRUE,
    hessian_pd = TRUE,
    estimate = 0.21,
    se = 0.1,
    alpha = 0.05,
    df = 5
  )
  expect_equal(t_row$hit_p, FALSE)
  expect_equal(t_row$hit_ci, FALSE)
  expect_equal(t_row$p_value, 2 * stats::pt(-2.1, df = 5))
})

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

test_that(".simulate_between_subject_demand composes two arms correctly", {
  # Direct structural check of the load-bearing composition helper (no fit):
  # odd N splits ceiling/floor, each subject sits in exactly one arm, ids are
  # unique 1:n, and condition is a C1/C2 factor.
  prices <- c(0.1, 0.5, 1, 2, 5, 10)
  design <- utils::modifyList(
    .power_demand_design_defaults(),
    list(prices = prices)
  )
  set.seed(99)
  sim <- .simulate_between_subject_demand(
    n_subjects = 5,
    target_param = "Q0",
    delta = 0.6,
    design = design
  )
  expect_setequal(as.character(sim$id), as.character(1:5))
  expect_identical(levels(sim$condition), c("C1", "C2"))
  expect_equal(nrow(sim), 5 * length(prices))
  # Each subject appears in exactly one condition.
  per_id <- tapply(as.character(sim$condition), sim$id, function(x) {
    length(unique(x))
  })
  expect_true(all(per_id == 1))
  # ceiling(5/2) = 3 subjects in C1, floor = 2 in C2.
  arm_sizes <- tapply(
    sim$id,
    sim$condition,
    function(x) length(unique(x))
  )
  expect_equal(as.integer(arm_sizes[["C1"]]), 3L)
  expect_equal(as.integer(arm_sizes[["C2"]]), 2L)
})

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

test_that("between-design default df is rejected when n_subjects is too small", {
  expect_error(
    power_demand(
      n_subjects = 2,
      effect = list(delta_q0 = 0.5),
      design_type = "between"
    ),
    "df"
  )
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

test_that("find_n_demand validates n_range and n_sim_max", {
  expect_error(
    find_n_demand(
      target_power = 0.8,
      effect = list(delta_q0 = 0.5),
      n_range = c(10, 5)
    ),
    "n_range"
  )
  expect_error(
    find_n_demand(
      target_power = 0.8,
      effect = list(delta_q0 = 0.5),
      n_sim = 100,
      n_sim_max = 50
    ),
    "n_sim_max"
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

fake_batch <- function(hit_rates_by_n) {
  # Returns a run_batch(n, batch_size, sim_offset) closure emitting
  # deterministic all-usable replicate tables with the given per-N hit rate.
  function(n, batch_size, sim_offset) {
    rate <- hit_rates_by_n[[as.character(n)]]
    if (is.null(rate)) {
      rate <- 0
    }
    hits <- round(batch_size * rate)
    hit <- c(rep(TRUE, hits), rep(FALSE, batch_size - hits))
    tibble::tibble(
      sim = seq_len(batch_size) + sim_offset,
      status = "ok",
      converged = TRUE,
      hessian_pd = TRUE,
      estimate = ifelse(hit, 1, 0),
      se = 0.1,
      statistic = ifelse(hit, 10, 0),
      p_value = ifelse(hit, 1e-6, 1),
      ci_lower = ifelse(hit, 0.8, -0.2),
      ci_upper = ifelse(hit, 1.2, 0.2),
      hit_p = hit,
      hit_ci = hit,
      message = NA_character_
    )
  }
}

test_that("the search confirms a clean minimum", {
  res <- beezdemand:::.power_find_n_search(
    fake_batch(list(
      `4` = 0.1,
      `7` = 0.2,
      `8` = 0.99,
      `10` = 0.99,
      `12` = 0.99
    )),
    target_power = 0.8,
    n_range = c(4, 12),
    n_sim = 200,
    n_sim_max = 400,
    verbose = FALSE
  )
  expect_equal(res$n, 8)
  expect_equal(res$status, "confirmed")
  expect_equal(res$uncertain, FALSE)
  expect_true(all(c("n_used", "usable_fraction") %in% names(res$evaluations)))
})

test_that("a failed confirmation returns NA with status unresolved", {
  # 8 looks "above" during the search; a fresh confirmation cannot re-clear
  # the target because eval order alternates via the deterministic fake --
  # emulate by making 8 sit exactly at an ambiguous rate that resolves
  # "above" by point estimate in search, then construct the contradiction
  # via n_sim_max = n_sim so the ambiguous decision is point-estimate based.
  # Simpler deterministic contradiction: rate at 8 is 0.79 (just below
  # target): the search reaches hi = 12 confirmed above, lo path pushes to
  # 8..; craft rates so search selects 8 via ambiguous_above, and
  # confirmation (same rate) yields ambiguous_above again -> uncertain, not
  # unresolved. To force TRUE unresolved, make the selected N's rate low
  # enough that confirmation is decisively "below": impossible with a
  # deterministic rate that previously read "above" -- so drive the search
  # with a stateful fake whose rate at N = 8 drops after the first call.
  calls <- new.env()
  calls$n8 <- 0L
  stateful <- function(n, batch_size, sim_offset) {
    base <- fake_batch(list(`4` = 0.1, `7` = 0.1, `8` = 0.99, `12` = 0.99))
    if (n == 8) {
      calls$n8 <- calls$n8 + 1L
      if (calls$n8 > 1L) {
        return(fake_batch(list(`8` = 0.1))(n, batch_size, sim_offset))
      }
    }
    base(n, batch_size, sim_offset)
  }
  res <- suppressWarnings(beezdemand:::.power_find_n_search(
    stateful,
    target_power = 0.8,
    n_range = c(4, 12),
    n_sim = 200,
    n_sim_max = 400,
    verbose = FALSE
  ))
  expect_equal(res$n, NA_integer_)
  expect_equal(res$status, "unresolved")
  expect_equal(res$uncertain, TRUE)
})

test_that("the lower bound is reconfirmed with fresh replicates before at_lower_bound", {
  # Both hi and lo clear the target: the search must re-evaluate lo (a
  # second row for n_subjects == 4) rather than trust a single look, so the
  # "re-confirmed before reporting" claim in the docs holds for this exit too.
  res <- beezdemand:::.power_find_n_search(
    fake_batch(list(`4` = 0.99, `12` = 0.99)),
    target_power = 0.8,
    n_range = c(4, 12),
    n_sim = 200,
    n_sim_max = 400,
    verbose = FALSE
  )
  expect_equal(res$n, 4)
  expect_equal(res$status, "at_lower_bound")
  expect_equal(res$uncertain, FALSE)
  expect_equal(sum(res$evaluations$n_subjects == 4), 2L)
})

test_that("a lower bound that fails reconfirmation is bisected past, not reported", {
  # First look at 4 reads above, the fresh look reads below: 4 is not
  # reliably above, so [4, 12] is a valid bracket and the search continues
  # upward. The contradictory first look stays in $evaluations and demotes
  # the final status to "uncertain" (a lower N once read above).
  calls <- new.env()
  calls$n4 <- 0L
  stateful <- function(n, batch_size, sim_offset) {
    if (n == 4) {
      calls$n4 <- calls$n4 + 1L
      rate <- if (calls$n4 > 1L) 0.1 else 0.99
      return(fake_batch(setNames(list(rate), "4"))(n, batch_size, sim_offset))
    }
    fake_batch(list(`12` = 0.99))(n, batch_size, sim_offset)
  }
  res <- suppressWarnings(beezdemand:::.power_find_n_search(
    stateful,
    target_power = 0.8,
    n_range = c(4, 12),
    n_sim = 200,
    n_sim_max = 400,
    verbose = FALSE
  ))
  expect_equal(res$n, 12)
  expect_equal(res$status, "uncertain")
  expect_equal(res$uncertain, TRUE)
  expect_equal(sum(res$evaluations$n_subjects == 4), 2L)
  expect_true(any(res$evaluations$n_subjects %in% c(8, 10, 11)))
})

test_that("a lower neighbor clearing the target on confirmation yields uncertain", {
  # Search: 8 above, 7 below; confirmation: 8 above again, but 7 flips
  # above -> minimality not established.
  calls <- new.env()
  calls$n7 <- 0L
  stateful <- function(n, batch_size, sim_offset) {
    if (n == 7) {
      calls$n7 <- calls$n7 + 1L
      rate <- if (calls$n7 > 1L) 0.99 else 0.1
      return(fake_batch(setNames(list(rate), "7"))(n, batch_size, sim_offset))
    }
    fake_batch(list(`4` = 0.1, `8` = 0.99, `12` = 0.99))(
      n,
      batch_size,
      sim_offset
    )
  }
  res <- beezdemand:::.power_find_n_search(
    stateful,
    target_power = 0.8,
    n_range = c(4, 12),
    n_sim = 200,
    n_sim_max = 400,
    verbose = FALSE
  )
  expect_equal(res$n, 8)
  expect_equal(res$status, "uncertain")
  expect_equal(res$uncertain, TRUE)
})
