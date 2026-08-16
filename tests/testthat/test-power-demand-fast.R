# Fast (no-model-fit) tests for power_demand()/find_n_demand(): argument
# validation, Wilson interval, replicate classification, between-arm
# composition, and the mocked bisection search. Split out of the heavy
# test-power-demand.R (BEEZ_FULL_TESTS-gated) so they still run in R CMD
# check / CI / CRAN. Shared fixtures live in helper-power.R.

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

