# Batch 3 (2026-09-12), audit F-BD4-1 side-finding: nlminb "false convergence"
# rescue in fit_demand_tmb(). R's nlminb() reports convergence 0/1 only; the
# PORT status ("false convergence (8)") is in $message.

test_that("false-convergence predicate keys on the nlminb message, not a code 8", {
  fc <- beezdemand:::.tmb_is_false_convergence
  expect_true(fc(list(convergence = 1L, message = "false convergence (8)"), "nlminb"))
  expect_false(fc(list(convergence = 1L, message = "iteration limit reached without convergence (10)"), "nlminb"))
  expect_false(fc(list(convergence = 0L, message = "relative convergence (4)"), "nlminb"))
  expect_false(fc(list(convergence = 1L, message = "false convergence (8)"), "L-BFGS-B"))
  expect_false(fc(list(convergence = 8L, message = NULL), "nlminb"))
})

test_that("rescue candidates need code 0, a small gradient and a PD Hessian", {
  # Quadratic objective with a known minimum at (1, 2); mimic obj$fn / obj$gr.
  obj <- list(
    fn = function(p) sum((p - c(1, 2))^2),
    gr = function(p) 2 * (p - c(1, 2))
  )
  ok <- beezdemand:::.tmb_rescue_candidate_ok
  good <- list(par = c(1, 2), convergence = 0L)
  expect_true(ok(obj, good, grad_tol = 1e-2)$ok)
  # Convergence 0 but a materially non-zero gradient: rejected.
  far <- list(par = c(2, 2), convergence = 0L)
  chk <- ok(obj, far, grad_tol = 1e-2)
  expect_false(chk$ok)
  expect_equal(chk$max_grad, 2)
  expect_false(ok(obj, list(par = c(1, 2), convergence = 1L), 1e-2)$ok)
  # Saddle: PD test fails.
  saddle <- list(fn = function(p) p[1]^2 - p[2]^2, gr = function(p) c(2 * p[1], -2 * p[2]))
  expect_false(ok(saddle, list(par = c(0, 0), convergence = 0L), 1e-2)$ok)
})

test_that("fit_demand_tmb rescues the apt_full exponential + gender fixed-k fit", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt_full, package = "beezdemand")

  # Without the rescue: today's loud failure (regression pin).
  expect_warning(
    fit_off <- suppressMessages(fit_demand_tmb(
      apt_full, equation = "exponential", factors = "gender", verbose = 0,
      tmb_control = list(rescue = FALSE))),
    class = "beezdemand_tmb_convergence_warning")
  expect_false(fit_off$converged)
  expect_true(beezdemand:::.tmb_is_false_convergence(fit_off$opt, "nlminb"))
  expect_null(fit_off$opt$rescued_from)

  fit <- suppressMessages(fit_demand_tmb(
    apt_full, equation = "exponential", factors = "gender", verbose = 0))
  expect_true(fit$converged)
  expect_identical(fit$opt$convergence, 0L)
  expect_true(isTRUE(fit$hessian_pd))
  expect_true(grepl("false convergence", fit$opt$rescued_from))
  expect_true(fit$opt$rescue_method %in% c("nlminb_restart", "lbfgsb", "free_k_warm_start"))
  expect_true(is.finite(fit$opt$objective))
  # The rescued point is stationary and a local minimum on the SAME objective;
  # NOT "<= the stalled NLL" -- the stalled ridge (NLL ~3308.7) has a gradient
  # norm ~1e11 and an indefinite Hessian and is inadmissible.
  g <- fit$tmb_obj$gr(fit$opt$par)
  expect_lte(max(abs(g)), 1e-2)
  H <- stats::optimHess(fit$opt$par, fit$tmb_obj$fn, fit$tmb_obj$gr)
  expect_gt(min(eigen((H + t(H)) / 2, symmetric = TRUE, only.values = TRUE)$values), 0)
  expect_equal(fit$opt$rescue_nll_before, fit_off$opt$objective, tolerance = 1e-6)
  # sdreport() must be evaluated at the rescued solution, not at the lower-NLL
  # ridge TMB's `last.par.best` still pointed to (caught by the first RED run).
  expect_equal(unname(fit$sdr$par.fixed), unname(fit$opt$par), tolerance = 1e-8)
  # k stayed fixed at 2 (the rescue never frees k on the reported fit).
  expect_equal(beezdemand:::.tmb_get_k(fit), 2)
  notes <- summary(fit)$notes
  expect_true(any(grepl("rescued via", notes)))
})

test_that("a fit that converges first time is untouched by the rescue path", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  data(apt, package = "beezdemand")
  fit <- suppressWarnings(fit_demand_tmb(apt, equation = "exponentiated", verbose = 0))
  expect_true(fit$converged)
  expect_null(fit$opt$rescued_from)
  expect_false(any(grepl("rescued via", summary(fit)$notes)))
  expect_error(fit_demand_tmb(apt, equation = "exponentiated", verbose = 0,
                              tmb_control = list(rescue = "yes")), "rescue")
})
