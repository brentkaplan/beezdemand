# Tests for .tmb_parametric_draws() + confint(method = "simulate") (TICKET-018)
#
# `method = "simulate"` draws R parametric Monte Carlo samples from the joint
# asymptotic Gaussian posterior N(beta_hat, Sigma_hat) (Sigma_hat = vcov(fit) =
# sdr$cov.fixed) and reports per-coefficient empirical quantiles. It is
# asymptotically Wald-equivalent on per-coefficient CIs (diagnostic), and the
# R x p draw matrix is the shared primitive consumed by boot_demand()
# (TICKET-024).

# Small, well-conditioned intercept-only TMB fit on apt.
.fit_apt_tmb <- function() {
  data(apt, package = "beezdemand")
  fit_demand_tmb(
    apt,
    y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  )
}

test_that("confint(method='wald') is identical to the default confint", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  ci_default <- confint(fit)
  ci_wald <- confint(fit, method = "wald")
  expect_identical(ci_default, ci_wald)
})

test_that("confint(method='simulate') returns Wald's tibble shape but distinct intervals", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  ci_wald <- confint(fit)
  ci_sim <- confint(fit, method = "simulate", R = 200, seed = 42)

  expect_s3_class(ci_sim, "tbl_df")
  expect_identical(names(ci_sim), names(ci_wald))
  expect_identical(ci_sim$term, ci_wald$term)
  expect_identical(nrow(ci_sim), nrow(ci_wald))

  # Simulate intervals are empirical quantiles, NOT the analytic +/- z*se, so
  # they must differ from Wald at finite R (this is what distinguishes the
  # methods; it fails if `method` is silently swallowed by `...`).
  expect_false(isTRUE(all.equal(ci_sim$conf.low, ci_wald$conf.low)))
  expect_false(isTRUE(all.equal(ci_sim$conf.high, ci_wald$conf.high)))
})

test_that("confint(method='simulate') leaves the point estimate unchanged", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  ci_wald <- confint(fit)
  ci_sim <- confint(fit, method = "simulate", R = 200, seed = 42)
  expect_equal(ci_sim$estimate, ci_wald$estimate)
})

test_that("confint simulate widths agree with Wald (~15% rel at R=2000)", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  ci_wald <- confint(fit)
  ci_sim <- confint(fit, method = "simulate", R = 2000, seed = 1)
  w_wald <- ci_wald$conf.high - ci_wald$conf.low
  w_sim <- ci_sim$conf.high - ci_sim$conf.low
  rel <- abs(w_sim - w_wald) / pmax(abs(w_wald), 1e-8)
  expect_true(all(rel < 0.15), info = paste("max rel diff:", round(max(rel), 4)))
})

test_that("confint simulate converges to Wald as R grows (R=10000, ~5%)", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  ci_wald <- confint(fit)
  ci_sim <- confint(fit, method = "simulate", R = 10000, seed = 1)
  w_wald <- ci_wald$conf.high - ci_wald$conf.low
  w_sim <- ci_sim$conf.high - ci_sim$conf.low
  rel <- abs(w_sim - w_wald) / pmax(abs(w_wald), 1e-8)
  expect_true(all(rel < 0.05), info = paste("max rel diff:", round(max(rel), 4)))
})

test_that("confint simulate is reproducible with seed", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  ci1 <- confint(fit, method = "simulate", R = 200, seed = 42)
  ci2 <- confint(fit, method = "simulate", R = 200, seed = 42)
  expect_identical(ci1, ci2)

  # A different seed yields different draws (and thus different intervals).
  ci3 <- confint(fit, method = "simulate", R = 200, seed = 7)
  expect_false(isTRUE(all.equal(ci1$conf.low, ci3$conf.low)))
})

test_that("confint simulate errors helpfully on invalid R", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  expect_error(confint(fit, method = "simulate", R = 50), "R")
  # Non-integer R must error, not silently floor with a recycling warning
  # inside .tmb_parametric_draws() (mirrors the boot_demand() guard).
  expect_error(confint(fit, method = "simulate", R = 150.5), "whole number")
})

test_that(".tmb_parametric_draws returns an R x p matrix matching coef/vcov moments", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  mu <- coef(fit, type = "internal")
  p <- length(mu)

  draws <- beezdemand:::.tmb_parametric_draws(fit, R = 100, seed = 1)
  expect_true(is.matrix(draws))
  expect_identical(dim(draws), c(100L, p))
  expect_identical(colnames(draws), names(mu))

  # Same seed -> identical draws.
  draws2 <- beezdemand:::.tmb_parametric_draws(fit, R = 100, seed = 1)
  expect_identical(draws, draws2)

  # Moments converge to coef (mean) and vcov (covariance) at large R.
  big <- beezdemand:::.tmb_parametric_draws(fit, R = 20000, seed = 3)
  expect_equal(unname(colMeans(big)), unname(mu), tolerance = 0.05)
  V <- vcov(fit)
  expect_equal(unname(diag(stats::cov(big))), unname(diag(V)), tolerance = 0.1)
})

test_that("confint simulate honors report_space='natural'", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  ci <- confint(fit,
    method = "simulate", R = 500, seed = 5,
    report_space = "natural"
  )
  q0 <- ci[grepl("^Q0", ci$term), ]
  expect_true(nrow(q0) >= 1)
  expect_true(all(q0$estimate > 0))
  expect_true(all(q0$conf.low < q0$estimate & q0$estimate < q0$conf.high))
})

test_that("confint accepts positional report_space (backward compatibility)", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  # report_space has been the 4th positional arg throughout the dev cycle;
  # adding method/R/seed must not shift it.
  ci_pos <- confint(fit, NULL, 0.95, "natural")
  ci_named <- confint(fit, report_space = "natural")
  expect_identical(ci_pos, ci_named)
})

test_that(".tmb_parametric_draws leaves the RNG state absent when none existed", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  saved <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit(
    {
      if (!is.null(saved)) {
        assign(".Random.seed", saved, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  beezdemand:::.tmb_parametric_draws(fit, R = 50, seed = 1)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that(".tmb_parametric_draws restores a pre-existing RNG state", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  set.seed(99)
  invisible(runif(1))
  before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  beezdemand:::.tmb_parametric_draws(fit, R = 50, seed = 1)
  after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  expect_identical(before, after)
})

test_that("confint simulate respects the parm filter (one and zero matches)", {
  skip_on_cran()
  fit <- .fit_apt_tmb()
  ci_all <- confint(fit, method = "simulate", R = 200, seed = 1)
  one_term <- ci_all$term[1]
  ci_one <- confint(fit, method = "simulate", R = 200, seed = 1, parm = one_term)
  expect_identical(nrow(ci_one), 1L)
  expect_identical(ci_one$term, one_term)

  ci_none <- confint(fit, method = "simulate", R = 200, seed = 1, parm = "no_such_param")
  expect_identical(nrow(ci_none), 0L)
})

test_that("confint simulate works on a k-fixed fit", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit_kfix <- fit_demand_tmb(
    apt,
    y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", estimate_k = FALSE, k = 2, verbose = 0
  )
  ci_w <- confint(fit_kfix)
  ci_s <- confint(fit_kfix, method = "simulate", R = 2000, seed = 1)
  expect_identical(ci_s$term, ci_w$term)
  expect_equal(ci_s$estimate, ci_w$estimate)
  w_w <- ci_w$conf.high - ci_w$conf.low
  w_s <- ci_s$conf.high - ci_s$conf.low
  expect_true(all(abs(w_s - w_w) / pmax(abs(w_w), 1e-8) < 0.15))
})


# --- TICKET-063: hessian_pd gate reaches .tmb_parametric_draws() via vcov() -

test_that(".tmb_parametric_draws-backed confint(simulate) surfaces the hessian_pd warning", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  fit <- .weak_pd_tmb_fit()
  skip_if(!isFALSE(fit$hessian_pd),
          "platform numerics did not produce a non-PD Hessian")
  skip_if(!all(is.finite(suppressWarnings(vcov(fit)))),
          "weak fixture's covariance is non-finite on this platform (draws unavailable by design)")

  # F-BD4-3 (audit 2026-09-06): a non-PD Hessian usually leaves a materially
  # INDEFINITE covariance, which the draws helper now refuses instead of
  # silently clamping. The hessian_pd warning is still raised exactly once
  # either way -- vcov() warns before the PSD gate runs -- so the dedup
  # behaviour this test exists for is asserted on both branches.
  V <- suppressWarnings(vcov(fit))
  ev <- eigen(V, symmetric = TRUE)$values
  indefinite <- any(ev < -max(abs(ev)) * sqrt(.Machine$double.eps))

  err <- NULL
  conds <- .capture_warning_conditions(
    draws <- tryCatch(
      beezdemand:::.tmb_parametric_draws(fit, R = 50, seed = 1),
      beezdemand_indefinite_vcov_error = function(e) {
        err <<- e
        NULL
      }
    )
  )
  expect_identical(.n_hessian_pd_warnings(conds), 1L)

  if (indefinite) {
    expect_s3_class(err, "beezdemand_indefinite_vcov_error")
    expect_null(draws)
  } else {
    expect_null(err)
    expect_equal(dim(draws), c(50, length(fit$model$coefficients)))
  }
})

# --- Release 0.3.0 CI fold: non-finite covariance -----------------------------
# On some platforms (cran-everything macos-26 / R 4.6.1, run 31949621191) the
# weak-fit fixture's sdreport covariance contains NaN/Inf; eigen() then dies
# with the opaque "infinite or missing values in 'x'". The draws helper must
# refuse such a covariance with a classed, informative error instead.

test_that(".tmb_parametric_draws refuses a non-finite covariance with a classed error", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .fit_apt_tmb()
  bad <- fit
  bad$sdr$cov.fixed[1, 1] <- NaN
  expect_error(
    suppressWarnings(beezdemand:::.tmb_parametric_draws(bad, R = 10, seed = 1)),
    class = "beezdemand_nonfinite_vcov_error"
  )
  expect_error(
    suppressWarnings(confint(bad, method = "simulate", R = 100, seed = 1)),
    class = "beezdemand_nonfinite_vcov_error"
  )
  # A finite covariance still works (sanity for the injection approach).
  expect_equal(
    dim(beezdemand:::.tmb_parametric_draws(fit, R = 10, seed = 1)),
    c(10, length(fit$model$coefficients))
  )
})

# --- F-BD4-3 (release-correctness audit 2026-09-06) --------------------------
# A finite but INDEFINITE covariance used to pass the non-finite guard and get
# silently repaired by `sqrt(pmax(e$values, 0))`, so draws came from a
# different (rank-deficient) distribution than the one requested, with no
# condition raised. It must now refuse with a classed error. Eigenvalues within
# numerical tolerance of zero are still clamped: a PSD-singular covariance is a
# legitimate degenerate Gaussian, not a defect.

.fake_tmb_fit_with_cov <- function(V) {
  nm <- colnames(V)
  structure(
    list(
      model = list(coefficients = stats::setNames(rep(0, ncol(V)), nm)),
      opt = list(par = stats::setNames(rep(0, ncol(V)), nm)),
      sdr = list(cov.fixed = V),
      converged = TRUE,
      hessian_pd = TRUE
    ),
    class = "beezdemand_tmb"
  )
}

.sym_from_eigen <- function(values, nm = paste0("p", seq_along(values))) {
  p <- length(values)
  Q <- qr.Q(qr(matrix(stats::rnorm(p * p), p, p)))
  V <- Q %*% diag(values, p, p) %*% t(Q)
  V <- (V + t(V)) / 2
  dimnames(V) <- list(nm, nm)
  V
}

test_that(".tmb_parametric_draws() refuses a materially indefinite covariance (F-BD4-3)", {
  V <- matrix(c(1, 2, 2, 1), 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_true(all(is.finite(V)))
  expect_true(min(eigen(V, symmetric = TRUE)$values) < -0.5)

  expect_error(
    .tmb_parametric_draws(.fake_tmb_fit_with_cov(V), R = 10, seed = 1),
    class = "beezdemand_indefinite_vcov_error"
  )
})

test_that(".tmb_parametric_draws() still accepts PSD-singular and near-zero-negative covariances (F-BD4-3)", {
  set.seed(11)

  # Exactly singular (a zero eigenvalue) -- a degenerate but valid Gaussian.
  V_sing <- .sym_from_eigen(c(2, 0.5, 0))
  d_sing <- .tmb_parametric_draws(.fake_tmb_fit_with_cov(V_sing), R = 20, seed = 1)
  expect_equal(dim(d_sing), c(20L, 3L))
  expect_true(all(is.finite(d_sing)))

  # Numerical noise: a negative eigenvalue far below the relative tolerance.
  V_noise <- .sym_from_eigen(c(2, 0.5, -1e-14))
  d_noise <- .tmb_parametric_draws(.fake_tmb_fit_with_cov(V_noise), R = 20, seed = 1)
  expect_equal(dim(d_noise), c(20L, 3L))
  expect_true(all(is.finite(d_noise)))

  # All-zero covariance: every draw equals the mean vector.
  V_zero <- matrix(0, 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  d_zero <- .tmb_parametric_draws(.fake_tmb_fit_with_cov(V_zero), R = 5, seed = 1)
  expect_true(all(d_zero == 0))
})

test_that("the indefinite-covariance gate propagates to confint(simulate) and boot_demand() (F-BD4-3)", {
  skip_on_cran()
  skip_if_not_installed("TMB")

  data(apt, package = "beezdemand")
  fit <- suppressWarnings(fit_demand_tmb(
    apt, y_var = "y", x_var = "x", id_var = "id",
    equation = "exponential", verbose = 0
  ))
  skip_if(is.null(fit$sdr$cov.fixed), "no sdreport covariance on this platform")

  V <- as.matrix(fit$sdr$cov.fixed)
  e <- eigen(V, symmetric = TRUE)
  # Flip the smallest eigenvalue to a materially negative value.
  e$values[length(e$values)] <- -max(abs(e$values))
  V_bad <- e$vectors %*% diag(e$values) %*% t(e$vectors)
  V_bad <- (V_bad + t(V_bad)) / 2
  dimnames(V_bad) <- dimnames(V)

  fit_bad <- fit
  fit_bad$sdr$cov.fixed <- V_bad

  expect_error(
    suppressWarnings(confint(fit_bad, method = "simulate", R = 100, seed = 1)),
    class = "beezdemand_indefinite_vcov_error"
  )
  expect_error(
    suppressWarnings(boot_demand(fit_bad, statistics = "Pmax", R = 100, seed = 1)),
    class = "beezdemand_indefinite_vcov_error"
  )
})

test_that("the PSD gate is scale-invariant (F-BD4-3 end-pass fold)", {
  # A spectrum-relative tolerance alone measures a small component's defect
  # against the largest eigenvalue: for diag(c(1e12, -1)) the tolerance is
  # ~1.5e4, so the negative variance would pass and that parameter would
  # silently become deterministic. The correlation-scaled test catches it.
  V_bad_scale <- diag(c(1e12, -1))
  dimnames(V_bad_scale) <- list(c("a", "b"), c("a", "b"))
  expect_error(
    .tmb_parametric_draws(.fake_tmb_fit_with_cov(V_bad_scale), R = 10, seed = 1),
    class = "beezdemand_indefinite_vcov_error"
  )

  # ... while a genuinely PSD covariance with a condition number of 1e24 is
  # still accepted: disparate scales are not by themselves a defect.
  V_ill <- diag(c(1e12, 1e-12))
  dimnames(V_ill) <- list(c("a", "b"), c("a", "b"))
  d_ill <- .tmb_parametric_draws(.fake_tmb_fit_with_cov(V_ill), R = 20, seed = 1)
  expect_equal(dim(d_ill), c(20L, 2L))
  expect_true(all(is.finite(d_ill)))
})
