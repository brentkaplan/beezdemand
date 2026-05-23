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
