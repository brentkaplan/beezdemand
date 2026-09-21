# Batch 3 (2026-09-12), F-BD9-7: hurdle marginal default "kde" -> "normal",
# (-Inf, Inf) normal integration, and a persisted flag when the random-effects
# covariance falls back to a diagonal approximation.

.b3_hurdle_sim <- function(n_subjects = 30, seed = 2026) {
  simulate_hurdle_data(n_subjects = n_subjects, seed = seed, stop_at_zero = FALSE)
}
.b3_hurdle_fit <- function(sim = .b3_hurdle_sim(), random_effects = c("zeros", "q0")) {
  fit_demand_hurdle(sim, y_var = "y", x_var = "x", id_var = "id",
                    random_effects = random_effects, verbose = 0)
}

test_that("predict()/plot() marginal_method default is 'normal'; explicit 'kde' still honoured", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .b3_hurdle_fit()
  prices <- c(0.5, 1, 5)
  out <- predict(fit, type = "probability", marginal = TRUE, prices = prices)
  expect_identical(attr(out, "marginal_method"), "normal")
  expect_equal(out$prob_zero,
               beezdemand:::.compute_marginal_pzero(fit, prices, method = "normal"))
  out_kde <- predict(fit, type = "probability", marginal = TRUE, prices = prices,
                     marginal_method = "kde")
  expect_identical(attr(out_kde, "marginal_method"), "kde")
  expect_false(isTRUE(all.equal(out$prob_zero, out_kde$prob_zero)))
  expect_identical(eval(formals(predict.beezdemand_hurdle)$marginal_method)[1], "normal")
  expect_identical(eval(formals(plot.beezdemand_hurdle)$marginal_method)[1], "normal")
  expect_identical(eval(formals(beezdemand:::.compute_marginal_pzero)$method), "normal")
})

test_that("'normal' marginal P(zero) integrates over the whole real line", {
  skip_on_cran()
  fun <- beezdemand:::.marginal_pzero_normal
  sigma_a <- 1.5; beta0 <- 0.4; beta1 <- 0.8; eps <- 0.01
  prices <- c(0, 1, 10)
  ref <- vapply(prices, function(p) {
    stats::integrate(function(a) stats::plogis(beta0 + a + beta1 * log(p + eps)) *
                       stats::dnorm(a, 0, sigma_a), -Inf, Inf)$value
  }, numeric(1))
  expect_equal(fun(sigma_a, beta0, beta1, prices, eps), ref, tolerance = 1e-7)
  # A very narrow random-effect SD must reduce to the conditional curve, not
  # to 0 (integrate() on the raw density missed the spike; Codex end pass).
  for (sd_small in c(1e-4, 1e-6, 1e-8, 0)) {
    expect_equal(fun(sd_small, beta0, beta1, prices, eps),
                 stats::plogis(beta0 + beta1 * log(prices + eps)), tolerance = 1e-6)
  }
  # Non-finite SD: Inf gives the 1/2 limit, NA/NaN abort (Codex END pass).
  expect_equal(fun(Inf, beta0, beta1, prices, eps), rep(0.5, length(prices)))
  expect_equal(fun(1e6, 10, 0, prices, eps), rep(0.5, length(prices)), tolerance = 1e-6)
  expect_error(fun(NA_real_, beta0, beta1, prices, eps), "sigma_a")
  expect_error(fun(NaN, beta0, beta1, prices, eps), "sigma_a")
})

test_that("a normal fit records re_cov_fallback = FALSE and prints no covariance note", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .b3_hurdle_fit()
  expect_identical(fit$re_cov_fallback, FALSE)
  expect_false(any(grepl("covariance", summary(fit)$notes)))
  expect_false(any(grepl("covariance", capture.output(print(fit)))))
  pred <- predict(fit, type = "demand", marginal = TRUE, prices = c(1, 5))
  expect_false(isTRUE(attr(pred, "re_cov_fallback")))
})

test_that("a diagonal-covariance fallback at fit time is persisted and surfaced (F-BD9-7)", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  real_chol <- chol
  n_warn <- 0L
  sim <- .b3_hurdle_sim()  # the simulator calls chol() itself; keep it outside the mock
  # Fail chol() only for non-diagonal matrices so the helper's own diagonal
  # fallback (and any diagonal chol elsewhere) keeps working. If TMB's
  # sdreport() ever calls R-level chol() on a non-diagonal matrix this mock
  # would break the fit itself; that would show up as a fit error here.
  fit <- withCallingHandlers(
    testthat::with_mocked_bindings(
      chol = function(x, ...) {
        if (isTRUE(all.equal(unname(x), diag(diag(x)), check.attributes = FALSE))) {
          real_chol(x, ...)
        } else {
          stop("forced non-PD (batch 3 flag test)")
        }
      },
      .package = "base",
      code = .b3_hurdle_fit(sim)
    ),
    beezdemand_hurdle_chol_fallback_warning = function(w) {
      n_warn <<- n_warn + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_gte(n_warn, 1L)
  expect_identical(fit$re_cov_fallback, TRUE)
  s <- summary(fit)
  expect_true(any(grepl("covariance", s$notes, ignore.case = TRUE)))
  out_print <- capture.output(print(fit))
  expect_true(any(grepl("covariance", out_print, ignore.case = TRUE)))
  out_sum <- capture.output(print(s))
  expect_true(any(grepl("covariance", out_sum, ignore.case = TRUE)))
})

test_that("predict(marginal = TRUE, type = 'demand') carries re_cov_fallback when its draw falls back", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  fit <- .b3_hurdle_fit()
  bad <- fit
  bad$model$coefficients[["rho_ab_raw"]] <- 40  # tanh(40) == 1 exactly -> singular Sigma
  pred <- suppressWarnings(predict(bad, type = "demand", marginal = TRUE, prices = c(1, 5)))
  expect_identical(attr(pred, "re_cov_fallback"), TRUE)
  expect_warning(predict(bad, type = "demand", marginal = TRUE, prices = c(1, 5)),
                 class = "beezdemand_hurdle_chol_fallback_warning")
})
