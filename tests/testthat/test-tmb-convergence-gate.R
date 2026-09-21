# Release-correctness audit 2026-09-06 (F-BD4-2): a beezdemand_tmb fit with
# converged = FALSE must announce itself at fit time regardless of `verbose`,
# and every inference/prediction surface must warn before returning numbers
# from it. Previously only hessian_pd was gated, so a fit that hit the
# iteration limit with a PD Hessian produced silent p-values and CIs.

skip_if_not_installed("TMB")

.nonconverged_fit <- function() {
  data("apt", package = "beezdemand")
  suppressWarnings(fit_demand_tmb(apt, equation = "exponentiated",
                                  estimate_k = FALSE, k = 2,
                                  random_effects = "q0", multi_start = FALSE,
                                  tmb_control = list(iter_max = 2), verbose = 0))
}

test_that("a non-converged TMB fit warns at fit time even with verbose = 0", {
  skip_on_cran()
  data("apt", package = "beezdemand")
  expect_warning(
    fit <- fit_demand_tmb(apt, equation = "exponentiated", estimate_k = FALSE,
                          k = 2, random_effects = "q0", multi_start = FALSE,
                          tmb_control = list(iter_max = 2), verbose = 0),
    class = "beezdemand_tmb_convergence_warning"
  )
  expect_false(fit$converged)
})

test_that("tidy/confint/vcov/predict warn on a non-converged TMB fit", {
  skip_on_cran()
  fit <- .nonconverged_fit()
  expect_false(fit$converged)
  # Force the Hessian-PD branch to be silent so the convergence gate alone
  # must fire.
  fit$hessian_pd <- TRUE
  expect_warning(tidy(fit), class = "beezdemand_tmb_convergence_warning")
  expect_warning(confint(fit), class = "beezdemand_tmb_convergence_warning")
  expect_warning(vcov(fit), class = "beezdemand_tmb_convergence_warning")
  expect_warning(predict(fit), class = "beezdemand_tmb_convergence_warning")
})

test_that("a converged TMB fit does not trigger the convergence gate", {
  skip_on_cran()
  data("apt", package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponentiated", estimate_k = FALSE,
                        k = 2, random_effects = "q0", multi_start = FALSE,
                        verbose = 0)
  skip_if(!isTRUE(fit$converged))
  expect_no_warning(tidy(fit), class = "beezdemand_tmb_convergence_warning")
  expect_no_warning(predict(fit), class = "beezdemand_tmb_convergence_warning")
})
