# v0.3.0 audit: hurdle Part II parameterization + retransformation correctness.

test_that("3-RE hurdle alpha_i is multiplicative log-scale exp(log_alpha + c_i)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id",
    random_effects = c("zeros", "q0", "alpha"), verbose = 0
  )
  sp <- get_subject_pars(fit)
  log_alpha <- fit$model$coefficients[["log_alpha"]]
  # Contract + C++ (src/HurdleDemand3RE.h): alpha_i = exp(log_alpha + c_i).
  expect_equal(sp$alpha, exp(log_alpha + sp$c_i), tolerance = 1e-6)
  # NOT the additive natural-scale form the pre-fix vignette displayed
  # (exp(-(alpha + c_i) * price)); that would give alpha_i = exp(log_alpha) + c_i.
  expect_false(isTRUE(all.equal(sp$alpha, exp(log_alpha) + sp$c_i)))
  # The multiplicative-log form guarantees positivity.
  expect_true(all(sp$alpha > 0))
})

test_that("hurdle predict applies the lognormal retransformation correction exp(sigma_e^2/2)", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_hurdle(
    apt, y_var = "y", x_var = "x", id_var = "id", verbose = 0
  )
  nd <- data.frame(id = apt$id[1], x = c(1, 5, 10))
  p_corr <- predict(fit, newdata = nd, type = "response", correction = TRUE)
  p_none <- predict(fit, newdata = nd, type = "response", correction = FALSE)
  sigma_e <- exp(fit$model$coefficients[["logsigma_e"]])
  cf <- exp(sigma_e^2 / 2)
  expect_equal(
    p_corr$predicted_consumption / p_none$predicted_consumption,
    rep(cf, nrow(nd)),
    tolerance = 1e-8
  )
  # prob_zero (Part I) is unaffected by the Part II correction.
  expect_equal(p_corr$prob_zero, p_none$prob_zero, tolerance = 1e-12)
})
