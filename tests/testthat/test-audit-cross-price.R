# tests/testthat/test-audit-cross-price.R
#
# Pre-CRAN audit: non-circular verification of the cross-price NLS engine
# (R/cross-price.R + R/cp-methods.R). The checks anchor to the published
# log10-parameterized equations in EQUATIONS_CONTRACT.md, never to the
# function's own output:
#
#   - Exponentiated: y = (10^log10_qalone) * 10^(I * exp(-(10^log10_beta) * x))
#   - Exponential:   log10(y) = log10_qalone + I * exp(-(10^log10_beta) * x)
#   - Additive:      y = (10^log10_qalone) + I * exp(-(10^log10_beta) * x)
#
# Three independent guards per form:
#   1. Parameter recovery from a NOISE-FREE data-generating process (also proves
#      the 10^() natural-scale reconstruction of Qalone/beta).
#   2. predict() reproduces the contract right-hand side evaluated by hand at the
#      fitted coefficients (guards predict.cp_model_nls against formula drift).
#   3. glance()$r.squared equals an independent 1 - SSres/SStot computed on the
#      correct response scale (log10(y) for exponential, natural y otherwise).

# Contract RHS, evaluated by hand from the log10-parameterized coefficients.
# Returns the NATURAL-scale prediction (y), matching predict()$y_pred.
.cp_hand_pred <- function(equation, log10_qalone, I, log10_beta, x) {
  beta <- 10^log10_beta
  switch(
    equation,
    exponentiated = (10^log10_qalone) * 10^(I * exp(-beta * x)),
    exponential   = 10^(log10_qalone + I * exp(-beta * x)),
    additive      = (10^log10_qalone) + I * exp(-beta * x)
  )
}

.cp_cases <- list(
  exponentiated = list(qalone = 10, I = 1.5, beta = 0.05),
  exponential   = list(qalone = 12, I = 1.2, beta = 0.08),
  additive      = list(qalone = 5,  I = 10,  beta = 0.05)
)

test_that("fit_cp_nls recovers known params from noise-free data (3 forms)", {
  skip_on_cran()
  x <- seq(1, 50, length.out = 40)
  for (eq in names(.cp_cases)) {
    p <- .cp_cases[[eq]]
    y <- .cp_hand_pred(eq, log10(p$qalone), p$I, log10(p$beta), x) # noise-free
    # Explicit (perturbed) starts force the deterministic nlsLM path so the
    # noise-free optimum is recovered to high precision.
    starts <- list(
      log10_qalone = log10(p$qalone) + 0.1,
      I = p$I * 0.9,
      log10_beta = log10(p$beta) - 0.1
    )
    fit <- fit_cp_nls(
      data.frame(x = x, y = y), equation = eq, start_values = starts
    )
    co <- coef(fit)
    expect_equal(unname(10^co[["log10_qalone"]]), p$qalone,
                 tolerance = 1e-4, info = paste(eq, "Qalone"))
    expect_equal(unname(10^co[["log10_beta"]]), p$beta,
                 tolerance = 1e-4, info = paste(eq, "beta"))
    expect_equal(unname(co[["I"]]), p$I,
                 tolerance = 1e-4, info = paste(eq, "I"))
  }
})

test_that("predict.cp_model_nls reproduces the contract formula at fitted coefs", {
  skip_on_cran()
  x <- seq(1, 50, length.out = 40)
  newx <- data.frame(x = c(2, 7, 15, 33, 48))
  for (eq in names(.cp_cases)) {
    p <- .cp_cases[[eq]]
    y <- .cp_hand_pred(eq, log10(p$qalone), p$I, log10(p$beta), x)
    fit <- fit_cp_nls(data.frame(x = x, y = y), equation = eq)
    co <- coef(fit)
    hand <- .cp_hand_pred(
      eq, co[["log10_qalone"]], co[["I"]], co[["log10_beta"]], newx$x
    )
    pr <- predict(fit, newdata = newx)
    # Identity holds at the FITTED coefficients regardless of fit quality, so a
    # non-deterministic multi-start fit cannot make this flaky.
    expect_equal(pr$y_pred, unname(hand), tolerance = 1e-8, info = eq)
    if (eq == "exponential") {
      hand_log10 <- co[["log10_qalone"]] +
        co[["I"]] * exp(-(10^co[["log10_beta"]]) * newx$x)
      expect_equal(pr$y_pred_log10, unname(hand_log10), tolerance = 1e-8)
    }
  }
})

test_that("glance()$r.squared equals 1 - SSres/SStot on the response scale", {
  skip_on_cran()
  set.seed(404)
  x <- seq(1, 50, length.out = 40)
  scales <- c(exponentiated = "nat", exponential = "log10", additive = "nat")
  sds <- c(exponentiated = 0.05, exponential = 0.05, additive = 0.4)
  for (eq in names(.cp_cases)) {
    p <- .cp_cases[[eq]]
    y_true <- .cp_hand_pred(eq, log10(p$qalone), p$I, log10(p$beta), x)
    # A little noise so R^2 < 1 and the residual sum is genuinely nonzero.
    if (scales[[eq]] == "log10") {
      y <- 10^(log10(y_true) + stats::rnorm(length(x), 0, sds[[eq]]))
    } else {
      y <- pmax(y_true + stats::rnorm(length(x), 0, sds[[eq]]), 0.05)
    }
    df <- data.frame(x = x, y = y)
    fit <- fit_cp_nls(df, equation = eq)
    co <- coef(fit)
    yhat <- .cp_hand_pred(eq, co[["log10_qalone"]], co[["I"]], co[["log10_beta"]], x)
    if (scales[[eq]] == "log10") {
      lhs <- log10(df$y)
      fit_scale <- log10(yhat)
    } else {
      lhs <- df$y
      fit_scale <- yhat
    }
    r2_indep <- 1 - sum((lhs - fit_scale)^2) / sum((lhs - mean(lhs))^2)
    expect_equal(glance(fit)$r.squared, r2_indep, tolerance = 1e-6, info = eq)
  }
})
