# Fast tests split out of the BEEZ_FULL_TESTS-gated test-anova-tmb.R so they
# still run in R CMD check / CI / CRAN.

test_that("anova S3 method is registered", {
  expect_false(is.null(getS3method("anova", "beezdemand_tmb", optional = TRUE)))
})


test_that("auto-grouped Wald tests map columns to the right model term with a factor plus a covariate", {
  skip_if_not_installed("TMB")
  skip_on_cran()
  # Regression: .tmb_term_assign_map indexed term labels with the raw `assign`
  # vector; the intercept's 0 dropped an element and shifted every label, so
  # "Q0 ~ grp" was really joint(grpB, z) and "Q0 ~ z" was grpC alone.
  set.seed(1)
  n <- 45
  prices <- c(0, 0.5, 1, 2, 3, 5, 8, 10, 15, 20)
  grp <- rep(0:2, length.out = n)
  z <- rnorm(n)
  pred <- function(lq0, la, P, k) exp(lq0 + k * log(10) * (exp(-exp(la) * exp(lq0) * P) - 1))
  d <- do.call(rbind, lapply(seq_len(n), function(i) {
    mu <- pred(log(10) + c(0, 0.3, -0.2)[grp[i] + 1] + rnorm(1, 0, 0.2),
               log(0.003) + c(0, 0.5, 0.2)[grp[i] + 1] + 0.4 * z[i] + rnorm(1, 0, 0.3), prices, 2)
    data.frame(id = i, x = prices, y = mu + rnorm(length(prices), 0, 0.6),
               grp = factor(c("A", "B", "C")[grp[i] + 1]), z = z[i])
  }))
  d$id <- factor(d$id)
  fit <- fit_demand_tmb(d, equation = "exponentiated", estimate_k = FALSE, k = 2,
                        factors = "grp", continuous_covariates = "z", verbose = 0)
  map <- beezdemand:::.tmb_term_assign_map(fit, "Q0")
  expect_identical(unname(map[c("grpB", "grpC", "z")]), c("grp", "grp", "z"))
  expect_true(is.na(map[["(Intercept)"]]))

  co <- coef(fit)
  V <- as.matrix(vcov(fit))
  iq <- which(names(co) == "beta_q0")
  hand <- function(idx) as.numeric(t(co[idx]) %*% solve(V[idx, idx]) %*% co[idx])
  a <- anova(fit)
  expect_equal(a$Chisq[a$Group == "Q0 ~ grp"], hand(iq[2:3]), tolerance = 1e-8)
  expect_equal(a$df[a$Group == "Q0 ~ grp"], 2L)
  expect_equal(a$Chisq[a$Group == "Q0 ~ z"], hand(iq[4]), tolerance = 1e-8)
})
