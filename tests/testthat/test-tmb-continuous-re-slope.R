# Tests for TICKET-051: continuous within-subject covariate random slopes in
# fit_demand_tmb() (dose-response demand). Covers the identifiability guard
# (re-timed + concrete thresholds + corrected message), slope-scaled start
# values, the numeric RE-RHS Z builder, parameter recovery, nlme equivalence,
# and strict additivity for factor/intercept fits.
#
# Always load via devtools::load_all(); never library(beezdemand).

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build long demand data where each subject sees a prescribed set of numeric
# covariate (`dose_c`) values. `doses_per_subject[[i]]` is the vector of dose
# values for subject i (length-1 => constant within subject).
.make_numeric_re_data <- function(doses_per_subject, prices = c(1, 5, 10)) {
  rows <- list()
  k <- 1L
  for (i in seq_along(doses_per_subject)) {
    for (d in doses_per_subject[[i]]) {
      for (p in prices) {
        rows[[k]] <- data.frame(
          id = i, dose_c = d, x = p,
          y = 10 * exp(-0.01 * p),
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
  }
  out <- do.call(rbind, rows)
  out$id <- factor(out$id)
  out
}

.parse_re <- function(re, data, cov = "pdSymm") {
  beezdemand:::.normalize_re_input(re, covariance_structure = cov, data = data)
}

# ---------------------------------------------------------------------------
# Simulator fixture sanity
# ---------------------------------------------------------------------------

test_that(".simulate_continuous_re_demand yields a within-subject continuous covariate", {
  dat <- beezdemand:::.simulate_continuous_re_demand(n_subjects = 8, seed = 1)
  expect_true(all(c("id", "dose_c", "x", "y") %in% names(dat)))
  expect_true(is.numeric(dat$dose_c))
  # dose_c varies within every subject (>= 2 distinct values)
  varies <- tapply(dat$dose_c, dat$id, function(v) length(unique(v)))
  expect_true(all(varies >= 2L))
  # truth attached
  expect_equal(attr(dat, "truth")$b1_q0, 0.10)
})

# ---------------------------------------------------------------------------
# Identifiability guard: thresholds + message (unit tests of .validate_re_input)
# ---------------------------------------------------------------------------

test_that("guard stops a between-subject continuous covariate with a continuous-aware message", {
  # Each subject has a single (constant) dose value -> 0 informative subjects.
  dat <- .make_numeric_re_data(lapply(1:20, function(i) (i %% 5) - 2))
  rp <- .parse_re(nlme::pdSymm(Q0 + alpha ~ dose_c), dat)
  expect_error(
    beezdemand:::.validate_re_input(rp, dat, "id"),
    "not estimable"
  )
  # The message must point at continuous_covariates, NOT say "factor".
  err <- tryCatch(beezdemand:::.validate_re_input(rp, dat, "id"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "continuous_covariates")
  expect_false(grepl("factor", err, ignore.case = TRUE))
})

test_that("guard stops when fewer than 2 subjects are informative", {
  # Only subject 1 varies; 2..20 constant -> 1 informative -> not estimable.
  dps <- c(list(c(-1, 1)), lapply(2:20, function(i) (i %% 3) - 1))
  dat <- .make_numeric_re_data(dps)
  rp <- .parse_re(nlme::pdSymm(Q0 + alpha ~ dose_c), dat)
  expect_error(
    beezdemand:::.validate_re_input(rp, dat, "id"),
    "not estimable"
  )
})

test_that("guard warns (not stops) when < 80% of subjects are informative", {
  # 10 of 20 subjects vary -> 50% informative -> warn, proceed.
  dps <- c(
    lapply(1:10, function(i) c(-1, 1)),
    lapply(11:20, function(i) (i %% 3) - 1)
  )
  dat <- .make_numeric_re_data(dps)
  rp <- .parse_re(nlme::pdSymm(Q0 + alpha ~ dose_c), dat)
  expect_warning(
    beezdemand:::.validate_re_input(rp, dat, "id"),
    "shrinkage"
  )
  # ... and it does not error.
  expect_error(
    suppressWarnings(beezdemand:::.validate_re_input(rp, dat, "id")),
    NA
  )
})

test_that("guard is silent when all subjects are informative (and centered)", {
  dps <- lapply(1:20, function(i) c(-2, -1, 0, 1, 2))  # mean 0 -> centered
  dat <- .make_numeric_re_data(dps)
  rp <- .parse_re(nlme::pdSymm(Q0 + alpha ~ dose_c), dat)
  expect_no_warning(beezdemand:::.validate_re_input(rp, dat, "id"))
})

test_that("guard warns when a continuous RE covariate is not centered", {
  # All subjects vary (so identifiability is fine) but the covariate mean is 3.
  dps <- lapply(1:20, function(i) c(2, 3, 4))
  dat <- .make_numeric_re_data(dps)
  rp <- .parse_re(nlme::pdSymm(Q0 + alpha ~ dose_c), dat)
  expect_warning(beezdemand:::.validate_re_input(rp, dat, "id"), "not centered")
})

test_that("guard preserves the factor message byte-for-byte (additivity)", {
  # Between-subject FACTOR term -> existing message must be unchanged.
  rows <- list()
  k <- 1L
  for (i in 1:20) {
    g <- if (i <= 10) "A" else "B"
    for (p in c(1, 5, 10)) {
      rows[[k]] <- data.frame(id = i, grp = g, x = p, y = 10 * exp(-0.01 * p),
                              stringsAsFactors = FALSE)
      k <- k + 1L
    }
  }
  dat <- do.call(rbind, rows)
  dat$id <- factor(dat$id)
  dat$grp <- factor(dat$grp)
  rp <- .parse_re(nlme::pdSymm(Q0 + alpha ~ grp), dat)
  expect_error(
    beezdemand:::.validate_re_input(rp, dat, "id"),
    "require a within-subject factor"
  )
})

# ---------------------------------------------------------------------------
# Guard re-timing: runs on the post-complete-case-filter data
# ---------------------------------------------------------------------------

test_that("guard re-times to post-filter data (within-subject pre-filter, between-subject after NA drop)", {
  skip_on_cran()
  dat <- .make_numeric_re_data(lapply(1:20, function(i) c(-1, 1)), prices = c(1, 5, 10))
  # Wipe out all dose_c == -1 observations via NA in y: after complete-case
  # filtering only dose_c == 1 remains -> dose_c becomes between-subjects.
  dat$y[dat$dose_c == -1] <- NA_real_
  expect_error(
    suppressWarnings(suppressMessages(fit_demand_tmb(
      dat, equation = "simplified",
      continuous_covariates = "dose_c",
      random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c),
      multi_start = FALSE, verbose = 0
    ))),
    "not estimable|continuous_covariates"
  )
})

# ---------------------------------------------------------------------------
# Numeric RE-RHS Z builder (current correct behavior -- lock it)
# ---------------------------------------------------------------------------

test_that("numeric RE-RHS builds the expected Z columns under ~ dose_c and ~ dose_c - 1", {
  dat <- beezdemand:::.simulate_continuous_re_demand(n_subjects = 5, seed = 7)

  rp1 <- .parse_re(nlme::pdSymm(Q0 + alpha ~ dose_c), dat)
  z1 <- beezdemand:::.tmb_build_z_matrices(rp1, dat, id_var = "id")
  expect_equal(colnames(z1$Z_q0), c("(Intercept)", "dose_c"))
  expect_equal(colnames(z1$Z_alpha), c("(Intercept)", "dose_c"))
  expect_equal(unname(z1$Z_q0[, "dose_c"]), dat$dose_c)
  expect_equal(z1$re_dim_q0, 2L)
  expect_equal(z1$re_dim_alpha, 2L)

  rp0 <- .parse_re(nlme::pdDiag(Q0 + alpha ~ dose_c - 1), dat, cov = "pdDiag")
  z0 <- beezdemand:::.tmb_build_z_matrices(rp0, dat, id_var = "id")
  expect_equal(colnames(z0$Z_q0), "dose_c")
  expect_equal(z0$re_dim_q0, 1L)
  expect_equal(unname(z0$Z_q0[, "dose_c"]), dat$dose_c)
})

# ---------------------------------------------------------------------------
# Slope-scaled start values
# ---------------------------------------------------------------------------

test_that("continuous-slope logsigma starts scale with covariate spread", {
  dat <- beezdemand:::.simulate_continuous_re_demand(
    n_subjects = 10, doses = c(-2, -1, 0, 1, 2), seed = 2
  )
  base <- log(0.5)
  rp <- .parse_re(nlme::pdSymm(Q0 + alpha ~ dose_c), dat)
  s <- beezdemand:::.tmb_re_logsigma_starts(rp, dat, base = base)
  # canonical order: Q0:(Intercept), Q0:dose_c, alpha:(Intercept), alpha:dose_c
  expect_length(s, 4L)
  expect_equal(s[c(1L, 3L)], c(base, base))         # intercepts at base
  spread <- stats::sd(dat$dose_c)
  expect_equal(s[c(2L, 4L)], rep(base - log(spread), 2L))  # slopes scaled

  # Larger spread => smaller (more negative) slope start.
  dat2 <- dat
  dat2$dose_c <- dat2$dose_c * 4
  rp2 <- .parse_re(nlme::pdSymm(Q0 + alpha ~ dose_c), dat2)
  s2 <- beezdemand:::.tmb_re_logsigma_starts(rp2, dat2, base = base)
  expect_lt(s2[2L], s[2L])
})

test_that("logsigma starts are base for intercept-only and factor fits (additivity)", {
  base <- log(0.5)
  dat <- beezdemand:::.simulate_continuous_re_demand(n_subjects = 6, seed = 3)
  rp_int <- .parse_re(nlme::pdSymm(Q0 + alpha ~ 1), dat)
  expect_equal(beezdemand:::.tmb_re_logsigma_starts(rp_int, dat, base), rep(base, 2L))

  fdat <- beezdemand:::.simulate_within_subject_demand(
    n_subjects = 6, n_conditions = 3, seed = 4
  )
  rp_fac <- .parse_re(nlme::pdSymm(Q0 + alpha ~ condition), fdat)
  s_fac <- beezdemand:::.tmb_re_logsigma_starts(rp_fac, fdat, base)
  expect_true(all(s_fac == base))
  # NULL data -> all base (defensive default)
  expect_equal(beezdemand:::.tmb_re_logsigma_starts(rp_int, NULL, base), rep(base, 2L))
})

# ---------------------------------------------------------------------------
# Parameter recovery (SND / simplified -- DGP-matching equation)
# ---------------------------------------------------------------------------

test_that("continuous RE slope recovers fixed slopes + RE SDs on simplified (SND)", {
  skip_on_cran()
  dat <- beezdemand:::.simulate_continuous_re_demand(
    n_subjects = 60, doses = c(-2, -1, 0, 1, 2),
    prices = c(0, 1, 2, 4, 8, 16),
    log_q0_pop = log(20), log_alpha_pop = log(0.006),
    b1_q0 = 0.10, b1_alpha = -0.15,
    sd_u_q0 = 0.30, sd_w_q0 = 0.10, sd_u_alpha = 0.30, sd_w_alpha = 0.10,
    rho_q0 = 0.3, rho_alpha = 0.3, sigma_e = 0.05, seed = 20260620
  )
  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    dat, equation = "simplified",
    continuous_covariates = "dose_c",
    random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c),
    multi_start = FALSE, verbose = 0
  )))
  expect_true(isTRUE(fit$converged))

  co <- coef(fit)
  beta_q0 <- unname(co[names(co) == "beta_q0"])
  beta_alpha <- unname(co[names(co) == "beta_alpha"])
  logsig <- unname(co[names(co) == "logsigma"])

  # Fixed dose slopes (log scale): position 2 = the dose_c slope. Absolute
  # bands chosen for the estimator's sampling variability at this n plus the
  # known small bias from the lognormal-noise / Gaussian-likelihood mismatch
  # (landmine #5); a broken slope would be far outside these.
  expect_lt(abs(beta_q0[2] - 0.10), 0.05)
  expect_lt(abs(beta_alpha[2] - (-0.15)), 0.06)
  # Population intercepts at reference dose (dose_c = 0).
  expect_lt(abs(beta_q0[1] - log(20)), 0.15)
  expect_lt(abs(beta_alpha[1] - log(0.006)), 0.20)

  # RE SDs on the natural-log-parameter scale (exp of logsigma). Order:
  # Q0:int, Q0:slope, alpha:int, alpha:slope (mean-structure recovery only;
  # the lognormal-noise residual SD is intentionally not checked -- landmine #5).
  re_sd <- exp(logsig)
  expect_lt(abs(re_sd[1] - 0.30), 0.10)  # sd_u_q0
  expect_lt(abs(re_sd[2] - 0.10), 0.05)  # sd_w_q0
  expect_lt(abs(re_sd[3] - 0.30), 0.10)  # sd_u_alpha
  expect_lt(abs(re_sd[4] - 0.10), 0.05)  # sd_w_alpha
})

test_that("fixed dose-response slope requires continuous_covariates AND the RE slope", {
  skip_on_cran()
  dat <- beezdemand:::.simulate_continuous_re_demand(
    n_subjects = 50, doses = c(-2, -1, 0, 1, 2),
    prices = c(0, 1, 2, 4, 8, 16),
    b1_q0 = 0.10, b1_alpha = -0.15, seed = 99
  )
  # With the fixed term present, the dose slope on Q0 is recovered.
  fit_both <- suppressWarnings(suppressMessages(fit_demand_tmb(
    dat, equation = "simplified",
    continuous_covariates = "dose_c",
    random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c),
    multi_start = FALSE, verbose = 0
  )))
  co <- coef(fit_both)
  beta_q0 <- unname(co[names(co) == "beta_q0"])
  expect_equal(length(beta_q0), 2L)  # intercept + dose_c
  expect_lt(abs(beta_q0[2] - 0.10), 0.05)

  # Random-only slope (no continuous_covariates): the FIXED design has no
  # dose_c column, so there is a single beta_q0 (intercept only).
  fit_re_only <- suppressWarnings(suppressMessages(fit_demand_tmb(
    dat, equation = "simplified",
    random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c),
    multi_start = FALSE, verbose = 0
  )))
  co2 <- coef(fit_re_only)
  expect_equal(length(unname(co2[names(co2) == "beta_q0"])), 1L)
})

test_that("continuous RE slope fits, converges, and reports the slope on all four equations", {
  skip_on_cran()
  base <- beezdemand:::.simulate_continuous_re_demand(
    n_subjects = 30, doses = c(-2, -1, 0, 1, 2),
    prices = c(0, 1, 2, 4, 8, 16),
    b1_q0 = 0.10, b1_alpha = -0.15, seed = 4242
  )
  base$y_ll4 <- ll4(base$y)  # zben fits the LL4-transformed column

  for (eq in c("simplified", "exponentiated", "exponential", "zben")) {
    yv <- if (eq == "zben") "y_ll4" else "y"
    fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
      base, equation = eq, y_var = yv,
      continuous_covariates = "dose_c",
      random_effects = nlme::pdDiag(Q0 + alpha ~ dose_c),
      multi_start = FALSE, verbose = 0
    )))
    expect_true(isTRUE(fit$converged), info = paste("equation:", eq))
    # The dose_c slope must surface in ranef() for every equation.
    rn <- ranef(fit)
    expect_true("q0_dose_c" %in% names(rn), info = paste("equation:", eq))
    expect_true("alpha_dose_c" %in% names(rn), info = paste("equation:", eq))
    # Q0 dose slope recovers (Q0 = Q at C = 0 is shared across the
    # exponential-family equations; zben is on the LL4 scale so only the
    # machinery/convergence is asserted there).
    if (eq != "zben") {
      co <- coef(fit)
      beta_q0 <- unname(co[names(co) == "beta_q0"])
      expect_true(abs(beta_q0[2] - 0.10) < 0.10, info = paste("equation:", eq))
    }
  }
})

# ---------------------------------------------------------------------------
# nlme equivalence oracle
# ---------------------------------------------------------------------------

test_that("TMB continuous RE slope agrees with the nlme oracle (fit_demand_mixed)", {
  skip_on_cran()
  dat <- beezdemand:::.simulate_continuous_re_demand(
    n_subjects = 50, doses = c(-2, -1, 0, 1, 2),
    prices = c(0, 1, 2, 4, 8, 16),
    b1_q0 = 0.10, b1_alpha = -0.15, seed = 313
  )

  fit_tmb <- suppressWarnings(suppressMessages(fit_demand_tmb(
    dat, equation = "simplified",
    continuous_covariates = "dose_c",
    random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c),
    multi_start = FALSE, verbose = 0
  )))
  co <- coef(fit_tmb)
  # TMB parameterizes log Q0 / log alpha (natural log).
  tmb_q0_slope <- unname(co[names(co) == "beta_q0"])[2]
  tmb_alpha_slope <- unname(co[names(co) == "beta_alpha"])[2]

  # nlme oracle: identical log-linear dose-response, but param_space = "log10"
  # parameterizes log10(Q0)/log10(alpha). Convert its slopes to natural log
  # (x ln(10)) before comparing -- the two backends fit the same model.
  fit_nlme <- tryCatch(
    suppressWarnings(suppressMessages(fit_demand_mixed(
      dat, y_var = "y", x_var = "x", id_var = "id",
      equation_form = "simplified", param_space = "log10",
      continuous_covariates = "dose_c",
      random_effects = Q0 + alpha ~ dose_c,
      covariance_structure = "pdSymm"
    ))),
    error = function(e) NULL
  )
  skip_if(is.null(fit_nlme), "nlme oracle did not fit")

  fx <- nlme::fixef(fit_nlme$model)
  nlme_q0_slope <- unname(fx[["Q0.dose_c"]]) * log(10)
  nlme_alpha_slope <- unname(fx[["alpha.dose_c"]]) * log(10)

  expect_lt(abs(tmb_q0_slope - nlme_q0_slope), 0.02)
  expect_lt(abs(tmb_alpha_slope - nlme_alpha_slope), 0.03)
})

# ---------------------------------------------------------------------------
# Phase 2 reporting: subject-level slope exposure + at/newdata reconciliation
# ---------------------------------------------------------------------------

.fit_cont_re <- function(seed = 808, n = 30) {
  dat <- beezdemand:::.simulate_continuous_re_demand(
    n_subjects = n, doses = c(-2, -1, 0, 1, 2),
    prices = c(0, 1, 2, 4, 8, 16),
    b1_q0 = 0.10, b1_alpha = -0.15, seed = seed
  )
  suppressWarnings(suppressMessages(fit_demand_tmb(
    dat, equation = "simplified",
    continuous_covariates = "dose_c",
    random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c),
    multi_start = FALSE, verbose = 0
  )))
}

test_that("get_subject_pars surfaces per-subject slope columns and finite Q0/alpha", {
  skip_on_cran()
  fit <- .fit_cont_re()
  sp <- get_subject_pars(fit)
  # Per-subject slope deviation columns are present (matching ranef naming).
  expect_true(all(c("q0_dose_c", "alpha_dose_c") %in% names(sp)))
  expect_true(all(is.finite(sp$q0_dose_c)))
  expect_true(all(is.finite(sp$alpha_dose_c)))
  # Q0/alpha are evaluated at the reference (dose_c = 0), not NA-collapsed.
  expect_true(all(is.finite(sp$Q0)))
  expect_true(all(is.finite(sp$alpha)))
  # Slope columns equal ranef()'s slope columns exactly.
  rn <- ranef(fit)
  m <- match(as.character(sp$id), as.character(rn$id))
  expect_equal(sp$q0_dose_c, rn$q0_dose_c[m], tolerance = 1e-8)
  expect_equal(sp$alpha_dose_c, rn$alpha_dose_c[m], tolerance = 1e-8)
})

test_that("predict(type='parameters') reconciles with get_subject_pars (no NA)", {
  skip_on_cran()
  fit <- .fit_cont_re()
  pp <- predict(fit, type = "parameters")
  sp <- get_subject_pars(fit)
  expect_true(all(is.finite(pp$Q0)))
  expect_true(all(is.finite(pp$alpha)))
  expect_true(all(c("q0_dose_c", "alpha_dose_c") %in% names(pp)))
  # Same Q0/alpha as get_subject_pars (the two surfaces agree).
  expect_equal(pp$Q0, sp$Q0, tolerance = 1e-8)
  expect_equal(pp$alpha, sp$alpha, tolerance = 1e-8)
})

test_that("get_subject_pars(at=) evaluates per-subject parameters at a covariate value", {
  skip_on_cran()
  fit <- .fit_cont_re()
  co <- coef(fit)
  b1_q0 <- unname(co[names(co) == "beta_q0"])[2]
  b1_alpha <- unname(co[names(co) == "beta_alpha"])[2]

  sp0 <- get_subject_pars(fit, at = c(dose_c = 0))  # explicit reference
  sp2 <- get_subject_pars(fit, at = c(dose_c = 2))  # dose_c = 2

  # Default (at = NULL) conditions at the subject mean; for this balanced
  # centered design the subject mean equals the reference (dose_c = 0).
  sp_def <- get_subject_pars(fit)
  expect_equal(sp_def$Q0, sp0$Q0, tolerance = 1e-8)

  # Different evaluation point -> different per-subject Q0/alpha.
  expect_false(isTRUE(all.equal(sp0$Q0, sp2$Q0)))

  # log(Q0_i(2) / Q0_i(0)) == 2 * (fixed slope + subject slope deviation).
  m <- match(as.character(sp2$id), as.character(sp0$id))
  expect_equal(
    log(sp2$Q0 / sp0$Q0[m]),
    2 * (b1_q0 + sp0$q0_dose_c[m]),
    tolerance = 1e-6
  )
  expect_equal(
    log(sp2$alpha / sp0$alpha[m]),
    2 * (b1_alpha + sp0$alpha_dose_c[m]),
    tolerance = 1e-6
  )
})

test_that("predict(type='response', newdata=) evaluates the subject curve at row covariate values", {
  skip_on_cran()
  fit <- .fit_cont_re()
  # Same subject, same price, two dose values -> different fitted consumption
  # (the random + fixed dose slope move the curve).
  nd <- data.frame(
    id = factor(c("1", "1"), levels = levels(fit$data$id)),
    dose_c = c(-2, 2), x = c(4, 4)
  )
  pr <- predict(fit, newdata = nd, type = "response")
  expect_equal(nrow(pr), 2L)
  expect_false(isTRUE(all.equal(pr$.fitted[1], pr$.fitted[2])))
})

# ---------------------------------------------------------------------------
# Phase 2 reporting: covariate-term-name variance-component labels
# ---------------------------------------------------------------------------

test_that("variance-component labels name the continuous RE term (summary/tidy/VarCorr)", {
  skip_on_cran()
  fit <- .fit_cont_re()
  s <- summary(fit)
  comp <- s$variance_components$Component

  # Individual-parameter summaries are finite (not NA-collapsed) for a
  # continuous fit (Codex Recommended 4).
  expect_true(all(is.finite(s$individual_metrics$Q0)))
  expect_true(all(is.finite(s$individual_metrics$alpha)))

  # Slope SDs named with the covariate term (not positional sigma_b[2]/sigma_c[2]).
  expect_true(any(grepl("sigma_b\\[dose_c\\]", comp)))
  expect_true(any(grepl("sigma_c\\[dose_c\\]", comp)))
  expect_false(any(grepl("sigma_b\\[2\\]", comp)))
  expect_false(any(grepl("sigma_c\\[2\\]", comp)))
  # Intercept SD keeps the positional index (additive with factor fits).
  expect_true(any(grepl("sigma_b\\[1\\]", comp)))

  # Intercept-slope correlations name the covariate term.
  expect_true(any(grepl("dose_c", s$correlations$Component)))

  # tidy(effects = "ran_pars") carries the same labels.
  td <- tidy(fit, effects = "ran_pars")
  expect_true(any(grepl("dose_c", as.character(unlist(td)))))

  # VarCorr row names include the term.
  vc <- VarCorr(fit)
  expect_true(any(grepl("dose_c", rownames(vc))))
})

# ---------------------------------------------------------------------------
# Strict additivity: factor / intercept-only fits show NO continuous artifacts
# ---------------------------------------------------------------------------

test_that("additivity: intercept-only and factor fits keep historical reporting", {
  skip_on_cran()

  # Intercept-only: no slope columns, bracket-free SD labels, `at` warns.
  fit_int <- suppressWarnings(suppressMessages(fit_demand_tmb(
    beezdemand::apt, equation = "simplified",
    random_effects = nlme::pdSymm(Q0 + alpha ~ 1),
    multi_start = FALSE, verbose = 0
  )))
  sp <- get_subject_pars(fit_int)
  expect_false(any(grepl("^(q0|alpha)_", names(sp))))
  comp_int <- summary(fit_int)$variance_components$Component
  expect_true(any(grepl("^sigma_b \\(Q0 RE SD\\)$", comp_int)))
  expect_false(any(grepl("\\[", comp_int)))  # no positional/named brackets
  expect_warning(get_subject_pars(fit_int, at = c(dose_c = 1)), "no continuous")
  # predict(type='parameters') unchanged (no slope columns).
  pp_int <- predict(fit_int, type = "parameters")
  expect_false(any(grepl("^(q0|alpha)_", names(pp_int))))

  # Within-id FACTOR: positional variance labels retained, NOT term-named.
  fdat <- beezdemand:::.simulate_within_subject_demand(
    n_subjects = 16, n_conditions = 3, seed = 11
  )
  fit_fac <- suppressWarnings(suppressMessages(fit_demand_tmb(
    fdat, equation = "simplified", factors = "condition",
    random_effects = nlme::pdDiag(Q0 + alpha ~ condition),
    multi_start = FALSE, verbose = 0
  )))
  comp_fac <- summary(fit_fac)$variance_components$Component
  expect_true(any(grepl("sigma_b\\[1\\]", comp_fac)))
  expect_false(any(grepl("sigma_b\\[condition", comp_fac)))
})

# ---------------------------------------------------------------------------
# Phase 2 diagnostics: TMB near-singular intercept/slope covariance
# ---------------------------------------------------------------------------

test_that("check_demand_model flags a near-singular intercept/slope covariance", {
  skip_on_cran()
  fit <- .fit_cont_re()

  # A well-behaved fit (rho = 0.3 DGP) is not flagged.
  diag_ok <- check_demand_model(fit)
  expect_false(isTRUE(diag_ok$random_effects$near_singular))

  # Force a near-singular Q0 intercept/slope correlation: the first rho_raw is
  # the [2,1] partial = marginal correlation; tanh(6) ~ 1.
  fit_deg <- fit
  idx <- which(names(fit_deg$model$coefficients) == "rho_raw")
  fit_deg$model$coefficients[idx[1]] <- 6
  diag_deg <- check_demand_model(fit_deg)
  expect_true(isTRUE(diag_deg$random_effects$near_singular))
  expect_true(any(grepl("near-singular", diag_deg$issues, ignore.case = TRUE)))
})

# ---------------------------------------------------------------------------
# Codex review follow-ups (TICKET-051)
# ---------------------------------------------------------------------------

test_that("diagnostics additivity: factor/intercept fits keep the historical random_effects shape", {
  skip_on_cran()
  fit_int <- suppressWarnings(suppressMessages(fit_demand_tmb(
    beezdemand::apt, equation = "simplified",
    random_effects = nlme::pdSymm(Q0 + alpha ~ 1),
    multi_start = FALSE, verbose = 0
  )))
  re_int <- check_demand_model(fit_int)$random_effects
  expect_setequal(names(re_int), c("variances", "near_zero", "sd_internal_log"))
  expect_false("near_singular" %in% names(re_int))

  # A continuous fit DOES gain the near_singular field.
  re_cont <- check_demand_model(.fit_cont_re())$random_effects
  expect_true("near_singular" %in% names(re_cont))
})

test_that("guard validates the exponential equation's positive-consumption rows", {
  skip_on_cran()
  # dose_c varies within subject only among zero-consumption rows; the
  # exponential equation drops y == 0, leaving dose_c constant per subject, so
  # the guard (run on the modeled rows) must catch it.
  rows <- list(); k <- 1L
  for (i in 1:20) {
    for (p in c(1, 5, 10)) {
      rows[[k]] <- data.frame(id = i, dose_c = -1, x = p, y = 0); k <- k + 1L
    }
    for (p in c(1, 5, 10)) {
      rows[[k]] <- data.frame(id = i, dose_c = 1, x = p, y = 10 * exp(-0.01 * p)); k <- k + 1L
    }
  }
  dat <- do.call(rbind, rows); dat$id <- factor(dat$id)
  expect_error(
    suppressWarnings(suppressMessages(fit_demand_tmb(
      dat, equation = "exponential", continuous_covariates = "dose_c",
      random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c),
      multi_start = FALSE, verbose = 0
    ))),
    "not estimable|continuous_covariates"
  )
})

test_that("predict(type='parameters') surfaces slopes even with validate_subject_pars = FALSE", {
  skip_on_cran()
  dat <- beezdemand:::.simulate_continuous_re_demand(n_subjects = 30, seed = 55)
  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    dat, equation = "simplified", continuous_covariates = "dose_c",
    random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c),
    validate_subject_pars = FALSE, multi_start = FALSE, verbose = 0
  )))
  pp <- predict(fit, type = "parameters")
  expect_true(all(c("q0_dose_c", "alpha_dose_c") %in% names(pp)))
  expect_true(all(is.finite(pp$Q0)))
})

test_that("get_subject_pars default conditions a continuous slope at the subject mean (uncentered)", {
  skip_on_cran()
  # Uncentered dose ladder: subject mean = 2, NOT the reference 0.
  dat <- beezdemand:::.simulate_continuous_re_demand(
    n_subjects = 40, doses = c(0, 1, 2, 3, 4),
    prices = c(0, 1, 2, 4, 8), seed = 7
  )
  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    dat, equation = "simplified", continuous_covariates = "dose_c",
    random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c),
    multi_start = FALSE, verbose = 0
  )))
  sp_def  <- get_subject_pars(fit)                   # default = subject mean (2)
  sp_mean <- get_subject_pars(fit, at = c(dose_c = 2))
  sp_ref  <- get_subject_pars(fit, at = c(dose_c = 0))
  expect_equal(sp_def$Q0, sp_mean$Q0, tolerance = 1e-8)    # default == subject mean
  expect_false(isTRUE(all.equal(sp_def$Q0, sp_ref$Q0)))    # != reference 0
})

test_that("near-singular message is generic for a slope-only block (no false intercept claim)", {
  skip_on_cran()
  dat <- beezdemand:::.simulate_continuous_re_demand(n_subjects = 30, seed = 909)
  fit <- suppressWarnings(suppressMessages(fit_demand_tmb(
    dat, equation = "simplified", continuous_covariates = "dose_c",
    random_effects = nlme::pdSymm(Q0 + alpha ~ dose_c - 1),  # slopes only, no RE intercept
    multi_start = FALSE, verbose = 0
  )))
  # Force the Q0-slope / alpha-slope correlation near 1 (the single rho_raw).
  idx <- which(names(fit$model$coefficients) == "rho_raw")
  fit$model$coefficients[idx[1]] <- 6
  diag <- check_demand_model(fit)
  expect_true(isTRUE(diag$random_effects$near_singular))
  ns_issue <- diag$issues[grepl("near-singular", diag$issues, ignore.case = TRUE)]
  expect_length(ns_issue, 1L)
  # The block has no intercept RE, so the message must not claim "intercept".
  expect_false(grepl("intercept", ns_issue, ignore.case = TRUE))
})
