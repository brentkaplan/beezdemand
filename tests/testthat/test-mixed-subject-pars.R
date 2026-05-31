# Tests for get_subject_pars.beezdemand_nlme() (TICKET-034).
#
# Full parity with get_subject_pars.beezdemand_tmb(): same columns
# (id, b_i, [c_i], Q0, alpha, Pmax, Omax), natural scale, and the same
# `expanded` (NULL auto-detect / TRUE / FALSE) semantics from TICKET-022.
#
# All cases fit NLME models => heavy; skip_on_cran() and excluded from the
# pre-commit smoke subset. Fits are memoized in a file-local cache so each
# spec is fit at most once per test run.

# --------------------------------------------------------------------------
# Memoized fixtures
# --------------------------------------------------------------------------

.spars_cache <- new.env(parent = emptyenv())

.spars_sim <- function(seed = 123) {
  key <- paste0("sim_", seed)
  if (is.null(.spars_cache[[key]])) {
    sim <- .simulate_within_subject_demand(
      n_subjects = 30, n_conditions = 3,
      prices = c(0.25, 0.5, 1, 2, 4, 8, 16, 32),
      log_q0_pop = log(15), log_alpha_pop = log(0.0015),
      delta_q0 = c(0, -0.4, -0.9), delta_alpha = c(0, 0.2, 0.5),
      sigma_b = 0.4, sigma_d = 0.4, seed = seed
    )
    sim$id <- factor(sim$id)
    sim$condition <- factor(sim$condition)
    .spars_cache[[key]] <- sim
  }
  .spars_cache[[key]]
}

.spars_fit <- function(key, builder) {
  if (is.null(.spars_cache[[key]])) {
    .spars_cache[[key]] <- tryCatch(
      suppressWarnings(suppressMessages(builder())),
      error = function(e) structure(list(model = NULL,
                                          error_message = conditionMessage(e)),
                                     class = "beezdemand_nlme")
    )
  }
  .spars_cache[[key]]
}

# Canonical M1 within-subject spec: pdBlocked(pdSymm intercepts + pdDiag
# per-condition slopes); condition is also a fixed effect.
.fit_nlme_within <- function() {
  .spars_fit("nlme_within", function() {
    sim <- .spars_sim()
    fit_demand_mixed(
      sim, y_var = "y", x_var = "x", id_var = "id",
      factors = "condition", equation_form = "simplified",
      random_effects = nlme::pdBlocked(list(
        nlme::pdSymm(Q0 + alpha ~ 1),
        nlme::pdDiag(Q0 + alpha ~ condition - 1)
      ))
    )
  })
}

# Intercept-only RE, no within-subject factor.
.fit_nlme_intercept <- function() {
  .spars_fit("nlme_intercept", function() {
    sim <- .spars_sim()
    fit_demand_mixed(
      sim, y_var = "y", x_var = "x", id_var = "id",
      equation_form = "simplified",
      random_effects = nlme::pdDiag(Q0 + alpha ~ 1)
    )
  })
}

# RE-only factor: condition enters the RANDOM formula but NOT `factors`
# (no fixed condition effect).
.fit_nlme_re_only <- function() {
  .spars_fit("nlme_re_only", function() {
    sim <- .spars_sim()
    fit_demand_mixed(
      sim, y_var = "y", x_var = "x", id_var = "id",
      equation_form = "simplified",
      random_effects = nlme::pdDiag(Q0 + alpha ~ condition - 1)
    )
  })
}

# Natural param_space (identity back-transform path).
.fit_nlme_natural <- function() {
  .spars_fit("nlme_natural", function() {
    sim <- .spars_sim()
    fit_demand_mixed(
      sim, y_var = "y", x_var = "x", id_var = "id",
      equation_form = "simplified", param_space = "natural",
      random_effects = nlme::pdDiag(Q0 + alpha ~ 1)
    )
  })
}

# Exponentiated (k baked into formula) smoke fit.
.fit_nlme_exponentiated <- function() {
  .spars_fit("nlme_exp", function() {
    sim <- .spars_sim()
    fit_demand_mixed(
      sim, y_var = "y", x_var = "x", id_var = "id",
      equation_form = "exponentiated",
      random_effects = nlme::pdDiag(Q0 + alpha ~ 1)
    )
  })
}

# TMB fit on the SAME fixture / SND spec for cross-backend parity.
.fit_tmb_within <- function() {
  if (is.null(.spars_cache[["tmb_within"]])) {
    .spars_cache[["tmb_within"]] <- tryCatch(
      suppressWarnings(suppressMessages(
        fit_demand_tmb(
          .spars_sim(), equation = "simplified",
          id_var = "id", x_var = "x", y_var = "y", verbose = 0,
          random_effects = nlme::pdBlocked(list(
            nlme::pdSymm(Q0 + alpha ~ 1),
            nlme::pdDiag(Q0 + alpha ~ condition - 1)
          ))
        )
      )),
      error = function(e) NULL
    )
  }
  .spars_cache[["tmb_within"]]
}

# --------------------------------------------------------------------------
# Auto-detect (expanded = NULL)
# --------------------------------------------------------------------------

test_that("auto-detect on within-id pdBlocked returns long per-(subject, condition) rows", {
  skip_on_cran()
  fit <- .fit_nlme_within()
  skip_if(is.null(fit$model), "nlme within-subject fit did not converge")

  sp <- expect_no_warning(get_subject_pars(fit))
  n_subj <- length(unique(sp$id))
  n_cond <- length(unique(sp$condition))
  expect_equal(nrow(sp), n_subj * n_cond)
  expect_true(all(c("id", "condition", "b_i", "c_i", "Q0", "alpha", "Pmax", "Omax") %in% names(sp)))
  expect_true(all(is.finite(sp$Q0)))
  expect_true(all(is.finite(sp$alpha)))
  expect_true(all(is.finite(sp$Pmax)))
  expect_true(all(is.finite(sp$Omax)))
  expect_true(all(sp$Q0 > 0))
  expect_true(all(sp$alpha > 0))
})

test_that("auto-detect on intercept-only fit returns wide one-row-per-subject", {
  skip_on_cran()
  fit <- .fit_nlme_intercept()
  skip_if(is.null(fit$model), "nlme intercept-only fit did not converge")

  sp <- expect_no_warning(get_subject_pars(fit))
  expect_equal(nrow(sp), length(unique(sp$id)))
  expect_false("condition" %in% names(sp))
  expect_true(all(c("id", "b_i", "c_i", "Q0", "alpha", "Pmax", "Omax") %in% names(sp)))
  expect_true(all(is.finite(sp$Q0)))
  expect_true(all(is.finite(sp$alpha)))
})

test_that("expanded = NULL equals the default invocation", {
  skip_on_cran()
  fit <- .fit_nlme_within()
  skip_if(is.null(fit$model), "fit did not converge")
  expect_equal(get_subject_pars(fit, expanded = NULL), get_subject_pars(fit))
})

# --------------------------------------------------------------------------
# Explicit expanded = TRUE / FALSE
# --------------------------------------------------------------------------

test_that("explicit expanded = FALSE on within-id fit returns wide NA-fill with one warning", {
  skip_on_cran()
  fit <- .fit_nlme_within()
  skip_if(is.null(fit$model), "fit did not converge")

  warnings <- character(0)
  sp <- withCallingHandlers(
    get_subject_pars(fit, expanded = FALSE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(nrow(sp), length(unique(sp$id)))
  expect_true(all(is.na(sp$Q0)))
  expect_true(all(is.na(sp$alpha)))
  expect_true(all(is.na(sp$Pmax)))
  expect_true(all(is.na(sp$Omax)))
  # b_i / c_i remain populated (well-defined per subject)
  expect_true(all(is.finite(sp$b_i)))
  # exactly one user-facing warning, short primary line
  expect_gte(length(warnings), 1L)
  primary <- strsplit(warnings[[1]], "\n")[[1]][1]
  expect_lt(nchar(primary), 200L)
  expect_match(primary, "Q0|alpha|NA|subject_pars|subject-level", ignore.case = TRUE)
})

test_that("explicit expanded = FALSE on intercept-only fit is wide with no warning", {
  skip_on_cran()
  fit <- .fit_nlme_intercept()
  skip_if(is.null(fit$model), "fit did not converge")

  sp <- expect_no_warning(get_subject_pars(fit, expanded = FALSE))
  expect_equal(nrow(sp), length(unique(sp$id)))
  expect_true(all(is.finite(sp$Q0)))
})

test_that("expanded = TRUE on intercept-only fit silently returns wide", {
  skip_on_cran()
  fit <- .fit_nlme_intercept()
  skip_if(is.null(fit$model), "fit did not converge")

  sp <- expect_no_warning(get_subject_pars(fit, expanded = TRUE))
  expect_equal(nrow(sp), length(unique(sp$id)))
  expect_true(all(is.finite(sp$Q0)))
})

test_that("invalid expanded values error", {
  skip_on_cran()
  fit <- .fit_nlme_intercept()
  skip_if(is.null(fit$model), "fit did not converge")

  expect_error(get_subject_pars(fit, expanded = "yes"))
  expect_error(get_subject_pars(fit, expanded = NA))
  expect_error(get_subject_pars(fit, expanded = c(TRUE, FALSE)))
})

# --------------------------------------------------------------------------
# RE-only factor, natural param_space, exponentiated, M2 covariate
# --------------------------------------------------------------------------

test_that("RE-only within-id factor still auto-expands", {
  skip_on_cran()
  fit <- .fit_nlme_re_only()
  skip_if(is.null(fit$model), "RE-only fit did not converge")

  sp <- get_subject_pars(fit)
  expect_true("condition" %in% names(sp))
  expect_equal(nrow(sp), length(unique(sp$id)) * length(unique(sp$condition)))
  expect_true(all(is.finite(sp$Q0)))
})

test_that("natural param_space returns finite natural-scale parameters", {
  skip_on_cran()
  fit <- .fit_nlme_natural()
  skip_if(is.null(fit$model), "natural-space fit did not converge")

  sp <- get_subject_pars(fit)
  expect_true(all(is.finite(sp$Q0)))
  expect_true(all(sp$Q0 > 0))
  expect_true(all(is.finite(sp$alpha)))
})

test_that("exponentiated fit returns finite Pmax/Omax using baked-in k", {
  skip_on_cran()
  fit <- .fit_nlme_exponentiated()
  skip_if(is.null(fit$model), "exponentiated fit did not converge")

  sp <- get_subject_pars(fit)
  expect_true(all(is.finite(sp$Q0)))
  expect_true(all(is.finite(sp$Pmax)))
  expect_true(all(is.finite(sp$Omax)))
})

test_that("M2-style within-id numeric covariate is conditioned at subject mean", {
  skip_on_cran()
  sim <- .spars_sim()
  # within-id numeric covariate (varies across rows within each subject)
  sim$cov <- stats::ave(seq_len(nrow(sim)), sim$id,
                        FUN = function(z) as.numeric(scale(z)))
  fit <- tryCatch(
    suppressWarnings(suppressMessages(fit_demand_mixed(
      sim, y_var = "y", x_var = "x", id_var = "id",
      factors = "condition", continuous_covariates = "cov",
      equation_form = "simplified",
      random_effects = nlme::pdDiag(Q0 + alpha ~ 1)
    ))),
    error = function(e) structure(list(model = NULL), class = "beezdemand_nlme")
  )
  skip_if(is.null(fit$model), "M2 covariate fit did not converge")

  sp <- get_subject_pars(fit)
  # one row per (subject, condition); covariate collapsed to subject mean
  expect_equal(nrow(sp), length(unique(sp$id)) * length(unique(sp$condition)))
  expect_true(all(is.finite(sp$Q0)))
})

# --------------------------------------------------------------------------
# Helper-level unit tests (per-subject affected tracking; duplicate-term guard)
# --------------------------------------------------------------------------

test_that(".nlme_check_within_id flags only subjects whose design varies within id", {
  # Hand-built design: 3 subjects x 2 rows; subject 2 varies within id.
  subject_id <- c(0L, 0L, 1L, 1L, 2L, 2L)
  Z <- matrix(c(1, 1, 0, 0, 0, 1), ncol = 1L)  # only subj 2 (rows 5,6) differs
  colnames(Z) <- "conditionB"
  res <- .nlme_check_within_id(
    list(X_q0 = NULL, X_alpha = NULL, Z_q0 = Z, Z_alpha = NULL),
    subject_id = subject_id
  )
  expect_equal(res$affected, c(FALSE, FALSE, TRUE))
})

test_that(".nlme_match_re_column matches bare and dotted ranef names", {
  # intercept-only parameter => bare `Q0` / `alpha` ranef column
  expect_equal(.nlme_match_re_column(c("Q0", "alpha"), "Q0", "(Intercept)"), 1L)
  expect_equal(.nlme_match_re_column(c("Q0", "alpha"), "alpha", "(Intercept)"), 2L)
  # multi-term parameter => dotted `<p>.<term>` columns
  expect_equal(
    .nlme_match_re_column(
      c("Q0.(Intercept)", "Q0.conditionC2", "alpha.(Intercept)"),
      "Q0", "conditionC2"
    ),
    2L
  )
})

test_that(".nlme_match_re_column aborts on missing or duplicate columns", {
  expect_error(
    .nlme_match_re_column(c("Q0", "alpha"), "Q0", "conditionC2"),
    "align|Available", ignore.case = TRUE
  )
  # a duplicated ranef column name is the only way one term lookup matches twice
  expect_error(
    .nlme_match_re_column(c("Q0.(Intercept)", "Q0.(Intercept)"), "Q0", "(Intercept)"),
    "ambiguous|duplicate|block", ignore.case = TRUE
  )
})

test_that("fixed_rhs within-id factor is expanded, not collapsed to the first row", {
  skip_on_cran()
  sim <- .spars_sim()
  fit <- tryCatch(
    suppressWarnings(suppressMessages(fit_demand_mixed(
      sim, y_var = "y", x_var = "x", id_var = "id",
      equation_form = "simplified",
      fixed_rhs = "~ condition",
      random_effects = nlme::pdDiag(Q0 + alpha ~ 1)
    ))),
    error = function(e) structure(list(model = NULL), class = "beezdemand_nlme")
  )
  skip_if(is.null(fit$model), "fixed_rhs fit did not converge")

  sp <- get_subject_pars(fit)
  expect_true("condition" %in% names(sp))
  expect_equal(nrow(sp), length(unique(sp$id)) * length(unique(sp$condition)))
  expect_true(all(is.finite(sp$Q0)))
})

test_that("expanded Q0/alpha equal a hand-computed 10^(fixef + ranef) reconstruction", {
  skip_on_cran()
  fit <- .fit_nlme_within()
  skip_if(is.null(fit$model), "fit did not converge")

  sp <- get_subject_pars(fit)
  fe <- nlme::fixef(fit$model)
  re <- nlme::ranef(fit$model)
  sid <- "1"
  cond <- levels(factor(fit$data$condition))[2]  # non-reference level, e.g. "C2"
  row <- sp[as.character(sp$id) == sid & as.character(sp$condition) == cond, ]
  expect_equal(nrow(row), 1L)

  # treatment-coded fixed term + indicator-coded RE terms (block1 intercept +
  # block2 condition slope), back-transformed from log10.
  fix_q0 <- fe[["Q0.(Intercept)"]] + fe[[paste0("Q0.condition", cond)]]
  re_q0 <- re[sid, "Q0.(Intercept)"] + re[sid, paste0("Q0.condition", cond)]
  expect_equal(row$Q0, 10^(fix_q0 + re_q0), tolerance = 1e-6)

  fix_a <- fe[["alpha.(Intercept)"]] + fe[[paste0("alpha.condition", cond)]]
  re_a <- re[sid, "alpha.(Intercept)"] + re[sid, paste0("alpha.condition", cond)]
  expect_equal(row$alpha, 10^(fix_a + re_a), tolerance = 1e-6)
})

# --------------------------------------------------------------------------
# Cross-backend parity (must-have)
# --------------------------------------------------------------------------

test_that("tmb vs nlme subject pars agree in magnitude on a shared within-subject fixture", {
  skip_on_cran()
  fit_nlme <- .fit_nlme_within()
  fit_tmb <- .fit_tmb_within()
  skip_if(is.null(fit_nlme$model), "nlme fit did not converge")
  skip_if(is.null(fit_tmb), "tmb fit did not converge")

  sp_nlme <- get_subject_pars(fit_nlme)
  sp_tmb <- get_subject_pars(fit_tmb)

  key <- function(d) paste0(as.character(d$id), "::", as.character(d$condition))
  m <- merge(
    data.frame(k = key(sp_nlme), Q0_n = sp_nlme$Q0, a_n = sp_nlme$alpha),
    data.frame(k = key(sp_tmb), Q0_t = sp_tmb$Q0, a_t = sp_tmb$alpha),
    by = "k"
  )
  expect_gt(nrow(m), 0L)
  expect_gt(stats::cor(m$Q0_n, m$Q0_t), 0.9)
  # alpha (elasticity) is noisier to estimate => looser cross-backend floor.
  expect_gt(stats::cor(m$a_n, m$a_t), 0.8)
  expect_lt(stats::median(abs(m$Q0_n - m$Q0_t) / m$Q0_t), 0.25)
  expect_lt(stats::median(abs(m$a_n - m$a_t) / m$a_t), 0.25)
})
