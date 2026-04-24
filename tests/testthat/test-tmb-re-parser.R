# TICKET-011 Phase 1: parser + dispatch for formula/pdMat random_effects on
# fit_demand_tmb(). These tests exercise the new R/random-effects-utils.R
# helpers (parser-only, no TMB compile) plus a few equivalence checks
# against the character-vector API. The Phase-1 template gate is also
# verified — richer RE shapes error with a clear "Phase 2 not yet shipped"
# message.

# ---------------------------------------------------------------------------
# .classify_re_input() — dispatch type detection
# ---------------------------------------------------------------------------

test_that(".classify_re_input recognises character vector", {
  expect_equal(.classify_re_input(c("q0", "alpha")), "character")
  expect_equal(.classify_re_input("q0"), "character")
})

test_that(".classify_re_input recognises formula", {
  expect_equal(.classify_re_input(Q0 + alpha ~ 1), "formula")
  expect_equal(.classify_re_input(Q0 ~ 1), "formula")
  expect_equal(.classify_re_input(Q0 + alpha ~ condition), "formula")
})

test_that(".classify_re_input recognises a single pdMat", {
  expect_equal(
    .classify_re_input(nlme::pdDiag(Q0 + alpha ~ 1)),
    "pdmat_single"
  )
  expect_equal(
    .classify_re_input(nlme::pdSymm(Q0 ~ 1)),
    "pdmat_single"
  )
})

test_that(".classify_re_input recognises list of pdMat", {
  x <- list(
    nlme::pdSymm(Q0 + alpha ~ 1),
    nlme::pdDiag(Q0 + alpha ~ condition - 1)
  )
  expect_equal(.classify_re_input(x), "pdmat_list")
})

test_that(".classify_re_input recognises pdBlocked", {
  x <- nlme::pdBlocked(list(
    nlme::pdSymm(Q0 + alpha ~ 1),
    nlme::pdDiag(Q0 + alpha ~ condition - 1)
  ))
  expect_equal(.classify_re_input(x), "pdblocked")
})

test_that(".classify_re_input rejects nonsense", {
  expect_error(.classify_re_input(42), "unrecognized")
  expect_error(.classify_re_input(NULL), "unrecognized")
  expect_error(.classify_re_input(list(1, 2, 3)), "unrecognized")
})

# ---------------------------------------------------------------------------
# .normalize_re_input() — canonical block representation
# ---------------------------------------------------------------------------

test_that(".normalize_re_input: character c('q0','alpha') maps to single pdSymm block with intercept terms", {
  out <- .normalize_re_input(c("q0", "alpha"), covariance_structure = "pdSymm")
  expect_equal(out$source, "character")
  expect_length(out$blocks, 1L)
  b <- out$blocks[[1]]
  expect_equal(b$pdmat_class, "pdSymm")
  expect_equal(b$terms_q0, "(Intercept)")
  expect_equal(b$terms_alpha, "(Intercept)")
  expect_equal(b$dim, 2L)
})

test_that(".normalize_re_input: character c('q0') maps to single pdDiag block with Q0-only intercept", {
  out <- .normalize_re_input(c("q0"), covariance_structure = "pdSymm")
  expect_equal(out$source, "character")
  expect_length(out$blocks, 1L)
  b <- out$blocks[[1]]
  expect_equal(b$pdmat_class, "pdDiag")
  expect_equal(b$terms_q0, "(Intercept)")
  expect_true(length(b$terms_alpha) == 0L)
  expect_equal(b$dim, 1L)
})

test_that(".normalize_re_input: character c('alpha','q0') accepted with either order", {
  out1 <- .normalize_re_input(c("alpha", "q0"), covariance_structure = "pdSymm")
  out2 <- .normalize_re_input(c("q0", "alpha"), covariance_structure = "pdSymm")
  expect_equal(out1$blocks[[1]]$terms_q0, out2$blocks[[1]]$terms_q0)
  expect_equal(out1$blocks[[1]]$terms_alpha, out2$blocks[[1]]$terms_alpha)
})

test_that(".normalize_re_input: formula Q0+alpha~1 with pdSymm equals character c('q0','alpha')", {
  from_chr <- .normalize_re_input(c("q0", "alpha"), covariance_structure = "pdSymm")
  from_frm <- .normalize_re_input(Q0 + alpha ~ 1, covariance_structure = "pdSymm")
  expect_equal(
    from_frm$blocks[[1]]$pdmat_class,
    from_chr$blocks[[1]]$pdmat_class
  )
  expect_equal(
    from_frm$blocks[[1]]$terms_q0,
    from_chr$blocks[[1]]$terms_q0
  )
  expect_equal(
    from_frm$blocks[[1]]$terms_alpha,
    from_chr$blocks[[1]]$terms_alpha
  )
})

test_that(".normalize_re_input: formula Q0~1 has only Q0 terms", {
  out <- .normalize_re_input(Q0 ~ 1, covariance_structure = "pdDiag")
  expect_length(out$blocks, 1L)
  expect_equal(out$blocks[[1]]$terms_q0, "(Intercept)")
  expect_true(length(out$blocks[[1]]$terms_alpha) == 0L)
})

test_that(".normalize_re_input: formula with a factor expands condition contrasts", {
  # Data supplied so the parser knows the factor levels.
  dat <- data.frame(
    id = rep(1:4, each = 3),
    condition = factor(rep(c("A", "B", "C"), 4)),
    x = 1:12,
    y = runif(12)
  )
  out <- .normalize_re_input(
    Q0 + alpha ~ condition,
    covariance_structure = "pdDiag",
    data = dat
  )
  expect_length(out$blocks, 1L)
  # With `~ condition` (default contrasts, with intercept), expect
  # "(Intercept)", "conditionB", "conditionC" for each param.
  expect_true("(Intercept)" %in% out$blocks[[1]]$terms_q0)
  expect_true("conditionB"  %in% out$blocks[[1]]$terms_q0)
  expect_true("conditionC"  %in% out$blocks[[1]]$terms_q0)
})

test_that(".normalize_re_input: pdBlocked preserves multiple blocks", {
  blocked <- nlme::pdBlocked(list(
    nlme::pdSymm(Q0 + alpha ~ 1),
    nlme::pdDiag(Q0 + alpha ~ 1)
  ))
  out <- .normalize_re_input(blocked, covariance_structure = "pdDiag")
  expect_length(out$blocks, 2L)
  expect_equal(out$blocks[[1]]$pdmat_class, "pdSymm")
  expect_equal(out$blocks[[2]]$pdmat_class, "pdDiag")
})

test_that(".normalize_re_input: list-of-pdMat treated like pdBlocked", {
  lst <- list(
    nlme::pdSymm(Q0 + alpha ~ 1),
    nlme::pdDiag(Q0 + alpha ~ 1)
  )
  out <- .normalize_re_input(lst, covariance_structure = "pdDiag")
  expect_length(out$blocks, 2L)
  expect_equal(out$blocks[[1]]$pdmat_class, "pdSymm")
  expect_equal(out$blocks[[2]]$pdmat_class, "pdDiag")
})

test_that(".normalize_re_input rejects invalid character", {
  expect_error(
    .normalize_re_input(c("foo"), covariance_structure = "pdSymm"),
    "must be a subset"
  )
})

# ---------------------------------------------------------------------------
# .re_is_phase1_fittable() / Phase-1 gate
# ---------------------------------------------------------------------------

test_that(".re_is_phase1_fittable accepts intercept-only single block", {
  out1 <- .normalize_re_input(c("q0", "alpha"), covariance_structure = "pdSymm")
  out2 <- .normalize_re_input(c("q0"), covariance_structure = "pdSymm")
  out3 <- .normalize_re_input(Q0 + alpha ~ 1, covariance_structure = "pdSymm")
  expect_true(.re_is_phase1_fittable(out1))
  expect_true(.re_is_phase1_fittable(out2))
  expect_true(.re_is_phase1_fittable(out3))
})

test_that(".re_is_phase1_fittable rejects multi-block or non-intercept", {
  blocked <- nlme::pdBlocked(list(
    nlme::pdSymm(Q0 + alpha ~ 1),
    nlme::pdDiag(Q0 + alpha ~ 1)
  ))
  out_multi <- .normalize_re_input(blocked, covariance_structure = "pdDiag")
  expect_false(.re_is_phase1_fittable(out_multi))

  dat <- data.frame(
    id = rep(1:4, each = 3),
    condition = factor(rep(c("A", "B", "C"), 4)),
    x = 1:12,
    y = runif(12)
  )
  out_cond <- .normalize_re_input(
    Q0 + alpha ~ condition,
    covariance_structure = "pdDiag",
    data = dat
  )
  expect_false(.re_is_phase1_fittable(out_cond))
})

# ---------------------------------------------------------------------------
# fit_demand_tmb() Phase-1 dispatch + deprecation
# ---------------------------------------------------------------------------

test_that("fit_demand_tmb with formula ~ 1 is bit-identical to character c('q0','alpha')", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit_chr <- suppressMessages(fit_demand_tmb(
    apt,
    equation = "simplified",
    random_effects = c("q0", "alpha"),
    verbose = 0
  ))
  fit_frm <- fit_demand_tmb(
    apt,
    equation = "simplified",
    random_effects = Q0 + alpha ~ 1,
    verbose = 0
  )
  expect_equal(fit_chr$loglik, fit_frm$loglik, tolerance = 1e-10)
  expect_equal(
    unname(fit_chr$model$coefficients),
    unname(fit_frm$model$coefficients),
    tolerance = 1e-8
  )
})

test_that("fit_demand_tmb with formula Q0 ~ 1 matches character c('q0')", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit_chr <- suppressMessages(fit_demand_tmb(
    apt,
    equation = "simplified",
    random_effects = "q0",
    verbose = 0
  ))
  fit_frm <- fit_demand_tmb(
    apt,
    equation = "simplified",
    random_effects = Q0 ~ 1,
    verbose = 0
  )
  expect_equal(fit_chr$loglik, fit_frm$loglik, tolerance = 1e-10)
})

test_that("fit_demand_tmb(random_effects = character) emits soft deprecation", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  rlang::reset_warning_verbosity("fit_demand_tmb_chr_re")
  expect_warning(
    fit_demand_tmb(
      apt,
      equation = "simplified",
      random_effects = c("q0", "alpha"),
      verbose = 0
    ),
    regexp = "character-vector|deprecated|formula"
  )
})

test_that("fit_demand_tmb errors for formula shapes the Phase-1 template cannot fit", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  # Add a within-subject factor (just to satisfy the formula parser; the
  # gate fires before fitting)
  apt_cond <- apt
  apt_cond$cond <- factor(rep_len(c("A", "B"), nrow(apt_cond)))

  expect_error(
    fit_demand_tmb(
      apt_cond,
      equation = "simplified",
      random_effects = Q0 + alpha ~ cond,
      verbose = 0
    ),
    regexp = "Phase 2"
  )
  expect_error(
    fit_demand_tmb(
      apt_cond,
      equation = "simplified",
      random_effects = nlme::pdBlocked(list(
        nlme::pdSymm(Q0 + alpha ~ 1),
        nlme::pdDiag(Q0 + alpha ~ 1)
      )),
      verbose = 0
    ),
    regexp = "Phase 2"
  )
})

test_that("fit object carries parsed RE metadata", {
  skip_on_cran()
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(
    apt,
    equation = "simplified",
    random_effects = Q0 + alpha ~ 1,
    verbose = 0
  )
  expect_true(!is.null(fit$param_info$random_effects_parsed))
  expect_equal(fit$param_info$random_effects_parsed$source, "formula")
  expect_equal(
    fit$param_info$random_effects_parsed$blocks[[1]]$pdmat_class,
    "pdSymm"
  )
})
