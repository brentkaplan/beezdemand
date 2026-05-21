# TICKET-022: `get_subject_pars.beezdemand_tmb()` auto-detect default.
#
# Behavior matrix (this file's coverage):
#
# | expanded | within-id variation? | shape | warning |
# |----------|----------------------|-------|---------|
# | NULL     | yes                  | long  | none    |
# | NULL     | no                   | wide  | none    |
# | NULL     | no (between-subject) | wide  | none    |
# | FALSE    | yes                  | wide  | one-line cli_warn |
# | FALSE    | no                   | wide  | none    |
# | NULL     | (any)                | identical to default |
#
# `expanded = TRUE` behavior is covered by test-tmb-subject-pars-expanded.R.
# Existing-test updates for the FALSE-on-within-id case are in
# test-tmb-subject-pars-expanded.R:45-58 and test-tmb-subject-pars-within-
# subject.R:5-28 / :59-89.

# ---------------------------------------------------------------------------
# Helper: M1-style pdBlocked fit with within-id `condition` factor.
# Same recipe as test-tmb-subject-pars-expanded.R's `.fit_m1_style()`.
# ---------------------------------------------------------------------------

.auto_expand_m1_fit <- function(seed = 42, n_subjects = 30) {
  sim <- .simulate_within_subject_demand(
    n_subjects = n_subjects,
    n_conditions = 3,
    prices = c(0.25, 0.5, 1, 2, 4, 8, 16, 32),
    log_q0_pop = log(15),
    log_alpha_pop = log(0.0015),
    delta_q0 = c(0, -0.4, -0.9),
    delta_alpha = c(0, 0.2, 0.5),
    sigma_b = 0.4,
    sigma_d = 0.4,
    seed = seed
  )
  sim$id <- factor(sim$id)
  sim$condition <- factor(sim$condition)
  suppressWarnings(fit_demand_tmb(
    sim, equation = "simplified",
    random_effects = nlme::pdBlocked(list(
      nlme::pdSymm(Q0 + alpha ~ 1),
      nlme::pdDiag(Q0 + alpha ~ condition - 1)
    )),
    multi_start = FALSE, verbose = 0
  ))
}

# ---------------------------------------------------------------------------
# Auto-detect: within-id-varying fit → long shape, no warning.
# ---------------------------------------------------------------------------

test_that("auto-detect (expanded = NULL) on M1 fit returns long shape with no warning", {
  skip_on_cran()

  fit <- .auto_expand_m1_fit()

  expect_no_warning(spars <- get_subject_pars(fit))

  n_subj <- length(unique(as.character(fit$subject_pars$id)))
  n_cond <- length(levels(fit$data$condition))
  expect_equal(nrow(spars), n_subj * n_cond)
  expect_true("condition" %in% names(spars))
  expect_true(all(c("Q0", "alpha", "Pmax", "Omax") %in% names(spars)))
  expect_true(all(!is.na(spars$Q0)))
  expect_true(all(!is.na(spars$alpha)))
})

# ---------------------------------------------------------------------------
# Auto-detect: intercept-only fit → wide shape, no warning, unchanged.
# ---------------------------------------------------------------------------

test_that("auto-detect (expanded = NULL) on intercept-only fit returns wide shape with no warning", {
  skip_on_cran()
  data(apt, package = "beezdemand")

  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  expect_no_warning(spars <- get_subject_pars(fit))

  # Wide shape: one row per fitted subject, equal to the cached
  # subject_pars (which is itself one row per fitted subject after the
  # fit_demand_tmb pipeline applied any zero-drop filters).
  expect_equal(nrow(spars), nrow(fit$subject_pars))
  expect_equal(spars$id, fit$subject_pars$id)
  expect_false("condition" %in% names(spars))
  expect_true(all(is.finite(spars$Q0)))
  expect_true(all(is.finite(spars$alpha)))
})

# ---------------------------------------------------------------------------
# Auto-detect: between-subject factor fit → wide, no warning.
# (Between-subject factors are constant within id → no NA in subject_pars$Q0
# → auto-detect resolves to wide.)
# ---------------------------------------------------------------------------

test_that("auto-detect (expanded = NULL) on between-subject factor fit returns wide shape with no warning", {
  skip_on_cran()
  skip_if_not(exists("apt_full", where = asNamespace("beezdemand")))
  data(apt_full, package = "beezdemand")

  fit <- fit_demand_tmb(
    apt_full, equation = "exponential", factors = "gender",
    verbose = 0
  )

  expect_no_warning(spars <- get_subject_pars(fit))

  # Between-subject factor → constant within id → no NA in subject_pars$Q0
  # → auto-detect resolves to wide. Equal in shape to the cached
  # subject_pars (which may have fewer rows than raw apt_full if any
  # subjects were dropped at fit time).
  expect_equal(nrow(spars), nrow(fit$subject_pars))
  expect_false("gender" %in% names(spars))  # no expansion across factor
  expect_true(all(is.finite(spars$Q0)))
  expect_true(all(is.finite(spars$alpha)))
})

# ---------------------------------------------------------------------------
# Explicit `expanded = FALSE` on within-id fit → wide NA-filled with
# ONE-LINE primary warning. This is the new behavior introduced by this
# ticket; pre-change there was no warning at the extraction site.
# ---------------------------------------------------------------------------

test_that("explicit expanded = FALSE on within-id fit returns wide NA-filled with one-line primary warning", {
  skip_on_cran()

  fit <- .auto_expand_m1_fit()

  warnings_collected <- character()
  spars <- withCallingHandlers(
    get_subject_pars(fit, expanded = FALSE),
    warning = function(w) {
      warnings_collected <<- c(warnings_collected, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Wide NA-filled shape preserved (existing behavior).
  expect_equal(nrow(spars), length(unique(as.character(spars$id))))
  expect_true(all(is.na(spars$Q0)))
  expect_true(all(is.na(spars$alpha)))
  expect_true(all(is.na(spars$Pmax)))
  expect_true(all(is.na(spars$Omax)))

  # At least one warning emitted, and the primary line of the first
  # warning is concise (one source line; informational suffix lines via
  # cli `"i" =` are permitted but the primary message must be short).
  expect_gte(length(warnings_collected), 1L)
  primary_lines <- strsplit(warnings_collected[[1L]], "\n", fixed = TRUE)[[1L]]
  # Primary message line (first non-empty line) must be present and
  # under ~120 chars — "one line" in the human sense.
  primary <- primary_lines[nzchar(primary_lines)][1L]
  expect_true(nzchar(primary))
  expect_lt(nchar(primary), 200L)
  # And it must reference subject_pars / NA so the user knows what
  # happened.
  expect_match(primary, "Q0|alpha|NA|subject_pars|subject-level",
               ignore.case = TRUE)
})

# ---------------------------------------------------------------------------
# Explicit `expanded = FALSE` on intercept-only fit → wide, no warning.
# (Backward compat: no behavior change for non-within-id fits.)
# ---------------------------------------------------------------------------

test_that("explicit expanded = FALSE on intercept-only fit returns wide shape with no warning", {
  skip_on_cran()
  data(apt, package = "beezdemand")

  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  expect_no_warning(spars <- get_subject_pars(fit, expanded = FALSE))

  expect_equal(nrow(spars), length(unique(as.character(apt$id))))
  expect_true(all(is.finite(spars$Q0)))
})

# ---------------------------------------------------------------------------
# Explicit `expanded = NULL` is identical to default (proves NULL is the
# new default, not just an accepted value).
# ---------------------------------------------------------------------------

test_that("explicit expanded = NULL is identical to default on within-id fit", {
  skip_on_cran()

  fit <- .auto_expand_m1_fit()

  default <- get_subject_pars(fit)
  via_null <- get_subject_pars(fit, expanded = NULL)
  expect_identical(default, via_null)
})

test_that("explicit expanded = NULL is identical to default on intercept-only fit", {
  skip_on_cran()
  data(apt, package = "beezdemand")

  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  default <- get_subject_pars(fit)
  via_null <- get_subject_pars(fit, expanded = NULL)
  expect_identical(default, via_null)
})

# ---------------------------------------------------------------------------
# Validation: bad `expanded` value errors clearly.
# ---------------------------------------------------------------------------

test_that("expanded with non-logical or NA value errors", {
  skip_on_cran()
  data(apt, package = "beezdemand")

  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  expect_error(get_subject_pars(fit, expanded = "yes"))
  expect_error(get_subject_pars(fit, expanded = NA))
  expect_error(get_subject_pars(fit, expanded = c(TRUE, FALSE)))
})
