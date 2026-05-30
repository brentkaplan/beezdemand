# =============================================================================
# Tests for calc_group_metrics.beezdemand_tmb()
#
# TICKET-011 Phase 5C: replaced Phase 0.5's warn-and-label-at-covariate=0
# behavior with proper conditioning. Default behavior now evaluates
# continuous covariates at their training mean and marginalizes factors
# across observed levels (equal weights). The new `at` argument lets
# callers override either. The Phase 0.5 warning is retired entirely
# because the default is statistically defensible; the `conditioned_on`
# field still labels the actual conditioning point.
#
# Marginalization order: parameter-first. log-Q0 and log-alpha EMMs are
# averaged across reference grid cells, then Pmax/Omax/Qmax are derived
# from the marginalized parameters. (Not "compute metrics per cell, then
# average" — the two approaches differ for nonlinear transforms.)
#
# Fixtures: the ~18 fits here reduce to a handful of distinct specs — mostly on
# the same 50-subject `apt_full` subsample (the two no-covariate blocks use the
# small built-in `apt`) — so they are fit ONCE and memoized at file level
# (new.env cache, mirroring test-anova-tmb.R / test-boot-demand.R)
# to keep this file off the CI test-phase critical path. Tests do not mutate
# fits (calc_group_metrics()/get_demand_param_emms()/get_demand_comparisons()
# are read-only over the fit), so the cached objects stay pristine. The
# collapse-spec fits previously ran on the FULL Male/Female set (~1,100
# subjects / ~18,700 rows); they now use the same 25/group subsample, which
# still populates all three age_group levels — these tests assert structure
# (collapsed factor names, error/no-error), not data-dependent values.
# =============================================================================

helper_subsample_apt_full <- function(n_per_group = 25) {
  data(apt_full, package = "beezdemand")
  d <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
  d$gender <- droplevels(as.factor(d$gender))
  ids_keep <- unlist(lapply(levels(d$gender), function(g) {
    ids_g <- unique(d$id[d$gender == g])
    head(ids_g[order(ids_g)], n_per_group)
  }))
  d <- d[d$id %in% ids_keep, ]
  d$id <- droplevels(as.factor(d$id))
  d
}

.cgmt_cache <- new.env(parent = emptyenv())

# 50-subject subsample, fit once and reused.
.cgmt_data <- function() {
  if (is.null(.cgmt_cache$data)) .cgmt_cache$data <- helper_subsample_apt_full()
  .cgmt_cache$data
}

# Plain apt exponential (no covariate / no factor).
.cgmt_apt_exp <- function() {
  if (is.null(.cgmt_cache$apt_exp)) {
    data(apt, package = "beezdemand")
    .cgmt_cache$apt_exp <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  }
  .cgmt_cache$apt_exp
}

# Subsample + continuous_covariates = "age".
.cgmt_fit_age <- function() {
  if (is.null(.cgmt_cache$fit_age)) {
    .cgmt_cache$fit_age <- fit_demand_tmb(
      .cgmt_data(), equation = "exponential",
      continuous_covariates = "age", verbose = 0
    )
  }
  .cgmt_cache$fit_age
}

# Subsample + factors = "gender".
.cgmt_fit_gender <- function() {
  if (is.null(.cgmt_cache$fit_gender)) {
    .cgmt_cache$fit_gender <- fit_demand_tmb(
      .cgmt_data(), equation = "exponential",
      factors = "gender", verbose = 0
    )
  }
  .cgmt_cache$fit_gender
}

# Subsample + factors = "gender" + continuous_covariates = "age".
.cgmt_fit_gender_age <- function() {
  if (is.null(.cgmt_cache$fit_gender_age)) {
    .cgmt_cache$fit_gender_age <- fit_demand_tmb(
      .cgmt_data(), equation = "exponential",
      factors = "gender", continuous_covariates = "age", verbose = 0
    )
  }
  .cgmt_cache$fit_gender_age
}

# Subsample + age_group factor (3 levels) for the asymmetric-collapse tests.
.cgmt_collapse_data <- function() {
  if (is.null(.cgmt_cache$collapse_data)) {
    d <- helper_subsample_apt_full()
    d$age_group <- factor(cut(d$age, c(0, 25, 35, Inf),
                              labels = c("young", "mid", "old")))
    d$id <- droplevels(as.factor(d$id))
    .cgmt_cache$collapse_data <- d
  }
  .cgmt_cache$collapse_data
}

# Asymmetric collapse fit: Q0 -> 2 levels (junior/old), alpha -> 3 levels.
.cgmt_fit_collapse <- function() {
  if (is.null(.cgmt_cache$fit_collapse)) {
    .cgmt_cache$fit_collapse <- suppressWarnings(fit_demand_tmb(
      .cgmt_collapse_data(), equation = "exponential",
      factors = "age_group",
      collapse_levels = list(
        Q0    = list(age_group = list(junior = c("young", "mid"), old = "old")),
        alpha = list(age_group = list(young = "young", mid = "mid", old = "old"))
      ),
      verbose = 0
    ))
  }
  .cgmt_cache$fit_collapse
}

# ---------------------------------------------------------------------------
# Default behavior: training-mean continuous covariates; factor marginal.
# ---------------------------------------------------------------------------

test_that("calc_group_metrics is silent for no-covariate TMB fits", {
  skip_on_cran()
  fit <- .cgmt_apt_exp()

  expect_no_warning(metrics <- calc_group_metrics(fit))
  # No covariates and no factors -> conditioned_on is NULL.
  expect_null(metrics$conditioned_on)
  expect_true(is.finite(metrics$Pmax))
  expect_true(is.finite(metrics$Omax))
})

test_that("calc_group_metrics is silent for covariate fits (Phase 0.5 warning retired)", {
  skip_on_cran()
  fit <- .cgmt_fit_age()

  expect_no_warning(metrics <- calc_group_metrics(fit))
  expect_true("conditioned_on" %in% names(metrics))
  expect_true("covariates" %in% names(metrics$conditioned_on))
  # Default conditioning evaluates `age` at the training mean of the
  # post-fit data (fit$data, after zero-drop filtering), NOT 0.
  expect_equal(
    metrics$conditioned_on$covariates[["age"]],
    mean(fit$data$age, na.rm = TRUE)
  )
  expect_false(metrics$conditioned_on$covariates[["age"]] == 0)
})

# ---------------------------------------------------------------------------
# `at` argument: explicit conditioning of continuous covariates.
# ---------------------------------------------------------------------------

test_that("calc_group_metrics(at = list(cov = X)) conditions at the supplied value", {
  skip_on_cran()
  fit <- .cgmt_fit_age()

  metrics_default <- calc_group_metrics(fit)
  metrics_at_30 <- calc_group_metrics(fit, at = list(age = 30))
  metrics_at_50 <- calc_group_metrics(fit, at = list(age = 50))

  # conditioned_on reflects the actual value used.
  expect_equal(metrics_at_30$conditioned_on$covariates[["age"]], 30)
  expect_equal(metrics_at_50$conditioned_on$covariates[["age"]], 50)

  # The metrics differ across `at` values when the covariate enters Q0
  # or alpha. (For an exponential demand model the covariate enters
  # log-Q0 / log-alpha, so any non-zero coefficient produces a
  # non-degenerate change. We pin a relative-difference assertion to
  # avoid depending on a specific direction.)
  expect_true(
    metrics_at_30$Pmax != metrics_at_50$Pmax ||
      metrics_at_30$Omax != metrics_at_50$Omax
  )
})

# ---------------------------------------------------------------------------
# `at` argument: factor-level conditioning.
# ---------------------------------------------------------------------------

test_that("calc_group_metrics(at = list(factor = level)) conditions on a level", {
  skip_on_cran()
  fit <- .cgmt_fit_gender()

  metrics_default <- calc_group_metrics(fit)
  metrics_male <- calc_group_metrics(fit, at = list(gender = "Male"))
  metrics_female <- calc_group_metrics(fit, at = list(gender = "Female"))

  # conditioned_on reflects the supplied factor level.
  expect_equal(metrics_male$conditioned_on$factors$gender, "Male")
  expect_equal(metrics_female$conditioned_on$factors$gender, "Female")
  # Default conditioning marks the factor as marginal.
  expect_equal(metrics_default$conditioned_on$factors$gender, "marginal")

  # Metrics differ by level when gender enters Q0 or alpha.
  expect_true(metrics_male$Pmax != metrics_female$Pmax)
})

# ---------------------------------------------------------------------------
# Generic dispatch: calling the generic with `at` works through ... .
# ---------------------------------------------------------------------------

test_that("calc_group_metrics() generic dispatches `at` arg", {
  skip_on_cran()
  fit <- .cgmt_fit_age()

  # Calling the generic (not the method directly) must propagate `at`.
  m <- calc_group_metrics(fit, at = list(age = 40))
  expect_equal(m$conditioned_on$covariates[["age"]], 40)
})

# ---------------------------------------------------------------------------
# summary.beezdemand_tmb prints the conditioning line.
# ---------------------------------------------------------------------------

test_that("summary.beezdemand_tmb prints 'Metrics conditioned at:' line for covariate fits", {
  skip_on_cran()
  fit <- .cgmt_fit_age()

  out <- capture.output(summary(fit))
  expect_true(any(grepl("Metrics conditioned at:", out, fixed = TRUE)))
  expect_true(any(grepl("age=", out, fixed = TRUE)))
})

test_that("summary.beezdemand_tmb omits the conditioning line for plain fits", {
  skip_on_cran()
  fit <- .cgmt_apt_exp()

  out <- capture.output(summary(fit))
  # No covariates and no factors -> no conditioning_on -> no print line.
  expect_false(any(grepl("Metrics conditioned at:", out, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# `at` validation: catch typos and bad values BEFORE grid construction so
# users can't get silent default-marginal results from a misspelled name
# or NA metrics from an off-grid factor level. Phase 5C release blocker.
# ---------------------------------------------------------------------------

test_that("calc_group_metrics aborts on `at` with unknown name (typo)", {
  skip_on_cran()
  fit <- .cgmt_fit_gender()

  # Mistyped factor name `gendr` must abort, NOT silently return default
  # marginal metrics.
  expect_error(
    calc_group_metrics(fit, at = list(gendr = "Male")),
    regexp = "Unknown name|gendr"
  )
})

test_that("calc_group_metrics aborts on `at` with off-grid factor level", {
  skip_on_cran()
  fit <- .cgmt_fit_gender()

  # Conditioning on an unobserved factor level must abort, NOT return
  # NA metrics with conditioned_on labelled with the off-grid value.
  expect_error(
    calc_group_metrics(fit, at = list(gender = "NoSuchLevel")),
    regexp = "not an observed level|NoSuchLevel"
  )
})

test_that("calc_group_metrics aborts on non-numeric `at` value for continuous covariate", {
  skip_on_cran()
  fit <- .cgmt_fit_age()

  # Non-numeric value (e.g., character that doesn't coerce) must abort.
  expect_error(
    calc_group_metrics(fit, at = list(age = "not-number")),
    regexp = "not finite numeric|not-number"
  )

  # Infinite or NA also aborts.
  expect_error(
    calc_group_metrics(fit, at = list(age = NA_real_)),
    regexp = "not finite numeric"
  )
  expect_error(
    calc_group_metrics(fit, at = list(age = Inf)),
    regexp = "not finite numeric"
  )
})

test_that("calc_group_metrics aborts on zero-length `at` value", {
  skip_on_cran()
  fit <- .cgmt_fit_age()

  # Zero-length numeric: any(is.na(numeric(0))) is FALSE, so the
  # finite-numeric check passes; then `[1]` returns NA. Pre-fix this
  # produced silent NA Pmax/Omax/Qmax. Validation must abort.
  expect_error(
    calc_group_metrics(fit, at = list(age = numeric(0))),
    regexp = "length 0"
  )
  expect_error(
    calc_group_metrics(fit, at = list(age = c())),
    regexp = "length 0"
  )
})

test_that("calc_group_metrics warns ONCE on multi-value `at` continuous (one-shot)", {
  skip_on_cran()
  fit <- .cgmt_fit_age()

  # calc_group_metrics() builds Q0 and alpha grids in one user call.
  # Pre-fix the validation/warning fired inside the helper, so each
  # grid call emitted its own warning -> 2 warnings per public call.
  # The fix validates ONCE at the top and passes validate=FALSE to
  # both helper calls. Capture all warnings and assert exactly one
  # multi-value warning landed.
  ws <- character(0)
  metrics <- withCallingHandlers(
    calc_group_metrics(fit, at = list(age = c(30, 50))),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  multi_warns <- grep("length 2|using first value", ws, value = TRUE)
  expect_length(multi_warns, 1L)
  expect_equal(metrics$conditioned_on$covariates[["age"]], 30)
})

test_that("get_demand_param_emms warns ONCE on multi-value `at` continuous", {
  skip_on_cran()
  fit <- .cgmt_fit_age()

  ws <- character(0)
  withCallingHandlers(
    get_demand_param_emms(fit, param = "Q0", at = list(age = c(30, 50))),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  multi_warns <- grep("length 2|using first value|first only", ws, value = TRUE)
  expect_length(multi_warns, 1L)
})

test_that("get_demand_comparisons warns ONCE on multi-value `at` continuous", {
  skip_on_cran()
  fit <- .cgmt_fit_gender_age()

  # get_demand_comparisons() calls get_demand_param_emms() (1 grid call)
  # then builds its own grid (2 grid calls total). Pre-fix that meant
  # 2-3 duplicate warnings. The validate=FALSE plumbing collapses to 1.
  ws <- character(0)
  withCallingHandlers(
    get_demand_comparisons(fit, param = "Q0", at = list(age = c(30, 50))),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  multi_warns <- grep("length 2|using first value|first only", ws, value = TRUE)
  expect_length(multi_warns, 1L)
})

test_that("calc_group_metrics aborts on empty `at`-filtered grid", {
  skip_on_cran()
  fit <- .cgmt_fit_gender()

  # The previous `at = list(gender = "NoSuchLevel")` test catches the
  # off-grid level path; this test covers the (rare) path where the
  # supplied factor levels exist individually but the cross-product
  # filters out all rows. With a single factor that can't happen
  # without an off-grid level, so we pin the off-grid branch.
  expect_error(
    calc_group_metrics(fit, at = list(gender = c())),
    regexp = "Unknown|empty|level"
  )
})

test_that("calc_group_metrics rejects collapse-aliased original factor name in `at`", {
  skip_on_cran()
  # Asymmetric collapse: Q0 -> 2 levels (young+mid into "junior"), alpha
  # keeps all 3 levels. This makes factors_q0 = "age_group_Q0" and
  # factors_alpha = "age_group_alpha" (both differ from the original
  # "age_group" name in param_info$factors).
  fit <- .cgmt_fit_collapse()

  # Fixture sanity: the 25/group subsample must retain all three age_group
  # levels, else the asymmetric collapse below degenerates (Q0 -> {junior, old},
  # alpha -> {young, mid, old}) and these tests would pass vacuously. Guards
  # against a future subsample-size / id-order change silently weakening them.
  expect_setequal(
    as.character(unique(.cgmt_collapse_data()$age_group)),
    c("young", "mid", "old")
  )

  # Sanity: factors_q0 and factors_alpha should now be collapsed names.
  expect_true(any(grepl("age_group_(Q0|alpha)", c(
    fit$param_info$factors_q0, fit$param_info$factors_alpha
  ))))

  # Pre-fix, validation accepted the original name (because
  # `param_info$factors` includes "age_group") and the helper silently
  # ignored it (because `use_factors` keys off the collapsed columns).
  # Post-fix, validation REJECTS with a targeted message that names the
  # collapsed columns the user should condition on instead.
  expect_error(
    calc_group_metrics(fit, at = list(age_group = "young")),
    regexp = "collapsed|age_group_Q0|age_group_alpha"
  )

  # The collapsed columns themselves are accepted.
  expect_no_error(
    calc_group_metrics(
      fit,
      at = list(age_group_Q0 = "junior", age_group_alpha = "young")
    )
  )
})

test_that("get_demand_param_emms rejects cross-param `at` factor name (asymmetric collapse)", {
  skip_on_cran()
  # Asymmetric collapse: Q0 -> 2 levels (junior/old), alpha -> 3 levels.
  # factors_q0 = "age_group_Q0"; factors_alpha = "age_group_alpha".
  fit <- .cgmt_fit_collapse()

  # Pre-fix, get_demand_param_emms(param = "Q0", at = list(age_group_alpha
  # = "young")) accepted the alpha-only name (validation took the union)
  # but the Q0 grid silently ignored it. Post-fix, scoped validation
  # rejects names not in factors_q0 for a Q0 EMM call.
  expect_error(
    get_demand_param_emms(fit, param = "Q0", at = list(age_group_alpha = "young")),
    regexp = "Unknown name|age_group_alpha"
  )
  expect_error(
    get_demand_param_emms(fit, param = "alpha", at = list(age_group_Q0 = "junior")),
    regexp = "Unknown name|age_group_Q0"
  )

  # Each param accepts its own collapsed name.
  expect_no_error(
    get_demand_param_emms(fit, param = "Q0", at = list(age_group_Q0 = "junior"))
  )
  expect_no_error(
    get_demand_param_emms(fit, param = "alpha", at = list(age_group_alpha = "young"))
  )

  # calc_group_metrics() accepts BOTH (it builds both grids in one call).
  expect_no_error(
    calc_group_metrics(
      fit,
      at = list(age_group_Q0 = "junior", age_group_alpha = "young")
    )
  )
})

test_that("calc_group_metrics aborts on unnamed `at` element", {
  skip_on_cran()
  fit <- .cgmt_fit_gender()

  # Mixed named/unnamed list — unnamed entries must abort.
  expect_error(
    calc_group_metrics(fit, at = list("Male", gender = "Male")),
    regexp = "named"
  )
})
