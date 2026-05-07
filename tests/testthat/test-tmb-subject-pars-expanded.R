# Phase 5A acceptance: get_subject_pars(fit, expanded = TRUE) returns
# long-form per-(subject, factor-level) rows. Default expanded = FALSE
# preserves the wide one-row-per-subject shape with NA in Q0/alpha/Pmax/
# Omax for fits where within-subject design columns vary within id.
#
# Includes regression tests for downstream consumers (predict, ranef
# preserve correctness; plot, amplitude/persistence abort with a
# targeted message pointing at expanded = TRUE).

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.fit_m1_style <- function(seed = 42, n_subjects = 30) {
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
# Default wide shape (expanded = FALSE) on M1-style fit returns NA in
# Q0/alpha/Pmax/Omax. Pre-Phase-5A returned first-observed-row values
# silently.
# ---------------------------------------------------------------------------

test_that("expanded = FALSE on M1 fit returns NA in Q0/alpha/Pmax/Omax", {
  skip_on_cran()

  fit <- .fit_m1_style()
  spars <- get_subject_pars(fit)

  expect_equal(nrow(spars), length(unique(spars$id)))
  expect_true(all(is.na(spars$Q0)))
  expect_true(all(is.na(spars$alpha)))
  expect_true(all(is.na(spars$Pmax)))
  expect_true(all(is.na(spars$Omax)))
  # b_i / c_i remain populated (subject-level RE intercepts).
  expect_true(all(!is.na(spars$b_i)))
})

# ---------------------------------------------------------------------------
# Expanded shape returns long-form rows: n_subjects x n_within_factor_levels.
# ---------------------------------------------------------------------------

test_that("expanded = TRUE returns long-form per-(subject, condition) rows", {
  skip_on_cran()

  fit <- .fit_m1_style()
  exp_pars <- get_subject_pars(fit, expanded = TRUE)

  n_subj <- length(unique(exp_pars$id))
  n_cond <- length(levels(exp_pars$condition))
  expect_equal(nrow(exp_pars), n_subj * n_cond)

  # Required columns: id, condition, b_i, c_i, Q0, alpha, Pmax, Omax.
  expect_true(all(
    c("id", "condition", "b_i", "c_i", "Q0", "alpha", "Pmax", "Omax")
    %in% names(exp_pars)
  ))
  expect_true(all(!is.na(exp_pars$Q0)))
  expect_true(all(!is.na(exp_pars$alpha)))

  # b_i / c_i repeat across rows for the same subject.
  by_id <- split(exp_pars$b_i, exp_pars$id)
  same_b_per_id <- vapply(by_id, function(v) length(unique(v)) == 1L, logical(1))
  expect_true(all(same_b_per_id))
})

# ---------------------------------------------------------------------------
# Generic dispatch: calling the generic (not the method directly) with the
# new argument must work end-to-end.
# ---------------------------------------------------------------------------

test_that("get_subject_pars() generic dispatches `expanded` arg", {
  skip_on_cran()

  fit <- .fit_m1_style()
  exp_pars <- get_subject_pars(fit, expanded = TRUE)
  expect_s3_class(exp_pars, "data.frame")
  expect_true("condition" %in% names(exp_pars))
})

# ---------------------------------------------------------------------------
# Predict and ranef: must work unchanged on M1-style fit (they don't read
# Q0/alpha from subject_pars; predict uses match() on unique IDs and
# re_q0_mat / re_alpha_mat attributes).
# ---------------------------------------------------------------------------

test_that("predict() works on M1-style fit", {
  skip_on_cran()

  fit <- .fit_m1_style()
  preds <- predict(fit)
  expect_s3_class(preds, "data.frame")
  expect_true(nrow(preds) > 0L)
  expect_true(all(is.finite(preds$.fitted)))
})

test_that("ranef() works on M1-style fit and surfaces ALL per-block RE columns", {
  skip_on_cran()

  fit <- .fit_m1_style()
  re <- nlme::ranef(fit)
  expect_s3_class(re, "data.frame")
  expect_equal(nrow(re), length(unique(re$id)))
  expect_true("id" %in% names(re))

  # Phase 5A regression: pre-fix ranef() returned ONLY id/b_i/c_i, which
  # for an M1-spec fit silently dropped the block-2 condition-slope
  # random effects. Post-fix, ranef() must surface every column of
  # re_q0_mat / re_alpha_mat as q0_<term> / alpha_<term>.
  q0_cols <- grep("^q0_", names(re), value = TRUE)
  alpha_cols <- grep("^alpha_", names(re), value = TRUE)
  re_q0_mat <- attr(fit$subject_pars, "re_q0_mat")
  re_alpha_mat <- attr(fit$subject_pars, "re_alpha_mat")
  expect_equal(length(q0_cols), ncol(re_q0_mat))
  expect_equal(length(alpha_cols), ncol(re_alpha_mat))

  # M1 spec has block 1 (1 intercept) + block 2 (3 condition slopes) -> 4
  # RE columns per parameter. So at minimum 4 q0_* columns and 4 alpha_*
  # columns must be present.
  expect_gte(length(q0_cols), 4L)
  expect_gte(length(alpha_cols), 4L)

  # Backward compat: b_i / c_i still present (first RE column aliases).
  expect_true("b_i" %in% names(re))
  expect_true("c_i" %in% names(re))
  # And they numerically equal the corresponding first per-block column.
  expect_equal(re$b_i, re_q0_mat[, 1L], tolerance = 1e-10)
  expect_equal(re$c_i, re_alpha_mat[, 1L], tolerance = 1e-10)
})

test_that("ranef() disambiguates duplicate term names across blocks", {
  skip_on_cran()

  # Two blocks share the same `(Intercept)` term — without
  # disambiguation, ranef()'s `out[[col_name]] <- mat[, j]` loop
  # would silently overwrite block-1's column with block-2's.
  data(apt, package = "beezdemand")
  apt_dup <- apt
  fit <- suppressWarnings(fit_demand_tmb(
    apt_dup, equation = "simplified",
    random_effects = nlme::pdBlocked(list(
      nlme::pdSymm(Q0 ~ 1),
      nlme::pdSymm(Q0 ~ 1)
    )),
    multi_start = FALSE, verbose = 0
  ))

  re_q0 <- attr(fit$subject_pars, "re_q0_mat")
  expect_equal(ncol(re_q0), 2L)
  # Disambiguated colnames: each (Intercept) gets a _block<N> suffix.
  expect_true(any(grepl("block1", colnames(re_q0))))
  expect_true(any(grepl("block2", colnames(re_q0))))

  re_df <- nlme::ranef(fit)
  q0_cols <- grep("^q0_", names(re_df), value = TRUE)
  expect_equal(length(q0_cols), 2L)
  # Both block columns must be preserved (not overwritten).
  expect_false(identical(re_df[[q0_cols[1]]], re_df[[q0_cols[2]]]))
})

test_that("ranef() preserves intercept-only output shape (backward compat)", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
  re <- nlme::ranef(fit)

  # For an intercept-only fit, b_i / c_i remain (existing tests at
  # test-fit_demand_tmb.R:274 hardcode these names). The new per-block
  # columns appear too as q0_(Intercept) / alpha_(Intercept), but
  # callers relying on b_i / c_i continue to work.
  expect_true(all(c("id", "b_i", "c_i") %in% names(re)))
})

# ---------------------------------------------------------------------------
# Plot and amplitude/persistence: must abort with targeted message
# pointing at `expanded = TRUE`.
# ---------------------------------------------------------------------------

test_that("plot(type='individual') aborts on M1 fit with expanded=TRUE pointer", {
  skip_on_cran()

  fit <- .fit_m1_style()
  expect_error(
    plot(fit, type = "individual"),
    regexp = "expanded = TRUE"
  )
})

test_that("calculate_amplitude_persistence() aborts on M1 fit with expanded=TRUE pointer", {
  skip_on_cran()

  fit <- .fit_m1_style()
  expect_error(
    calculate_amplitude_persistence(fit),
    regexp = "expanded = TRUE"
  )
})

# ---------------------------------------------------------------------------
# Numeric within-id-varying RE-RHS: condition at subject mean (no row
# expansion from the numeric variable).
# ---------------------------------------------------------------------------

test_that("expanded = TRUE conditions numeric within-id RE-RHS at subject mean (matches manual mean-conditioned newdata; differs from first-row)", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  apt_num <- apt
  # Numeric within-id-varying variable (e.g., trial number). The Z-column
  # NA detection flags this in default wide subject_pars; expanded shape
  # should condition at subject mean rather than fall through to first-
  # observed-row values.
  apt_num$trial_num <- as.numeric(stats::ave(seq_len(nrow(apt_num)),
                                              apt_num$id, FUN = seq_along))

  fit <- suppressWarnings(fit_demand_tmb(
    apt_num, equation = "simplified",
    random_effects = nlme::pdDiag(Q0 + alpha ~ trial_num),
    multi_start = FALSE, verbose = 0
  ))

  # Default wide: NA in Q0/alpha (Z-column variation flagged).
  default_pars <- get_subject_pars(fit)
  expect_true(all(is.na(default_pars$Q0)))

  # Expanded: numeric within-id RE-RHS conditions at subject mean.
  exp_pars <- get_subject_pars(fit, expanded = TRUE)
  expect_equal(nrow(exp_pars), length(unique(fit$subject_pars$id)))
  expect_true(all(!is.na(exp_pars$Q0)))
  expect_true(all(!is.na(exp_pars$alpha)))

  # Hard check: expanded Q0/alpha must MATCH the value derived from a
  # manual mean-conditioned newdata, and must DIFFER from the value
  # derived from a first-observed-row newdata. trial_num runs 1:N per
  # subject, so subject mean = (N+1)/2 vs first-row = 1.
  for (sid in head(as.character(exp_pars$id), 3)) {
    subj_rows <- apt_num[as.character(apt_num$id) == sid, ]
    trial_mean <- mean(subj_rows$trial_num)
    trial_first <- subj_rows$trial_num[1]
    expect_gt(trial_mean, trial_first)

    nd_mean <- data.frame(
      id = sid, x = subj_rows$x[1], y = subj_rows$y[1],
      trial_num = trial_mean
    )
    nd_first <- data.frame(
      id = sid, x = subj_rows$x[1], y = subj_rows$y[1],
      trial_num = trial_first
    )
    bp_mean <- beezdemand:::.tmb_build_predicted_pars(fit, nd_mean)
    bp_first <- beezdemand:::.tmb_build_predicted_pars(fit, nd_first)

    exp_q0 <- exp_pars$Q0[as.character(exp_pars$id) == sid]
    exp_alpha <- exp_pars$alpha[as.character(exp_pars$id) == sid]

    expect_equal(exp_q0, bp_mean$Q0, tolerance = 1e-8,
                 info = paste("subject", sid, "Q0 mean-conditioned"))
    expect_equal(exp_alpha, bp_mean$alpha, tolerance = 1e-8,
                 info = paste("subject", sid, "alpha mean-conditioned"))
    # Differs from first-row alternative — confirms NOT first-row fallback.
    expect_false(isTRUE(all.equal(exp_q0, bp_first$Q0, tolerance = 1e-6)),
                 info = paste("subject", sid, "Q0 NOT first-row"))
  }
})

test_that("expanded = TRUE conditions a within-id continuous covariate at subject mean (matches manual mean-conditioned newdata; differs from first-row)", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  set.seed(101)
  apt_cov <- apt
  # Within-id-varying continuous covariate placed in `continuous_covariates`
  # rather than `random_effects`. Pre-fix this fell through to the
  # copy-first-row branch because continuous_covariates was missing from
  # the candidate-variable discovery.
  apt_cov$within_cov <- stats::rnorm(nrow(apt_cov))

  fit <- suppressWarnings(fit_demand_tmb(
    apt_cov, equation = "simplified",
    continuous_covariates = "within_cov",
    verbose = 0
  ))

  exp_pars <- get_subject_pars(fit, expanded = TRUE)
  expect_true(all(!is.na(exp_pars$Q0)))

  # Hard check on a few subjects: expanded Q0/alpha must match the
  # mean-conditioned newdata path and differ from the first-row path.
  for (sid in head(as.character(exp_pars$id), 3)) {
    subj_rows <- apt_cov[as.character(apt_cov$id) == sid, ]
    cov_mean <- mean(subj_rows$within_cov)
    cov_first <- subj_rows$within_cov[1]
    if (isTRUE(all.equal(cov_mean, cov_first))) next  # nothing to assert

    nd_mean <- data.frame(
      id = sid, x = subj_rows$x[1], y = subj_rows$y[1],
      within_cov = cov_mean
    )
    nd_first <- data.frame(
      id = sid, x = subj_rows$x[1], y = subj_rows$y[1],
      within_cov = cov_first
    )
    bp_mean <- beezdemand:::.tmb_build_predicted_pars(fit, nd_mean)
    bp_first <- beezdemand:::.tmb_build_predicted_pars(fit, nd_first)

    exp_q0 <- exp_pars$Q0[as.character(exp_pars$id) == sid]
    exp_alpha <- exp_pars$alpha[as.character(exp_pars$id) == sid]

    expect_equal(exp_q0, bp_mean$Q0, tolerance = 1e-8,
                 info = paste("subject", sid, "Q0 mean-conditioned"))
    expect_equal(exp_alpha, bp_mean$alpha, tolerance = 1e-8,
                 info = paste("subject", sid, "alpha mean-conditioned"))
    expect_false(isTRUE(all.equal(exp_q0, bp_first$Q0, tolerance = 1e-6)),
                 info = paste("subject", sid, "Q0 NOT first-row"))
  }
})

test_that("plot(type='individual', ids=...) filters before the NA guard", {
  skip_on_cran()

  # Construct a fit where SOME subjects have NA Q0 and others do not.
  # We do this by manually setting Q0/alpha to NA for a subset of
  # subject_pars rows after fitting an intercepts-only model, which
  # mirrors the structural state the M1 spec produces.
  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  # Snapshot non-NA ids and the ones we're going to NA out.
  non_na_ids <- as.character(fit$subject_pars$id[c(1L, 2L)])
  na_ids <- as.character(fit$subject_pars$id[3L])

  # Inject NA for a subset, simulating the within-id-varying state.
  na_rows <- as.character(fit$subject_pars$id) %in% na_ids
  fit$subject_pars$Q0[na_rows] <- NA_real_
  fit$subject_pars$alpha[na_rows] <- NA_real_
  fit$subject_pars$Pmax[na_rows] <- NA_real_
  fit$subject_pars$Omax[na_rows] <- NA_real_

  # Plotting a non-NA subset works (filter applied before the guard).
  expect_no_error(
    p <- plot(fit, type = "individual", ids = non_na_ids)
  )
  expect_s3_class(p, "ggplot")

  # Plotting an NA subset aborts with the targeted message.
  expect_error(
    plot(fit, type = "individual", ids = na_ids),
    regexp = "expanded = TRUE"
  )
})

# ---------------------------------------------------------------------------
# Existing intercept-only / between-subject-factor fits unchanged.
# ---------------------------------------------------------------------------

test_that("expanded = TRUE on intercept-only fit returns wide shape unchanged", {
  skip_on_cran()

  data(apt, package = "beezdemand")
  fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)

  default_pars <- get_subject_pars(fit)
  expanded_pars <- get_subject_pars(fit, expanded = TRUE)
  # No within-subject factor -> expanded result is identical to default.
  expect_equal(nrow(expanded_pars), nrow(default_pars))
  expect_equal(expanded_pars$id, default_pars$id)
  expect_equal(expanded_pars$Q0, default_pars$Q0)
})
