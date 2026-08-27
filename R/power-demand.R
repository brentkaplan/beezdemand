# ==============================================================================
# Monte Carlo power analysis for within-subject demand designs
#
# power_demand() simulates data under assumed population parameters + a single
# fixed-effect delta using .simulate_within_subject_demand(), refits each
# replicate with fit_demand_tmb(), and estimates power as the proportion of
# usable fits whose Wald test on the target condition contrast rejects at the
# nominal alpha. find_n_demand() is a bisection-search wrapper.
# ==============================================================================

# Simulator-aligned design defaults (mirrors .simulate_within_subject_demand).
.power_demand_design_defaults <- function() {
  list(
    prices = c(0.1, 0.5, 1, 2, 5, 10, 20),
    log_q0_pop = log(20),
    log_alpha_pop = log(0.005),
    sigma_b = 0.3,
    sigma_d = 0.3,
    rho_bd = 0,
    sigma_e = 0.1
  )
}

#' Resolve the default random-effects specification for a design type
#'
#' Returns the user-supplied specification unchanged, or the design-appropriate
#' default when `random_effects` is `NULL`: per-condition subject effects for
#' the within-subject design, per-subject intercepts for the between-subject
#' design (each subject appears in only one condition there).
#' @keywords internal
#' @noRd
.power_resolve_random_effects <- function(random_effects, design_type) {
  if (!is.null(random_effects)) {
    return(random_effects)
  }
  if (design_type == "between") {
    nlme::pdDiag(Q0 + alpha ~ 1)
  } else {
    nlme::pdDiag(Q0 + alpha ~ condition - 1)
  }
}

#' Wilson score interval for a binomial proportion
#'
#' @param x Number of successes.
#' @param n Number of trials.
#' @param level Confidence level.
#' @return Numeric length-2 vector (lower, upper); `c(NA, NA)` when `n = 0`.
#' @keywords internal
#' @noRd
.power_wilson_ci <- function(x, n, level = 0.95) {
  if (n == 0) {
    return(c(NA_real_, NA_real_))
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half <- (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  c(max(0, center - half), min(1, center + half))
}

#' Classify one Monte Carlo replicate and compute its Wald verdicts
#'
#' Pure function so the exclusion rules are unit-testable: a replicate enters
#' the power denominator only when the fit converged, the Hessian is positive
#' definite, and the target-term SE is finite. Anything else is surfaced via
#' `status` and its hit indicators are `NA` -- never counted as "no effect
#' detected".
#'
#' The Wald statistic is referred to a t distribution with `df` degrees of
#' freedom rather than the asymptotic normal: with plug-in variance estimates
#' the null z statistics are t-like at study-relevant N (empirically,
#' SD(z) ~ 1.14 at n = 15, matching t with n - 1 df; the asymptotic z test
#' had Type I error 0.089 at nominal .05 in the calibration battery). `df =
#' Inf` recovers the z-test.
#'
#' @param converged,hessian_pd Logical fit diagnostics.
#' @param estimate,se Target-term estimate and SE on the estimation (natural
#'   log) scale.
#' @param alpha Nominal test level.
#' @param df Degrees of freedom for the t reference distribution.
#' @return List with status, estimate, se, statistic, p_value, ci_lower,
#'   ci_upper, hit_p, hit_ci.
#' @keywords internal
#' @noRd
.power_rep_row <- function(
  converged,
  hessian_pd,
  estimate,
  se,
  alpha,
  df = Inf
) {
  status <- if (!isTRUE(converged)) {
    "nonconverged"
  } else if (!isTRUE(hessian_pd)) {
    "hessian_not_pd"
  } else if (
    !is.numeric(estimate) ||
      !is.finite(estimate) ||
      !is.numeric(se) ||
      !is.finite(se) ||
      se <= 0
  ) {
    "se_unusable"
  } else {
    "ok"
  }

  est_out <- if (is.numeric(estimate) && length(estimate) == 1) {
    as.numeric(estimate)
  } else {
    NA_real_
  }
  se_out <- if (is.numeric(se) && length(se) == 1) as.numeric(se) else NA_real_

  if (status != "ok") {
    return(list(
      status = status,
      estimate = est_out,
      se = se_out,
      statistic = NA_real_,
      p_value = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      hit_p = NA,
      hit_ci = NA
    ))
  }

  z <- estimate / se
  p <- 2 * stats::pt(-abs(z), df = df)
  crit <- stats::qt(1 - alpha / 2, df = df)
  ci_lower <- estimate - crit * se
  ci_upper <- estimate + crit * se
  list(
    status = status,
    estimate = est_out,
    se = se_out,
    statistic = z,
    p_value = p,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    hit_p = p < alpha,
    hit_ci = ci_lower > 0 || ci_upper < 0
  )
}

#' Validate the single-delta effect specification
#' @keywords internal
#' @noRd
.power_validate_effect <- function(effect, allowed) {
  if (!is.list(effect)) {
    cli::cli_abort("{.arg effect} must be a list.")
  }
  .power_require_named_unique(effect, "effect")
  bad <- setdiff(names(effect), allowed)
  if (length(bad) > 0) {
    cli::cli_abort(
      "{.arg effect} may only contain {.val {allowed}}, not {.val {bad}}."
    )
  }
  supplied <- allowed[vapply(
    allowed,
    function(nm) !is.null(effect[[nm]]),
    logical(1)
  )]
  if (length(supplied) != 1) {
    cli::cli_abort(c(
      "Supply exactly one of {.val {allowed}} in {.arg effect}.",
      "i" = "A single fixed-effect delta is supported; the other element must be NULL."
    ))
  }
  delta <- effect[[supplied]]
  if (!is.numeric(delta) || length(delta) != 1 || !is.finite(delta)) {
    cli::cli_abort(
      "`effect${supplied}` must be a single finite number (0 is allowed)."
    )
  }
  list(name = supplied, delta = as.numeric(delta))
}

#' Validate and merge the design list over simulator defaults
#' @keywords internal
#' @noRd
.power_validate_design <- function(design, defaults) {
  if (!is.list(design)) {
    cli::cli_abort("{.arg design} must be a list.")
  }
  .power_require_named_unique(design, "design")
  bad <- setdiff(names(design), names(defaults))
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "Unknown {.arg design} elements: {.val {bad}}.",
      "i" = "Allowed elements: {.val {names(defaults)}}."
    ))
  }
  merged <- utils::modifyList(defaults, design)
  if (
    "rho_bd" %in% names(defaults) && !identical(as.numeric(merged$rho_bd), 0)
  ) {
    cli::cli_abort(c(
      "{.code design$rho_bd} must be 0 in this version.",
      "i" = "The default random-effects specification (independent per-condition
             Q0/alpha effects) is misspecified under correlated deviations, which
             would invalidate the calibration guarantees. Correlated random
             effects are future work."
    ))
  }
  for (nm in setdiff(
    names(defaults),
    c("prices", "delays", "family", "equation")
  )) {
    v <- merged[[nm]]
    if (!is.numeric(v) || length(v) != 1 || !is.finite(v)) {
      cli::cli_abort("`design${nm}` must be a single finite number.")
    }
  }
  sd_pars <- intersect(
    c("sigma_b", "sigma_d", "sigma_u", "sigma_e", "phi"),
    names(defaults)
  )
  for (nm in sd_pars) {
    if (merged[[nm]] <= 0) {
      cli::cli_abort("`design${nm}` must be > 0.")
    }
  }
  grid_nm <- if ("prices" %in% names(defaults)) "prices" else "delays"
  grid <- merged[[grid_nm]]
  if (
    !is.numeric(grid) ||
      length(grid) < 2 ||
      any(!is.finite(grid)) ||
      any(grid < 0)
  ) {
    cli::cli_abort(
      "`design${grid_nm}` must be a finite non-negative numeric vector of
       length >= 2."
    )
  }
  merged
}

#' Require every element of a user-supplied list to be uniquely named
#' @keywords internal
#' @noRd
.power_require_named_unique <- function(x, arg) {
  if (length(x) == 0) {
    return(invisible(NULL))
  }
  nms <- names(x)
  if (is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort(
      "Every element of {.arg {arg}} must be named (unnamed elements would be
       silently ignored)."
    )
  }
  if (anyDuplicated(nms)) {
    cli::cli_abort(
      "{.arg {arg}} contains duplicated names: {.val {unique(nms[duplicated(nms)])}}."
    )
  }
  invisible(NULL)
}

#' Validate the seed argument
#' @keywords internal
#' @noRd
.power_validate_seed <- function(seed) {
  if (is.null(seed)) {
    return(invisible(NULL))
  }
  if (
    !is.numeric(seed) ||
      length(seed) != 1 ||
      !is.finite(seed) ||
      seed != round(seed) ||
      abs(seed) > .Machine$integer.max
  ) {
    cli::cli_abort(
      "{.arg seed} must be NULL or a single whole number representable as an
       integer."
    )
  }
  invisible(NULL)
}


#' Validate the df argument, resolving NULL to the design-based default
#' @keywords internal
#' @noRd
.power_validate_df <- function(df, default_df) {
  if (is.null(df)) {
    if (!is.null(default_df) && default_df <= 0) {
      cli::cli_abort(
        "{.arg n_subjects} is too small for the design-based default {.arg df}
         ({default_df}); increase {.arg n_subjects} or supply {.arg df}."
      )
    }
    return(default_df)
  }
  if (!is.numeric(df) || length(df) != 1 || is.na(df) || df <= 0) {
    cli::cli_abort(
      "{.arg df} must be NULL (design-based default), a positive number, or Inf."
    )
  }
  df
}

#' Shared scalar-argument validation for the power engines
#' @keywords internal
#' @noRd
.power_validate_scalars <- function(n_subjects, n_sim, alpha) {
  if (
    !is.numeric(n_subjects) ||
      length(n_subjects) != 1 ||
      !is.finite(n_subjects) ||
      n_subjects != round(n_subjects) ||
      n_subjects < 2
  ) {
    cli::cli_abort("{.arg n_subjects} must be a single whole number >= 2.")
  }
  if (
    !is.numeric(n_sim) ||
      length(n_sim) != 1 ||
      !is.finite(n_sim) ||
      n_sim != round(n_sim) ||
      n_sim < 1
  ) {
    cli::cli_abort("{.arg n_sim} must be a single whole number >= 1.")
  }
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1 ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    cli::cli_abort(
      "{.arg alpha} must be a single number strictly between 0 and 1."
    )
  }
  invisible(NULL)
}

#' Compose a between-subject two-arm demand dataset
#'
#' Builds a between-subject design by calling
#' `.simulate_within_subject_demand()` **once per arm with `n_conditions = 1`**:
#' each subject is assigned to exactly one arm/condition, so the simulator's
#' per-(subject, condition) random effects degenerate to plain per-subject
#' random effects (`sigma_b`/`sigma_d` keep their meaning) and no new
#' data-generating process is introduced. Arm 1 (`ceiling(n/2)` subjects,
#' condition `"C1"`) carries no shift; arm 2 (`floor(n/2)` subjects, condition
#' `"C2"`) carries the single `delta` on the target parameter. Arm-2 subject
#' ids are offset so ids are unique across arms.
#'
#' @param n_subjects Total number of subjects across the two arms.
#' @param target_param `"Q0"` or `"alpha"`; the parameter the group difference
#'   acts on.
#' @param delta The condition-2 shift on natural-log Q0 (or log alpha).
#' @param design Merged design list (see `.power_demand_design_defaults`).
#' @return A long-format tibble (`id`, `condition`, `x`, `y`) with `condition`
#'   a factor with levels `c("C1", "C2")`.
#' @keywords internal
#' @noRd
.simulate_between_subject_demand <- function(
  n_subjects,
  target_param,
  delta,
  design
) {
  n1 <- ceiling(n_subjects / 2)
  n2 <- n_subjects - n1
  sim_arm <- function(n, d, label) {
    sim <- .simulate_within_subject_demand(
      n_subjects = n,
      n_conditions = 1,
      prices = design$prices,
      log_q0_pop = design$log_q0_pop,
      log_alpha_pop = design$log_alpha_pop,
      delta_q0 = if (target_param == "Q0") d else 0,
      delta_alpha = if (target_param == "alpha") d else 0,
      sigma_b = design$sigma_b,
      sigma_d = design$sigma_d,
      rho_bd = design$rho_bd,
      sigma_e = design$sigma_e,
      seed = NULL
    )
    sim$condition <- label
    sim
  }
  arm1 <- sim_arm(n1, 0, "C1")
  arm2 <- sim_arm(n2, delta, "C2")
  arm2$id <- factor(as.integer(arm2$id) + n1)
  out <- dplyr::bind_rows(arm1, arm2)
  out$id <- factor(out$id)
  out$condition <- factor(out$condition, levels = c("C1", "C2"))
  out
}

#' Run the demand Monte Carlo replicate loop
#'
#' Each replicate simulates a two-condition dataset (within-subject when
#' `design_type = "within"`, two independent between-subject arms when
#' `design_type = "between"`), refits it, and extracts the target contrast on
#' the estimation (natural log) scale via
#' `tidy(fit, report_space = "internal")`. Replicate-level errors are caught
#' and recorded as `status = "error"`, never propagated.
#'
#' @keywords internal
#' @noRd
.power_demand_replicates <- function(
  n_subjects,
  target_param,
  delta,
  design,
  n_sim,
  alpha,
  df,
  equation,
  random_effects,
  multi_start,
  fit_args,
  design_type = "within",
  sim_offset = 0L,
  verbose = FALSE
) {
  target_term <- paste0(target_param, ":conditionC2")
  rows <- vector("list", n_sim)
  # Force treatment coding for the duration of the loop so the target term
  # name and estimand (C2 - C1 on the log scale) survive user-level
  # options(contrasts = ...) such as contr.sum. The factor-attribute route is
  # not sufficient here: the fitter's internal factor handling can rebuild
  # the factor and drop a per-factor contrasts attribute.
  old_opts <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(old_opts), add = TRUE)
  if (verbose) {
    cli::cli_progress_bar(
      "Monte Carlo replicates (n_subjects = {n_subjects})",
      total = n_sim
    )
  }

  for (i in seq_len(n_sim)) {
    rows[[i]] <- tryCatch(
      {
        sim <- if (design_type == "between") {
          .simulate_between_subject_demand(
            n_subjects = n_subjects,
            target_param = target_param,
            delta = delta,
            design = design
          )
        } else {
          .simulate_within_subject_demand(
            n_subjects = n_subjects,
            n_conditions = 2,
            prices = design$prices,
            log_q0_pop = design$log_q0_pop,
            log_alpha_pop = design$log_alpha_pop,
            delta_q0 = if (target_param == "Q0") c(0, delta) else c(0, 0),
            delta_alpha = if (target_param == "alpha") c(0, delta) else c(0, 0),
            sigma_b = design$sigma_b,
            sigma_d = design$sigma_d,
            rho_bd = design$rho_bd,
            sigma_e = design$sigma_e,
            seed = NULL
          )
        }
        fit <- suppressWarnings(suppressMessages(do.call(
          fit_demand_tmb,
          c(
            list(
              data = sim,
              y_var = "y",
              x_var = "x",
              id_var = "id",
              equation = equation,
              factors = "condition",
              random_effects = random_effects,
              multi_start = multi_start,
              verbose = 0
            ),
            fit_args
          )
        )))

        est <- NA_real_
        se <- NA_real_
        extract_msg <- NA_character_
        if (!isTRUE(fit$converged)) {
          opt_msg <- tryCatch(
            as.character(fit$opt$message)[1],
            error = function(e) NA_character_
          )
          if (length(opt_msg) == 1 && !is.na(opt_msg) && nzchar(opt_msg)) {
            extract_msg <- opt_msg
          }
        }
        if (isTRUE(fit$converged)) {
          td <- tryCatch(
            tidy(fit, effects = "fixed", report_space = "internal"),
            error = function(e) {
              extract_msg <<- paste0("tidy() failed: ", conditionMessage(e))
              NULL
            }
          )
          if (!is.null(td)) {
            hit_row <- td[td$term == target_term, , drop = FALSE]
            if (nrow(hit_row) == 1) {
              est <- hit_row$estimate
              se <- hit_row$std.error
            } else {
              extract_msg <- sprintf(
                "target term '%s' matched %d rows in tidy() output",
                target_term,
                nrow(hit_row)
              )
            }
          }
        }

        rep_out <- .power_rep_row(
          converged = isTRUE(fit$converged),
          hessian_pd = isTRUE(fit$hessian_pd),
          estimate = est,
          se = se,
          alpha = alpha,
          df = df
        )
        c(
          list(
            sim = i + sim_offset,
            converged = isTRUE(fit$converged),
            hessian_pd = isTRUE(fit$hessian_pd)
          ),
          rep_out,
          list(message = extract_msg)
        )
      },
      error = function(e) {
        row <- c(
          list(sim = i + sim_offset, converged = FALSE, hessian_pd = FALSE),
          .power_rep_row(FALSE, FALSE, NA_real_, NA_real_, alpha, df),
          list(message = conditionMessage(e))
        )
        # A caught execution error is distinct from ordinary optimizer
        # nonconvergence (whose optimizer message is surfaced above).
        row$status <- "error"
        row
      }
    )
    if (verbose) cli::cli_progress_update()
  }
  if (verbose) {
    cli::cli_progress_done()
  }

  out <- dplyr::bind_rows(rows)
  out[, c(
    "sim",
    "status",
    "converged",
    "hessian_pd",
    "estimate",
    "se",
    "statistic",
    "p_value",
    "ci_lower",
    "ci_upper",
    "hit_p",
    "hit_ci",
    "message"
  )]
}

#' Summarize a replicate table into power estimates and diagnostics
#' @keywords internal
#' @noRd
.power_summarize <- function(replicates, mc_ci_level = 0.95) {
  ok <- replicates$status == "ok"
  n_used <- sum(ok)
  hits_ci <- sum(replicates$hit_ci[ok])
  hits_p <- sum(replicates$hit_p[ok])
  list(
    power = if (n_used > 0) hits_ci / n_used else NA_real_,
    power_mc_ci = .power_wilson_ci(hits_ci, n_used, mc_ci_level),
    hit_rate_p = if (n_used > 0) hits_p / n_used else NA_real_,
    hit_rate_ci = if (n_used > 0) hits_ci / n_used else NA_real_,
    n_sim = nrow(replicates),
    n_converged = sum(replicates$converged),
    n_hessian_pd = sum(replicates$hessian_pd),
    n_used = n_used
  )
}

#' Warn when too few replicates produced usable fits
#' @keywords internal
#' @noRd
.power_usable_fraction_warn <- function(n_used, n_sim, threshold = 0.95) {
  if (n_sim > 0 && n_used / n_sim < threshold) {
    cli::cli_warn(c(
      "!" = "Only {n_used}/{n_sim} replicate{?s} produced usable fits
             (converged, positive-definite Hessian, finite SE).",
      "i" = "The reported power is conditional on a usable fit and can be
             selected when convergence depends on the realized data. Inspect
             {.code $replicates$status} and consider a larger {.arg n_subjects},
             a simpler random-effects specification, or
             {.code multi_start = TRUE}."
    ))
  }
  invisible(NULL)
}

#' Monte Carlo power analysis for two-condition demand designs
#'
#' @description
#' Estimates statistical power to detect a single fixed-effect difference in a
#' demand parameter (`Q0` or `alpha`) between two conditions, by simulation:
#' each replicate (1) simulates a two-condition dataset from the mixed-effects
#' demand model in `.simulate_within_subject_demand()` under assumed
#' population parameters plus the effect `delta`, under either a
#' within-subject design (every subject observed in both conditions) or, with
#' `design_type = "between"`, a two-arm between-subject design (each subject
#' in one condition); (2) refits it with [fit_demand_tmb()]; and (3) tests the
#' condition contrast on the estimation (natural log) scale with a Wald test
#' at level `alpha`, referred to a t distribution with `df` degrees of freedom
#' (see the `df` argument). Power is the proportion of *usable* fits
#' (converged, positive-definite Hessian, finite standard error) that reject.
#'
#' Because the power estimate is a proportion from finitely many replicates,
#' it is reported with a Wilson score confidence interval (`power_mc_ci`).
#' Both a p-value verdict (`p < alpha`) and a confidence-interval verdict
#' (Wald CI excludes 0) are recorded per replicate; they use the same
#' standard error and reference distribution, so they coincide by
#' construction, and both rates are returned.
#'
#' @param n_subjects Number of simulated subjects per replicate. For
#'   `design_type = "within"` each subject is observed at every price in both
#'   conditions; for `"between"` this is the *total* sample, split
#'   `ceiling(n_subjects / 2)` to condition 1 and the rest to condition 2
#'   (an odd total therefore gives arms differing by one subject).
#' @param effect Named list supplying exactly one of `delta_q0` or
#'   `delta_alpha`: the true condition shift on natural-log Q0 (or natural-log
#'   alpha) for condition 2 relative to condition 1. `0` is allowed (useful
#'   for Type I error checks). E.g. `delta_q0 = log(1.5)` means condition 2's
#'   Q0 is 1.5 times condition 1's.
#' @param design Named list of data-generating settings, merged over the
#'   simulator defaults: `prices` (vector), `log_q0_pop`, `log_alpha_pop`,
#'   `sigma_b` (per-condition subject SD on log Q0), `sigma_d` (same for log
#'   alpha), `rho_bd` (must be 0 in this version), and `sigma_e` (residual SD
#'   on log consumption). All SDs must be strictly positive.
#' @param n_sim Number of Monte Carlo replicates. 500 (default) is suitable
#'   for interactive exploration; use 2000+ for grant-quality precision (see
#'   `vignette("power-analysis")`).
#' @param alpha Nominal two-sided test level.
#' @param df Degrees of freedom for the Wald test's t reference
#'   distribution. `NULL` (default) uses `n_subjects - 1` for `design_type =
#'   "within"` and `n_subjects - 2` for `design_type = "between"` (the
#'   two-sample df). This is an *empirically calibrated* small-sample correction
#'   rather than a model-derived df (the TMB fit has no exact t sampling
#'   theory): the asymptotic z-test was measurably anticonservative in the
#'   package's Type I calibration battery (empirical rate 0.089 at nominal
#'   .05 with 15 subjects), while the t reference passes the battery's null
#'   checks across the tested sample sizes, target parameters, residual-SD
#'   settings, and both designs. `Inf` gives the asymptotic z-test.
#' @param seed Optional integer seed; identical seeds give identical
#'   results. The caller's RNG state is restored on exit.
#' @param equation Demand equation passed to [fit_demand_tmb()]. The default
#'   `"simplified"` shares the simulator's mean function, so the simulated
#'   `delta` and the fitted contrast share the same scale. Note the error
#'   model is a *working model*: the simulator draws multiplicative
#'   lognormal errors while `"simplified"` fits additive Gaussian errors on
#'   raw consumption; the approximation is closest at small `sigma_e`, and
#'   Type I calibration is verified by the test suite at the default and a
#'   3x-larger `sigma_e`. Other equations are sensitivity analyses with a
#'   different estimand, so their contrasts are not on the scale of the
#'   simulated delta.
#' @param design_type Either `"within"` (default) or `"between"`. `"within"`
#'   simulates the two-condition within-subject design (every subject observed
#'   at every price in both conditions) and tests the within-subject condition
#'   contrast. `"between"` assigns each subject to exactly one of two arms
#'   (`ceiling(n/2)` to condition 1, the rest to condition 2) and tests the
#'   group difference; the two-arm dataset is composed from the same simulator
#'   run once per arm with a single condition, so no new data-generating
#'   process is introduced (see Details). The `df` and `random_effects`
#'   defaults track `design_type`.
#' @param random_effects Random-effects specification passed to
#'   [fit_demand_tmb()]. `NULL` (default) resolves to a specification matching
#'   `design_type`: `nlme::pdDiag(Q0 + alpha ~ condition - 1)` for
#'   `"within"` (independent per-condition subject effects on both parameters,
#'   matching the simulator's data-generating process when `rho_bd = 0`), and
#'   `nlme::pdDiag(Q0 + alpha ~ 1)` for `"between"` (per-subject intercepts,
#'   which are correctly specified because each subject appears in only one
#'   condition). Supply a specification to override.
#' @param multi_start Passed to [fit_demand_tmb()]. Defaults to `FALSE` for
#'   speed (roughly 3x fewer optimizations); non-convergent replicates are
#'   excluded and surfaced rather than biasing the estimate.
#' @param verbose Logical; show a progress bar.
#' @param ... Additional arguments passed to [fit_demand_tmb()] (e.g.
#'   `tmb_control`).
#'
#' @return An object of class `beezdemand_power`: a list with
#'   \describe{
#'     \item{power}{Estimated power: proportion of usable replicates whose
#'       Wald CI excludes 0 (equal to `hit_rate_ci`). `NA` if no replicate
#'       was usable.}
#'     \item{power_mc_ci}{Wilson 95% confidence interval on `power`,
#'       reflecting Monte Carlo uncertainty from `n_used` replicates.}
#'     \item{hit_rate_p}{Proportion of usable replicates with `p < alpha`.}
#'     \item{hit_rate_ci}{Proportion of usable replicates whose Wald CI
#'       excludes 0 (the same decision rule as `hit_rate_p`, since both use
#'       the same SE and t reference; both reported).}
#'     \item{n_sim}{Total replicates attempted.}
#'     \item{n_converged}{Replicates whose fit converged.}
#'     \item{n_hessian_pd}{Replicates with a positive-definite Hessian.}
#'     \item{n_used}{Replicates entering the power denominator (converged,
#'       positive-definite Hessian, finite SE).}
#'     \item{alpha}{Nominal test level.}
#'     \item{df}{Degrees of freedom of the t reference distribution actually
#'       used (`n_subjects - 1` for `"within"`, `n_subjects - 2` for
#'       `"between"`, unless overridden).}
#'     \item{effect}{The validated effect specification (name and delta).}
#'     \item{target_term}{The tested coefficient (e.g. `"Q0:conditionC2"`).}
#'     \item{design}{The merged design list actually used.}
#'     \item{n_subjects}{As supplied.}
#'     \item{replicates}{Tibble with one row per replicate: `sim`, `status`
#'       (`"ok"`, `"nonconverged"`, `"hessian_not_pd"`, `"se_unusable"`,
#'       `"error"`), `converged`, `hessian_pd`, `estimate`, `se`,
#'       `statistic`, `p_value`, `ci_lower`, `ci_upper`, `hit_p`, `hit_ci`,
#'       and `message` (error text, if any). Estimates are on the natural-log
#'       scale of the simulated delta.}
#'     \item{seed}{As supplied.}
#'     \item{settings}{List of `equation`, `design_type`, `multi_start`, and
#'       the deparsed random-effects specification.}
#'     \item{call}{The matched call.}
#'   }
#'
#' @details
#' A replicate whose fit fails (non-convergence, non-positive-definite
#' Hessian, unusable standard error, or an error) is excluded from the power
#' denominator and reported through the `n_*` counts and `$replicates$status`.
#' It is never counted as "no effect detected", which would bias power in
#' an unpredictable direction. A warning is issued when fewer than 95% of
#' replicates are usable, since power conditional on convergence can be
#' selected when convergence depends on the realized data.
#'
#' The v1 scope is a single fixed-effect delta. Joint Q0 + alpha effects,
#' power for derived measures (Pmax, Omax), and arbitrary designs are out of
#' scope; see `vignette("power-analysis")`.
#'
#' For `design_type = "between"`, the two arms are composed by running the
#' within-subject simulator once per arm with a single condition, then binding
#' the arms and refitting with per-subject intercept random effects. Because
#' each subject appears in only one condition, that random-effects *structure*
#' matches the composed data-generating process exactly, unlike the
#' within-subject default's per-condition effects. The additive-Gaussian
#' residual likelihood remains a *working model* for the simulator's
#' multiplicative-lognormal errors in both designs (closest at small
#' `sigma_e`). Type I error is calibrated by the test suite for both designs.
#'
#' @examples
#' \donttest{
#' # Tiny illustrative run (use n_sim >= 500 for real planning; the
#' # Monte Carlo interval at n_sim = 5 is uninformative by design)
#' res <- power_demand(
#'   n_subjects = 12,
#'   effect = list(delta_q0 = log(1.5)),
#'   n_sim = 5, seed = 1, verbose = FALSE
#' )
#' print(res)
#'
#' # Between-subject design: group difference in Q0 across two arms
#' res_b <- power_demand(
#'   n_subjects = 16,
#'   effect = list(delta_q0 = log(1.5)),
#'   design_type = "between",
#'   n_sim = 5, seed = 1, verbose = FALSE
#' )
#' print(res_b)
#' }
#'
#' @seealso [find_n_demand()] to search for the smallest adequate sample
#'   size; [fit_demand_tmb()] for the model being refit.
#' @family power-analysis
#' @export
power_demand <- function(
  n_subjects,
  effect = list(delta_q0 = NULL, delta_alpha = NULL),
  design = list(),
  n_sim = 500,
  alpha = 0.05,
  df = NULL,
  seed = NULL,
  equation = "simplified",
  random_effects = NULL,
  multi_start = FALSE,
  verbose = TRUE,
  design_type = c("within", "between"),
  ...
) {
  cl <- match.call()
  design_type <- match.arg(design_type)
  eff <- .power_validate_effect(effect, c("delta_q0", "delta_alpha"))
  design <- .power_validate_design(design, .power_demand_design_defaults())
  .power_validate_scalars(n_subjects, n_sim, alpha)
  .power_validate_seed(seed)
  df <- .power_validate_df(
    df,
    n_subjects - if (design_type == "within") 1L else 2L
  )
  random_effects <- .power_resolve_random_effects(random_effects, design_type)
  fit_args <- .power_check_fit_args(list(...))

  target_param <- if (eff$name == "delta_q0") "Q0" else "alpha"
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = globalenv())
      } else if (
        exists(".Random.seed", envir = globalenv(), inherits = FALSE)
      ) {
        rm(".Random.seed", envir = globalenv())
      },
      add = TRUE
    )
    set.seed(seed)
  }

  replicates <- .power_demand_replicates(
    n_subjects = n_subjects,
    target_param = target_param,
    delta = eff$delta,
    design = design,
    n_sim = n_sim,
    alpha = alpha,
    df = df,
    equation = equation,
    random_effects = random_effects,
    multi_start = multi_start,
    fit_args = fit_args,
    design_type = design_type,
    verbose = verbose
  )
  s <- .power_summarize(replicates)
  .power_usable_fraction_warn(s$n_used, s$n_sim)

  structure(
    c(
      s,
      list(
        alpha = alpha,
        df = df,
        effect = eff,
        target_term = paste0(target_param, ":conditionC2"),
        design = design,
        n_subjects = n_subjects,
        replicates = replicates,
        seed = seed,
        settings = list(
          equation = equation,
          design_type = design_type,
          multi_start = multi_start,
          random_effects = paste(deparse(random_effects), collapse = " ")
        ),
        call = cl
      )
    ),
    class = "beezdemand_power"
  )
}

#' Reject fit arguments that the power engine sets itself
#' @keywords internal
#' @noRd
.power_check_fit_args <- function(fit_args) {
  if (
    length(fit_args) > 0 &&
      (is.null(names(fit_args)) || any(!nzchar(names(fit_args))))
  ) {
    cli::cli_abort(
      "All arguments passed via {.arg ...} must be named (they are forwarded
       to the fitting function)."
    )
  }
  reserved <- c("data", "y_var", "x_var", "id_var", "factors")
  bad <- intersect(names(fit_args), reserved)
  if (length(bad) > 0) {
    cli::cli_abort(
      "Arguments {.val {bad}} are set by the power engine and cannot be
       overridden."
    )
  }
  if (!("validate_subject_pars" %in% names(fit_args))) {
    fit_args$validate_subject_pars <- FALSE
  }
  fit_args
}

#' @export
print.beezdemand_power <- function(x, ...) {
  cat("Monte Carlo power analysis (beezdemand)\n")
  design_type <- x$settings$design_type %||% "within"
  cat(sprintf(
    "  Design: %s-subject (2 conditions)\n",
    design_type
  ))
  cat(sprintf(
    "  Target: %s (%s = %.4g), two-sided alpha = %g, t reference (df = %g)\n",
    x$target_term,
    x$effect$name,
    x$effect$delta,
    x$alpha,
    x$df
  ))
  cat(sprintf(
    "  n_subjects = %d, n_sim = %d (converged %d, usable %d)\n",
    as.integer(x$n_subjects),
    as.integer(x$n_sim),
    as.integer(x$n_converged),
    as.integer(x$n_used)
  ))
  if (is.na(x$power)) {
    cat("  Power: NA (no usable fits)\n")
  } else {
    cat(sprintf(
      "  Power (CI-exclusion): %.3f [95%% MC CI %.3f, %.3f]\n",
      x$power,
      x$power_mc_ci[1],
      x$power_mc_ci[2]
    ))
    cat(sprintf("  p-value hit rate:     %.3f\n", x$hit_rate_p))
  }
  invisible(x)
}

# ==============================================================================
# Sample-size search
# ==============================================================================

#' Evaluate power at one N with adaptive replication at ambiguous results
#' @keywords internal
#' @noRd
.power_eval_adaptive <- function(
  run_batch,
  n,
  target_power,
  n_sim,
  n_sim_max,
  verbose = FALSE
) {
  replicates <- run_batch(n, n_sim, 0L)
  repeat {
    s <- .power_summarize(replicates)
    if (s$n_used == 0) {
      cli::cli_abort(c(
        "No usable fits at {.arg n_subjects} = {n} ({s$n_sim} replicates
         attempted).",
        "i" = "Power cannot be evaluated here; check the design or effect
               specification with {.fn power_demand} directly."
      ))
    }
    ci <- s$power_mc_ci
    decision <- if (ci[1] >= target_power) {
      "above"
    } else if (ci[2] < target_power) {
      "below"
    } else {
      "ambiguous"
    }
    if (decision != "ambiguous" || s$n_sim >= n_sim_max) {
      break
    }
    batch <- min(n_sim, n_sim_max - s$n_sim)
    replicates <- dplyr::bind_rows(
      replicates,
      run_batch(n, batch, nrow(replicates))
    )
  }

  uncertain <- decision == "ambiguous"
  if (uncertain) {
    decision <- if (s$power >= target_power) {
      "ambiguous_above"
    } else {
      "ambiguous_below"
    }
  }
  if (verbose) {
    cli::cli_inform(
      "n_subjects = {n}: power {sprintf('%.3f', s$power)}
       [{sprintf('%.3f', ci[1])}, {sprintf('%.3f', ci[2])}]
       ({s$n_sim} sims) -> {decision}"
    )
  }
  list(
    row = tibble::tibble(
      n_subjects = n,
      n_sim_total = s$n_sim,
      n_used = s$n_used,
      usable_fraction = s$n_used / s$n_sim,
      power = s$power,
      ci_lower = ci[1],
      ci_upper = ci[2],
      decision = decision
    ),
    above = decision %in% c("above", "ambiguous_above"),
    uncertain = uncertain,
    power = s$power
  )
}

#' Is `n_range` a well-formed sample-size search bracket?
#'
#' Two whole numbers with `2 <= n_range[1] < n_range[2]`. Shared by the
#' search's own validation and the between-design lower-bound guard so the two
#' predicates never drift apart.
#' @keywords internal
#' @noRd
.power_n_range_wellformed <- function(n_range) {
  is.numeric(n_range) &&
    length(n_range) == 2 &&
    all(is.finite(n_range)) &&
    all(n_range == round(n_range)) &&
    n_range[1] >= 2 &&
    n_range[1] < n_range[2]
}

#' Shared bisection search over n_subjects
#' @keywords internal
#' @noRd
.power_find_n_search <- function(
  run_batch,
  target_power,
  n_range,
  n_sim,
  n_sim_max,
  verbose
) {
  if (!.power_n_range_wellformed(n_range)) {
    cli::cli_abort(
      "{.arg n_range} must be two whole numbers with 2 <= n_range[1] < n_range[2]."
    )
  }
  if (
    !is.numeric(target_power) ||
      length(target_power) != 1 ||
      !is.finite(target_power) ||
      target_power <= 0 ||
      target_power >= 1
  ) {
    cli::cli_abort(
      "{.arg target_power} must be a single number strictly between 0 and 1."
    )
  }

  lo <- as.integer(n_range[1])
  hi <- as.integer(n_range[2])
  evals <- list()
  any_uncertain <- FALSE

  eval_n <- function(n) {
    res <- .power_eval_adaptive(
      run_batch,
      n,
      target_power,
      n_sim,
      n_sim_max,
      verbose = verbose
    )
    evals[[length(evals) + 1]] <<- res$row
    any_uncertain <<- any_uncertain || res$uncertain
    res
  }

  ev_hi <- eval_n(hi)
  if (!ev_hi$above) {
    cli::cli_abort(c(
      "Estimated power at the upper bound of {.arg n_range}
       ({.val {hi}} subjects) is {sprintf('%.3f', ev_hi$power)}, which does
       not reach the target of {target_power}.",
      "i" = "Increase {.arg n_range}, the effect size, or reconsider the design."
    ))
  }

  finish <- function(n, status, uncertain) {
    evaluations <- dplyr::bind_rows(evals)
    min_usable <- min(evaluations$usable_fraction)
    if (min_usable < 0.95) {
      cli::cli_warn(c(
        "!" = "At least one evaluated sample size had fewer than 95% usable
               fits (minimum usable fraction
               {sprintf('%.2f', min_usable)}).",
        "i" = "Power at those N is conditional on a usable fit; inspect
               {.code $evaluations} and consider {.code multi_start = TRUE}."
      ))
    }
    list(
      n = n,
      status = status,
      uncertain = uncertain,
      evaluations = evaluations
    )
  }

  ev_lo <- eval_n(lo)
  if (ev_lo$above) {
    # The lower bound gets the same fresh-replicate reconfirmation as any
    # selected N: a single look that happened to clear the target is not
    # evidence enough to report it. No lower neighbour exists inside
    # `n_range`, so minimality is NOT claimed ("at_lower_bound").
    conf_lo <- eval_n(lo)
    if (conf_lo$above) {
      return(finish(lo, "at_lower_bound", any_uncertain))
    }
    # Reconfirmation failed: `lo` is not reliably above the target, which is
    # exactly the bracket condition bisection needs -- keep `lo` as the lower
    # end and search upward. (The first look at `lo` stays in $evaluations and
    # marks the final status "uncertain" via the monotonicity check below.)
  }

  while (hi - lo > 1) {
    mid <- as.integer(floor((lo + hi) / 2))
    if (eval_n(mid)$above) hi <- mid else lo <- mid
  }

  # Confirmation pass: re-evaluate the selected N and its lower neighbor with
  # fresh replicates before claiming anything. Three outcomes:
  # - selected N re-confirms above and N - 1 below: "confirmed" (or
  #   "uncertain" if any decision along the way used a point estimate);
  # - selected N FAILS reconfirmation: the search evidence is contradicted;
  #   return n = NA with status "unresolved" rather than a number the run
  #   itself does not support;
  # - N - 1 also clears the target on reconfirmation: N reaches the target
  #   but may not be minimal -> "uncertain".
  conf_hi <- eval_n(hi)
  if (!conf_hi$above) {
    return(finish(NA_integer_, "unresolved", TRUE))
  }
  conf_lo_above <- if (hi - 1 >= n_range[1]) eval_n(hi - 1)$above else FALSE
  # Bisection assumes power is monotone in N. Each N is judged from its own
  # independent replicates, so an evaluated N below the selected one that
  # nevertheless read "above" contradicts the assumption -- the search may
  # have stepped past a lower crossing. Report that as uncertain rather than
  # confirmed. (Only evaluated N can be checked; N never visited cannot be.)
  evaluated <- dplyr::bind_rows(evals)
  lower_above <- any(
    evaluated$n_subjects < hi &
      evaluated$decision %in% c("above", "ambiguous_above")
  )
  status <- if (conf_lo_above || lower_above || any_uncertain) {
    "uncertain"
  } else {
    "confirmed"
  }

  finish(hi, status, any_uncertain || status != "confirmed")
}

#' Find the smallest sample size reaching a target power (demand)
#'
#' @description
#' Bisection search over `n_subjects` for the smallest N whose Monte Carlo
#' power estimate from [power_demand()] reaches `target_power`. The search
#' accounts for Monte Carlo noise: at each evaluated N, replicates are added
#' in batches (up to `n_sim_max`) until the Wilson interval for power lies
#' wholly above or below the target; if it still straddles the target at the
#' cap, the decision falls back to the point estimate and the result is
#' flagged `uncertain`. The selected N and its lower neighbor are then
#' re-evaluated with fresh replicates before minimality is claimed.
#'
#' The returned `n` is an *estimated minimum under Monte Carlo uncertainty*
#' rather than an exact bound. For grant-quality reporting, rerun
#' [power_demand()] at
#' the returned `n` with a large `n_sim` (2000+) and report that estimate
#' with its Monte Carlo confidence interval.
#'
#' **Monotonicity assumption.** Bisection presumes that power is
#' non-decreasing in `n_subjects`. Because every evaluated N is judged from
#' its own independent replicates (and a convergence-conditioned
#' denominator), a Monte Carlo fluctuation can make a lower N read "below"
#' when its true power is above the target, so the search may step past a
#' lower crossing that it never revisits. Evaluated N that contradict the
#' assumption (a lower N reading "above" the selected N) demote the status to
#' `"uncertain"`; N that were never evaluated cannot be checked. Widen
#' `n_sim`/`n_sim_max` when the reported `n` matters. When the target is
#' already met at `n_range[1]`, that bound is likewise re-evaluated with
#' fresh replicates before `"at_lower_bound"` is reported; if the second look
#' does not clear the target the bound is treated as below and the bisection
#' proceeds upward.
#'
#' @param target_power Target power in (0, 1).
#' @inheritParams power_demand
#' @param n_range Integer bracket `c(lower, upper)` to search
#'   (`2 <= lower < upper`). The search errors (rather than extrapolating) if
#'   the target is not reached at `upper`.
#' @param n_sim Replicates per evaluation batch. Smaller than the
#'   [power_demand()] default because several N values are evaluated; the
#'   adaptive rule adds batches where the verdict is close.
#' @param n_sim_max Maximum replicates per evaluated N (default `4 * n_sim`).
#' @param df Degrees of freedom for the Wald test's t reference. `NULL`
#'   (default) tracks the evaluated sample size as `n - 1` for `design_type =
#'   "within"` and `n - 2` for `design_type = "between"`; a numeric value
#'   (or `Inf` for the asymptotic z-test) is used at every evaluated N. With
#'   the default `df` and `design_type = "between"`, `n_range[1]` must be
#'   `>= 3` (df = n - 2 needs n >= 3).
#' @param verbose Logical; report each evaluation.
#' @param ... Additional arguments passed to [fit_demand_tmb()].
#'
#' @return An object of class `beezdemand_power_n`: a list with
#'   \describe{
#'     \item{n}{Estimated smallest `n_subjects` reaching `target_power`;
#'       `NA` when the confirmation pass contradicted the search
#'       (`status = "unresolved"`).}
#'     \item{target_power}{As supplied.}
#'     \item{status}{`"confirmed"` (selected N re-confirmed above target and
#'       N - 1 below), `"uncertain"` (a decision relied on a point estimate,
#'       N - 1 also cleared the target on reconfirmation, or an evaluated
#'       lower N read above the target, so the returned N may not be
#'       minimal), `"unresolved"` (the selected N failed its
#'       own reconfirmation; `n` is `NA`), or `"at_lower_bound"` (the target
#'       was already met at `n_range[1]` on two independent looks; smaller N
#'       was not explored; widen `n_range` downward if that matters). These
#'       labels describe a heuristic Monte Carlo decision rule (repeated
#'       looks at ordinary Wilson intervals across several N) rather than a
#'       formal sequential error guarantee.}
#'     \item{uncertain}{Logical; `TRUE` when any search decision was made on
#'       a point estimate rather than a conclusive Wilson interval, or the
#'       status is not `"confirmed"`/`"at_lower_bound"`.}
#'     \item{evaluations}{Tibble of every evaluation: `n_subjects`,
#'       `n_sim_total`, `n_used`, `usable_fraction`, `power`, `ci_lower`,
#'       `ci_upper`, `decision`. A warning fires when any evaluation had
#'       fewer than 95% usable fits.}
#'     \item{alpha, df, effect, design, n_range, n_sim, n_sim_max, seed,
#'       settings, call}{Echoed inputs and effective settings.}
#'   }
#'
#' @examples
#' \donttest{
#' # Tiny search for demonstration only (use n_sim >= 200 and a wide
#' # n_range for real planning; see vignette("power-analysis"))
#' res <- find_n_demand(
#'   target_power = 0.8,
#'   effect = list(delta_q0 = log(2.5)),
#'   n_range = c(4, 8), n_sim = 5, n_sim_max = 10, seed = 1, verbose = FALSE
#' )
#' print(res)
#' }
#'
#' @seealso [power_demand()] for the Monte Carlo engine.
#' @family power-analysis
#' @export
find_n_demand <- function(
  target_power = 0.8,
  effect = list(delta_q0 = NULL, delta_alpha = NULL),
  design = list(),
  n_range = c(6, 200),
  n_sim = 200,
  n_sim_max = 4 * n_sim,
  alpha = 0.05,
  df = NULL,
  seed = NULL,
  equation = "simplified",
  random_effects = NULL,
  multi_start = FALSE,
  verbose = TRUE,
  design_type = c("within", "between"),
  ...
) {
  cl <- match.call()
  design_type <- match.arg(design_type)
  eff <- .power_validate_effect(effect, c("delta_q0", "delta_alpha"))
  design <- .power_validate_design(design, .power_demand_design_defaults())
  .power_validate_scalars(n_subjects = 2, n_sim = n_sim, alpha = alpha)
  .power_validate_seed(seed)
  if (!is.null(df)) {
    .power_validate_df(df, NULL)
  }
  # With the default df (n - 2 for the between-subject design) the search must
  # not evaluate n < 3. Fire this design-specific guard only for an otherwise
  # well-formed range whose lower bound is too small (a range that would
  # otherwise pass the search's general check); a malformed n_range is left to
  # the search's own dedicated validation, which reports the precise problem.
  # Both call sites share `.power_n_range_wellformed()` so they cannot drift.
  if (
    design_type == "between" &&
      is.null(df) &&
      .power_n_range_wellformed(n_range) &&
      n_range[1] < 3
  ) {
    cli::cli_abort(
      "{.code n_range[1]} must be >= 3 for a between-subject design when
       {.arg df} is NULL (the default df is n - 2)."
    )
  }
  random_effects <- .power_resolve_random_effects(random_effects, design_type)
  if (
    !is.numeric(n_sim_max) ||
      length(n_sim_max) != 1 ||
      !is.finite(n_sim_max) ||
      n_sim_max != round(n_sim_max) ||
      n_sim_max < n_sim
  ) {
    cli::cli_abort(
      "{.arg n_sim_max} must be a single whole number >= {.arg n_sim}."
    )
  }
  fit_args <- .power_check_fit_args(list(...))
  target_param <- if (eff$name == "delta_q0") "Q0" else "alpha"

  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = globalenv())
      } else if (
        exists(".Random.seed", envir = globalenv(), inherits = FALSE)
      ) {
        rm(".Random.seed", envir = globalenv())
      },
      add = TRUE
    )
    set.seed(seed)
  }

  default_df_offset <- if (design_type == "within") 1 else 2
  run_batch <- function(n, batch_size, sim_offset) {
    .power_demand_replicates(
      n_subjects = n,
      target_param = target_param,
      delta = eff$delta,
      design = design,
      n_sim = batch_size,
      alpha = alpha,
      df = if (is.null(df)) n - default_df_offset else df,
      equation = equation,
      random_effects = random_effects,
      multi_start = multi_start,
      fit_args = fit_args,
      design_type = design_type,
      sim_offset = sim_offset,
      verbose = FALSE
    )
  }

  search <- .power_find_n_search(
    run_batch,
    target_power,
    n_range,
    n_sim,
    n_sim_max,
    verbose
  )

  structure(
    c(
      search,
      list(
        target_power = target_power,
        alpha = alpha,
        df = df,
        effect = eff,
        design = design,
        n_range = n_range,
        n_sim = n_sim,
        n_sim_max = n_sim_max,
        seed = seed,
        settings = list(
          equation = equation,
          design_type = design_type,
          multi_start = multi_start,
          random_effects = paste(deparse(random_effects), collapse = " ")
        ),
        call = cl
      )
    ),
    class = "beezdemand_power_n"
  )
}

#' @export
print.beezdemand_power_n <- function(x, ...) {
  cat("Sample-size search (Monte Carlo power)\n")
  cat(sprintf(
    "  Design: %s-subject (2 conditions)\n",
    x$settings$design_type %||% "within"
  ))
  cat(sprintf(
    "  Target power %.2f for %s = %.4g at alpha = %g\n",
    x$target_power,
    x$effect$name,
    x$effect$delta,
    x$alpha
  ))
  if (is.na(x$n)) {
    cat("  No sample size confirmed (status: unresolved) --\n")
    cat("  the confirmation pass contradicted the search; rerun with a\n")
    cat("  larger n_sim / n_sim_max or inspect $evaluations.\n")
  } else {
    cat(sprintf(
      "  Estimated minimum n_subjects = %d (status: %s)\n",
      as.integer(x$n),
      x$status
    ))
    cat("  This is an estimated minimum under Monte Carlo uncertainty;\n")
    cat(
      "  rerun the power function at this N with a large n_sim to report it.\n"
    )
  }
  cat("\n  Evaluations:\n")
  print(as.data.frame(x$evaluations), row.names = FALSE)
  invisible(x)
}
