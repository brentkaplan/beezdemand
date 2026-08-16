#' @title Fixed-Effect Demand Curve Fitting
#' @description Modern wrapper for fitting individual demand curves via nonlinear
#'   least squares. Returns a structured S3 object with standard methods.
#' @name fixed-demand
NULL

#' Fit Fixed-Effect Demand Curves
#'
#' Modern interface for fitting individual demand curves via nonlinear
#' least squares. Returns a structured S3 object with standard methods
#' including `summary()`, `tidy()`, and `glance()`.
#'
#' @param data Data frame in long format with columns: `id`, `x` (price), `y` (consumption).
#' @param equation Character. Equation type: `"hs"` (Hursh & Silberberg, 2008),
#'   `"koff"` (Koffarnus et al., 2015), `"simplified"` (Rzeszutek et al., 2025;
#'   simplified exponential with normalized decay, no `k` parameter), or
#'   `"linear"`. The modern aliases `"exponential"` (equivalent to `"hs"`) and
#'   `"exponentiated"` (equivalent to `"koff"`) are also accepted. Default `"hs"`.
#' @param k Scaling constant. Numeric value (fixed), `"ind"` (individual),
#'   `"fit"` (free parameter), or `"range"` (data-driven). Default `2`.
#' @param agg Character. Aggregation method: `"Mean"`, `"Pooled"`, or `NULL`
#'   for individual fits. Default `NULL`.
#' @param x_var Character. Name of the price column. Default `"x"`.
#' @param y_var Character. Name of the consumption column. Default `"y"`.
#' @param id_var Character. Name of the subject identifier column. Default `"id"`.
#' @param param_space Character. Parameterization used for fitting. One of:
#'   - `"natural"`: fit `Q0`, `alpha` (and `k` if `k = "fit"`) on their natural scale
#'   - `"log10"`: fit `log10(Q0)`, `log10(alpha)` (and `log10(k)` if `k = "fit"`)
#' @param by Optional character vector of column names to group by.
#'   When supplied, fits are run separately within each unique
#'   combination of the `by` columns. Returns a
#'   `beezdemand_fixed_grouped` object with per-group child fits.
#'   Default `NULL` (no grouping).
#' @param multistart Logical. If `TRUE` (the default), subjects whose
#'   production-heuristic fit is not strict-converged (`converged_strict`;
#'   see Details) are automatically re-fit from `S - 1` additional sampled
#'   starting values. Subjects that strict-converge on the production start
#'   are never refit, so their results are byte-identical whether
#'   `multistart` is `TRUE` or `FALSE`. Set to `FALSE` (or `S = 1`) to
#'   reproduce the legacy single-start behavior exactly. Not applicable to
#'   `equation = "linear"` (closed-form; never multistarted).
#' @param S Integer or `NULL`. Total number of starts to try per subject
#'   (including the production start), when `multistart = TRUE`. Default
#'   `NULL` uses a tiered budget: 8 for 2-parameter forms (hs/koff/simplified
#'   with a fixed `k`), 32 when `k = "fit"`. Ignored for `equation =
#'   "linear"`. If supplied, must be a single finite integer `>= 1`.
#'
#'   Note: `multistart` and `S` were added AFTER `by` in the argument list
#'   (Codex 2F review fold, TICKET-047 item 1) specifically so that
#'   pre-existing positional calls -- e.g.
#'   `fit_demand_fixed(data, "hs", 2, NULL, "x", "y", "id", "natural",
#'   "group_col")`, where the 9th positional argument is `by` -- continue to
#'   bind correctly. Always pass `multistart`/`S` by name.
#' @param ... Additional arguments passed to the underlying `FitCurves()` engine.
#'
#' @return An object of class `beezdemand_fixed` with components:
#'   \describe{
#'     \item{results}{Data frame of fitted parameters for each subject.
#'       Gains `n_starts_tried`, `n_starts_converged`, and `start_source`
#'       (`"production"`, `"sampled"`, or `"none"`) from the multi-start
#'       protocol; see Details.}
#'     \item{fits}{List of model fit objects (if `detailed = TRUE` internally)}
#'     \item{predictions}{List of prediction data frames}
#'     \item{data_used}{List of data frames used for each fit}
#'     \item{multistart}{List describing the multi-start protocol: `multistart`,
#'       `S` (resolved budget), `equation`, `eligible` (whether this equation
#'       supports rescue), and `summary` (per-subject start metadata, or
#'       `NULL` when not applicable)}
#'     \item{call}{The original function call}
#'     \item{equation}{The equation form used}
#'     \item{k_spec}{Description of k specification}
#'     \item{agg}{Aggregation method used}
#'     \item{n_total}{Total number of subjects/fits attempted}
#'     \item{n_success}{Number of successful fits}
#'     \item{n_fail}{Number of failed fits}
#'   }
#'
#' @details
#' This function is a modern wrapper around the legacy `FitCurves()` function.
#' It provides the same fitting capabilities but returns a structured S3 object
#' with standardized methods for model interrogation.
#'
#' ## Multi-start rescue protocol (TICKET-047)
#'
#' `fit_demand_fixed()` always runs `FitCurves()`'s existing heuristic start
#' exactly as before -- the "production start". A subject whose production
#' fit is strict-converged (`converged_strict`: the optimizer's own
#' convergence flag AND finite coefficients/objective AND not sitting on a
#' user-supplied bound) is accepted immediately; no sampled starts are ever
#' run for it, so its row, fitted model, predictions, and data are
#' byte-identical to the `multistart = FALSE` / `S = 1` protocol by
#' construction. Only subjects whose production fit is NOT strict-converged
#' are re-fit from `S - 1` additional starts, sampled log-uniformly in
#' interpretable (Q0, Pmax) coordinates and mapped to each equation's native
#' (Q0, alpha) parameterization via the same closed forms used by
#' `beezdemand_calc_pmax_omax()`. Among the sampled attempts that themselves
#' strict-converge, the minimum-residual-SS start wins (ties broken by draw
#' order). If none of the sampled starts strict-converge, the original
#' (non-converged) production row is kept. `equation = "linear"` is a
#' closed-form fit and is never multistarted. `FitCurves()` itself is
#' unchanged; sampling draws from the ambient RNG stream (call `set.seed()`
#' before `fit_demand_fixed()` for reproducibility -- the helpers never call
#' `set.seed()` themselves).
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_fixed(apt, equation = "hs", k = 2)
#' print(fit)
#' summary(fit)
#' tidy(fit)
#' glance(fit)
#'
#' # Grouped analysis -- fit separately by gender (subset keeps it fast)
#' data(apt_full)
#' ids <- unique(apt_full[c("id", "gender")])
#' ids <- ids[ids$gender %in% c("Male", "Female"), ]
#' keep <- unlist(lapply(split(ids$id, ids$gender), head, 40))
#' dat <- apt_full[apt_full$id %in% keep, ]
#' fit_g <- fit_demand_fixed(dat, equation = "hs", k = 2, by = "gender")
#' tidy(fit_g)   # group column prepended
#' glance(fit_g)  # one row per group
#' }
#'
#' @seealso [fit_demand_tmb()] for TMB mixed-effects models,
#'   [fit_demand_mixed()] for NLME mixed-effects models,
#'   [fit_demand_hurdle()] for hurdle models.
#' @family demand-fitting
#'
#' @export
fit_demand_fixed <- function(
  data,
  equation = c(
    "hs",
    "koff",
    "simplified",
    "linear",
    "exponential",
    "exponentiated"
  ),
  k = 2,
  agg = NULL,
  x_var = "x",
  y_var = "y",
  id_var = "id",
  param_space = c("natural", "log10"),
  by = NULL,
  multistart = TRUE,
  S = NULL,
  ...
) {
  equation <- match.arg(equation)
  equation <- normalize_equation(equation)
  param_space <- match.arg(param_space)
  call <- match.call()

  # TICKET-047 Codex 2F fold, item 6: fail fast on a malformed S rather than
  # silently misbehaving deep inside the rescue loop.
  if (!is.null(S)) {
    if (
      !is.numeric(S) || length(S) != 1 || is.na(S) || !is.finite(S) ||
        S != as.integer(S) || S < 1
    ) {
      stop(
        "`S` must be NULL or a single finite integer >= 1 (got: ",
        paste(deparse(S), collapse = " "),
        ").",
        call. = FALSE
      )
    }
    S <- as.integer(S)
  }

  # Warn if user explicitly passes k with simplified equation
  if (equation == "simplified" && !missing(k)) {
    warning(
      "k parameter is not used with equation = 'simplified'; ignoring k.",
      call. = FALSE
    )
  }

  # --- grouped dispatch ---
  if (!is.null(by)) {
    split_out <- beezdemand_split_by(data, by, function(slice, key_row) {
      fit_demand_fixed(
        data = slice,
        equation = equation,
        k = k,
        agg = agg,
        x_var = x_var,
        y_var = y_var,
        id_var = id_var,
        param_space = param_space,
        multistart = multistart,
        S = S,
        by = NULL,
        ...
      )
    })

    # Determine k_spec for metadata
    k_spec <- if (equation == "simplified") {
      "none (simplified equation)"
    } else if (is.numeric(k)) {
      paste0("fixed (", k, ")")
    } else {
      k
    }

    return(structure(
      list(
        groups = split_out$results,
        group_keys = split_out$group_keys,
        by_var = by,
        call = call,
        equation = equation,
        k_spec = k_spec,
        k_value = if (equation == "simplified") NA_real_ else if (is.numeric(k)) k else NA_real_,
        agg = agg,
        x_var = x_var,
        y_var = y_var,
        id_var = id_var,
        param_space = param_space
      ),
      class = c("beezdemand_fixed_grouped", "list")
    ))
  }

  # Call legacy engine with detailed = TRUE to get all outputs
  legacy_warnings <- character(0)
  legacy_result <- withCallingHandlers(
    FitCurves(
      dat = data,
      equation = equation,
      k = k,
      agg = agg,
      detailed = TRUE,
      xcol = x_var,
      ycol = y_var,
      idcol = id_var,
      param_space = param_space,
      ...
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      legacy_warnings <<- c(legacy_warnings, msg)
      # Legacy FitCurves can emit high-frequency data warnings; capture them
      # but avoid spamming downstream consumers/tests.
      if (
        grepl("Zeros found in data not compatible with equation", msg) ||
          grepl("k parameter is not used", msg)
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # Determine k specification mode for display
  k_spec <- if (equation == "simplified") {
    "none (simplified equation)"
  } else if (is.numeric(k)) {
    paste0("fixed (", k, ")")
  } else {
    k
  }

  # Extract results - handle both list and data.frame returns
  if (is.list(legacy_result) && "dfres" %in% names(legacy_result)) {
    results <- legacy_result$dfres
    fits <- legacy_result$fits
    predictions <- legacy_result$newdats
    data_used <- legacy_result$adfs
  } else {
    # Simple data frame return
    results <- legacy_result
    fits <- NULL
    predictions <- NULL
    data_used <- NULL
  }

  # TICKET-047: multi-start rescue protocol. Runs on the production-start
  # results/fits/predictions/data_used above; only subjects that are NOT
  # strict-converged get refit. Inserted BEFORE the success/failure
  # bookkeeping below so it sees the post-rescue converged_strict verdicts.
  multistart_info <- NULL
  if (is.data.frame(results) && nrow(results) > 0) {
    ms <- .fixed_multistart_apply(
      results = results,
      fits = fits,
      predictions = predictions,
      data_used = data_used,
      equation = equation,
      k = k,
      agg = agg,
      param_space = param_space,
      multistart = multistart,
      S = S,
      dots = list(...)
    )
    results <- ms$results
    fits <- ms$fits
    predictions <- ms$predictions
    data_used <- ms$data_used
    multistart_info <- ms$multistart_info
  }

  # Count successes/failures.

  if (is.data.frame(results) && nrow(results) > 0) {
    n_total <- nrow(results)

    if ("converged_strict" %in% names(results)) {
      # TICKET-069: the nonlinear (hs/koff/simplified) legacy engine now
      # records a per-fit verdict derived from the optimizer's own
      # convInfo$isConv (plus finite coefficients/objective and
      # not-at-a-bound, and demoted on domain-invalid Q0<=0/Alpha<=0) --
      # use that instead of string-matching Notes, which cannot distinguish
      # a numerically-converged-but-degenerate fit from a genuine one.
      success_flag <- results$converged_strict
      success_flag[is.na(success_flag)] <- FALSE
    } else {
      # Linear equation (no fallback-verification chain / no recorded
      # verdict): fall back to the Notes-grep + parameter-validity heuristic.
      success_flag <- rep(TRUE, n_total)

      if ("Notes" %in% names(results)) {
        notes_lower <- tolower(results$Notes)
        failed_notes <- grepl("failed|reverted|singular|error", notes_lower)
        success_flag <- success_flag & !failed_notes
      }

      if ("Alpha" %in% names(results)) {
        success_flag <- success_flag & !is.na(results$Alpha)
      }
      if ("Q0d" %in% names(results)) {
        success_flag <- success_flag & !is.na(results$Q0d)
      }

      # Check for physiologically implausible values (negative Q0 or alpha)
      if ("Q0d" %in% names(results)) {
        success_flag <- success_flag & (is.na(results$Q0d) | results$Q0d >= 0)
      }
      if ("Alpha" %in% names(results)) {
        success_flag <- success_flag & (is.na(results$Alpha) | results$Alpha >= 0)
      }

      # Handle linear equation parameters
      if (all(c("L", "b", "a") %in% names(results))) {
        success_flag <- success_flag &
          !is.na(results$L) &
          !is.na(results$b) &
          !is.na(results$a)
      }
    }

    n_success <- sum(success_flag)
    n_fail <- n_total - n_success

    # Add/overwrite the converged column with the derived verdict for
    # downstream use.
    results$converged <- success_flag
  } else {
    n_total <- n_success <- n_fail <- NA_integer_
  }

  structure(
    list(
      results = results,
      fits = fits,
      predictions = predictions,
      data_used = data_used,
      multistart = multistart_info,
      legacy_warnings = unique(legacy_warnings),
      call = call,
      equation = equation,
      k_spec = k_spec,
      k_value = if (equation == "simplified") {
        NA_real_
      } else if (is.numeric(k)) {
        k
      } else {
        NA_real_
      },
      agg = agg,
      x_var = x_var,
      y_var = y_var,
      id_var = id_var,
      param_space = param_space,
      param_space_details = beezdemand_param_space_details_core(
        internal_names = list(Q0 = "q0", alpha = "alpha", k = "k"),
        internal_spaces = list(
          Q0 = if (param_space == "log10") "log10" else "natural",
          alpha = if (param_space == "log10") "log10" else "natural",
          k = if (
            is.character(k) && identical(k, "fit") && param_space == "log10"
          ) {
            "log10"
          } else {
            "natural"
          }
        )
      ),
      n_total = n_total,
      n_success = n_success,
      n_fail = n_fail
    ),
    class = c("beezdemand_fixed", "list")
  )
}
