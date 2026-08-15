#' @title Multi-Start Rescue Protocol for fit_demand_fixed()
#' @description Internal helpers implementing the multi-start rescue protocol
#'   (TICKET-047, shipped spec = release-train plan section 1, decision D3).
#'   `fit_demand_fixed()` always runs the legacy `FitCurves()` heuristic
#'   exactly as before -- this is the "production start". A subject whose
#'   production fit is strict-converged (`converged_strict`, TICKET-069:
#'   optimizer `isConv` AND finite coefficients/objective AND not sitting on
#'   a user-supplied bound) is ACCEPTED immediately and no sampled starts are
#'   ever run for it -- its row/fit/prediction/data entries stay
#'   byte-identical to the single-start (`S = 1`) protocol by construction.
#'   Only subjects whose production fit is NOT strict-converged are re-fit
#'   from `S - 1` additional sampled starting values; among the sampled
#'   attempts that themselves strict-converge, the minimum-`AbsSS` (residual
#'   sum of squares) start wins, ties resolved by draw order (first index).
#' @name fixed-multistart
#' @keywords internal
NULL

# Equations eligible for multi-start rescue -- these are the equations for
# which a closed-form (Q0, Pmax) -> alpha mapping exists (mirroring
# beezdemand_calc_pmax_omax(), R/pmax-omax-engine.R). `equation = "linear"`
# is a closed-form fit and is NEVER multistarted.
.fixed_multistart_eligible_equations <- c("hs", "koff", "simplified")

#' Default multi-start budget for an equation/k combination
#'
#' Tiered by parameter count: 2-parameter forms (hs/koff/simplified with a
#' fixed `k`) get `S = 8`; 3-parameter forms (`k = "fit"`) get `S = 32`.
#' `equation = "linear"` is never multistarted (`S = 1`, forced).
#'
#' @param equation Character. Canonical (post `normalize_equation()`)
#'   equation name: `"hs"`, `"koff"`, `"simplified"`, or `"linear"`.
#' @param k The `k` argument as supplied by the caller of
#'   `fit_demand_fixed()` (numeric, or a character mode string such as
#'   `"fit"`, `"ind"`, `"range"`, `"share"`).
#' @return Integer scalar default budget.
#' @keywords internal
.fixed_multistart_default_S <- function(equation, k) {
  if (!(equation %in% .fixed_multistart_eligible_equations)) {
    return(1L)
  }
  if (is.character(k) && length(k) == 1 && identical(k, "fit")) {
    return(32L)
  }
  8L
}

#' Draw S - 1 log-uniform (Q0, Pmax) sampled starts for one subject
#'
#' Sampling region mirrors the qualtrics-panel-discounting-2 Tier-1
#' reference implementation
#' (`R/battery/11-multistart-protocol.R::sample_starts()`):
#' `Q0 ~ log-uniform[0.25 * max(y+), 4 * max(y+)]`,
#' `Pmax ~ log-uniform[min(x+) / 2, 4 * max(x+)]`, where `y+`/`x+` are the
#' subject's strictly-positive observed consumption/price values.
#'
#' @param x Numeric price vector for the subject.
#' @param y Numeric consumption vector for the subject.
#' @param n Number of starts to draw (`S - 1`).
#' @return List with elements `q0` and `pmax`, each length `n` (or length 0
#'   if sampling is not possible, e.g. no strictly-positive observations).
#' @keywords internal
.fixed_multistart_sample_qp <- function(x, y, n) {
  if (is.null(n) || n <= 0) {
    return(list(q0 = numeric(0), pmax = numeric(0)))
  }
  ypos <- y[is.finite(y) & y > 0]
  xpos <- x[is.finite(x) & x > 0]
  if (length(ypos) == 0 || length(xpos) == 0) {
    return(list(q0 = numeric(0), pmax = numeric(0)))
  }

  q0_lo <- 0.25 * max(ypos)
  q0_hi <- 4 * max(ypos)
  if (!(q0_lo > 0) || !(q0_hi > q0_lo)) {
    q0_lo <- max(ypos, 1e-6)
    q0_hi <- q0_lo * 4
  }

  pmax_lo <- min(xpos) / 2
  pmax_hi <- 4 * max(xpos)
  if (!(pmax_lo > 0) || !(pmax_hi > pmax_lo)) {
    pmax_lo <- 1e-3
    pmax_hi <- max(xpos, pmax_lo * 4)
  }

  list(
    q0 = exp(stats::runif(n, log(q0_lo), log(q0_hi))),
    pmax = exp(stats::runif(n, log(pmax_lo), log(pmax_hi)))
  )
}

#' Map sampled (Q0, Pmax) to alpha using the package's own closed forms
#'
#' Mirrors `beezdemand_calc_pmax_omax()` (`R/pmax-omax-engine.R`), verified
#' by round-trip: fixed-effect `"hs"` and `"koff"` fit the identical mean
#' function (`Q(p) = Q0 * 10^(k * (exp(-alpha * Q0 * p) - 1))`, just on
#' different scales) and share the Lambert-W relation used by
#' `.pmax_analytic_hs()`:
#' `Pmax = -W_0(-1 / (k * ln(10))) / (alpha * Q0)`
#' `=> alpha = -W_0(-1 / (k * ln(10))) / (Pmax * Q0)`.
#' `"simplified"`/SND uses `.pmax_analytic_snd()`'s closed form:
#' `Pmax = 1 / (alpha * Q0) => alpha = 1 / (Pmax * Q0)`.
#'
#' If `k` is too small for a real principal-branch Lambert-W solution to
#' exist at this scale (`k <= exp(1) / log(10)`, mirroring
#' `.pmax_analytic_hs()`'s own existence check) the function falls back to
#' the SND-style direct mapping so a sampler always returns a usable (if
#' less precise) starting alpha rather than failing outright.
#'
#' @param equation `"hs"`, `"koff"`, or `"simplified"`.
#' @param k_nat Natural-scale `k` (ignored for `"simplified"`).
#' @param q0 Numeric vector (or scalar, recycled) of sampled/fixed Q0.
#' @param pmax Numeric vector of sampled Pmax.
#' @return Numeric vector of alpha starting values (same length as `pmax`).
#' @keywords internal
.fixed_multistart_qp_to_alpha <- function(equation, k_nat, q0, pmax) {
  if (identical(equation, "simplified")) {
    return(1 / (pmax * q0))
  }

  threshold <- exp(1) / log(10)
  if (!is.finite(k_nat) || k_nat <= threshold) {
    return(1 / (pmax * q0))
  }

  w_val <- tryCatch(
    lambertW(z = -1 / (k_nat * log(10))),
    error = function(e) NA_real_
  )
  if (!is.finite(w_val)) {
    return(1 / (pmax * q0))
  }

  -w_val / (pmax * q0)
}

#' Resolve a natural-scale k value for the (Q0, Pmax) -> alpha mapping
#'
#' Prefers the production fit's own recorded `K` (natural scale, already
#' back-transformed by `ExtractCoefs()`) since that is the exact value used
#' for this subject regardless of the caller's `k` mode (`"ind"`, `"fit"`,
#' `"range"`, `"share"`, or a plain number). Falls back to the caller's `k`
#' if numeric, else to `GetK()` computed on the subject's own data -- both
#' are only used to seed a *starting* alpha, so approximate is acceptable.
#'
#' @param k_production Numeric scalar; the production row's `K` value
#'   (may be `NA` if the production fit was a total failure).
#' @param k_arg The `k` argument as supplied by the caller of
#'   `fit_demand_fixed()`.
#' @param adf Single-subject data frame (post `CheckCols()`, columns
#'   `id`/`x`/`y`) used as a last-resort fallback for `GetK()`.
#' @return Numeric scalar natural-scale k (may be `NA_real_` if nothing
#'   works).
#' @keywords internal
.fixed_multistart_k_nat <- function(k_production, k_arg, adf) {
  if (is.finite(k_production)) {
    return(as.numeric(k_production))
  }
  if (is.numeric(k_arg) && length(k_arg) == 1) {
    return(as.numeric(k_arg))
  }
  tryCatch(GetK(adf), error = function(e) NA_real_)
}

#' Apply the multi-start rescue protocol to a completed FitCurves() run
#'
#' Called from `fit_demand_fixed()` after the production `FitCurves()` call
#' but before its success/failure bookkeeping, so downstream logic (which
#' derives `results$converged` from `results$converged_strict`) sees the
#' post-rescue verdicts automatically.
#'
#' @param results `dfres` data frame from the production `FitCurves()` call.
#' @param fits `fits` list from the production call.
#' @param predictions `newdats` list from the production call.
#' @param data_used `adfs` list from the production call.
#' @param equation Canonical (post `normalize_equation()`) equation name.
#' @param k The `k` argument as supplied to `fit_demand_fixed()`.
#' @param param_space `"natural"` or `"log10"`.
#' @param multistart Logical; multi-start on/off.
#' @param S Integer budget, or `NULL` to use the tiered default.
#' @param dots Named list of additional arguments forwarded from
#'   `fit_demand_fixed()`'s `...` (e.g. `lobound`, `hibound`,
#'   `constrainq0`) -- passed through to each rescue `FitCurves()` call,
#'   with any user-supplied `startq0`/`startalpha` stripped (the sampled
#'   starts replace them for rescue attempts).
#' @return List with elements `results`, `fits`, `predictions`,
#'   `data_used` (post-rescue, same shapes as the inputs) and
#'   `multistart_info` (settings + per-subject summary data frame, for the
#'   `beezdemand_fixed` object's `$multistart` field).
#' @keywords internal
.fixed_multistart_apply <- function(
  results,
  fits,
  predictions,
  data_used,
  equation,
  k,
  param_space,
  multistart,
  S,
  dots = list()
) {
  eligible <- equation %in% .fixed_multistart_eligible_equations
  S_resolved <- if (is.null(S)) {
    .fixed_multistart_default_S(equation, k)
  } else {
    as.integer(S)
  }
  # equation = "linear" is never multistarted, regardless of caller args.
  if (!eligible) {
    S_resolved <- 1L
  }

  n <- if (is.data.frame(results)) nrow(results) else 0L

  if (n == 0L || !is.data.frame(results)) {
    return(list(
      results = results,
      fits = fits,
      predictions = predictions,
      data_used = data_used,
      multistart_info = list(
        multistart = isTRUE(multistart),
        S = S_resolved,
        equation = equation,
        eligible = eligible,
        summary = NULL
      )
    ))
  }

  has_strict <- "converged_strict" %in% names(results)
  base_conv <- if (has_strict) {
    v <- results$converged_strict
    v[is.na(v)] <- FALSE
    v
  } else if ("converged" %in% names(results)) {
    v <- results$converged
    v[is.na(v)] <- FALSE
    v
  } else {
    rep(TRUE, n)
  }

  n_starts_tried <- rep(1L, n)
  n_starts_converged <- as.integer(base_conv)
  start_source <- ifelse(base_conv, "production", "none")

  do_rescue <- eligible && isTRUE(multistart) && S_resolved > 1L

  if (do_rescue) {
    constrainq0 <- dots$constrainq0 %||% NULL
    dots_i <- dots
    dots_i$startq0 <- NULL
    dots_i$startalpha <- NULL

    for (i in seq_len(n)) {
      # Production-start short-circuit: already strict-converged subjects
      # are NEVER refit, so their rows/fits/predictions/data stay
      # byte-identical to the S = 1 protocol by construction.
      if (isTRUE(base_conv[i])) {
        next
      }

      id_i <- results$id[i]
      adf <- data_used[[i]]
      if (is.null(adf) || nrow(adf) == 0) {
        next
      }

      k_nat <- .fixed_multistart_k_nat(results$K[i], k, adf)
      qp <- .fixed_multistart_sample_qp(adf$x, adf$y, S_resolved - 1L)
      n_attempt <- length(qp$q0)
      if (n_attempt == 0L) {
        next
      }

      q0_for_alpha <- if (!is.null(constrainq0)) constrainq0 else qp$q0
      alpha_vec <- .fixed_multistart_qp_to_alpha(
        equation, k_nat, q0_for_alpha, qp$pmax
      )

      if (identical(param_space, "log10")) {
        q0_start_vec <- suppressWarnings(log10(qp$q0))
        alpha_start_vec <- suppressWarnings(log10(alpha_vec))
      } else {
        q0_start_vec <- qp$q0
        alpha_start_vec <- alpha_vec
      }

      best_row <- NULL
      best_abs_ss <- Inf
      best_fit <- NULL
      best_newdat <- NULL
      best_adf <- NULL
      n_conv_j <- 0L

      for (j in seq_len(n_attempt)) {
        sq0 <- q0_start_vec[j]
        salpha <- alpha_start_vec[j]
        if (!is.finite(sq0) || !is.finite(salpha)) {
          next
        }

        args_j <- c(
          list(
            dat = adf[, c("id", "x", "y"), drop = FALSE],
            equation = equation,
            k = k,
            agg = NULL,
            detailed = TRUE,
            xcol = "x",
            ycol = "y",
            idcol = "id",
            param_space = param_space,
            startq0 = sq0,
            startalpha = salpha
          ),
          dots_i
        )

        legacy_j <- suppressWarnings(suppressMessages(try(
          do.call(FitCurves, args_j),
          silent = TRUE
        )))
        if (inherits(legacy_j, "try-error") || is.null(legacy_j$dfres)) {
          next
        }
        row_j <- legacy_j$dfres[1, , drop = FALSE]
        if (!isTRUE(row_j$converged_strict)) {
          next
        }

        n_conv_j <- n_conv_j + 1L
        if (is.finite(row_j$AbsSS) && row_j$AbsSS < best_abs_ss) {
          best_abs_ss <- row_j$AbsSS
          best_row <- row_j
          best_fit <- legacy_j$fits[[1]]
          best_newdat <- legacy_j$newdats[[1]]
          best_adf <- legacy_j$adfs[[1]]
        }
      }

      n_starts_tried[i] <- 1L + n_attempt
      n_starts_converged[i] <- n_conv_j

      if (!is.null(best_row)) {
        best_row$id <- id_i
        common_cols <- intersect(names(results), names(best_row))
        results[i, common_cols] <- best_row[, common_cols]
        fits[[i]] <- best_fit
        predictions[[i]] <- best_newdat
        data_used[[i]] <- best_adf
        start_source[i] <- "sampled"
      }
    }
  }

  results$n_starts_tried <- n_starts_tried
  results$n_starts_converged <- n_starts_converged
  results$start_source <- start_source

  summary_df <- data.frame(
    id = results$id,
    n_starts_tried = n_starts_tried,
    n_starts_converged = n_starts_converged,
    start_source = start_source,
    stringsAsFactors = FALSE
  )

  list(
    results = results,
    fits = fits,
    predictions = predictions,
    data_used = data_used,
    multistart_info = list(
      multistart = isTRUE(multistart),
      S = S_resolved,
      equation = equation,
      eligible = eligible,
      summary = summary_df
    )
  )
}
