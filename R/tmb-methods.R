# ==============================================================================
# S3 Methods for beezdemand_tmb Objects
# ==============================================================================

#' Build display term names from TMB model coefficient names
#'
#' Maps raw optimizer names (beta_q0, beta_alpha, etc.) to readable display
#' names (Q0:(Intercept), alpha:genderMale, etc.) using design matrix column
#' names. Used by summary(), tidy(), and confint() to avoid duplicated logic.
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param nms Character vector of raw parameter names (from
#'   \code{names(coef(object))}). If NULL, extracted from object.
#'
#' @return A list with components:
#'   \describe{
#'     \item{term}{Character vector of display names.}
#'     \item{q0_idx}{Integer vector of beta_q0 positions.}
#'     \item{alpha_idx}{Integer vector of beta_alpha positions.}
#'     \item{other_idx}{Integer vector of non-beta positions.}
#'   }
#' @keywords internal
.tmb_build_term_names <- function(object, nms = NULL) {
  if (is.null(nms)) {
    nms <- names(object$model$coefficients)
  }

  q0_idx <- which(nms == "beta_q0")
  alpha_idx <- which(nms == "beta_alpha")
  other_idx <- which(!nms %in% c("beta_q0", "beta_alpha"))

  q0_colnames <- colnames(object$formula_details$X_q0)
  alpha_colnames <- colnames(object$formula_details$X_alpha)

  term <- character(length(nms))
  term[q0_idx] <- paste0("Q0:", q0_colnames)
  term[alpha_idx] <- paste0("alpha:", alpha_colnames)
  term[other_idx] <- nms[other_idx]

  list(
    term = term,
    q0_idx = q0_idx,
    alpha_idx = alpha_idx,
    other_idx = other_idx
  )
}


#' Get k value from TMB model (handles both estimated and fixed k)
#' @keywords internal
.tmb_get_k <- function(object) {
  coefs <- object$model$coefficients
  if ("log_k" %in% names(coefs)) {
    exp(coefs[["log_k"]])
  } else if (!is.null(object$param_info$k_fixed)) {
    object$param_info$k_fixed
  } else {
    NA_real_
  }
}

#' Map a TMB design-matrix column to its originating model term
#' @keywords internal
.tmb_term_assign_map <- function(object, param) {
  X <- if (param == "Q0") object$formula_details$X_q0 else
    object$formula_details$X_alpha
  asn <- attr(X, "assign")
  cn  <- colnames(X)
  f   <- if (param == "Q0") stats::formula(object)$Q0 else
    stats::formula(object)$alpha
  if (is.null(asn)) {
    # Rebuild to recover the `assign` attribute (Task 0, Step 3 fallback).
    X   <- stats::model.matrix(f, data = object$data)
    asn <- attr(X, "assign")
    cn  <- colnames(X)
  }
  labs <- attr(stats::terms(f), "term.labels")
  # assign == 0 -> intercept (NA term label); k -> labs[k].
  stats::setNames(ifelse(asn == 0L, NA_character_, labs[asn]), cn)
}

#' Group beezdemand_tmb fixed effects into testable blocks for anova()
#' @keywords internal
.tmb_group_terms <- function(object, terms = NULL, group_by = "auto") {
  tn <- .tmb_build_term_names(object)
  # Fixed-effect (beta) positions ONLY — never log_k / logsigma / rho_raw.
  fe_idx   <- c(tn$q0_idx, tn$alpha_idx)
  fe_param <- c(rep("Q0", length(tn$q0_idx)),
                rep("alpha", length(tn$alpha_idx)))
  fe_term  <- tn$term[fe_idx]
  raw      <- names(object$model$coefficients)

  # Explicit named-list grouping overrides group_by.
  if (is.list(terms)) {
    if (length(terms) == 0L) {
      cli::cli_abort("`terms` is an empty list: no terms to test.")
    }
    return(lapply(names(terms), function(lbl) {
      want <- terms[[lbl]]
      hit  <- fe_idx[fe_term %in% want | raw[fe_idx] %in% want]
      if (length(hit) == 0L) {
        cli::cli_abort("term group {.val {lbl}} matched no fixed effects.")
      }
      list(label = lbl, idx = hit)
    }))
  }

  # Character-vector `terms`: restrict the universe.
  if (is.character(terms)) {
    if (length(terms) == 0L) cli::cli_abort("`terms` is empty: no terms to test.")
    unknown <- setdiff(terms, c(fe_term, raw[fe_idx]))
    if (length(unknown) > 0L) {
      cli::cli_abort(c(
        "Unknown term(s): {.val {unknown}}.",
        i = "Valid terms: {.val {fe_term}}."
      ))
    }
    keep     <- fe_term %in% terms | raw[fe_idx] %in% terms
    fe_idx   <- fe_idx[keep]
    fe_param <- fe_param[keep]
    fe_term  <- fe_term[keep]
  }
  if (length(fe_idx) == 0L) cli::cli_abort("no terms to test.")

  if (group_by == "term") {
    return(lapply(seq_along(fe_idx), function(k) {
      list(label = fe_term[k], idx = fe_idx[k])
    }))
  }
  if (group_by == "parameter") {
    return(lapply(unique(fe_param), function(p) {
      list(label = p, idx = fe_idx[fe_param == p])
    }))
  }
  # group_by == "auto": group non-intercept columns by originating model term.
  groups <- list()
  for (p in unique(fe_param)) {
    sel <- fe_param == p
    map <- .tmb_term_assign_map(object, p)
    for (k in which(sel)) {
      col <- sub("^(Q0|alpha):", "", fe_term[k])
      tl  <- map[[col]]
      if (is.null(tl) || is.na(tl)) next            # intercept -> skip
      key <- paste0(p, " ~ ", tl)
      groups[[key]] <- c(groups[[key]], fe_idx[k])
    }
  }
  if (length(groups) == 0L) {
    cli::cli_abort(c(
      "No non-intercept fixed effects to test under {.code group_by = \"auto\"}.",
      i = "Use {.code group_by = \"parameter\"} or {.code group_by = \"term\"}."
    ))
  }
  lapply(names(groups), function(k) list(label = k, idx = groups[[k]]))
}

#' Sequential LRT / AIC table for nested beezdemand_tmb fits
#' @keywords internal
.tmb_anova_multifit <- function(object, extra, test) {
  fits <- c(list(object), extra)
  if (!all(vapply(fits, inherits, logical(1), "beezdemand_tmb"))) {
    cli::cli_abort(c(
      "All models must be {.cls beezdemand_tmb} fits.",
      i = "Use {.fn compare_models} for mixed backends."
    ))
  }
  if (test == "Wald") {
    cli::cli_abort(c(
      "{.code test = \"Wald\"} is a single-model joint test; it does not apply to a multi-fit comparison.",
      i = "Use {.code test = \"LRT\"} or {.code test = \"AIC\"} when comparing multiple fits."
    ))
  }
  df   <- vapply(fits, function(f) length(f$opt$par), numeric(1))
  ll   <- vapply(fits, function(f) as.numeric(f$loglik), numeric(1))
  aic  <- vapply(fits, function(f) as.numeric(f$AIC), numeric(1))
  nob  <- vapply(fits, function(f) as.numeric(nobs(f)), numeric(1))

  if (length(unique(nob)) > 1L) {
    cli::cli_abort("Models were fit to different numbers of observations; not comparable.")
  }
  ord <- order(df)
  df  <- df[ord]; ll <- ll[ord]; aic <- aic[ord]

  chisq <- c(NA_real_, 2 * diff(ll))
  ddf   <- c(NA_real_, diff(df))
  # A valid sequential LRT needs strict nesting: each step must add df
  # AND not lose log-likelihood. `ddf[-1] <= 0` catches equal-df (or
  # non-increasing) pairs; `chisq[-1] < 0` catches a higher-df fit that
  # fits worse. Either symptom => not nested.
  if (test == "LRT" && (any(ddf[-1] <= 0) || any(chisq[-1] < -1e-8))) {
    cli::cli_abort(c(
      "Models are not nested; LRT not applicable.",
      i = "Sequential fits must have strictly increasing degrees of freedom and log-likelihood.",
      i = "Try {.code test = \"AIC\"}."
    ))
  }
  pval <- ifelse(is.na(chisq) | ddf <= 0, NA_real_,
                 stats::pchisq(chisq, df = ddf, lower.tail = FALSE))

  tibble::tibble(
    Model          = paste0("Model", seq_along(df)),
    df             = as.integer(df),
    AIC            = aic,
    Chisq          = if (test == "AIC") NA_real_ else chisq,
    `Pr(>Chisq)`   = if (test == "AIC") NA_real_ else pval
  )
}

# --- print ---

#' Print Method for TMB Mixed-Effects Demand Model
#'
#' @param x An object of class \code{beezdemand_tmb}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' print(fit)
#' }
#'
#' @export
print.beezdemand_tmb <- function(x, ...) {
  cat("\nTMB Mixed-Effects Demand Model\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat("Equation:", x$param_info$equation, "\n")
  cat("Convergence:", ifelse(x$converged, "Yes", "No"), "\n")
  cat("Number of subjects:", x$param_info$n_subjects, "\n")
  cat("Number of observations:", x$param_info$n_obs, "\n")
  if (x$param_info$n_dropped > 0) {
    cat("Observations dropped (zeros):", x$param_info$n_dropped, "\n")
  }
  cat(
    "Random effects:",
    x$param_info$n_random_effects,
    paste0("(", paste(x$param_info$random_effects_spec, collapse = ", "), ")"),
    "\n"
  )
  cat("Log-likelihood:", round(x$loglik, 2), "\n")
  cat("AIC:", round(x$AIC, 2), "\n")
  cat("\nFixed Effects:\n")

  # Print named fixed effects
  coefs <- x$model$coefficients
  .print_tmb_coefficients(coefs, x$param_info)

  cat("\nUse summary() for full results.\n")
  invisible(x)
}


#' @keywords internal
.print_tmb_coefficients <- function(coefs, param_info) {
  # Create readable names
  nms <- names(coefs)
  display <- coefs

  # Group beta_q0 and beta_alpha parameters
  q0_idx <- which(nms == "beta_q0")
  alpha_idx <- which(nms == "beta_alpha")

  disp_names <- character(length(nms))
  disp_names[q0_idx] <- paste0("Q0.", seq_along(q0_idx) - 1)
  disp_names[alpha_idx] <- paste0("alpha.", seq_along(alpha_idx) - 1)

  other_idx <- which(!nms %in% c("beta_q0", "beta_alpha"))
  disp_names[other_idx] <- nms[other_idx]

  names(display) <- disp_names
  print(round(display, 4))
}


# --- summary ---

#' Summarize a TMB Mixed-Effects Demand Model Fit
#'
#' @param object An object of class \code{beezdemand_tmb}.
#' @param report_space Character. Reporting space for core demand parameters.
#'   One of `"internal"`, `"natural"`, `"log10"`. `estimate`/`std.error` are
#'   reported on this scale; `statistic`/`p.value` are always computed on the
#'   estimation scale (the Wald test is defined there and is not recomputed
#'   after back-transforming, so on the natural scale
#'   `statistic != estimate/std.error`, by design).
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{summary.beezdemand_tmb} (also inherits
#'   from \code{beezdemand_summary}). The \code{variance_components} element
#'   reports the Q0 and alpha random-effect SDs on the \strong{log10 scale}:
#'   the random effects are estimated on the natural-log scale internally and
#'   divided by \code{log(10)} for reporting, so they are directly comparable
#'   with \code{nlme::VarCorr()} on a \code{fit_demand_mixed()} fit using the
#'   default \code{param_space = "log10"}. The residual SD is reported on the
#'   model's likelihood scale (equation-dependent) and the random-effect
#'   correlations are scale-invariant; neither is rescaled.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' summary(fit)
#' summary(fit, report_space = "log10")
#' }
#'
#' @export
summary.beezdemand_tmb <- function(
  object,
  report_space = c("natural", "log10", "internal"),
  ...
) {
  report_space <- match.arg(report_space)

  coefs <- object$model$coefficients
  se_vec <- object$model$se
  nms <- names(coefs)

  # Create term names from parameter vectors
  tn <- .tmb_build_term_names(object, nms)
  term <- tn$term
  q0_idx <- tn$q0_idx
  alpha_idx <- tn$alpha_idx

  # Determine component and scale for each coefficient
  component <- character(length(nms))
  # TICKET-031: align with tidy.beezdemand_tmb() ("fixed" label, post-TICKET-017)
  # so summary()$coefficients and tidy() use identical component vocabulary on
  # the same fit. `derived_metrics$component` (below) keeps "consumption" by
  # explicit ticket scope -- those rows describe derived demand metrics, not
  # fitted coefficients, so the label has different meaning.
  component[q0_idx] <- "fixed"
  component[alpha_idx] <- "fixed"
  component[nms == "log_k"] <- "fixed"
  # Match bare `logsigma` (Phase 2 vector parameter) as well as
  # `logsigma_e` and the legacy `logsigma_b` / `logsigma_c` names.
  component[grepl("^logsigma($|_)|^rho_", nms)] <- "variance"

  estimate_scale <- rep("log", length(nms))
  estimate_scale[grepl("^logsigma($|_)|^rho_", nms)] <- "natural"

  # Build coefficient table
  z_val <- coefs / se_vec
  p_val <- 2 * stats::pnorm(-abs(z_val))

  coefficients <- tibble::tibble(
    term = term,
    estimate = unname(coefs),
    std.error = unname(se_vec),
    statistic = unname(z_val),
    p.value = unname(p_val),
    component = component,
    estimate_scale = estimate_scale,
    term_display = term
  )

  coefficients <- beezdemand_transform_coef_table(
    coef_tbl = coefficients,
    report_space = report_space,
    internal_space = "natural"
  )

  # `statistic`/`p.value` stay on the estimation (log) scale for every
  # `report_space` (broom/emmeans convention); only `estimate`/`std.error` are
  # back-transformed above. Recomputing the Wald test on the back-transformed
  # scale is degenerate (statistic = 1/(c*SE), independent of -- and dropping the
  # sign of -- the estimate). See tests/testthat/test-report-space-test-invariance.R.

  # Compute group-level demand metrics
  group_metrics <- calc_group_metrics(object)

  derived_metrics <- dplyr::bind_rows(
    beezdemand_empty_derived_metrics(),
    tibble::tibble(
      metric = c("pmax_model", "omax_model", "q_at_pmax_model",
                 "elasticity_at_pmax_model"),
      estimate = c(group_metrics$Pmax, group_metrics$Omax,
                   group_metrics$Qmax, group_metrics$elasticity_at_pmax),
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      method = group_metrics$method %||% "unknown",
      component = "consumption",
      level = "population",
      id = NA_character_
    )
  )

  # Individual parameter summaries. For a continuous random-slope fit the cached
  # wide subject_pars holds NA (no single Q0/alpha per subject); summarize the
  # reconciled per-subject table (conditioned at the subject mean) instead so the
  # Individual-Parameter section is not all-NA (TICKET-051). Factor/intercept
  # fits keep the cached table (byte-identical).
  cont_terms_ip <- .tmb_continuous_re_terms(
    object$param_info$random_effects_parsed, object$data
  )
  spars <- if (length(cont_terms_ip) > 0L) {
    tryCatch(get_subject_pars(object), error = function(e) object$subject_pars)
  } else {
    object$subject_pars
  }
  individual_metrics <- list(
    Q0 = summary(spars$Q0),
    alpha = summary(spars$alpha),
    Pmax = summary(spars$Pmax),
    Omax = summary(spars$Omax)
  )

  # Variance components
  vc <- .tmb_format_variance_components(object)

  # Notes
  notes <- character(0)
  if (!object$converged) {
    notes <- c(notes, "WARNING: Model did not converge.")
  }
  if (object$param_info$n_dropped > 0) {
    notes <- c(notes, sprintf(
      "%d zero-consumption observations dropped for equation='exponential'.",
      object$param_info$n_dropped
    ))
  }
  if (isFALSE(object$se_available)) {
    notes <- c(notes, "Standard errors unavailable (sdreport failed); CIs will be NA.")
  }
  if (isFALSE(object$hessian_pd)) {
    notes <- c(notes,
      "Warning: Hessian not positive definite \u2014 standard errors may be unreliable."
    )
  }
  if (length(object$opt_warnings %||% character(0)) > 0) {
    notes <- c(notes, sprintf(
      "Optimizer produced %d warning(s) during fitting.",
      length(object$opt_warnings)
    ))
  }
  if (!is.null(object$param_info$factors) && length(object$param_info$factors) > 0) {
    notes <- c(notes,
      "Population metrics reflect reference level. Use get_demand_param_emms() for per-group estimates."
    )
  }

  result <- structure(
    list(
      call = object$call,
      model_class = "beezdemand_tmb",
      backend = "TMB_mixed",
      equation = object$param_info$equation,
      coefficients = coefficients,
      variance_components = vc$table,
      correlations = vc$correlations,
      derived_metrics = derived_metrics,
      n_subjects = object$param_info$n_subjects,
      nobs = object$param_info$n_obs,
      converged = object$converged,
      logLik = object$loglik,
      AIC = object$AIC,
      BIC = object$BIC,
      group_metrics = group_metrics,
      individual_metrics = individual_metrics,
      notes = notes
    ),
    class = c("summary.beezdemand_tmb", "beezdemand_summary")
  )

  result
}


#' @keywords internal
.tmb_format_variance_components <- function(object) {
  coefs <- object$model$coefficients
  re_parsed <- object$param_info$random_effects_parsed
  bmap <- if (!is.null(re_parsed)) {
    .tmb_build_block_map(re_parsed)
  } else {
    list(n_blocks = 0L, block_q0_dim = integer(0), block_alpha_dim = integer(0),
         block_types = integer(0), n_logsigma = 0L, n_rho = 0L)
  }

  logsigma_full <- unname(coefs[names(coefs) == "logsigma"])
  rho_raw_full <- unname(coefs[names(coefs) == "rho_raw"])

  # TICKET-015: random-effect SDs are estimated on the natural-log scale
  # (src/MixedDemand.h: Q0_i = exp(log_q0_i), so the RE perturbs log_q0_i).
  # Report the Q0/alpha RE SDs on the log10 scale -- divide by log(10) -- so
  # they are directly comparable with nlme::VarCorr() on a param_space =
  # "log10" NLME fit. The residual SD (sigma_e) is on the model's likelihood
  # scale and the RE correlations are scale-invariant; neither is rescaled.
  ln10 <- log(10)

  rows <- list()
  correlations <- NULL
  sigma_offset <- 0L
  rho_offset <- 0L

  # TICKET-051: name continuous (numeric) RE-slope variance components with the
  # covariate term rather than a positional index; intercept and factor-dummy
  # columns keep their positional label (factor/intercept fits byte-identical).
  cont_terms <- .tmb_continuous_re_terms(re_parsed, object$data)
  .vc_idx <- function(terms_vec, j) {
    tj <- if (length(terms_vec) >= j) terms_vec[[j]] else NA_character_
    if (!is.na(tj) && tj %in% cont_terms) tj else as.character(j)
  }

  for (b in seq_len(bmap$n_blocks)) {
    d_q0 <- bmap$block_q0_dim[b]
    d_alpha <- bmap$block_alpha_dim[b]
    d <- d_q0 + d_alpha
    if (d == 0L) next

    # Per-RE-column SDs.
    block_label_prefix <- if (bmap$n_blocks > 1L) sprintf("block%d ", b) else ""
    pdmat_label <- if (bmap$block_types[b] == 1L) "pdSymm" else "pdDiag"
    blk_terms_q0 <- if (!is.null(re_parsed)) re_parsed$blocks[[b]]$terms_q0 else character(0)
    blk_terms_alpha <- if (!is.null(re_parsed)) re_parsed$blocks[[b]]$terms_alpha else character(0)
    if (d_q0 > 0L) {
      for (j in seq_len(d_q0)) {
        is_cont <- length(blk_terms_q0) >= j && (blk_terms_q0[[j]] %in% cont_terms)
        nm <- if (d_q0 == 1L && !is_cont) "sigma_b (Q0 RE SD)" else
              sprintf("sigma_b[%s%s] (Q0 RE SD)", block_label_prefix,
                      .vc_idx(blk_terms_q0, j))
        rows[[length(rows) + 1L]] <- data.frame(
          Component = nm,
          Estimate = exp(logsigma_full[sigma_offset + j]) / ln10,
          stringsAsFactors = FALSE
        )
      }
    }
    if (d_alpha > 0L) {
      for (j in seq_len(d_alpha)) {
        is_cont <- length(blk_terms_alpha) >= j && (blk_terms_alpha[[j]] %in% cont_terms)
        nm <- if (d_alpha == 1L && !is_cont) "sigma_c (alpha RE SD)" else
              sprintf("sigma_c[%s%s] (alpha RE SD)", block_label_prefix,
                      .vc_idx(blk_terms_alpha, j))
        rows[[length(rows) + 1L]] <- data.frame(
          Component = nm,
          Estimate = exp(logsigma_full[sigma_offset + d_q0 + j]) / ln10,
          stringsAsFactors = FALSE
        )
      }
    }

    # Off-diagonal correlations (pdSymm only). The C++ template treats
    # rho_raw entries as PARTIAL correlations consumed by the LKJ-
    # Cholesky construction (see src/MixedDemand.h). For d == 2 the
    # single partial correlation equals the marginal correlation
    # (tanh(rho_raw[0])), so the legacy "rho_bc" label is correct. For
    # d > 2, the marginal correlations are the off-diagonals of
    # R_corr = L_corr %*% t(L_corr) -- reconstruct L_corr here using
    # the same recurrence as the template, then derive R_corr.
    # The prior code reported tanh(rho_raw) directly as rho[j,k],
    # which is a silent statistical wrong answer for any pdSymm block
    # of size > 2.
    if (bmap$block_types[b] == 1L && d > 1L) {
      n_off <- d * (d - 1L) / 2L

      # Reconstruct the LKJ correlation Cholesky.
      L_corr <- matrix(0, nrow = d, ncol = d)
      L_corr[1L, 1L] <- 1
      idx <- 0L
      for (j in 2L:d) {
        sum_sq <- 0
        for (k in seq_len(j - 1L)) {
          idx <- idx + 1L
          r <- tanh(rho_raw_full[rho_offset + idx])
          if (k == 1L) {
            L_corr[j, k] <- r
          } else {
            L_corr[j, k] <- r * sqrt(max(0, 1 - sum_sq))
          }
          sum_sq <- sum_sq + L_corr[j, k]^2
        }
        L_corr[j, j] <- sqrt(max(0, 1 - sum_sq))
      }
      R_corr <- L_corr %*% t(L_corr)

      # Column labels for this block (q0 columns then alpha columns), used to
      # name correlations that involve a continuous RE term (TICKET-051).
      blk_col_terms <- c(blk_terms_q0, blk_terms_alpha)
      blk_col_param <- c(rep("Q0", d_q0), rep("alpha", d_alpha))
      block_has_cont <- any(blk_col_terms %in% cont_terms)
      cor_rows <- list()
      for (j in 2L:d) {
        for (k in seq_len(j - 1L)) {
          marginal_r <- R_corr[j, k]
          nm <- if (d_q0 == 1L && d_alpha == 1L && d == 2L && !block_has_cont) {
            "rho_bc (Q0-alpha correlation)"
          } else if (block_has_cont && length(blk_col_terms) == d) {
            sprintf("rho[%s%s:%s, %s:%s]", block_label_prefix,
                    blk_col_param[j], blk_col_terms[j],
                    blk_col_param[k], blk_col_terms[k])
          } else {
            sprintf("rho[%s%d,%d]", block_label_prefix, j, k)
          }
          cor_rows[[length(cor_rows) + 1L]] <- data.frame(
            Component = nm,
            Estimate = marginal_r,
            stringsAsFactors = FALSE
          )
        }
      }
      rho_offset <- rho_offset + n_off
      if (length(cor_rows) > 0L) {
        cor_df <- do.call(rbind, cor_rows)
        correlations <- if (is.null(correlations)) cor_df else
          rbind(correlations, cor_df)
      }
    }

    sigma_offset <- sigma_offset + d
  }

  rows[[length(rows) + 1L]] <- data.frame(
    Component = "sigma_e (Residual SD)",
    Estimate = exp(coefs[["logsigma_e"]]),
    stringsAsFactors = FALSE
  )

  list(
    table = do.call(rbind, rows),
    correlations = correlations
  )
}


#' Print Method for TMB Model Summary
#'
#' @param x A \code{summary.beezdemand_tmb} object.
#' @param digits Number of significant digits.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns the input object \code{x}; called for its
#'   side effect of printing a formatted model summary to the console.
#' @export
print.summary.beezdemand_tmb <- function(x, digits = 4, ...) {
  cat("\nTMB Mixed-Effects Demand Model Summary\n")
  cat(strrep("=", 50), "\n\n")
  cat("Equation:", x$equation, "\n")
  cat("Backend:", x$backend, "\n")
  cat("Convergence:", ifelse(x$converged, "Yes", "No"), "\n")
  cat("Subjects:", x$n_subjects, " Observations:", x$nobs, "\n\n")

  cat("--- Fixed Effects ---\n")
  coef_display <- x$coefficients[, c("term", "estimate", "std.error",
                                      "statistic", "p.value")]
  coef_display$estimate <- round(coef_display$estimate, digits)
  coef_display$std.error <- round(coef_display$std.error, digits)
  coef_display$statistic <- round(coef_display$statistic, digits)
  coef_display$p.value <- format.pval(coef_display$p.value, digits = 3)
  print(as.data.frame(coef_display), row.names = FALSE)

  cat("\n--- Variance Components ---\n")
  cat("(Q0/alpha RE SDs on log10 scale; residual SD on likelihood scale)\n")
  if (!is.null(x$variance_components)) {
    vc <- x$variance_components
    vc$Estimate <- round(vc$Estimate, digits)
    print(vc, row.names = FALSE)
  }

  if (!is.null(x$correlations)) {
    cat("\n--- RE Correlations ---\n")
    corr <- x$correlations
    corr$Estimate <- round(corr$Estimate, digits)
    print(corr, row.names = FALSE)
  }

  cat("\n--- Fit Statistics ---\n")
  cat("Log-likelihood:", round(x$logLik, 2), "\n")
  cat("AIC:", round(x$AIC, 2), "\n")
  cat("BIC:", round(x$BIC, 2), "\n")

  cat("\n--- Population Demand Metrics ---\n")
  dm <- x$group_metrics
  if (!is.null(dm)) {
    cat(sprintf("Pmax: %.4f  Omax: %.4f  Method: %s\n",
                dm$Pmax, dm$Omax, dm$method %||% "unknown"))
    # Phase 5C: surface the conditioning point used to derive these
    # metrics. Continuous covariates default to training mean; factors
    # default to marginal across observed levels (equal weights).
    co <- dm$conditioned_on
    if (!is.null(co)) {
      parts <- character(0)
      if (!is.null(co$covariates) && length(co$covariates) > 0L) {
        cv_strs <- mapply(
          function(nm, val) sprintf("%s=%.4g", nm, val),
          names(co$covariates), co$covariates
        )
        parts <- c(parts, cv_strs)
      }
      if (!is.null(co$factors) && length(co$factors) > 0L) {
        fac_strs <- mapply(
          function(nm, val) sprintf("%s=%s", nm,
            if (length(val) == 1L && val == "marginal") "marginal"
            else paste(val, collapse = "/")),
          names(co$factors), co$factors
        )
        parts <- c(parts, fac_strs)
      }
      if (length(parts) > 0L) {
        cat("Metrics conditioned at:", paste(parts, collapse = ", "), "\n")
      }
    }
  }

  cat("\n--- Individual Parameter Summaries ---\n")
  for (nm in names(x$individual_metrics)) {
    cat(sprintf("  %s: ", nm))
    s <- x$individual_metrics[[nm]]
    cat(sprintf("Min=%.4f  Med=%.4f  Mean=%.4f  Max=%.4f\n",
                s["Min."], s["Median"], s["Mean"], s["Max."]))
  }

  if (length(x$notes) > 0) {
    cat("\nNotes:\n")
    for (note in x$notes) cat("  *", note, "\n")
  }

  invisible(x)
}


# --- coef ---

# TICKET-019 coef() return-shape helpers.
.tmb_coef_internal <- function(object) {
  object$model$coefficients
}

.tmb_coef_subject <- function(object) {
  tibble::as_tibble(get_subject_pars(object, expanded = NULL))
}

.tmb_coef_fixed <- function(object) {
  co <- object$model$coefficients
  tn <- .tmb_build_term_names(object)
  fe_idx <- c(tn$q0_idx, tn$alpha_idx)
  tibble::as_tibble(stats::setNames(
    as.list(as.numeric(co[fe_idx])),
    tn$term[fe_idx]
  ))
}

#' Extract Coefficients from TMB Model
#'
#' @description
#' Extract coefficients from a fitted \code{beezdemand_tmb} model. The
#' \code{type} argument selects the return shape. The default,
#' \code{"internal"}, is unchanged: a named numeric vector of the
#' optimizer's flat parameterization (entries include \code{beta_q0},
#' \code{beta_alpha}, \code{logsigma*}, and any covariance
#' hyperparameters; intercepts are on the log scale because the optimizer
#' works in unconstrained space). This is the numeric-vector escape hatch
#' consumed by tooling such as \code{car::deltaMethod} and
#' \code{multcomp::glht}.
#'
#' @details
#' \code{type = "subject"} (alias \code{"combined"}) returns the
#' per-subject parameter tibble from \code{\link{get_subject_pars}} (with
#' \code{expanded = NULL}, so within-id factor expansion is auto-detected).
#' This is concept-parity with \code{coef.beezdemand_nlme(type = "combined")}
#' but not column-identical: it returns resolved per-subject parameters
#' (\code{Q0}, \code{alpha}, ...), not a per-design-term coefficient
#' matrix. \code{type = "fixed"} returns a one-row tibble of the
#' fixed-effect coefficients only (the \code{beta_q0} / \code{beta_alpha}
#' block on the internal parameterization), excluding \code{log_k},
#' \code{logsigma*}, and \code{rho*}.
#'
#' Scale conversion is not performed here: supplying \code{report_space}
#' through \code{...} is an error. Use \code{\link{get_subject_pars}} or
#' \code{\link{predict.beezdemand_tmb}} for natural-scale parameters.
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param type One of \code{"internal"} (default; raw optimizer vector),
#'   \code{"subject"} or its alias \code{"combined"} (per-subject
#'   parameter tibble), or \code{"fixed"} (one-row tibble of fixed-effect
#'   coefficients).
#' @param ... Additional arguments (currently unused; supplying
#'   \code{report_space} is an error).
#'
#' @return For \code{type = "internal"}, a named numeric vector. For
#'   \code{type = "subject"}/\code{"combined"}, a tibble with one row per
#'   subject (or one row per subject-by-within-id-factor-level cell when
#'   the fit has within-id factor variation). For \code{type = "fixed"}, a
#'   one-row tibble of fixed-effect coefficients.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' coef(fit)                    # raw optimizer vector (default, "internal")
#' coef(fit, type = "subject")  # per-subject parameter tibble
#' coef(fit, type = "fixed")    # fixed-effect coefficients
#' }
#'
#' @export
coef.beezdemand_tmb <- function(object,
                                type = c("internal", "subject", "combined", "fixed"),
                                ...) {
  type <- match.arg(type)
  if ("report_space" %in% names(list(...))) {
    cli::cli_abort(c(
      "{.arg report_space} is not supported by {.fn coef.beezdemand_tmb}.",
      "i" = "{.fn coef} does not convert scale here; call {.fn get_subject_pars} or {.fn predict} for natural-scale parameters."
    ))
  }
  if (type == "combined") type <- "subject"
  switch(
    type,
    internal = .tmb_coef_internal(object),
    subject  = .tmb_coef_subject(object),
    fixed    = .tmb_coef_fixed(object)
  )
}


# --- logLik / AIC / BIC ---

#' @export
logLik.beezdemand_tmb <- function(object, ...) {
  ll <- object$loglik
  attr(ll, "df") <- length(object$opt$par)
  attr(ll, "nobs") <- object$param_info$n_obs
  class(ll) <- "logLik"
  ll
}

#' @export
AIC.beezdemand_tmb <- function(object, ..., k = 2) {
  if (k != 2) {
    # Recompute with custom penalty multiplier
    nll <- -object$loglik
    n_params <- length(object$opt$par)
    return(2 * nll + k * n_params)
  }
  object$AIC
}

#' @export
BIC.beezdemand_tmb <- function(object, ...) {
  object$BIC
}

#' @export
nobs.beezdemand_tmb <- function(object, ...) {
  object$param_info$n_obs %||% nrow(object$data)
}


# --- fixef / ranef ---

#' Extract Fixed Effects from TMB Model
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param ... Additional arguments.
#'
#' @return Named numeric vector of fixed effects.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' nlme::fixef(fit)
#' }
#'
#' @export
fixef.beezdemand_tmb <- function(object, ...) {
  # Pin to the flat optimizer vector explicitly so this stays correct if a
  # future ticket flips coef()'s default away from "internal".
  coef(object, type = "internal")
}

#' Extract Random Effects from TMB Model
#'
#' Returns subject-level random effect deviations on the natural (log) scale.
#' These are the Cholesky-transformed deviations, not standardized scores.
#' To obtain the standardized random effects (`u` matrix), access
#' `object$tmb_obj` directly.
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param ... Additional arguments.
#'
#' @return Data frame with subject-level random effects. Columns:
#'   \itemize{
#'     \item `id`: subject identifier
#'     \item `b_i`, `c_i` (when present): first-column convenience aliases
#'       for `q0_(Intercept)` and `alpha_(Intercept)`. Preserved for
#'       backward compatibility with older callers.
#'     \item `q0_<term>`: per-block random-effect coefficients for log-Q0,
#'       one column per random-effects design column from the parsed
#'       block structure. For factor-expanded or multi-block fits, these
#'       expose the per-condition slope REs that `b_i` / `c_i` alone do
#'       not surface.
#'     \item `alpha_<term>`: analogous columns for log-alpha.
#'   }
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' head(nlme::ranef(fit))
#' }
#'
#' @export
ranef.beezdemand_tmb <- function(object, ...) {
  spars <- object$subject_pars
  re_q0 <- attr(spars, "re_q0_mat")
  re_alpha <- attr(spars, "re_alpha_mat")

  out <- data.frame(id = spars$id, stringsAsFactors = FALSE)

  # Backward compat: include `b_i` / `c_i` when present (first RE column
  # from each block per the existing convention). Older callers that
  # hardcoded these names continue to work; new callers should prefer
  # the `q0_<term>` / `alpha_<term>` columns below.
  if ("b_i" %in% names(spars)) out$b_i <- spars$b_i
  if ("c_i" %in% names(spars)) out$c_i <- spars$c_i

  # Phase 5A: surface ALL per-block RE columns. Critical for multi-
  # block / factor-expanded fits where block-2+ random effects (e.g.,
  # per-condition slopes) are otherwise omitted from ranef() output and
  # downstream diagnostics inspect an incomplete RE structure.
  add_re_cols <- function(out, mat, prefix) {
    if (is.null(mat) || ncol(mat) == 0L) return(out)
    nms <- colnames(mat)
    if (is.null(nms)) nms <- paste0("re_", seq_len(ncol(mat)))
    for (j in seq_len(ncol(mat))) {
      col_name <- sprintf("%s_%s", prefix, nms[j])
      out[[col_name]] <- mat[, j]
    }
    out
  }
  out <- add_re_cols(out, re_q0, "q0")
  out <- add_re_cols(out, re_alpha, "alpha")

  out
}


# --- predict ---

#' Predict from TMB Mixed-Effects Demand Model
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param newdata Optional data frame. If NULL, predicts for original data.
#' @param type Character. One of `"response"` (fitted values on response scale),
#'   `"parameters"` (subject-specific parameters), or `"demand"` (population
#'   demand curve).
#' @param level Character, used when `type = "response"`. `"subject"`
#'   (default) conditions on each subject's random effects and requires the
#'   model's ID column in `newdata` (named `id` unless a custom `id_var` was
#'   set at fit time); `"population"` evaluates at the fixed-effect
#'   coefficients with all random effects set to zero on the fitting scale
#'   (the population-mean curve) and does not require the ID column. Pass
#'   `c("population", "subject")` to return both predictions in one call.
#'   Note that `"population"` is the random-effects-at-zero curve, not a
#'   marginal prediction integrating over the random-effects distribution
#'   (see Note). Unlike [predict.beezdemand_nlme()], which forwards the
#'   `nlme`-style numeric `level` (`0` / `1`) to `nlme::predict.lme()`, this
#'   method accepts the character form only; a numeric `level` is rejected.
#' @param prices Optional numeric vector of prices for population prediction.
#' @param scale Character. Output scale for predictions: `"model"` returns values
#'   on the model's native scale (e.g., LL4-transformed for zben, log for
#'   exponential), while `"natural"` automatically back-transforms to the
#'   natural consumption scale. Default is `"model"` for backward compatibility.
#'
#'   When `scale = "natural"` and `equation = "exponential"`, the lognormal
#'   retransformation correction `exp(sigma_e^2/2)` is applied by default to
#'   produce the conditional mean (not median). Set `correction = FALSE` to
#'   obtain the median (geometric mean) instead. For `"exponentiated"` and
#'   `"simplified"` equations, predictions are already on the natural scale
#'   and no correction is needed. For `"zben"`, `ll4_inv()` is applied;
#'   because ll4_inv is nonlinear, this gives the value corresponding to the
#'   conditional mean on the LL4 scale (approximately the median on the
#'   natural scale).
#' @param correction Logical. If `TRUE` (default), applies the lognormal
#'   retransformation correction when `scale = "natural"`. Set to `FALSE` to
#'   obtain the median prediction. Only affects the `"exponential"` equation.
#' @param at Optional named numeric vector/list (e.g. `c(dose_c = 1)`) giving
#'   the covariate value(s) at which to evaluate per-subject `Q0`/`alpha` when
#'   `type = "parameters"` and the fit has a continuous random-effect slope
#'   (TICKET-051). Defaults to each subject's mean of the covariate (= the
#'   reference 0 for a centered, balanced design). Ignored (with a warning)
#'   otherwise.
#' @param ... Additional arguments.
#'
#' @return Depends on `type`:
#'   - `"response"`, `level = "subject"`: tibble of `newdata` plus a
#'     `.fitted` column (the historical column name, retained for backward
#'     compatibility).
#'   - `"response"`, `level = "population"`: tibble of `newdata` plus a
#'     `predict.fixed` column.
#'   - `"response"`, `level = c("population", "subject")`: tibble of
#'     `newdata` plus `predict.fixed` and `predict.id` columns, matching the
#'     `nlme::predict.lme(level = 0:1)` schema so `nlme`-based plotting code
#'     runs unchanged.
#'   - `"parameters"`: tibble of subject-specific parameters.
#'   - `"demand"`: tibble with `price` and `.fitted` columns.
#'
#' @note Population-averaged (marginal) predictions integrating over the
#'   random effects distribution are not yet implemented for this model tier.
#'   The `type = "demand"` prediction and `level = "population"` both use
#'   RE = 0 (population fixed effects only). For marginal integration
#'   accounting for Jensen's inequality, use [predict.beezdemand_hurdle()]
#'   with `marginal = TRUE`.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#'
#' # Fitted values (subject-conditional, the default)
#' head(predict(fit, type = "response"))
#'
#' # Population-mean predictions: no `id` column needed in newdata
#' nd <- data.frame(x = c(0.01, 1, 5, 10))
#' predict(fit, newdata = nd, level = "population")
#'
#' # Subject-conditional and population side by side in one call
#' nd_id <- data.frame(x = c(0.01, 1, 5, 10), id = unique(apt$id)[1])
#' predict(fit, newdata = nd_id, level = c("population", "subject"))
#'
#' # Population demand curve at specific prices
#' predict(fit, type = "demand", prices = c(0, 1, 5, 10, 20))
#'
#' # Subject-level parameters
#' head(predict(fit, type = "parameters"))
#' }
#'
#' @seealso [predict.beezdemand_nlme()] for the `nlme`-backed equivalent,
#'   which uses the numeric `level` convention.
#' @export
predict.beezdemand_tmb <- function(
  object,
  newdata = NULL,
  type = c("response", "parameters", "demand"),
  level = "subject",
  prices = NULL,
  scale = c("model", "natural"),
  correction = TRUE,
  at = NULL,
  ...
) {
  type <- match.arg(type)
  scale <- match.arg(scale)
  # `level` accepts one or both of "subject"/"population". A numeric
  # nlme-style level (0/1) is rejected here by match.arg(); see the @param
  # note contrasting this with predict.beezdemand_nlme().
  level <- match.arg(level, c("subject", "population"), several.ok = TRUE)

  if (type == "parameters") {
    re_parsed <- object$param_info$random_effects_parsed
    cont_terms <- .tmb_continuous_re_terms(re_parsed, object$data)
    if (length(cont_terms) > 0L) {
      # Reconcile with get_subject_pars(): per-subject parameters evaluated at
      # `at` (default reference 0) with slope columns -- never the NA collapse.
      return(tibble::as_tibble(get_subject_pars(object, at = at)))
    }
    if (!is.null(at)) {
      cli::cli_warn(
        "{.arg at} is ignored: this fit has no continuous random-effect slope."
      )
    }
    return(tibble::as_tibble(object$subject_pars))
  }

  equation <- object$param_info$equation
  x_var <- object$param_info$x_var
  id_var <- object$param_info$id_var
  coefs <- object$model$coefficients
  has_k <- object$param_info$has_k

  if (type == "demand") {
    # Population-level demand curve
    if (is.null(prices)) {
      max_price <- max(object$data[[x_var]], na.rm = TRUE)
      prices <- seq(0, max_price, length.out = 200)
    }

    # Get population intercepts (reference level only)
    beta_q0_idx <- which(names(coefs) == "beta_q0")
    beta_alpha_idx <- which(names(coefs) == "beta_alpha")
    log_q0 <- coefs[beta_q0_idx[1]]
    log_alpha <- coefs[beta_alpha_idx[1]]
    Q0 <- exp(log_q0)
    alpha_val <- exp(log_alpha)

    # The population curve is drawn at beta_q0[1] and beta_alpha[1],
    # which correspond to the reference level of every factor AND all
    # continuous covariates equal to zero. Warn in either case so users
    # know the curve is not mean-centered. Proper population-average
    # curves (covariates at training means, factors integrated over
    # observed frequencies) land in TICKET-011 Phase 5.
    has_factors <- !is.null(object$param_info$factors) &&
      length(object$param_info$factors) > 0L
    has_covariates <- !is.null(object$param_info$continuous_covariates) &&
      length(object$param_info$continuous_covariates) > 0L
    if (has_factors && has_covariates) {
      warning(
        "Demand curve reflects reference level for factors and ",
        "covariates = 0. Use get_demand_param_emms() for conditional ",
        "curves at specific factor levels / covariate values.",
        call. = FALSE
      )
    } else if (has_factors) {
      warning(
        "Demand curve reflects reference level only. ",
        "Use get_demand_param_emms() for per-group curves.",
        call. = FALSE
      )
    } else if (has_covariates) {
      warning(
        "Demand curve reflects covariates = 0, not the training mean. ",
        "Use get_demand_param_emms() for curves at specific covariate ",
        "values.",
        call. = FALSE
      )
    }

    fitted <- .tmb_predict_equation(
      prices, Q0, alpha_val,
      k = if (has_k) .tmb_get_k(object) else NA,
      log_q0 = log_q0,
      equation = equation
    )

    ## Back-transform to natural scale if requested
    if (scale == "natural") {
      se <- exp(coefs[["logsigma_e"]])
      fitted <- .tmb_backtransform(fitted, equation, sigma_e = se,
                                    correction = correction)
    }

    return(tibble::tibble(
      price = prices,
      .fitted = fitted
    ))
  }

  # type == "response": fitted values at the requested random-effect
  # level(s). Fixed-effect linear predictors are rebuilt from newdata so
  # that factor and continuous-covariate values propagate into Q0 and
  # alpha; see .tmb_build_predicted_pars().
  if (is.null(newdata)) {
    newdata <- object$data
  }

  out <- tibble::as_tibble(newdata)

  # Single subject path keeps the historical `.fitted` column name so that
  # existing callers and the cross-tier predict() contract are unchanged.
  if (identical(level, "subject")) {
    out$.fitted <- .tmb_predict_subject(object, newdata, scale = scale,
                                        correction = correction)
    return(out)
  }

  # Population-only or both-levels: use the nlme-style `predict.fixed` /
  # `predict.id` column names so nlme-based plotting code runs unchanged.
  if ("population" %in% level) {
    out$predict.fixed <- .tmb_predict_population(
      object, newdata, scale = scale, correction = correction
    )
  }
  if ("subject" %in% level) {
    out$predict.id <- .tmb_predict_subject(
      object, newdata, scale = scale, correction = correction
    )
  }
  out
}


#' Subject-conditional response predictions for a TMB demand fit
#'
#' Rebuilds per-row Q0 and alpha conditioning on each subject's random
#' effects, evaluates the demand equation, and (optionally) back-transforms
#' to the natural scale. Requires an `id` column in `newdata`.
#'
#' @param object A `beezdemand_tmb` fit.
#' @param newdata Data frame with the model's `id`, price, factor and
#'   covariate columns.
#' @param scale Character, `"model"` or `"natural"` (see
#'   [predict.beezdemand_tmb()]).
#' @param correction Logical lognormal retransformation flag (see
#'   [predict.beezdemand_tmb()]).
#' @return Numeric vector of fitted values, one per row of `newdata`.
#' @keywords internal
.tmb_predict_subject <- function(object, newdata, scale = "model",
                                 correction = TRUE) {
  bp <- .tmb_build_predicted_pars(object, newdata, level = "subject")
  .tmb_response_from_pars(object, newdata, bp, scale, correction)
}

#' Population-mean response predictions for a TMB demand fit
#'
#' Rebuilds per-row Q0 and alpha at the fixed-effect coefficients with all
#' random effects set to zero on the fitting scale (the population mean),
#' evaluates the demand equation, and (optionally) back-transforms. Does
#' not require an `id` column in `newdata`.
#'
#' @inheritParams .tmb_predict_subject
#' @return Numeric vector of fitted values, one per row of `newdata`.
#' @keywords internal
.tmb_predict_population <- function(object, newdata, scale = "model",
                                    correction = TRUE) {
  bp <- .tmb_build_predicted_pars(object, newdata, level = "population")
  .tmb_response_from_pars(object, newdata, bp, scale, correction)
}

#' Evaluate the demand equation from rebuilt per-row parameters
#'
#' Shared back end for [.tmb_predict_subject()] and
#' [.tmb_predict_population()]: evaluates the fit's equation at the
#' supplied per-row parameters and applies the natural-scale
#' back-transformation when requested.
#'
#' @inheritParams .tmb_predict_subject
#' @param bp List with `Q0`, `alpha`, and `log_q0` (the output of
#'   [.tmb_build_predicted_pars()]).
#' @return Numeric vector of fitted values, one per row of `newdata`.
#' @keywords internal
.tmb_response_from_pars <- function(object, newdata, bp, scale, correction) {
  pinfo    <- object$param_info
  equation <- pinfo$equation
  has_k    <- isTRUE(pinfo$has_k)
  k_val    <- if (has_k) .tmb_get_k(object) else NA
  fitted   <- .tmb_predict_equation(
    newdata[[pinfo$x_var]], bp$Q0, bp$alpha,
    k = k_val, log_q0 = bp$log_q0, equation = equation
  )
  if (scale == "natural") {
    se <- exp(object$model$coefficients[["logsigma_e"]])
    fitted <- .tmb_backtransform(fitted, equation, sigma_e = se,
                                 correction = correction)
  }
  fitted
}

#' Predict Single Observation for Each Equation
#'
#' @keywords internal
.tmb_predict_equation <- function(price, Q0, alpha, k, log_q0, equation) {
  switch(equation,
    exponential = {
      # Returns log(Q): ln(Q) = ln(Q0) + k*ln(10)*(exp(-α*Q0*C) - 1)
      log_q0 + k * log(10) * (exp(-alpha * Q0 * price) - 1)
    },
    exponentiated = {
      # Returns raw Q
      log_Q_pred <- log_q0 + k * log(10) * (exp(-alpha * Q0 * price) - 1)
      exp(log_Q_pred)
    },
    simplified = {
      # Returns raw Q
      Q0 * exp(-alpha * Q0 * price)
    },
    zben = {
      # Returns LL4(Q) scale
      Q0_log10 <- log_q0 / log(10)
      # Clamp to positive minimum to avoid division by zero (Q0_nat = 1)
      # and sign-flip divergence (Q0_nat < 1 → negative Q0_log10 →
      # negative decay rate → demand increases with price)
      Q0_log10 <- pmax(Q0_log10, 1e-3)
      rate <- (alpha / Q0_log10) * Q0
      Q0_log10 * exp(-rate * price)
    }
  )
}


#' Rebuild per-row Q0 and alpha from newdata for predict.beezdemand_tmb
#'
#' For each row of `newdata`, reconstruct the fixed-effect linear predictor
#' from the stored formula RHS and beta coefficients, add the subject's
#' random-effect deviate (or zero throughout when `level = "population"`),
#' and return `Q0 = exp(eta_q0)` and `alpha = exp(eta_alpha)`. This is what
#' makes `predict()` respect factor and continuous-covariate values in
#' `newdata`.
#'
#' @param object A `beezdemand_tmb` fit.
#' @param newdata A data frame with the modeling columns used at fit time
#'   (`id_var`, `x_var`, factor columns, continuous covariate columns).
#'   The `id_var` column is not required when `level = "population"`.
#' @param level Character, `"subject"` (default) adds each subject's
#'   random-effect deviate; `"population"` sets every random effect to zero.
#' @return A list with elements `Q0`, `alpha`, and `log_q0` (each of length
#'   `nrow(newdata)`).
#' @keywords internal
.tmb_build_predicted_pars <- function(object, newdata,
                                      level = c("subject", "population")) {
  level <- match.arg(level)
  pinfo <- object$param_info
  spars <- object$subject_pars
  coefs <- object$model$coefficients

  beta_q0    <- unname(coefs[names(coefs) == "beta_q0"])
  beta_alpha <- unname(coefs[names(coefs) == "beta_alpha"])

  # 1. Validate required columns are present. Phase 2 also requires
  # variables that appear only in the RE formula RHS (not in `factors`):
  # without them, .tmb_build_z_matrices() in step 4 below crashes with
  # cryptic `model.matrix()` errors.
  re_parsed_pre <- pinfo$random_effects_parsed
  re_rhs_vars_pre <- character(0)
  if (!is.null(re_parsed_pre)) {
    for (b in re_parsed_pre$blocks) {
      rhs_form <- stats::as.formula(paste("~", deparse1(b$formula[[3]])))
      re_rhs_vars_pre <- c(re_rhs_vars_pre, all.vars(rhs_form))
    }
    re_rhs_vars_pre <- unique(re_rhs_vars_pre)
  }
  # Population predictions condition on no subject, so the `id` column and
  # any variables appearing only in the random-effect formula RHS are not
  # required (the RE block in step 4 is skipped entirely for that level).
  if (level == "population") {
    needed <- unique(c(pinfo$x_var, pinfo$factors_q0, pinfo$factors_alpha,
                       pinfo$continuous_covariates))
  } else {
    needed <- unique(c(pinfo$id_var, pinfo$x_var,
                       pinfo$factors_q0, pinfo$factors_alpha,
                       pinfo$continuous_covariates, re_rhs_vars_pre))
  }
  needed <- needed[!is.null(needed) & nzchar(needed)]
  missing_cols <- setdiff(needed, names(newdata))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg newdata} is missing required column{?s}: {.field {missing_cols}}"
    )
  }

  # NAs in any model-matrix column (including x_var / price) propagate
  # into mismatched-shape Z / X arrays downstream. Reject them up front
  # with a clear error so the user can clean their newdata.
  na_cols <- needed[vapply(needed, function(c) any(is.na(newdata[[c]])),
                            logical(1))]
  if (length(na_cols) > 0) {
    cli::cli_abort(c(
      "{.arg newdata} has missing values in column{?s}: {.field {na_cols}}",
      "i" = "Drop NA rows or impute before calling {.fun predict}."
    ))
  }

  # 2. Coerce newdata factor columns to the training-time level sets so
  #    model.matrix builds the same columns (and errors loudly on unseen
  #    levels). Phase 2 also coerces RE-only RHS factors (those that
  #    appear in the RE formula but NOT in `factors`) -- otherwise
  #    .tmb_build_z_matrices() in step 4 below builds a Z with fewer
  #    columns than re_q0_mat / re_alpha_mat have rows.
  train <- object$data
  re_parsed <- pinfo$random_effects_parsed
  re_factor_vars <- character(0)
  if (!is.null(re_parsed)) {
    for (b in re_parsed$blocks) {
      rhs_form <- stats::as.formula(paste("~", deparse1(b$formula[[3]])))
      re_factor_vars <- c(re_factor_vars, all.vars(rhs_form))
    }
    re_factor_vars <- unique(re_factor_vars)
  }
  factors_to_coerce <- unique(c(
    pinfo$factors_q0, pinfo$factors_alpha, re_factor_vars
  ))
  for (f in factors_to_coerce) {
    if (is.null(f) || !nzchar(f)) next
    if (!(f %in% names(train))) next
    if (!is.factor(train[[f]])) next
    if (!(f %in% names(newdata))) next
    train_levels <- levels(train[[f]])
    new_vals <- as.character(newdata[[f]])
    bad <- setdiff(unique(new_vals[!is.na(new_vals)]), train_levels)
    if (length(bad) > 0) {
      cli::cli_abort(c(
        "Factor {.field {f}} in {.arg newdata} contains levels not seen in training: {.val {bad}}",
        "i" = "Refit with these levels in the data, or recode {.arg newdata}."
      ))
    }
    newdata[[f]] <- factor(new_vals, levels = train_levels)
  }

  # 3. Rebuild per-row design matrices using the stored RHS.
  X_q0_new <- stats::model.matrix(
    stats::as.formula(object$formula_details$rhs_q0),
    data = newdata
  )
  X_alpha_new <- stats::model.matrix(
    stats::as.formula(object$formula_details$rhs_alpha),
    data = newdata
  )

  if (ncol(X_q0_new) != length(beta_q0)) {
    cli::cli_abort(
      "Rebuilt X_q0 has {ncol(X_q0_new)} column{?s} but beta_q0 has {length(beta_q0)}."
    )
  }
  if (ncol(X_alpha_new) != length(beta_alpha)) {
    cli::cli_abort(
      "Rebuilt X_alpha has {ncol(X_alpha_new)} column{?s} but beta_alpha has {length(beta_alpha)}."
    )
  }

  log_q0_fix    <- as.numeric(X_q0_new    %*% beta_q0)
  log_alpha_fix <- as.numeric(X_alpha_new %*% beta_alpha)

  # 4. Random-effect deviates. `level = "population"` sets every random
  #    effect to zero (so it needs no `id` lookup); `level = "subject"`
  #    adds each subject's empirical-Bayes deviate via the helper below.
  if (level == "population") {
    re_q0_contrib    <- numeric(nrow(newdata))
    re_alpha_contrib <- numeric(nrow(newdata))
  } else {
    re_dev <- .tmb_subject_re_deviates(object, newdata, re_parsed)
    re_q0_contrib    <- re_dev$re_q0_contrib
    re_alpha_contrib <- re_dev$re_alpha_contrib
  }

  log_q0_total    <- log_q0_fix    + re_q0_contrib
  log_alpha_total <- log_alpha_fix + re_alpha_contrib
  list(
    Q0     = exp(log_q0_total),
    alpha  = exp(log_alpha_total),
    log_q0 = log_q0_total
  )
}


#' Per-subject random-effect deviates for predict.beezdemand_tmb
#'
#' For each row of `newdata`, returns the random-effect contribution to the
#' Q0 and alpha linear predictors, looked up by `id`; errors if any id is
#' not a subject in the fit. Extracted from [.tmb_build_predicted_pars()] so
#' the population-level prediction path can skip it entirely.
#'
#' @param object A `beezdemand_tmb` fit.
#' @param newdata A data frame containing the model's `id` column.
#' @param re_parsed The fit's parsed random-effects specification
#'   (`object$param_info$random_effects_parsed`); may be `NULL`.
#' @return A list with `re_q0_contrib` and `re_alpha_contrib`, each a
#'   numeric vector of length `nrow(newdata)`.
#' @keywords internal
.tmb_subject_re_deviates <- function(object, newdata, re_parsed) {
  pinfo <- object$param_info
  spars <- object$subject_pars
  n_re  <- pinfo$n_random_effects

  subj_ids <- as.character(newdata[[pinfo$id_var]])
  subj_match <- match(subj_ids, spars$id)
  if (anyNA(subj_match)) {
    unknown_ids <- unique(subj_ids[is.na(subj_match)])
    cli::cli_abort(c(
      "{cli::qty(unknown_ids)} id{?s} in {.arg newdata} not found in the fitted model: {.val {unknown_ids}}.",
      i = "{.code level = \"subject\"} conditions on each subject's estimated random effects, which exist only for subjects in the fit.",
      i = "Use {.code level = \"population\"} for the population-mean prediction (random effects set to zero)."
    ))
  }
  # For factor-expanded RE specs the
  # per-subject RE contribution is `Z[i, ] %*% re_mat[subj_i, ]`, NOT
  # just spars$b_i (which holds only the FIRST RE column for backward
  # compat). Build Z from newdata via the same helper used at fit time;
  # for intercept-only fits Z is a column of 1s and the dot product
  # collapses to spars$b_i, preserving backward compatibility.
  re_q0_mat <- attr(spars, "re_q0_mat")
  re_alpha_mat <- attr(spars, "re_alpha_mat")

  re_q0_contrib <- numeric(length(subj_ids))
  re_alpha_contrib <- numeric(length(subj_ids))

  if (!is.null(re_parsed) && !is.null(re_q0_mat) && !is.null(re_alpha_mat)) {
    z_new <- .tmb_build_z_matrices(re_parsed, newdata, id_var = pinfo$id_var)
    if (z_new$re_dim_q0 > 0L && ncol(re_q0_mat) > 0L) {
      for (i in seq_along(subj_ids)) {
        sm <- subj_match[i]
        if (!is.na(sm)) {
          re_q0_contrib[i] <- sum(z_new$Z_q0[i, ] * re_q0_mat[sm, ])
        }
      }
    }
    if (z_new$re_dim_alpha > 0L && ncol(re_alpha_mat) > 0L) {
      for (i in seq_along(subj_ids)) {
        sm <- subj_match[i]
        if (!is.na(sm)) {
          re_alpha_contrib[i] <- sum(z_new$Z_alpha[i, ] * re_alpha_mat[sm, ])
        }
      }
    }
  } else {
    # Fallback path for fits that pre-date Phase 2.4 (no attached
    # re_q0_mat / re_alpha_mat attribute on subject_pars).
    re_q0_contrib <- ifelse(is.na(subj_match), 0, spars$b_i[subj_match])
    if (n_re == 2 && "c_i" %in% names(spars)) {
      re_alpha_contrib <- ifelse(is.na(subj_match), 0, spars$c_i[subj_match])
    }
  }
  list(re_q0_contrib = re_q0_contrib, re_alpha_contrib = re_alpha_contrib)
}


#' Back-transform predictions from model scale to natural consumption scale
#'
#' @param fitted Numeric vector of predictions on model scale.
#' @param equation Character. The equation used for fitting.
#' @param sigma_e Numeric scalar. Residual standard deviation on the model scale.
#'   Used for lognormal retransformation correction when `correction = TRUE`.
#' @param correction Logical. If `TRUE`, applies the lognormal retransformation
#'   correction `exp(sigma_e^2 / 2)` for the exponential equation. Default `TRUE`.
#'
#' @return Numeric vector of predictions on the natural (consumption) scale.
#' @keywords internal
.tmb_backtransform <- function(fitted, equation, sigma_e = NULL, correction = TRUE) {
  switch(equation,
    zben = ll4_inv(fitted),
    exponential = {
      # Lognormal retransformation: E[Q|Q>0] = exp(mu + sigma_e^2/2)
      cf <- if (isTRUE(correction) && !is.null(sigma_e)) {
        exp(sigma_e^2 / 2)
      } else {
        1
      }
      exp(fitted) * cf
    },
    fitted # exponentiated, simplified already on natural scale
  )
}


# --- get_subject_pars ---

# Resolve the `expanded` argument for get_subject_pars.beezdemand_tmb.
# NULL → auto-detect: any NA in cached subject_pars$Q0 means within-id
# variation was flagged at fit time (R/tmb-demand.R:865-908) and the wide
# table is unusable; the long expanded shape is the correct default.
# TRUE/FALSE → explicit override, with a one-line warning on the
# FALSE-on-within-id case so the user knows they're getting NAs.
.resolve_subject_pars_expanded <- function(object, expanded) {
  if (is.null(expanded)) {
    sp <- object$subject_pars
    if (is.null(sp) || !("Q0" %in% names(sp))) return(FALSE)
    return(any(is.na(sp$Q0)))
  }
  if (!is.logical(expanded) || length(expanded) != 1L || is.na(expanded)) {
    cli::cli_abort(c(
      "{.arg expanded} must be {.code TRUE}, {.code FALSE}, or {.code NULL}.",
      "i" = "Got {.cls {class(expanded)[1]}} of length {length(expanded)}."
    ))
  }
  if (!expanded) {
    sp <- object$subject_pars
    if (!is.null(sp) && "Q0" %in% names(sp) && any(is.na(sp$Q0))) {
      cli::cli_warn(c(
        "{.field subject_pars} returned with {.field Q0}/{.field alpha} as {.val NA} for affected subjects.",
        "i" = "Call {.code get_subject_pars(fit)} (auto-detect) or {.code get_subject_pars(fit, expanded = TRUE)} for per-(subject, factor-level) values."
      ))
    }
  }
  expanded
}

# Resolve the requested evaluation value for a continuous RE term `v` from the
# `at` argument (TICKET-051). `at` may be a named numeric vector/list keyed by
# covariate name (`c(dose_c = 2)`), or an unnamed length-1 scalar applied to a
# single continuous term. Returns NULL when `v` has no requested value.
.tmb_at_value <- function(at, v) {
  if (is.null(at)) return(NULL)
  if (!is.null(names(at)) && v %in% names(at)) return(at[[v]])
  if (is.null(names(at)) && length(at) == 1L) return(at[[1L]])
  NULL
}

#' Get Subject-Specific Parameters from TMB Model
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param expanded Controls return shape for fits with within-id-varying
#'   design columns (factor-expanded random effects, within-id
#'   continuous covariates, or multi-block \code{pdBlocked} specs).
#'   \itemize{
#'     \item \code{NULL} (default): auto-detect. When fit-time within-id
#'       variation caused \code{NA} in cached \code{subject_pars$Q0},
#'       runs the expansion machinery: rows are expanded across
#'       within-id factor levels (one row per (subject, factor-level)
#'       cell), and within-id numeric covariates are conditioned at
#'       the subject's mean (no row expansion from numerics). When the
#'       cached \code{Q0} has no \code{NA}, returns the wide
#'       one-row-per-subject shape unchanged.
#'     \item \code{TRUE}: always attempt expansion. On a fit with no
#'       within-id variation, silently returns the wide shape.
#'     \item \code{FALSE}: always return the wide shape. Emits a
#'       one-line warning on a fit with within-id variation (the
#'       returned \code{Q0}, \code{alpha}, \code{Pmax}, \code{Omax}
#'       are \code{NA}).
#'   }
#' @param at Optional named numeric vector/list (e.g. \code{c(dose_c = 1)})
#'   giving the covariate value(s) at which to evaluate per-subject
#'   \code{Q0}/\code{alpha} for continuous random-effect slope terms
#'   (TICKET-051). Defaults to each subject's mean of the covariate (which
#'   equals the reference 0 for a centered, balanced design). The per-subject
#'   slope deviations are always returned as \code{q0_<term>} /
#'   \code{alpha_<term>} columns regardless of \code{at}. Ignored (with a
#'   warning) for fits without a continuous random slope.
#' @param ... Additional arguments (currently unused).
#'
#' @return When the resolved \code{expanded} is \code{FALSE}: data
#'   frame with columns \code{id}, \code{b_i}, \code{c_i} (if 2 RE),
#'   \code{Q0}, \code{alpha}, \code{Pmax}, \code{Omax}. When the
#'   resolved \code{expanded} is \code{TRUE}, the shape depends on the
#'   kind of within-id variation: for fits with within-id factors, the
#'   within-subject factor columns are added and rows are expanded to one
#'   per (subject, factor-level) cell with per-cell \code{Q0},
#'   \code{alpha}, \code{Pmax}, \code{Omax}; for fits whose only within-id
#'   variation is numeric, the numerics are conditioned at the subject's
#'   mean and the return is one row per subject (no added factor columns)
#'   with finite \code{Q0} / \code{alpha}.
#'
#' @section Per-block random-effect matrices:
#'   For factor-expanded or multi-block fits, the wide table's
#'   \code{b_i} / \code{c_i} columns hold the first RE column from each
#'   block (intercept slot for the M1 baseline block, for example) for
#'   backward compatibility with downstream consumers. Power users who
#'   need the full per-block RE structure can access
#'   \code{attr(subject_pars, "re_q0_mat")} and
#'   \code{attr(subject_pars, "re_alpha_mat")} as
#'   \code{n_subjects x re_dim} matrices ordered by block.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' head(get_subject_pars(fit))
#' }
#'
#' @export
get_subject_pars.beezdemand_tmb <- function(object, expanded = NULL, at = NULL, ...) {
  pinfo <- object$param_info
  data <- object$data
  spars <- object$subject_pars
  re_parsed <- pinfo$random_effects_parsed
  id_var <- pinfo$id_var

  # TICKET-051: continuous within-subject RE slope terms. When present, the
  # per-subject Q0/alpha are a function of the covariate -- evaluate them at
  # `at` (default the reference 0) and surface per-subject slope deviations
  # rather than the NA wide collapse. Factor/intercept fits (no numeric RE
  # term) keep the existing behavior byte-for-byte.
  cont_terms <- .tmb_continuous_re_terms(re_parsed, data)
  expanded_was_null <- is.null(expanded)

  expanded <- .resolve_subject_pars_expanded(object, expanded)
  if (length(cont_terms) > 0L) {
    if (expanded_was_null) {
      # Continuous slope: the reconciled per-subject table (slope columns + `at`)
      # is the right default even when subject_pars$Q0 is non-NA (e.g. a fit with
      # validate_subject_pars = FALSE). An explicit expanded = FALSE still
      # returns the wide table.
      expanded <- TRUE
    }
    if (!is.null(at) && !expanded) {
      cli::cli_warn(
        "{.arg at} is ignored when {.code expanded = FALSE}; the wide table holds row-order-dependent or NA parameters."
      )
    } else if (!is.null(at) && !is.null(names(at))) {
      bad <- setdiff(names(at), cont_terms)
      if (length(bad) > 0L) {
        cli::cli_warn(
          "{.arg at} name{?s} {.val {bad}} {?is/are} not a continuous RE term and will be ignored."
        )
      }
    }
  } else if (!is.null(at)) {
    cli::cli_warn(
      "{.arg at} is ignored: this fit has no continuous random-effect slope."
    )
  }
  if (!expanded) {
    return(object$subject_pars)
  }

  if (is.null(data)) {
    cli::cli_abort(c(
      "Cannot construct expanded {.field subject_pars}: training {.field data} not attached to fit object.",
      "i" = "This indicates an old fit produced before {.field expanded} support was added."
    ))
  }

  # 1. Discover candidate variables: union of fixed-effect factors,
  #    continuous covariates, and RE-RHS variables from the parsed block
  #    formulas. continuous_covariates must be in the candidate set so
  #    that within-id-varying covariates (e.g. trial_num) are conditioned
  #    at subject mean rather than falling through to copy-first-row.
  fe_factors <- unique(c(
    pinfo$factors,
    pinfo$factors_q0,
    pinfo$factors_alpha
  ))
  re_rhs_vars <- character(0)
  if (!is.null(re_parsed)) {
    for (b in re_parsed$blocks) {
      rhs <- b$formula[[3L]]
      re_rhs_vars <- c(re_rhs_vars, all.vars(stats::as.formula(
        paste("~", deparse1(rhs))
      )))
    }
    re_rhs_vars <- unique(re_rhs_vars)
  }
  candidate_vars <- unique(c(
    fe_factors,
    pinfo$continuous_covariates,
    re_rhs_vars
  ))
  candidate_vars <- candidate_vars[
    nzchar(candidate_vars) & !is.na(candidate_vars) &
    candidate_vars %in% names(data)
  ]

  # 2. Classify each candidate by type and within-id variation.
  classify <- function(var) {
    vals <- data[[var]]
    if (is.factor(vals) || is.character(vals)) {
      type <- "factor"
      lvls <- if (is.factor(vals)) levels(vals) else sort(unique(vals))
    } else if (is.numeric(vals) || is.integer(vals)) {
      type <- "numeric"
      lvls <- NULL
    } else {
      cli::cli_abort(c(
        "Cannot expand {.field subject_pars} over RE term {.field {var}} of type {.cls {class(vals)[1]}}.",
        "i" = "Pass {.code expanded = FALSE} to force the wide NA-fill, or pre-process the variable into a factor or numeric before fitting."
      ))
    }
    by_id <- split(vals, data[[id_var]])
    varies <- any(vapply(by_id, function(v) length(unique(v)) > 1L, logical(1)))
    list(var = var, type = type, varies = varies, levels = lvls)
  }
  classification <- lapply(candidate_vars, classify)
  names(classification) <- candidate_vars

  # 3. Within-subject factors drive cross-product expansion. Numeric
  #    within-id-varying variables are NOT expanded — they are
  #    conditioned at the subject's mean below.
  expand_factors <- candidate_vars[
    vapply(classification, function(cls) cls$type == "factor" && cls$varies,
           logical(1))
  ]

  # If no within-id variation of any kind exists, the wide spars table
  # is already well-defined; just return it. Otherwise we still need
  # per-subject row construction to condition numeric within-id
  # variables at subject mean.
  any_within_id <- any(vapply(classification, function(cls) cls$varies,
                              logical(1)))
  if (!any_within_id) {
    return(spars)
  }

  if (length(expand_factors) > 0L) {
    expand_grid <- expand.grid(
      lapply(expand_factors, function(var) classification[[var]]$levels),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    names(expand_grid) <- expand_factors
  } else {
    # No factor expansion: one row per subject. expand.grid() on an empty
    # list returns a 1-row 0-col data.frame.
    expand_grid <- data.frame(.row = 1L)[, FALSE, drop = FALSE]
  }

  # 4. Build long-form newdata: one row per (subject, factor-cell).
  subj_ids <- as.character(spars$id)
  newdata_rows <- vector("list", length(subj_ids))
  for (i in seq_along(subj_ids)) {
    sid <- subj_ids[i]
    subj_rows <- data[as.character(data[[id_var]]) == sid, , drop = FALSE]
    if (nrow(subj_rows) == 0L) next

    if (ncol(expand_grid) > 0L) {
      cell_rows <- expand_grid
    } else {
      # No factor expansion: a single conditioned row per subject.
      cell_rows <- data.frame(.placeholder = NA)[, FALSE, drop = FALSE]
      cell_rows[1L, ".placeholder"] <- NA  # ensure 1 row, 0 cols
      cell_rows <- cell_rows[, character(0), drop = FALSE]
    }
    cell_rows[[id_var]] <- sid

    # x_var (price): use first observed price (Pmax/Omax search range
    # uses per-subject price_list later, so the row-level x_var only
    # needs to satisfy .tmb_build_predicted_pars()'s validation).
    if (!is.null(pinfo$x_var) && nzchar(pinfo$x_var)) {
      cell_rows[[pinfo$x_var]] <- subj_rows[[pinfo$x_var]][1]
    }

    # Other variables: between-subject use actual; within-id-varying
    # numeric use subject mean; factors handled via expand_grid.
    other_vars <- setdiff(
      names(data),
      c(id_var, pinfo$x_var, pinfo$y_var, expand_factors)
    )
    for (v in other_vars) {
      cls <- classification[[v]]
      if (is.null(cls)) {
        # Variable not in candidate set: copy first observed value.
        # (Variables outside factors / continuous_covariates / RE-RHS
        # are passed through to satisfy .tmb_build_predicted_pars()
        # column validation; their values do not enter the linear
        # predictor.)
        cell_rows[[v]] <- subj_rows[[v]][1]
      } else if (cls$type == "factor" && !cls$varies) {
        cell_rows[[v]] <- subj_rows[[v]][1]
      } else if (cls$type == "numeric" && cls$varies) {
        # Within-id numeric: condition at the subject mean by default
        # (TICKET-022). For a continuous RE-slope term (TICKET-051) an explicit
        # `at` value overrides the mean so users can evaluate per-subject
        # Q0/alpha at a chosen covariate value (e.g. the reference,
        # at = c(dose_c = 0)). For a centered covariate the two coincide.
        at_v <- if (v %in% cont_terms) .tmb_at_value(at, v) else NULL
        cell_rows[[v]] <- if (!is.null(at_v)) {
          at_v
        } else {
          mean(subj_rows[[v]], na.rm = TRUE)
        }
      } else if (cls$type == "numeric" && !cls$varies) {
        cell_rows[[v]] <- subj_rows[[v]][1]
      }
      # factor + varies handled by expand_grid (skip here)
    }

    # Restore factor levels lost via expand.grid character coercion.
    for (v in expand_factors) {
      orig_levels <- classification[[v]]$levels
      cell_rows[[v]] <- factor(cell_rows[[v]], levels = orig_levels)
    }

    newdata_rows[[i]] <- cell_rows
  }
  newdata_long <- do.call(rbind, newdata_rows)
  rownames(newdata_long) <- NULL

  # 5. Compute per-row Q0 and alpha via the same machinery predict() uses.
  bp <- .tmb_build_predicted_pars(object, newdata_long)
  Q0 <- bp$Q0
  alpha <- bp$alpha

  # 6. Compute per-row Pmax/Omax. price_list is per-subject (price ranges
  #    don't typically vary by condition); replicated across cells per
  #    subject.
  has_k <- isTRUE(pinfo$has_k)
  if (has_k) {
    coefs <- object$model$coefficients
    if ("log_k" %in% names(coefs)) {
      k_val <- exp(coefs[["log_k"]])
    } else if (!is.null(pinfo$k_fixed)) {
      k_val <- pinfo$k_fixed
    } else {
      k_val <- 2
    }
    model_type <- "hs"
    params_df <- data.frame(alpha = alpha, q0 = Q0, k = rep(k_val, length(Q0)))
    param_scales <- list(alpha = "natural", q0 = "natural", k = "natural")
  } else {
    # zben has no SND closed form (its LL4-scale decay differs from SND);
    # route through the engine's numerical fallback instead (GH #19).
    model_type <- if (identical(pinfo$equation, "zben")) "zben" else "snd"
    params_df <- data.frame(alpha = alpha, q0 = Q0)
    param_scales <- list(alpha = "natural", q0 = "natural")
  }

  # Build per-row price_list using the subject's training price range.
  row_subj_ids <- as.character(newdata_long[[id_var]])
  price_per_subject <- split(data[[pinfo$x_var]], data[[id_var]])
  price_list <- lapply(row_subj_ids, function(sid) {
    ps <- price_per_subject[[sid]]
    if (is.null(ps)) numeric(0) else ps
  })

  omax_pmax <- beezdemand_calc_pmax_omax_vec(
    params_df = params_df,
    model_type = model_type,
    param_scales = param_scales,
    price_list = price_list,
    compute_observed = FALSE
  )

  # 7. Assemble output: id + factor cols + b_i/c_i (replicated) + Q0/alpha/Pmax/Omax.
  out <- data.frame(id = newdata_long[[id_var]], stringsAsFactors = FALSE)
  for (v in expand_factors) {
    out[[v]] <- newdata_long[[v]]
  }
  spars_match <- match(as.character(out$id), as.character(spars$id))
  if ("b_i" %in% names(spars)) out$b_i <- spars$b_i[spars_match]
  if ("c_i" %in% names(spars)) out$c_i <- spars$c_i[spars_match]

  # TICKET-051: per-subject slope deviations for continuous RE terms (the same
  # values ranef() exposes), so the dose-response is visible at the subject
  # level alongside the point Q0/alpha evaluated at `at`.
  if (length(cont_terms) > 0L) {
    re_q0_mat <- attr(spars, "re_q0_mat")
    re_alpha_mat <- attr(spars, "re_alpha_mat")
    for (tm in cont_terms) {
      if (!is.null(re_q0_mat) && tm %in% colnames(re_q0_mat)) {
        out[[paste0("q0_", tm)]] <- re_q0_mat[spars_match, tm]
      }
      if (!is.null(re_alpha_mat) && tm %in% colnames(re_alpha_mat)) {
        out[[paste0("alpha_", tm)]] <- re_alpha_mat[spars_match, tm]
      }
    }
  }

  out$Q0 <- Q0
  out$alpha <- alpha
  out$Pmax <- omax_pmax$pmax_model
  out$Omax <- omax_pmax$omax_model
  # GH #19 follow-up: surface the numerical
  # zben Pmax search's expansion-cap flag (see .tmb_compute_subject_pars()
  # for the fit-time/wide-shape counterpart of this column).
  out$pmax_at_bound <- omax_pmax$is_boundary_model

  out
}


# --- plot ---

#' Plot TMB Mixed-Effects Demand Model
#'
#' @param x A \code{beezdemand_tmb} object.
#' @param type Character. One of `"demand"` (population curve with data),
#'   `"individual"` (per-subject curves), `"parameters"` (parameter
#'   distributions).
#' @param ids Character vector of subject IDs to plot (for individual type).
#' @param prices Optional numeric vector of prices for curve generation.
#' @param show_population Logical. Show population curve overlay.
#' @param show_observed Logical. Show observed data points.
#' @param show_pred Character. Which predictions to show: `"population"`,
#'   `"individual"`, or `"both"`. If `NULL` (default), determined by `type`.
#' @param x_trans Character. X-axis transformation.
#' @param y_trans Character. Y-axis transformation. If `NULL` (default),
#'   uses `"pseudo_log"`, which is defined at zero.
#' @param inv_fun Optional function to back-transform y-axis. For `zben` and
#'   `exponential` equations, the inverse link is applied automatically by
#'   default so all demand plots are on the consumption scale.
#' @param x_limits,y_limits Numeric length-2 vectors for axis limits.
#' @param x_lab Character. X-axis label.
#' @param y_lab Character. Y-axis label.
#' @param style Character. Plot style: "modern" or "apa".
#' @param observed_point_alpha,observed_point_size Numeric. Aesthetics for
#'   observed data points.
#' @param pop_line_alpha,pop_line_size Numeric. Aesthetics for population curve.
#' @param ind_line_alpha,ind_line_size Numeric. Aesthetics for individual curves.
#' @param ... Additional arguments.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#'
#' # Population demand curve
#' plot(fit, type = "demand")
#'
#' # Individual curves for selected subjects
#' plot(fit, type = "individual", ids = c("19", "51"))
#'
#' # Parameter distributions
#' plot(fit, type = "parameters")
#' }
#'
#' @export
plot.beezdemand_tmb <- function(
  x,
  type = c("demand", "individual", "parameters"),
  ids = NULL,
  prices = NULL,
  show_population = TRUE,
  show_observed = TRUE,
  show_pred = NULL,
  x_trans = c("log10", "log", "linear", "pseudo_log"),
  y_trans = NULL,
  inv_fun = NULL,
  x_limits = NULL,
  y_limits = NULL,
  x_lab = NULL,
  y_lab = NULL,
  style = c("modern", "apa"),
  observed_point_alpha = 0.3,
  observed_point_size = 1.5,
  pop_line_alpha = 1.0,
  pop_line_size = 1.2,
  ind_line_alpha = 0.3,
  ind_line_size = 0.5,
  ...
) {
  type <- match.arg(type)
  x_trans <- match.arg(x_trans)
  y_trans_missing <- is.null(y_trans)
  if (y_trans_missing) {
    y_trans <- "pseudo_log"
  }
  y_trans <- match.arg(y_trans, c("log10", "log", "linear", "pseudo_log"))
  style <- match.arg(style)

  equation <- x$param_info$equation
  x_var <- x$param_info$x_var
  id_var <- x$param_info$id_var

  # Auto-apply inverse link for equations that predict on a transformed scale,
  # so demand plots always show the consumption scale by default
  if (is.null(inv_fun) && equation %in% c("zben", "exponential")) {
    # For axis-scale back-transformation (plotting), use correction = FALSE
    # to show the median curve; retransformation correction applies to E[Y]
    inv_fun <- function(y) .tmb_backtransform(y, equation, correction = FALSE)
    attr(inv_fun, "auto") <- TRUE
  }

  x_lab <- x_lab %||% "Price"
  y_lab <- y_lab %||% "Consumption"

  # Price sequence
  if (is.null(prices)) {
    max_price <- max(x$data[[x_var]], na.rm = TRUE)
    prices <- seq(0, max_price, length.out = 200)
  }

  if (type == "parameters") {
    par_trans <- list(...)$par_trans
    return(.tmb_plot_parameters(x, style = style, par_trans = par_trans))
  }

  # Resolve show_pred
  if (!is.null(show_pred)) {
    show_pred <- match.arg(show_pred, c("population", "individual", "both"),
                           several.ok = TRUE)
    if ("both" %in% show_pred) show_pred <- c("population", "individual")
  }

  # Population prediction
  pop_pred <- predict(x, type = "demand", prices = prices)

  if (!is.null(inv_fun)) {
    pop_pred$.fitted <- inv_fun(pop_pred$.fitted)
  }

  p <- ggplot2::ggplot()

  # Observed data overlay (shown for both demand and individual types)
  if (show_observed) {
    obs_data <- x$data
    y_obs <- obs_data[[x$param_info$y_var]]
    # Only back-transform observed data when it is stored on the model scale.
    # For "exponential", data is already natural-scale (zeros dropped by fit);
    # for "zben", data is on the LL4-transformed scale and needs inv_fun.
    # User-supplied inv_fun always applies (they know what they're doing).
    obs_needs_transform <- !is.null(inv_fun) &&
      (equation %in% c("zben") || !isTRUE(attr(inv_fun, "auto")))
    if (obs_needs_transform) y_obs <- inv_fun(y_obs)
    obs_df <- data.frame(
      price = obs_data[[x_var]],
      consumption = y_obs,
      id = obs_data[[id_var]]
    )
    p <- p + ggplot2::geom_point(
      data = obs_df,
      ggplot2::aes(x = .data$price, y = .data$consumption,
                   group = .data$id),
      alpha = observed_point_alpha, size = observed_point_size
    )
  }

  if (type == "individual") {
    # Subject-specific curves
    spars <- x$subject_pars
    coefs <- x$model$coefficients
    has_k <- x$param_info$has_k

    # Filter by `ids` first (if supplied) so users can plot a subset of
    # subjects whose Q0/alpha are well-defined even when other subjects
    # have NA in the wide subject_pars. The Phase 5A guard below fires
    # only on the post-filter rows.
    if (!is.null(ids)) {
      spars <- spars[spars$id %in% ids, , drop = FALSE]
    }

    # Phase 5A guard: when within-subject random-effects design columns vary
    # within id (e.g. M1-style multi-block fits with `condition` slopes),
    # subject-level Q0/alpha are NA in the default wide subject_pars because
    # no single subject value is well-defined. Per-(subject, condition) plot
    # support is deferred; abort with a targeted message until it lands.
    if (any(is.na(spars$Q0)) || any(is.na(spars$alpha))) {
      cli::cli_abort(c(
        "Subject-level {.field Q0}/{.field alpha} are {.val NA} for this fit.",
        "i" = "Default {.code subject_pars} has no well-defined per-subject value when a within-subject factor varies within id (factor-expanded or multi-block RE specs).",
        "i" = "Call {.code get_subject_pars(fit, expanded = TRUE)} for per-(subject, factor-level) parameters and reduce/aggregate before plotting, or fit a different RE structure.",
        "x" = "Per-(subject, factor-level) plotting is planned for a follow-up release."
      ))
    }

    subj_curves <- do.call(rbind, lapply(seq_len(nrow(spars)), function(j) {
      Q0_j <- spars$Q0[j]
      alpha_j <- spars$alpha[j]
      log_q0_j <- log(Q0_j)
      k_val <- if (has_k) .tmb_get_k(x) else NA

      y_pred <- .tmb_predict_equation(
        prices, Q0_j, alpha_j,
        k = k_val, log_q0 = log_q0_j, equation = equation
      )
      if (!is.null(inv_fun)) y_pred <- inv_fun(y_pred)

      data.frame(
        price = prices,
        .fitted = y_pred,
        id = spars$id[j],
        stringsAsFactors = FALSE
      )
    }))

    p <- p + ggplot2::geom_line(
      data = subj_curves,
      ggplot2::aes(x = .data$price, y = .data$.fitted, group = .data$id),
      alpha = ind_line_alpha, linewidth = ind_line_size
    )
  }

  if (show_population) {
    p <- p + ggplot2::geom_line(
      data = pop_pred,
      ggplot2::aes(x = .data$price, y = .data$.fitted),
      color = beezdemand_style_color(style, "primary"),
      linewidth = pop_line_size,
      alpha = pop_line_alpha
    )
  }

  # Axis transforms
  x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
  y_limits <- beezdemand_resolve_limits(y_limits, y_trans, axis = "y")

  p <- p +
    ggplot2::scale_x_continuous(
      trans = beezdemand_get_trans(x_trans),
      limits = x_limits
    ) +
    ggplot2::scale_y_continuous(
      trans = beezdemand_get_trans(y_trans),
      limits = y_limits
    )

  p <- p +
    ggplot2::labs(x = x_lab, y = y_lab) +
    theme_beezdemand(style = style)

  p
}


#' @keywords internal
.tmb_plot_parameters <- function(x, style = "modern", par_trans = NULL) {
  # Default transforms: log10 for alpha (always extremely right-skewed)
  default_trans <- list(alpha = "log10")
  if (!is.null(par_trans)) {
    default_trans[names(par_trans)] <- par_trans
  }
  par_trans <- default_trans

  spars <- x$subject_pars
  plot_data <- tidyr::pivot_longer(
    spars,
    cols = c("Q0", "alpha", "Pmax", "Omax"),
    names_to = "parameter",
    values_to = "value"
  )

  # Apply per-parameter transforms and update facet labels
  plot_data$display_param <- plot_data$parameter
  for (pname in names(par_trans)) {
    tfun <- par_trans[[pname]]
    if (is.character(tfun)) {
      tfun_name <- tfun
      tfun <- switch(tfun,
        log10 = log10,
        log = log,
        sqrt = sqrt,
        identity = identity,
        stop("Unknown transform: ", tfun, call. = FALSE)
      )
    } else {
      tfun_name <- "f"
    }
    idx <- plot_data$parameter == pname
    if (any(idx)) {
      vals <- plot_data$value[idx]
      # Filter to valid values for the transform
      valid <- is.finite(vals) & vals > 0
      plot_data$value[idx & !valid] <- NA
      plot_data$value[idx & valid] <- tfun(vals[valid])
      if (!identical(tfun_name, "identity")) {
        plot_data$display_param[idx] <- paste0(tfun_name, "(", pname, ")")
      }
    }
  }

  plot_data <- plot_data[is.finite(plot_data$value), ]

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_histogram(
      bins = 20,
      fill = beezdemand_style_color(style, "accent"),
      color = "white",
      alpha = 0.7
    ) +
    ggplot2::facet_wrap(~ display_param, scales = "free") +
    ggplot2::labs(
      title = "Distribution of Subject-Specific Parameters",
      x = "Value",
      y = "Count"
    ) +
    theme_beezdemand(style = style)

  p
}


# --- tidy / glance / augment ---

#' Tidy a beezdemand_tmb Model
#'
#' @param x A \code{beezdemand_tmb} object.
#' @param effects Character. Which effects to return: `"fixed"` for the
#'   fixed-effect (core demand parameter) rows, `"ran_pars"` for the
#'   random-effect variance components, or both (the default). Matches the
#'   `effects` argument of [tidy.beezdemand_nlme()].
#' @param report_space Character. Reporting space for the fixed-effect
#'   (core demand parameter) rows. One of `"natural"`, `"log10"`, or
#'   `"internal"`. Variance-component rows are unaffected (see Details).
#'   `estimate`/`std.error` follow this scale; `statistic`/`p.value` are always
#'   on the estimation scale (transformation-invariant).
#' @param ... Additional arguments.
#'
#' @return A tibble of model terms with columns `term`, `estimate`,
#'   `std.error`, `statistic`, `p.value`, `component`, `estimate_scale`,
#'   and `term_display`. An `estimate_internal` column (the pre-transform
#'   estimate) is additionally present whenever `effects` includes
#'   `"fixed"`. Fixed-effect rows carry `component == "fixed"` (matching
#'   [tidy.beezdemand_nlme()] and the nlme/lme4 convention);
#'   variance-component rows carry `component == "variance"`. A
#'   `hessian_warning` attribute (character scalar, or absent) is attached
#'   depending on `x$hessian_pd`: absent (no attribute) when `hessian_pd`
#'   is `TRUE` or `NULL` (the field is missing on a legacy fit object, so
#'   there is nothing to report); a message noting the Hessian is not positive definite
#'   when `hessian_pd` is `FALSE`; a message noting
#'   positive-definiteness is unknown (because `TMB::sdreport()` failed
#'   entirely, so SEs/CIs are unavailable, not merely unreliable) when
#'   `hessian_pd` is `NA`. This attribute is not printed by an ordinary
#'   tibble print (see [summary.beezdemand_tmb()] or
#'   [check_demand_model()] for the surfaced versions of the same
#'   diagnostic).
#'
#' @details
#' Variance-component rows (`effects = "ran_pars"`) are exactly the rows of
#' `summary(x)$variance_components`: the Q0 and alpha random-effect standard
#' deviations on the **log10 scale** and the residual standard deviation on
#' the model's likelihood scale. They are not the raw internal `logsigma`
#' optimizer coefficients and do not respond to `report_space`; `std.error`
#' is `NA` for them. Random-effect *correlations* are not tidied here (see
#' `summary(x)$correlations` or `VarCorr(x)` for those). The NLME sibling
#' [tidy.beezdemand_nlme()] likewise reports SDs, so backend-agnostic code can
#' consume the `estimate` column without dispatch logic on either side.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' tidy(fit)
#' tidy(fit, effects = "fixed", report_space = "log10")
#' tidy(fit, effects = "ran_pars")
#' }
#'
#' @export
tidy.beezdemand_tmb <- function(
  x,
  effects = c("fixed", "ran_pars"),
  report_space = c("natural", "log10", "internal"),
  ...
) {
  effects <- match.arg(effects, several.ok = TRUE)
  report_space <- match.arg(report_space)

  result <- tibble::tibble()

  if ("fixed" %in% effects) {
    coefs <- x$model$coefficients
    se <- x$model$se
    nms <- names(coefs)

    tn <- .tmb_build_term_names(x, nms)
    term <- tn$term
    q0_idx <- tn$q0_idx
    alpha_idx <- tn$alpha_idx

    # Fixed-effect rows carry the canonical "fixed" label (matching
    # tidy.beezdemand_nlme() and the nlme/lme4 convention). The variance
    # regex flags optimizer variance parameters so they are excluded here;
    # the user-facing variance rows are built from the TICKET-015 formatter
    # below, not from the raw `logsigma` / `rho_raw` coefficients.
    component <- character(length(nms))
    component[q0_idx] <- "fixed"
    component[alpha_idx] <- "fixed"
    component[nms == "log_k"] <- "fixed"
    component[grepl("^logsigma($|_)|^rho_", nms)] <- "variance"

    estimate_scale <- rep("log", length(nms))
    estimate_scale[grepl("^logsigma($|_)|^rho_", nms)] <- "natural"

    z_val <- coefs / se
    p_val <- 2 * stats::pnorm(-abs(z_val))

    fixed <- tibble::tibble(
      term = term,
      estimate = unname(coefs),
      std.error = unname(se),
      statistic = unname(z_val),
      p.value = unname(p_val),
      component = component,
      estimate_scale = estimate_scale,
      term_display = term
    )
    fixed <- fixed[fixed$component == "fixed", , drop = FALSE]

    fixed <- beezdemand_transform_coef_table(
      coef_tbl = fixed,
      report_space = report_space,
      internal_space = "natural"
    )

    # Keep the estimation-scale `statistic`/`p.value` (broom convention); only
    # `estimate`/`std.error` are back-transformed. (No degenerate natural-scale
    # Wald recompute -- see test-report-space-test-invariance.R.)

    result <- dplyr::bind_rows(result, fixed)
  }

  if ("ran_pars" %in% effects) {
    # Variance components on the TICKET-015 reporting convention: Q0/alpha
    # RE SDs on the log10 scale, residual SD on the likelihood scale --
    # exactly the rows in summary(x)$variance_components, not the raw
    # `logsigma` optimizer coefficients. RE correlations are intentionally
    # excluded: they live in summary(x)$correlations and VarCorr(), and
    # omitting them keeps the row structure aligned with
    # tidy.beezdemand_nlme(), which likewise reports no correlation rows.
    vc <- .tmb_format_variance_components(x)
    sd_tbl <- vc$table
    is_resid <- grepl("Residual", sd_tbl$Component)

    ran <- tibble::tibble(
      term = sd_tbl$Component,
      estimate = sd_tbl$Estimate,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      component = "variance",
      estimate_scale = ifelse(is_resid, "natural", "log10"),
      term_display = sd_tbl$Component
    )

    result <- dplyr::bind_rows(result, ran)
  }

  # TICKET-067: `hessian_pd` may be NULL
  # on a fit predating this field (an older saved object) or on a
  # deliberately-stripped object; `is.na(NULL)` is length-0, so calling
  # `if()` on it directly errors ("argument is of length zero"). Read it
  # into a local first and gate on `length(hp) == 1L` before `is.na()`.
  hp <- x$hessian_pd
  if (isFALSE(hp)) {
    attr(result, "hessian_warning") <- paste0(
      "Hessian is not positive definite (pdHess = FALSE). ",
      "Standard errors, p-values, and confidence intervals may be unreliable."
    )
  } else if (length(hp) == 1L && is.na(hp)) {
    # TICKET-067 (E4): hessian_pd = NA means "unknowable" -- TMB::sdreport()
    # failed entirely, not that it succeeded and reported a non-PD Hessian.
    # summary()'s se_available note already distinguishes this case; tidy()
    # previously attached nothing.
    attr(result, "hessian_warning") <- paste0(
      "Hessian positive-definiteness is unknown (TMB::sdreport() failed). ",
      "Standard errors, p-values, and confidence intervals are unavailable."
    )
  }
  # hp NULL (field absent on a legacy object): no attribute, no warning.

  result
}


#' Glance at a beezdemand_tmb Model
#'
#' @param x A \code{beezdemand_tmb} object.
#' @param ... Additional arguments.
#'
#' @return A one-row tibble of model-level statistics with columns:
#'   - `model_class`: `"beezdemand_tmb"`
#'   - `backend`: `"TMB_mixed"`
#'   - `equation_form`: The demand equation that was fitted
#'   - `nobs`: Number of observations
#'   - `n_subjects`: Number of subjects
#'   - `n_random_effects`: Total number of random-effect columns per subject
#'   - `converged`: Convergence status
#'   - `logLik`, `AIC`, `BIC`: Model fit statistics
#'
#'   The canonical columns match [glance.beezdemand_nlme()], so
#'   backend-agnostic code needs no dispatch glue.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' glance(fit)
#' }
#'
#' @export
glance.beezdemand_tmb <- function(x, ...) {
  tibble::tibble(
    model_class = "beezdemand_tmb",
    backend = "TMB_mixed",
    equation_form = x$param_info$equation,
    nobs = x$param_info$n_obs,
    n_subjects = x$param_info$n_subjects,
    n_random_effects = x$param_info$n_random_effects,
    converged = x$converged,
    logLik = x$loglik,
    AIC = x$AIC,
    BIC = x$BIC
  )
}


# Global (row_j, col_k) matrix positions for each off-diagonal RE correlation of
# a TMB pdBlocked structure, generated in the SAME block/j/k order that
# .tmb_format_variance_components() emits summary()$correlations. Each pdSymm
# block contributes its d*(d-1)/2 off-diagonals with the block's global row
# offset (the cumulative RE dimension of all earlier blocks) added, so a
# correlation between global REs j > k lands on row j, correlation-column k --
# nlme's VarCorr layout. Used by VarCorr.beezdemand_tmb(); fixes release-audit
# C3, where local block indices were used as global positions (correct only when
# the correlated block happened to be first).
.tmb_varcorr_corr_positions <- function(bmap) {
  pos <- list()
  if (is.null(bmap) || isTRUE(bmap$n_blocks < 1L)) return(pos)
  offset <- 0L
  for (b in seq_len(bmap$n_blocks)) {
    d <- bmap$block_q0_dim[b] + bmap$block_alpha_dim[b]
    if (is.na(d) || d == 0L) next
    if (isTRUE(bmap$block_types[b] == 1L) && d > 1L) {
      for (j in 2L:d) {
        for (k in seq_len(j - 1L)) {
          pos[[length(pos) + 1L]] <- c(offset + j, offset + k)
        }
      }
    }
    offset <- offset + d
  }
  pos
}


#' Random-Effect Variance Components for a TMB Demand Model
#'
#' Extracts the random-effect variance components from a \code{beezdemand_tmb}
#' fit in the matrix layout produced by \code{nlme::VarCorr()}, so users
#' familiar with \pkg{nlme} or \pkg{lme4} can introspect a TMB fit with the
#' accessor they already know. The reported values are the same ones returned
#' by \code{\link{summary.beezdemand_tmb}}: the Q0 and alpha random-effect
#' standard deviations on the \strong{log10 scale} and the residual standard
#' deviation on the model's likelihood scale. This is a presentation shim that
#' formats already-computed values and recomputes nothing.
#'
#' @param x A \code{beezdemand_tmb} object.
#' @param sigma Present for signature compatibility with
#'   \code{nlme::VarCorr()}. The TMB summary reports variance components as
#'   absolute standard deviations, so there is no residual scale factor to
#'   apply; any value other than the default (\code{1}) is an error.
#' @param rdig Integer. Number of significant digits used when formatting the
#'   displayed values. Default \code{3}.
#' @param ... Unused; present for generic compatibility.
#'
#' @return A character matrix of class \code{"VarCorr.lme"} with one row per
#'   random-effect term plus a final \code{"Residual"} row, columns
#'   \code{"Variance"} and \code{"StdDev"}, and, for fits with correlated
#'   random effects (\code{pdSymm}), a \code{"Corr"} column. \code{print()}
#'   dispatches to \code{nlme}'s \code{print.VarCorr.lme()}.
#'
#' @note The \code{Corr} column is placed using \code{nlme}'s convention,
#'   with each correlation on the row of its higher-indexed random effect. For
#'   multi-block \code{pdBlocked} fits the correlations are placed on the
#'   correct global rows (each correlated block's off-diagonals are offset by
#'   the cumulative random-effect dimension of the earlier blocks);
#'   \code{summary(x)$correlations} remains available for the labelled values.
#'
#' @seealso \code{\link[nlme]{VarCorr}}, \code{\link{summary.beezdemand_tmb}}
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' VarCorr(fit)
#' }
#'
#' @importFrom lme4 VarCorr
#' @export
VarCorr.beezdemand_tmb <- function(x, sigma = 1, rdig = 3, ...) {
  if (!isTRUE(all.equal(sigma, 1))) {
    cli::cli_abort(c(
      "{.arg sigma} is not supported for {.cls beezdemand_tmb} fits.",
      i = "Unlike {.fn nlme::VarCorr}, the TMB summary reports variance components as absolute standard deviations (Q0/alpha on the log10 scale, residual on the likelihood scale), so there is no residual scale factor to apply.",
      i = "Call {.code VarCorr(x)} without {.arg sigma}."
    ))
  }
  s <- summary(x)
  vc <- s$variance_components
  corr <- s$correlations

  sd_vals <- vc$Estimate
  n <- length(sd_vals)

  # nlme-style row names: the parameter token inside the component label
  # ("sigma_b (Q0 RE SD)" -> "Q0", "sigma_e (Residual SD)" -> "Residual"),
  # de-duplicated for factor-expanded fits with repeated terms.
  param <- sub("^.*\\(([A-Za-z0-9]+)\\b.*$", "\\1", vc$Component)
  # TICKET-051: when a component is a named continuous RE slope
  # ("sigma_b[dose_c] (Q0 RE SD)"), suffix the row name with the term so the
  # slope is identifiable. Positional ("sigma_b[1]") and bracket-free
  # intercept-only labels keep the historical de-duplicated names.
  bracket <- sub("^sigma_[bc]\\[(.*)\\] .*$", "\\1", vc$Component)
  bracket_term <- sub("^block[0-9]+ ", "", bracket)
  named <- grepl("^sigma_[bc]\\[", vc$Component) &
           bracket != vc$Component &
           is.na(suppressWarnings(as.integer(bracket_term)))
  rn <- ifelse(named, paste0(param, ".", bracket_term), param)
  rn <- make.unique(rn, sep = ".")

  variance <- sd_vals^2

  # Correlation layout. Each summary()$correlations row is one off-diagonal of a
  # pdSymm block, placed at the GLOBAL row of its higher-indexed random effect.
  # Positions are derived from the same block map summary() used to generate the
  # correlations (identical block/j/k order), so multi-block pdBlocked fits add
  # each block's global row offset rather than treating local block indices as
  # global (release-audit C3).
  jk <- list()
  if (!is.null(corr) && nrow(corr) > 0L) {
    re_parsed <- x$param_info$random_effects_parsed
    bmap <- if (!is.null(re_parsed)) .tmb_build_block_map(re_parsed) else NULL
    jk <- .tmb_varcorr_corr_positions(bmap)

    # Fallback (block map unavailable, or its off-diagonal count disagrees with
    # the reported correlations): parse the trailing indices from the component
    # labels. Correct for single-block fits; preserves pre-C3 behavior.
    if (length(jk) != nrow(corr)) {
      jk <- lapply(corr$Component, function(comp) {
        if (grepl("^rho_bc", comp)) return(c(2L, 1L))
        nums <- as.integer(regmatches(comp, gregexpr("[0-9]+", comp))[[1]])
        if (length(nums) >= 2L) {
          c(nums[length(nums) - 1L], nums[length(nums)])
        } else {
          c(NA_integer_, NA_integer_)
        }
      })
    }
  }
  ok <- vapply(jk, function(v) all(is.finite(v)), logical(1))
  max_k <- if (any(ok)) max(vapply(jk[ok], function(v) v[2L], integer(1))) else 0L

  col_nm <- c("Variance", "StdDev",
              if (max_k > 0L) c("Corr", rep("", max_k - 1L)))
  out <- matrix("", nrow = n, ncol = length(col_nm),
                dimnames = list(rn, col_nm))
  out[, 1L] <- format(signif(variance, rdig))
  out[, 2L] <- format(signif(sd_vals, rdig))

  for (i in which(ok)) {
    re_row <- jk[[i]][1L]
    corr_col <- 2L + jk[[i]][2L]
    if (re_row >= 1L && re_row <= n && corr_col <= length(col_nm)) {
      out[re_row, corr_col] <- format(signif(corr$Estimate[i], rdig))
    }
  }

  class(out) <- "VarCorr.lme"
  out
}


# Internal: compute fitted values and (response) residuals on a requested
# scale and random-effect level. Centralizes the scale/level conventions
# shared by fitted(), residuals(), and augment() so the three accessors
# cannot drift apart.
.tmb_fitted_resid <- function(x,
                              scale = c("model", "natural"),
                              level = c("subject", "population"),
                              newdata = NULL) {
  scale <- match.arg(scale)
  level <- match.arg(level)
  data_used <- if (is.null(newdata)) x$data else newdata
  pred <- predict(x, newdata = data_used, type = "response",
                  level = level, scale = scale)
  # predict.beezdemand_tmb() names the fitted column `.fitted` for the
  # single subject path and `predict.fixed` for the population path.
  fitted_vals <- if (level == "population") {
    pred$predict.fixed
  } else {
    pred$.fitted
  }
  equation <- x$param_info$equation
  y_var <- x$param_info$y_var
  y_obs <- data_used[[y_var]]
  if (scale == "model" && equation == "exponential") {
    # Model is on log scale; y_obs is natural. Zero rows are NA on log scale.
    y_on_scale <- ifelse(y_obs > 0, log(y_obs), NA_real_)
  } else if (scale == "natural" && equation == "zben") {
    # y_var for zben is LL4-transformed at fit time (the caller supplies
    # ll4(y, lambda = 4)); back-transform it here so natural-scale residuals
    # subtract two genuinely natural-scale quantities. Without this, y_obs
    # stays on the LL4 scale while fitted_vals has already been
    # back-transformed by predict(..., scale = "natural"), producing a
    # scale-mixed residual (GH #18).
    y_on_scale <- ll4_inv(y_obs)
  } else {
    # exponentiated/simplified on model scale (already natural), zben on
    # model scale (LL4, matches y_obs as-is), OR any equation on the natural
    # scale that needs no back-transform of y_obs.
    y_on_scale <- y_obs
  }
  list(.fitted = fitted_vals, .resid = y_on_scale - fitted_vals)
}


#' Augment a beezdemand_tmb Model
#'
#' @param x A \code{beezdemand_tmb} object.
#' @param newdata Optional data frame.
#' @param ... Additional arguments.
#'
#' @return A tibble with original data plus `.fitted`, `.resid`, and
#'   `.std_resid` columns. Residuals are computed on the model's native scale
#'   (log scale for `"exponential"`, natural/LL4 scale for others) to match the
#'   C++ likelihood.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' head(augment(fit))
#' }
#'
#' @export
augment.beezdemand_tmb <- function(x, newdata = NULL, ...) {
  fr <- .tmb_fitted_resid(x, scale = "model", level = "subject", newdata = newdata)
  data_used <- if (is.null(newdata)) x$data else newdata
  out <- tibble::as_tibble(data_used)
  out$.fitted <- fr$.fitted
  out$.resid <- fr$.resid
  # Standardized Pearson residuals on model scale: (y - mu) / sigma_e
  sigma_e <- exp(x$model$coefficients[["logsigma_e"]])
  out$.std_resid <- out$.resid / sigma_e
  out
}


# --- vcov / fitted / residuals (TICKET-026) ---

#' Warn once when an inference surface consumes a non-PD-Hessian covariance
#'
#' `sdr$cov.fixed` for a fit with `hessian_pd == FALSE` is a pseudo-inverse of
#' an indefinite Hessian -- SEs/CIs/p-values/draws computed from it are
#' unreliable even though the point estimates (and `converged`) are fine.
#' Shared by both TMB-backed classes (`beezdemand_tmb`, `beezdemand_hurdle`);
#' called once per user-facing entry point that reads `cov.fixed` (TICKET-063).
#' `isFALSE()` treats `NA`/`NULL` (unknown / old objects) as "no warning".
#' @param object A `beezdemand_tmb` or `beezdemand_hurdle` fit.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
.tmb_warn_if_hessian_not_pd <- function(object) {
  if (isFALSE(object$hessian_pd)) {
    cli::cli_warn(
      c(
        "!" = "Hessian is not positive definite; standard errors, intervals,
               and draws are unreliable.",
        "i" = "See {.fn summary} / {.fn check_demand_model} for diagnostics."
      ),
      class = c("beezdemand_hessian_not_pd_warning", "beezdemand_warning")
    )
  }
  invisible(NULL)
}

#' Variance-covariance matrix for a beezdemand_tmb fit
#'
#' Returns the fixed-effect VCOV from the TMB sdreport, i.e., the inverse of
#' the negative Hessian at the MLE after Laplace-marginalizing the random
#' effects. Row/column names follow the optimizer's internal parameterization
#' (matching `names(coef(object, type = "internal"))`).
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param ... Unused.
#' @return Numeric symmetric matrix of dimension p x p.
#' @seealso [coef.beezdemand_tmb()], [confint.beezdemand_tmb()].
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' V <- vcov(fit)
#' isSymmetric(V)
#' }
#' @export
vcov.beezdemand_tmb <- function(object, ...) {
  .tmb_warn_if_hessian_not_pd(object)
  sdr <- object$sdr
  if (is.null(sdr) || is.null(sdr$cov.fixed)) {
    cli::cli_abort(
      "fit did not converge; cov.fixed unavailable. See {.code glance(fit)$converged}."
    )
  }
  V <- as.matrix(sdr$cov.fixed)
  if (is.null(rownames(V)) || all(rownames(V) == "")) {
    nm <- names(object$opt$par)
    rownames(V) <- colnames(V) <- nm
  }
  V
}


#' Fitted values for a beezdemand_tmb fit
#'
#' Default returns fitted values on the model's native likelihood scale
#' (log scale for `"exponential"`, natural/LL4 scale for others), matching
#' `augment(fit)$.fitted`. Set `scale = "natural"` to back-transform.
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param scale One of `"model"` (default) or `"natural"`.
#' @param level One of `"subject"` (default; conditions on the subject
#'   random effects) or `"population"` (random effects set to zero, giving
#'   the population-mean values). See [predict.beezdemand_tmb()].
#' @param ... Unused.
#' @return Numeric vector of length `nobs(object)`.
#' @seealso [predict.beezdemand_tmb()], [augment.beezdemand_tmb()],
#'   [residuals.beezdemand_tmb()].
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' head(fitted(fit))
#' }
#' @export
fitted.beezdemand_tmb <- function(object,
                                  scale = c("model", "natural"),
                                  level = c("subject", "population"),
                                  ...) {
  scale <- match.arg(scale)
  level <- match.arg(level)
  fr <- .tmb_fitted_resid(object, scale = scale, level = level)
  fr$.fitted
}


#' Residuals for a beezdemand_tmb fit
#'
#' Default returns response residuals (`y_on_scale - fitted`) on the model's
#' native scale. `type = "pearson"` divides by the residual SD on the model
#' scale (`exp(coef[["logsigma_e"]])`). Requesting `type = "pearson"` with
#' `scale = "natural"` falls back to `type = "response"` with a message
#' because a response-scale residual SD is not identified for the
#' exponential/zben variants without a separate variance assumption.
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param type One of `"response"` (default) or `"pearson"`.
#' @param scale One of `"model"` (default) or `"natural"`.
#' @param level One of `"subject"` (default; conditions on the subject
#'   random effects) or `"population"` (random effects set to zero, giving
#'   the population-mean values). See [predict.beezdemand_tmb()].
#' @param ... Unused.
#' @return Numeric vector of length `nobs(object)`.
#' @seealso [fitted.beezdemand_tmb()], [augment.beezdemand_tmb()].
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' head(residuals(fit))
#' head(residuals(fit, type = "pearson"))
#' }
#' @export
residuals.beezdemand_tmb <- function(object,
                                     type = c("response", "pearson"),
                                     scale = c("model", "natural"),
                                     level = c("subject", "population"),
                                     ...) {
  type <- match.arg(type)
  scale <- match.arg(scale)
  level <- match.arg(level)
  if (type == "pearson" && scale == "natural") {
    cli::cli_inform(
      "Pearson residuals on the natural scale are not identified for this model; returning response residuals on the natural scale."
    )
    type <- "response"
  }
  fr <- .tmb_fitted_resid(object, scale = scale, level = level)
  if (type == "response") {
    return(fr$.resid)
  }
  sigma_e <- tryCatch(
    exp(object$model$coefficients[["logsigma_e"]]),
    error = function(e) NA_real_
  )
  if (!is.finite(sigma_e)) {
    cli::cli_inform("sigma_e not finite; returning response residuals.")
    return(fr$.resid)
  }
  fr$.resid / sigma_e
}


# --- anova ---

#' Joint Wald and likelihood-ratio tests for a TMB demand fit
#'
#' For a single fit, computes joint Wald-chi-square tests on grouped
#' fixed-effect coefficients. For multiple fits passed via `...`, performs
#' sequential likelihood-ratio tests on nested models.
#'
#' @param object A \code{beezdemand_tmb} fit.
#' @param ... Additional \code{beezdemand_tmb} fits for nested comparison.
#' @param test One of `"Wald"`, `"LRT"`, `"AIC"`. Default: `"Wald"` for a
#'   single fit, `"LRT"` when extra fits are supplied.
#' @param terms `NULL` (all fixed effects), a character vector of term names,
#'   or a named list mapping group labels to term-name vectors. Term names
#'   match display names (`Q0:genderMale`) or raw names.
#' @param group_by One of `"auto"` (group non-intercept terms by
#'   parameter x factor/covariate), `"parameter"` (one group per Q0 / alpha),
#'   or `"term"` (one row per coefficient).
#'
#' @return For a single fit, a tibble with `Group`, `Chisq`, `df`, `p.value`.
#'   For multiple fits, a tibble with `Model`, `df`, `AIC`, `Chisq`,
#'   `` `Pr(>Chisq)` ``.
#'
#' @details The Wald statistic for a coefficient block is
#'   \eqn{W = \beta_g' \Sigma_{gg}^{-1} \beta_g}, asymptotically
#'   \eqn{\chi^2} on \code{length(beta_g)} df. An exactly rank-deficient
#'   (perfectly collinear) block has a singular \eqn{\Sigma_{gg}} and triggers
#'   an explicit error. A near-collinear block is not detected:
#'   \eqn{\Sigma_{gg}} stays invertible and \eqn{W} becomes large and unstable,
#'   so such a value should be interpreted with caution. For multiple fits,
#'   the likelihood-ratio test screens for detectable non-nesting (equal or
#'   decreasing degrees of freedom, or a larger model with lower
#'   log-likelihood) but cannot prove nesting from log-likelihood and df
#'   alone. Pass genuinely nested models. Rows of the multiple-fit table
#'   are ordered by ascending degrees of freedom, and the \code{Model}
#'   column labels them \code{Model1}, \code{Model2}, ... in that order.
#'
#' @seealso [anova.beezdemand_nlme()], [confint.beezdemand_tmb()].
#' @examples
#' \donttest{
#' data(apt_full)
#' # 40 subjects per gender keep the example fast; use the full data in practice
#' ids <- unique(apt_full[c("id", "gender")])
#' ids <- ids[ids$gender %in% c("Male", "Female"), ]
#' keep <- unlist(lapply(split(ids$id, ids$gender), head, 40))
#' dat <- apt_full[apt_full$id %in% keep, ]
#' fit <- fit_demand_tmb(dat, equation = "exponential",
#'                       factors = "gender", verbose = 0)
#' anova(fit)
#' anova(fit, group_by = "parameter")
#' }
#' @importFrom stats anova pchisq
#' @export
anova.beezdemand_tmb <- function(object, ...,
                                 test = c("Wald", "LRT", "AIC"),
                                 terms = NULL,
                                 group_by = c("auto", "parameter", "term")) {
  extra <- list(...)
  if (missing(test)) {                                  # C3
    test <- if (length(extra) > 0L) "LRT" else "Wald"
  }
  test <- match.arg(test)
  group_by <- match.arg(group_by)

  if (length(extra) > 0L) {
    return(.tmb_anova_multifit(object, extra, test = test))
  }

  V <- vcov(object)                # vcov() errors if the sdreport variance is unavailable
  coefs <- object$model$coefficients
  groups <- .tmb_group_terms(object, terms = terms, group_by = group_by)

  rows <- lapply(groups, function(g) {
    b <- unname(coefs[g$idx])
    S <- V[g$idx, g$idx, drop = FALSE]               # C4: beta blocks only
    Sinv_b <- tryCatch(solve(S, b), error = function(e) {
      cli::cli_abort(c(
        "Variance submatrix for group {.val {g$label}} is singular.",
        i = "Collinear or unidentified terms in this block."
      ))
    })
    W  <- as.numeric(crossprod(b, Sinv_b))
    df <- length(g$idx)
    tibble::tibble(
      Group   = g$label,
      Chisq   = W,
      df      = as.integer(df),
      p.value = stats::pchisq(W, df = df, lower.tail = FALSE)
    )
  })
  dplyr::bind_rows(rows)
}


# --- confint ---

#' Confidence Intervals for TMB Model Parameters
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param parm Character vector of parameter names.
#' @param level Confidence level (default 0.95).
#' @param report_space Character. `"internal"` or `"natural"`. When
#'   `"natural"`, `beta_q0`, `beta_alpha`, and `log_k` are exponentiated
#'   to the natural scale. For the intercept, this gives Q0 or alpha at the
#'   reference level. For non-intercept terms, the exponentiated value
#'   represents a **multiplicative fold-change** (ratio) relative to the
#'   reference level, not the absolute parameter value for that group.
#'   Variance parameters (`logsigma_*`, `rho_bc_raw`)
#'   remain on their internal scales; use [summary()] or
#'   `.tmb_format_variance_components()` for transformed variance components.
#' @param method Character. `"wald"` (default) returns Hessian-based
#'   Wald intervals (`coef +/- z * se`). `"simulate"` draws `R` parametric
#'   Monte Carlo samples from the joint asymptotic Gaussian posterior
#'   \eqn{N(\hat\beta, \hat\Sigma)} (with \eqn{\hat\Sigma = }`vcov(object)`)
#'   and reports per-coefficient empirical quantiles.
#' @param R Integer. Number of Monte Carlo draws for `method = "simulate"`.
#'   Must be `>= 100`; `>= 1000` is recommended for stable quantiles.
#'   Ignored for `method = "wald"`.
#' @param seed Optional integer seed for `method = "simulate"`
#'   reproducibility. When supplied, the caller's RNG state is restored on
#'   exit so the global stream is left unperturbed.
#' @param ... Additional arguments.
#'
#' @return A tibble with term, estimate, conf.low, conf.high, level.
#'
#' @details `method = "simulate"` is Monte Carlo simulation from the
#'   asymptotic Gaussian posterior (neither a data-resampling bootstrap nor
#'   a profile-likelihood interval). Because the sampled distribution is
#'   the same Gaussian that Wald assumes, the simulated per-coefficient
#'   quantiles converge to the Wald intervals as `R -> Inf`; the method does
#'   **not** improve on Wald at boundary cases and offers no positivity
#'   guarantee on the internal scale (`logsigma_*` intervals can be
#'   negative). Its value is (a) a diagnostic side-by-side check on the
#'   Gaussian approximation, and (b) a shared draw primitive
#'   (`.tmb_parametric_draws()`) for derived-metric confidence intervals.
#'
#' @seealso [confint.beezdemand_nlme()], [vcov.beezdemand_tmb()].
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' confint(fit)
#' confint(fit, report_space = "natural")
#' # Diagnostic Monte Carlo intervals (asymptotically Wald-equivalent):
#' confint(fit, method = "simulate", R = 1000, seed = 42)
#' }
#'
#' @export
confint.beezdemand_tmb <- function(
  object,
  parm = NULL,
  level = 0.95,
  report_space = c("internal", "natural"),
  method = c("wald", "simulate"),
  R = 1000L,
  seed = NULL,
  ...
) {
  method <- match.arg(method)
  report_space <- match.arg(report_space)

  coefs <- object$model$coefficients
  se_vec <- object$model$se
  nms <- names(coefs)

  # Build display names first (before filtering) so parm can match either

  tn <- .tmb_build_term_names(object, nms)
  term <- tn$term

  keep <- rep(TRUE, length(coefs))
  if (!is.null(parm)) {
    # Match against display names first, then fall back to raw names
    keep <- term %in% parm | nms %in% parm
    coefs <- coefs[keep]
    se_vec <- se_vec[keep]
    nms <- nms[keep]
    term <- term[keep]
  }

  estimates <- coefs

  if (method == "wald") {
    # method = "simulate" routes through .tmb_parametric_draws() -> vcov(),
    # which already warns once; only the wald branch needs its own explicit
    # check (it reads model$se directly, never calling vcov()).
    .tmb_warn_if_hessian_not_pd(object)
    z <- stats::qnorm((1 + level) / 2)
    conf_low <- coefs - z * se_vec
    conf_high <- coefs + z * se_vec
  } else {
    if (!is.numeric(R) || length(R) != 1L || !is.finite(R) ||
          R < 100 || R != round(R)) {
      cli::cli_abort(c(
        "{.arg R} must be a single whole number >= 100.",
        i = "Recommend {.code R >= 1000} for stable quantile estimates."
      ))
    }
    R <- as.integer(R)
    a <- (1 - level) / 2
    if (any(keep)) {
      # `keep` masks the full coefficient vector, so it indexes the draw
      # matrix columns (which are in the same opt$par order) directly.
      draws <- .tmb_parametric_draws(object, R = R, seed = seed)[, keep, drop = FALSE]
      qs <- apply(draws, 2L, stats::quantile, probs = c(a, 1 - a), names = FALSE)
      conf_low <- qs[1, ]
      conf_high <- qs[2, ]
    } else {
      conf_low <- numeric(0)
      conf_high <- numeric(0)
    }
  }

  # Re-derive indices for the (possibly filtered) vector
  q0_idx <- which(nms == "beta_q0")
  alpha_idx <- which(nms == "beta_alpha")

  # Transform if natural
  if (report_space == "natural") {
    # beta_q0 and beta_alpha intercepts are on log scale
    log_params <- c(q0_idx, alpha_idx, which(nms == "log_k"))
    if (length(log_params) > 0) {
      estimates[log_params] <- exp(coefs[log_params])
      conf_low[log_params] <- exp(conf_low[log_params])
      conf_high[log_params] <- exp(conf_high[log_params])
    }
  }

  tibble::tibble(
    term = term,
    estimate = unname(estimates),
    conf.low = unname(conf_low),
    conf.high = unname(conf_high),
    level = level
  )
}


# --- formula / model.matrix / update (TICKET-028) ---

#' Formula for a beezdemand_tmb fit
#'
#' Returns the fixed-effect RHS formulas for Q0 and alpha plus the
#' original random-effect specification preserved at fit time. The
#' Q0 and alpha formulas may differ when `collapse_levels` was used
#' to collapse factor levels asymmetrically.
#'
#' @param x A \code{beezdemand_tmb} object.
#' @param ... Unused.
#' @return Named list `list(Q0, alpha, random)`. `Q0` and `alpha` are
#'   one-sided formulas built from `fit$formula_details`. `random` is
#'   the original `random_effects` argument value (round-trippable back
#'   to `fit_demand_tmb()`).
#' @seealso [model.matrix.beezdemand_tmb()], [update.beezdemand_tmb()].
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' formula(fit)
#' }
#' @export
formula.beezdemand_tmb <- function(x, ...) {
  fd <- x$formula_details
  # `$original` is the user's raw input preserved by `.normalize_re_input()`
  # (random-effects-utils.R:71-75) for round-tripping. Returning the parsed
  # block structure directly would NOT round-trip because fit_demand_tmb()
  # accepts formulas / pdMat / character / pdBlocked, not the parsed list.
  rand_original <- x$param_info$random_effects_parsed$original
  if (is.null(rand_original)) {
    # Older fits that predate the parser-stored `$original`; fall back to
    # the raw spec slot.
    rand_original <- x$param_info$random_effects_spec
  }
  # rhs_q0 / rhs_alpha already include the leading "~" (see
  # build_fixed_rhs() in R/utils.R), so pass them straight to as.formula().
  # Prepending another "~" produces a nested formula (`~~grp`), which R
  # tolerates but is visibly malformed.
  list(
    Q0     = stats::as.formula(fd$rhs_q0),
    alpha  = stats::as.formula(fd$rhs_alpha),
    random = rand_original
  )
}


#' Design matrices for a beezdemand_tmb fit
#'
#' By default returns a named list of all four design matrices the TMB
#' template consumed: `X_q0`, `X_alpha`, `Z_q0`, `Z_alpha`. Use `what`
#' to select a single matrix. `X_q0` and `X_alpha` are zero-copy
#' references to `fit$formula_details`; `Z_q0` and `Z_alpha` are
#' recomputed via the internal builder.
#'
#' Returning a named list (vs the single matrix `lm`/`lme4` return) is
#' intentional: the TMB tier has two fixed-effect linear predictors
#' (one per nonlinear parameter), not one.
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param what `NULL` (default) returns the full named list. Otherwise one
#'   of `"X_q0"`, `"X_alpha"`, `"Z_q0"`, `"Z_alpha"`.
#' @param ... Unused.
#' @return Named list of numeric matrices, or a single numeric matrix when
#'   `what` is set. `NULL` (with a message) when a degenerate Z is requested.
#' @seealso [formula.beezdemand_tmb()].
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' str(model.matrix(fit))
#' head(model.matrix(fit, what = "X_q0"))
#' }
#' @export
model.matrix.beezdemand_tmb <- function(object, what = NULL, ...) {
  valid <- c("X_q0", "X_alpha", "Z_q0", "Z_alpha")
  if (!is.null(what) && !what %in% valid) {
    cli::cli_abort(
      c(
        "Invalid {.arg what}: {.val {what}}.",
        "i" = "Valid choices: {.val {valid}}.",
        "i" = "TMB fits have two fixed-effect design matrices (X_q0 and X_alpha)."
      )
    )
  }
  re_parsed <- object$param_info$random_effects_parsed
  z <- .tmb_build_z_matrices(
    re_parsed, object$data,
    id_var = object$param_info$id_var
  )
  Z_q0    <- if (ncol(z$Z_q0) > 0L)    z$Z_q0    else NULL
  Z_alpha <- if (ncol(z$Z_alpha) > 0L) z$Z_alpha else NULL
  full <- list(
    X_q0    = object$formula_details$X_q0,
    X_alpha = object$formula_details$X_alpha,
    Z_q0    = Z_q0,
    Z_alpha = Z_alpha
  )
  if (is.null(what)) return(full)
  out <- full[[what]]
  if (is.null(out) && grepl("^Z_", what)) {
    param <- sub("^Z_", "", what)
    cli::cli_inform("No random effect on {.field {param}}; returning NULL.")
  }
  out
}


#' Update a beezdemand_tmb fit
#'
#' Re-fits with named arguments substituted into the original call. Pass
#' any argument of `fit_demand_tmb()` (e.g., `factors = NULL`,
#' `random_effects = ~ 1`, `equation = "simplified"`). Does NOT support
#' formula-update syntax (`. - term`) because `fit_demand_tmb()` is
#' argument-driven, not formula-driven.
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param ... Named arguments to substitute into the original
#'   `fit_demand_tmb()` call.
#' @param evaluate If `TRUE` (default), re-evaluate the updated call and
#'   return the new fit. If `FALSE`, return the unevaluated call.
#' @return A new `beezdemand_tmb` object, or an unevaluated call.
#' @seealso [fit_demand_tmb()], [formula.beezdemand_tmb()].
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' update(fit, equation = "simplified", evaluate = FALSE)
#' }
#' @export
update.beezdemand_tmb <- function(object, ..., evaluate = TRUE) {
  call <- object$call
  if (is.null(call)) {
    cli::cli_abort(
      "Original call not stored on this fit; cannot update. Refit with the current version of {.fn fit_demand_tmb}."
    )
  }
  extras <- match.call(expand.dots = FALSE)$...
  if (length(extras)) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- as.call(c(as.list(call), extras[!existing]))
    }
  }
  if (!evaluate) return(call)
  # Re-evaluate the rebuilt call in the caller's frame. The do.call("eval",
  # ...) form matches the stats::update.default idiom and keeps the
  # parent-frame resolution explicit (so e.g. user-defined factor levels in
  # the caller's environment resolve as expected).
  do.call("eval", list(call, parent.frame()))
}


# --- EMMs and comparisons ---

#' Build the conditioned reference grid for TMB EMMs and comparisons
#'
#' Shared helper that constructs `level_combos` (the factor-level grid,
#' optionally filtered by `at`) and `ref_X` (the corresponding design matrix)
#' for a TMB demand fit. Both `get_demand_param_emms.beezdemand_tmb()` and
#' `get_demand_comparisons.beezdemand_tmb()` consume this helper so they
#' cannot drift apart on which cells the user requested.
#'
#' Continuous covariates are held at the training mean unless overridden via
#' `at`. Factor levels are filtered down to the requested values when `at`
#' names a factor.
#'
#' @param fit_obj A `beezdemand_tmb` object.
#' @param param Character. `"Q0"` or `"alpha"`.
#' @param at Named list of factor-level filters or covariate-value overrides.
#' @param factors_in_emm Character subset of fitted factors to include.
#'
#' @return A list with components:
#'   \describe{
#'     \item{level_combos}{Filtered grid as a data.frame.}
#'     \item{ref_X}{Filtered design matrix.}
#'     \item{use_factors}{Character vector of factors driving the grid.}
#'     \item{cov_names}{Character vector of continuous covariates.}
#'     \item{is_intercept_only}{Logical; `TRUE` when the fit has neither
#'       factors nor covariates.}
#'   }
#'
#' @keywords internal
#' Validate the `at` list for TMB EMM/comparison/metric helpers
#'
#' Extracted from `.tmb_build_emm_ref_grid()` so each public-facing
#' function (`get_demand_param_emms`, `get_demand_comparisons`,
#' `calc_group_metrics`) can validate once at the top of the call;
#' otherwise grid-builder calls inside one public call (Q0 grid + alpha
#' grid) emit duplicate warnings on multi-value continuous `at`.
#'
#' Aborts on:
#'   - Unnamed entries.
#'   - Names not in (active factors u continuous_covariates), where
#'     "active factors" defaults to factors_q0 u factors_alpha but can
#'     be narrowed to a single param's factor set via `param_scope`.
#'   - Factor values not in observed levels.
#'   - Continuous values that are zero-length, NA, Inf, or non-numeric
#'     (after suppressWarnings(as.numeric())).
#'
#' Warns once on:
#'   - Multi-value continuous `at` entries (uses first value).
#'
#' @param fit_obj A `beezdemand_tmb` object.
#' @param at User-supplied `at` list (or NULL).
#' @param param_scope `NULL` (default) accepts the union of factors_q0
#'   and factors_alpha, appropriate for callers that build both grids
#'   in one user call (e.g., calc_group_metrics). `"Q0"` or `"alpha"`
#'   restricts active factors to that param's set, so a Q0 EMM call
#'   doesn't silently accept an alpha-only collapsed factor name (and
#'   vice versa) under asymmetric `collapse_levels`.
#'
#' @keywords internal
#' @noRd
.tmb_validate_at <- function(fit_obj, at, param_scope = NULL) {
  if (is.null(at)) return(invisible(NULL))

  if (is.null(names(at)) || any(!nzchar(names(at)))) {
    cli::cli_abort(
      "All elements of {.arg at} must be named (use {.code list(factor = level, cov = value)})."
    )
  }

  cov_names <- fit_obj$param_info$continuous_covariates
  if (is.null(cov_names)) cov_names <- character(0)
  # Active factors are those that actually drive the per-parameter grid
  # the caller will build. Under `collapse_levels`, factors_q0 and
  # factors_alpha can diverge; a Q0 EMM call with `param_scope = "Q0"`
  # rejects alpha-only collapsed names rather than silently ignoring
  # them. callers that build BOTH grids (calc_group_metrics) pass
  # NULL to accept the union.
  if (is.null(param_scope)) {
    all_factors <- unique(c(
      fit_obj$param_info$factors_q0,
      fit_obj$param_info$factors_alpha
    ))
  } else if (identical(param_scope, "Q0")) {
    all_factors <- fit_obj$param_info$factors_q0
  } else if (identical(param_scope, "alpha")) {
    all_factors <- fit_obj$param_info$factors_alpha
  } else {
    cli::cli_abort("Internal error: unsupported {.arg param_scope}.")
  }
  if (is.null(all_factors)) all_factors <- character(0)
  all_factors <- all_factors[nzchar(all_factors) & !is.na(all_factors)]

  valid_names <- c(all_factors, cov_names)
  bad_names <- setdiff(names(at), valid_names)
  if (length(bad_names) > 0L) {
    # Detect the "user supplied collapse-aliased original name" case so
    # the error message points at the right collapsed columns.
    original_factors <- fit_obj$param_info$factors
    if (is.null(original_factors)) original_factors <- character(0)
    aliased <- intersect(bad_names, original_factors)
    msg <- c(
      "Unknown name{?s} in {.arg at}: {.field {bad_names}}.",
      "i" = "Valid names are the fit's factors and continuous covariates: {.field {valid_names}}."
    )
    if (length(aliased) > 0L) {
      # Identify the collapsed columns that the original name maps to.
      collapsed_for <- vapply(aliased, function(orig) {
        cands <- intersect(c(paste0(orig, "_Q0"), paste0(orig, "_alpha")),
                           all_factors)
        paste(cands, collapse = " or ")
      }, character(1))
      msg <- c(msg,
        "i" = "{.field {aliased}} was collapsed via {.arg collapse_levels}; condition on {.field {collapsed_for}} instead.")
    }
    msg <- c(msg, "x" = "Did you mistype a factor or covariate name?")
    cli::cli_abort(msg)
  }

  data_used <- fit_obj$data
  for (nm in names(at)) {
    v <- at[[nm]]
    if (length(v) < 1L) {
      cli::cli_abort(c(
        "{.field {nm}} = {.val {v}} has length 0.",
        "i" = "Each {.arg at} entry must be a non-empty vector."
      ))
    }
    if (nm %in% all_factors) {
      observed <- sort(unique(as.character(data_used[[nm]])))
      bad_vals <- setdiff(as.character(v), observed)
      if (length(bad_vals) > 0L) {
        cli::cli_abort(c(
          "{.field {nm}} = {.val {bad_vals}} not an observed level.",
          "i" = "Observed levels: {.val {observed}}.",
          "x" = "Conditioning on an unobserved factor level cannot be evaluated."
        ))
      }
    } else if (nm %in% cov_names) {
      v_num <- suppressWarnings(as.numeric(v))
      if (any(is.na(v_num)) || any(!is.finite(v_num))) {
        cli::cli_abort(c(
          "{.field {nm}} value{?s} {.val {as.character(v)}} not finite numeric.",
          "i" = "Continuous-covariate {.arg at} entries must be a single finite numeric value."
        ))
      }
      if (length(v) > 1L) {
        cli::cli_warn(c(
          "{.arg at${nm}} has length {length(v)}; using first value {.val {v_num[1]}}.",
          "i" = "Pass a single numeric value per continuous covariate."
        ))
      }
    }
  }
  invisible(NULL)
}

# Resolve user-requested retained factors against a parameter's fitted factor
# columns (F2). Under asymmetric `collapse_levels` the
# fitted columns are renamed per parameter (`age_group` -> `age_group_Q0` /
# `age_group_alpha`), but users pass the ORIGINAL name via `compare_specs`
# (mirroring the NLME backend's .get_actual_factors_for_param()). A plain
# `intersect()` would silently drop the original name and collapse to a
# grand-mean (empty contrasts). Map each requested name to this parameter's
# column (direct hit, else the `<name>_<param>` collapse alias); abort if a
# requested name cannot be resolved for this parameter rather than marginalize
# the user's comparison away. `param` is "Q0" or "alpha" (matches the collapse
# suffix). Returns the resolved retained-factor column names.
.tmb_resolve_retained_factors <- function(requested, fitted_factors, param) {
  resolved <- character(0)
  unresolved <- character(0)
  for (f in requested) {
    if (f %in% fitted_factors) {
      resolved <- c(resolved, f)
    } else if (paste0(f, "_", param) %in% fitted_factors) {
      resolved <- c(resolved, paste0(f, "_", param))
    } else {
      unresolved <- c(unresolved, f)
    }
  }
  if (length(unresolved) > 0L) {
    cli::cli_abort(c(
      "{cli::qty(unresolved)}Requested factor{?s} {.val {unresolved}} {?is/are} not in the {param} design.",
      "i" = "Fitted {param} factors: {.val {fitted_factors}}.",
      "x" = "Under asymmetric {.arg collapse_levels} a factor can differ between Q0 and alpha; pass the original factor name (mapped per parameter) or this parameter's collapsed column."
    ))
  }
  unique(resolved)
}

.tmb_build_emm_ref_grid <- function(
  fit_obj,
  param = c("Q0", "alpha"),
  at = NULL,
  factors_in_emm = NULL,
  validate = TRUE
) {
  param <- match.arg(param)

  cov_names <- fit_obj$param_info$continuous_covariates
  if (is.null(cov_names)) cov_names <- character(0)

  if (param == "Q0") {
    use_factors <- fit_obj$param_info$factors_q0
  } else {
    use_factors <- fit_obj$param_info$factors_alpha
  }
  if (is.null(use_factors)) use_factors <- character(0)
  if (!is.null(factors_in_emm)) {
    # NULL = retain all fitted factors; explicit character(0) (e.g. `~ 1`) =
    # marginalize everything to a grand mean; otherwise resolve the requested
    # names against this parameter's columns (collapse-aware; see helper).
    if (length(factors_in_emm) == 0L) {
      use_factors <- character(0)
    } else {
      use_factors <- .tmb_resolve_retained_factors(factors_in_emm, use_factors, param)
    }
  }

  # `at` validation: catch typos and bad values BEFORE grid construction.
  # Public-facing functions that call this helper twice (e.g.
  # calc_group_metrics() builds Q0 and alpha grids in one user call)
  # validate ONCE at their entry point and pass `validate = FALSE` so
  # the multi-value continuous warning fires only once per public call.
  if (isTRUE(validate)) {
    .tmb_validate_at(fit_obj, at, param_scope = param)
  }

  # `use_factors` (above) are the RETAINED factors (after `factors_in_emm`).
  # `fitted_factors` span the full design the beta vector was fit on; any
  # factor in fitted_factors but not retained is MARGINALIZED over.
  fitted_factors <- if (param == "Q0") {
    fit_obj$param_info$factors_q0
  } else {
    fit_obj$param_info$factors_alpha
  }
  if (is.null(fitted_factors)) fitted_factors <- character(0)
  retained_factors <- use_factors

  is_intercept_only <- length(fitted_factors) == 0L && length(cov_names) == 0L

  if (is_intercept_only) {
    return(list(
      level_combos = NULL,
      ref_X = NULL,
      use_factors = character(0),
      cov_names = character(0),
      is_intercept_only = TRUE
    ))
  }

  data_used <- fit_obj$data

  # Per-factor level set: training levels, restricted by `at` if supplied.
  # This is what gets crossed for both the full marginalization grid and the
  # retained reference grid (so `at` on an omitted factor shrinks the averaged
  # set, and `at` on a retained factor shrinks the reported cells).
  factor_level_set <- function(f) {
    lv <- levels(data_used[[f]])
    if (is.null(lv)) lv <- sort(unique(as.character(data_used[[f]])))
    if (!is.null(at) && f %in% names(at)) {
      lv <- lv[lv %in% as.character(at[[f]])]
    }
    lv
  }
  fitted_levels <- stats::setNames(
    lapply(fitted_factors, factor_level_set), fitted_factors
  )
  if (any(vapply(fitted_levels, length, integer(1)) == 0L)) {
    cli::cli_abort(c(
      "{.arg at} filter produced an empty reference grid.",
      "i" = "Check that the supplied factor levels exist in the data and are not mutually exclusive."
    ))
  }

  # Build a key from factor columns (factor labels, delimiter-safe).
  make_key <- function(df, cols) {
    if (length(cols) == 0L) return(rep("", nrow(df)))
    do.call(paste, c(lapply(cols, function(cc) as.character(df[[cc]])),
                     list(sep = "\r")))
  }
  as_training_factor <- function(values, f) {
    factor(values, levels = levels(data_used[[f]]) %||%
             sort(unique(as.character(data_used[[f]]))))
  }

  # Full factorial grid over ALL fitted factors (Decision 10, Option A) — the
  # model predicts every cell, so the equal-weight average is taken over the
  # full crossing, matching emmeans' default `weights = "equal"` (and hence
  # the NLME backend's omitted-factor averaging).
  if (length(fitted_factors) > 0L) {
    full_combos <- do.call(expand.grid, c(
      lapply(fitted_factors, function(f) as_training_factor(fitted_levels[[f]], f)),
      list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    ))
    names(full_combos) <- fitted_factors
  } else {
    full_combos <- data_used[1L, integer(0), drop = FALSE]
  }

  # Retained reference grid: full crossing of retained factors' (at-restricted)
  # levels, ordered by factor-level index (Decision 7), then filtered to
  # OBSERVED combinations — the genuine `semi_join` analog (separate from the
  # averaging weights). For a single retained factor the filter is a no-op.
  if (length(retained_factors) > 0L) {
    level_combos <- do.call(expand.grid, c(
      lapply(retained_factors, function(f) as_training_factor(fitted_levels[[f]], f)),
      list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    ))
    names(level_combos) <- retained_factors
    ord <- do.call(order, lapply(retained_factors,
                                 function(f) as.integer(level_combos[[f]])))
    level_combos <- level_combos[ord, , drop = FALSE]
    observed_keys <- make_key(
      unique(data_used[, retained_factors, drop = FALSE]), retained_factors
    )
    level_combos <- level_combos[
      make_key(level_combos, retained_factors) %in% observed_keys, ,
      drop = FALSE
    ]
    if (nrow(level_combos) == 0L) {
      cli::cli_abort(c(
        "{.arg at} filter produced an empty reference grid.",
        "i" = "Check that the supplied factor levels exist in the data and are not mutually exclusive."
      ))
    }
    rownames(level_combos) <- NULL
  } else {
    # All factors marginalized (or none fitted): a single grand-mean row.
    level_combos <- data_used[1L, integer(0), drop = FALSE]
  }

  # Continuous covariates: hold at training mean unless overridden via `at`.
  # Constant across grid rows; multi-value `at` warns above and uses the first.
  if (length(cov_names) > 0L) {
    for (cv in cov_names) {
      cv_value <- mean(data_used[[cv]], na.rm = TRUE)
      if (!is.null(at) && cv %in% names(at)) {
        cv_value <- as.numeric(at[[cv]][1])
      }
      full_combos[[cv]] <- cv_value
      level_combos[[cv]] <- cv_value
    }
  }

  # Pin the rebuilt basis to the FITTED design's contrasts (F1).
  # `model.matrix()` otherwise picks up whatever
  # `options("contrasts")` is in effect at call time; if that differs from fit
  # time the rebuilt basis can keep the same column count but encode a different
  # basis, silently multiplying the wrong columns by `beta`. Passing the fitted
  # `contrasts` attribute reproduces the fit-time basis, and we then verify the
  # columns match the fitted design (reordering if needed) and abort loudly
  # rather than compute against a mismatched basis.
  fitted_X <- if (param == "Q0") fit_obj$formula_details$X_q0 else
    fit_obj$formula_details$X_alpha
  X_full <- stats::model.matrix(
    stats::as.formula(build_fixed_rhs(
      factors = fitted_factors,
      factor_interaction = fit_obj$param_info$factor_interaction,
      continuous_covariates = cov_names,
      data = data_used
    )),
    data = full_combos,
    contrasts.arg = attr(fitted_X, "contrasts")
  )
  fitted_cols <- colnames(fitted_X)
  if (!is.null(fitted_cols)) {
    if (!setequal(colnames(X_full), fitted_cols)) {
      cli::cli_abort(c(
        "Could not reproduce the fitted {param} design matrix for the EMM grid.",
        "i" = "Rebuilt columns: {.val {colnames(X_full)}}.",
        "i" = "Fitted columns: {.val {fitted_cols}}.",
        "x" = "This can happen if the model's factor levels or contrasts changed after fitting."
      ))
    }
    X_full <- X_full[, fitted_cols, drop = FALSE]
  }

  # Averaging matrix A (n_retained x n_full): each retained cell places equal
  # weight 1/m on the m full-grid rows matching it (m = product of omitted
  # factors' level counts). X_marg = A %*% X_full keeps ncol == length(beta),
  # so downstream `sum(x*beta)` / `t(x) V x` are exact (linear-in-beta on the
  # log/linear-predictor scale). For no omitted factors, A selects the observed
  # cells (m = 1) and X_marg reduces to the per-cell design — bit-identical to
  # the pre-marginalization path apart from Decision 7 ordering.
  full_keys <- make_key(full_combos, retained_factors)
  ret_keys <- make_key(level_combos, retained_factors)
  A <- matrix(0, nrow = nrow(level_combos), ncol = nrow(full_combos))
  for (r in seq_len(nrow(level_combos))) {
    sel <- which(full_keys == ret_keys[r])
    A[r, sel] <- 1 / length(sel)
  }
  ref_X <- A %*% X_full
  colnames(ref_X) <- colnames(X_full)

  list(
    level_combos = level_combos,
    ref_X = ref_X,
    use_factors = retained_factors,
    cov_names = cov_names,
    is_intercept_only = FALSE
  )
}


#' Get Demand Parameter Estimated Marginal Means for TMB Model
#'
#' @description
#' Computes estimated marginal means (EMMs) for demand parameters from a
#' `beezdemand_tmb` model. Uses design matrices and beta vectors with vcov
#' from `TMB::sdreport()`.
#'
#' @param fit_obj A \code{beezdemand_tmb} object.
#' @param param Character. Which parameter to compute EMMs for: `"Q0"` or
#'   `"alpha"`.
#' @param factors_in_emm Character vector of factors to retain in the EMM
#'   reference grid. If it names a strict subset of the fitted factors, the
#'   omitted factors are **marginalized over** using equal weights across the
#'   full crossing of their levels (emmeans' default `weights = "equal"`),
#'   matching the NLME backend. If `NULL` (default), all fitted factors are
#'   retained (no marginalization). Under asymmetric `collapse_levels` you may
#'   name either the original factor or its collapsed per-parameter column; a
#'   name that resolves to neither for this parameter is rejected with an error.
#' @param at Named list specifying factor levels and continuous-covariate
#'   values for conditional EMMs. For continuous covariates, a single
#'   numeric value per covariate; multiple values produce a warning and
#'   only the first is used. `at` on a marginalized (omitted) factor
#'   restricts the level set averaged over.
#' @param ci_level Numeric. Confidence level for intervals.
#' @param ... Additional arguments.
#'
#' @return A tibble with columns: level, estimate, std.error, conf.low, conf.high.
#'
#' @note Marginalization is exact because `Q0`/`alpha` are linear in the
#'   fixed-effect coefficients on the log scale, so averaging the reference-grid
#'   design rows and then multiplying by the coefficient vector equals averaging
#'   the per-cell parameter predictions.
#'
#' @examples
#' \donttest{
#' data(apt_full)
#' # 40 subjects per gender keep the example fast; use the full data in practice
#' ids <- unique(apt_full[c("id", "gender")])
#' ids <- ids[ids$gender %in% c("Male", "Female"), ]
#' keep <- unlist(lapply(split(ids$id, ids$gender), head, 40))
#' dat <- apt_full[apt_full$id %in% keep, ]
#' fit <- fit_demand_tmb(dat, equation = "exponential",
#'                       factors = "gender", verbose = 0)
#' get_demand_param_emms(fit, param = "Q0")
#' get_demand_param_emms(fit, param = "alpha")
#' }
#'
#' @export
get_demand_param_emms.beezdemand_tmb <- function(
  fit_obj,
  param = c("Q0", "alpha"),
  factors_in_emm = NULL,
  at = NULL,
  ci_level = 0.95,
  ...
) {
  param <- match.arg(param)

  # Reads sdr$cov.fixed directly (not via vcov()), so it needs its own
  # explicit hessian_pd check (TICKET-063).
  .tmb_warn_if_hessian_not_pd(fit_obj)

  coefs <- fit_obj$model$coefficients
  sdr <- fit_obj$sdr

  # Get the right beta vector and design matrix
  if (param == "Q0") {
    beta_idx <- which(names(coefs) == "beta_q0")
    beta <- coefs[beta_idx]
    X <- fit_obj$formula_details$X_q0
    col_names <- colnames(X)
  } else {
    beta_idx <- which(names(coefs) == "beta_alpha")
    beta <- coefs[beta_idx]
    X <- fit_obj$formula_details$X_alpha
    col_names <- colnames(X)
  }

  # Get vcov submatrix
  vcov_mat <- NULL
  if (!is.null(sdr) && !is.null(sdr$cov.fixed)) {
    full_vcov <- as.matrix(sdr$cov.fixed)
    par_names <- names(fit_obj$opt$par)
    target_name <- if (param == "Q0") "beta_q0" else "beta_alpha"
    target_idx <- which(par_names == target_name)
    if (length(target_idx) == length(beta)) {
      vcov_mat <- full_vcov[target_idx, target_idx, drop = FALSE]
    }
  }

  if (is.null(vcov_mat)) {
    # Fallback: diagonal from SE
    se_vals <- fit_obj$model$se[beta_idx]
    vcov_mat <- diag(se_vals^2, nrow = length(se_vals))
  }

  # Validate `at` once at the public boundary, scoped to the requested
  # param so a Q0 EMM call doesn't silently accept an alpha-only
  # collapsed factor name (and vice versa) under asymmetric
  # `collapse_levels`. Subsequent helper calls below (and any nested
  # calls from get_demand_comparisons()) pass `validate = FALSE` so the
  # multi-value-continuous warning fires exactly once per public call
  # rather than once per param grid.
  .tmb_validate_at(fit_obj, at, param_scope = param)

  # Build reference grid via the shared helper so EMMs and comparisons
  # always consume the same conditioned grid (TICKET-011 Phase 0.4).
  grid <- .tmb_build_emm_ref_grid(
    fit_obj,
    param = param,
    at = at,
    factors_in_emm = factors_in_emm,
    validate = FALSE
  )

  if (grid$is_intercept_only) {
    # Truly intercept-only model: short-circuit to beta[1].
    est <- beta[1]
    se <- sqrt(vcov_mat[1, 1])
    z <- stats::qnorm((1 + ci_level) / 2)

    return(tibble::tibble(
      level = "(Intercept)",
      estimate = exp(est),
      estimate_log = est,
      std.error = se,
      conf.low = exp(est - z * se),
      conf.high = exp(est + z * se)
    ))
  }

  use_factors <- grid$use_factors
  cov_names <- grid$cov_names
  level_combos <- grid$level_combos
  ref_X <- grid$ref_X

  # Safety net: after marginalization (.tmb_build_emm_ref_grid averages over
  # the full omitted-factor grid) `ref_X` always shares the fitted beta's
  # column basis, so this should never fire. It guards only against a genuine
  # covariate-basis mismatch (e.g. a malformed fit object) that would otherwise
  # let `sum(x_ref * beta)` silently recycle a shorter vector.
  if (ncol(ref_X) != length(beta)) {
    cli::cli_abort(c(
      "Reference-grid design for {.field {param}} has {ncol(ref_X)} column{?s} \\
       but the fitted coefficient vector has {length(beta)}.",
      "x" = "Covariate/design basis mismatch; cannot evaluate EMMs."
    ))
  }

  z <- stats::qnorm((1 + ci_level) / 2)

  # Compute EMMs
  results <- lapply(seq_len(nrow(ref_X)), function(i) {
    x_ref <- ref_X[i, ]
    est <- sum(x_ref * beta)
    se <- sqrt(as.numeric(t(x_ref) %*% vcov_mat %*% x_ref))

    if (length(use_factors) > 0L) {
      level_label <- paste(
        vapply(use_factors, function(f) {
          paste0(f, "=", level_combos[[f]][i])
        }, character(1)),
        collapse = ", "
      )
    } else if (!is.null(cov_names) && length(cov_names) > 0L) {
      level_label <- paste(
        vapply(cov_names, function(cv) {
          paste0(cv, "=", level_combos[[cv]][i])
        }, character(1)),
        collapse = ", "
      )
    } else {
      level_label <- "(Intercept)"
    }

    tibble::tibble(
      level = level_label,
      estimate = exp(est),
      estimate_log = est,
      std.error = se,
      conf.low = exp(est - z * se),
      conf.high = exp(est + z * se)
    )
  })

  dplyr::bind_rows(results)
}


#' Get Demand Parameter Comparisons for TMB Model
#'
#' @description
#' Computes factor-level contrasts for demand parameters from a
#' `beezdemand_tmb` model. Returns a classed `beezdemand_comparison` object
#' (the same container the NLME backend returns), so
#' [tidy.beezdemand_comparison()] gives a backend-agnostic flat frame.
#'
#' @param fit_obj A \code{beezdemand_tmb} object.
#' @param param Character vector. Which parameter(s) to compare: any of
#'   `"Q0"`, `"alpha"`. Default `c("Q0", "alpha")` (both).
#' @param compare_specs Optional one-sided formula naming the factor subset to
#'   contrast (e.g. `~ gender`). Omitted fitted factors are marginalized over
#'   with equal weights across the full crossing of their levels (matching the
#'   NLME backend). If `NULL` (default), all fitted factors are retained. Under
#'   asymmetric `collapse_levels`, name the **original** factor (e.g.
#'   `~ age_group`); it resolves to that parameter's collapsed column
#'   (`age_group_Q0` / `age_group_alpha`), as on the NLME backend.
#' @param contrast_type Character. `"pairwise"` (all pairs, factor-level order)
#'   or `"trt.vs.ctrl"` (each level vs. the first/reference level).
#' @param contrast_by Optional `NULL` (default) or character vector of factor
#'   name(s) within `compare_specs` to condition the contrasts on. Within each
#'   observed combination of by-level(s), pairwise (or `trt.vs.ctrl`) contrasts
#'   are computed over the remaining (non-by) factors, with p-value adjustment
#'   applied **per by-cell**. The by-variable(s) must be named in
#'   `compare_specs` (per parameter after collapse-mapping); a `contrast_by`
#'   factor absent from `compare_specs` aborts. Unlike `compare_specs` (which
#'   aborts on an unresolvable name), `contrast_by` **soft-skips** a parameter
#'   for which it does not resolve under asymmetric `collapse_levels`. Numeric
#'   results match the NLME backend in shape and direction (TMB uses asymptotic
#'   *z* vs. NLME's *t*). Continuous covariates are held at the global training
#'   mean within by-levels (the same convention as `at`), not recomputed per
#'   by-level. Multi-by is accepted but currently untested on TMB (its
#'   two-factor fixed-effect cap precludes a `compare_specs` with the 3+ factors
#'   a multi-by Cartesian would require).
#' @param adjust Character. P-value adjustment method; must be one of
#'   `stats::p.adjust.methods` (default `"holm"`). emmeans-only methods such as
#'   `"tukey"`/`"sidak"` are rejected (the TMB backend uses asymptotic z +
#'   `stats::p.adjust()`).
#' @param at Named list specifying factor levels and/or continuous-covariate
#'   values to condition on, as in [get_demand_param_emms.beezdemand_tmb()].
#' @param ci_level Numeric. Confidence level for intervals. Default 0.95.
#' @param report_ratios Logical. If `TRUE` (default), include a
#'   `contrasts_ratio` block (multiplicative ratios) per parameter.
#' @param ... Additional arguments (reserved; `factors_in_emm` is accepted as a
#'   lower-level alternative to `compare_specs`).
#'
#' @return A `beezdemand_comparison` object: a list named by parameter, each
#'   element a list with `emmeans` (native cell means), `contrasts_log10`
#'   (log10-scale contrasts with `contrast`, `estimate`, `std.error`,
#'   `statistic`, `df`, `conf.low`, `conf.high`, `p.value`), and (if
#'   `report_ratios`) `contrasts_ratio`. When `contrast_by` is active, the
#'   contrast tables gain leading by-column(s) (user-requested original names)
#'   before `contrast`. Attributes `backend`, `adjustment_method`,
#'   `compare_specs_used`, `contrast_type_used`, `contrast_by_used`, and
#'   `contrast_by_map` (per-parameter original -> effective by-name map)
#'   describe the call.
#'
#' @seealso [tidy.beezdemand_comparison()] for the backend-agnostic frame.
#'
#' @examples
#' \donttest{
#' data(apt_full)
#' # 40 subjects per gender keep the example fast; use the full data in practice
#' ids <- unique(apt_full[c("id", "gender")])
#' ids <- ids[ids$gender %in% c("Male", "Female"), ]
#' keep <- unlist(lapply(split(ids$id, ids$gender), head, 40))
#' dat <- apt_full[apt_full$id %in% keep, ]
#' fit <- fit_demand_tmb(dat, equation = "exponential",
#'                       factors = "gender", verbose = 0)
#' res <- get_demand_comparisons(fit, param = "Q0")
#' tidy(res)
#' }
#'
#' @export
get_demand_comparisons.beezdemand_tmb <- function(
  fit_obj,
  param = c("Q0", "alpha"),
  compare_specs = NULL,
  contrast_type = c("pairwise", "trt.vs.ctrl"),
  contrast_by = NULL,
  adjust = "holm",
  at = NULL,
  ci_level = 0.95,
  report_ratios = TRUE,
  ...
) {
  param <- match.arg(param, c("Q0", "alpha"), several.ok = TRUE)
  contrast_type <- match.arg(contrast_type)

  # contrast_by (TICKET-032): NULL or character(1+). Boundary union-validation
  # (loud) catches typos here, once, before the per-param loop; per-parameter
  # collapse-aware resolution + soft-skip happens inside .tmb_compare_one_param().
  if (!is.null(contrast_by)) {
    if (!is.character(contrast_by)) {
      cli::cli_abort("{.arg contrast_by} must be {.code NULL} or a character vector of factor name(s).")
    }
    if (length(contrast_by) == 0L) {
      contrast_by <- NULL  # zero-length == no by-grouping
    } else {
      valid_by <- unique(c(
        fit_obj$param_info$factors,
        fit_obj$param_info$factors_q0,
        fit_obj$param_info$factors_alpha
      ))
      valid_by <- valid_by[!is.na(valid_by) & nzchar(valid_by)]
      bad_by <- setdiff(contrast_by, valid_by)
      if (length(bad_by) > 0L) {
        cli::cli_abort(c(
          "{.arg contrast_by} names factor{?s} not in the fit: {.val {bad_by}}.",
          "i" = "Fitted factors: {.val {valid_by}}."
        ))
      }
    }
  }

  # adjust: validate against the base-R set (Decision 6). emmeans-only methods
  # (tukey/sidak/scheffe/mvt) are not implementable with stats::p.adjust().
  if (!isTRUE(adjust %in% stats::p.adjust.methods)) {
    cli::cli_abort(c(
      "{.arg adjust} = {.val {adjust}} is not a valid p-value adjustment method.",
      "i" = "Valid methods: {.val {stats::p.adjust.methods}}.",
      "x" = "emmeans-only methods (e.g. {.val tukey}, {.val sidak}) are unavailable on the TMB backend (asymptotic z + {.fn stats::p.adjust})."
    ))
  }

  dots <- list(...)
  if ("p_adjust" %in% names(dots)) {
    cli::cli_abort(c(
      "{.arg p_adjust} has been renamed to {.arg adjust}.",
      "i" = "Pass {.code adjust = {.val {dots$p_adjust}}} instead."
    ))
  }

  # Resolve the retained factor set: `compare_specs` (canonical) wins, else the
  # lower-level `factors_in_emm` via `...` (backward compatible).
  factors_in_emm <- NULL
  if (!is.null(compare_specs)) {
    if (!inherits(compare_specs, "formula")) {
      cli::cli_abort("{.arg compare_specs} must be a one-sided formula (e.g. {.code ~ gender}).")
    }
    factors_in_emm <- all.vars(compare_specs)
    fitted_all <- unique(c(
      fit_obj$param_info$factors_q0,
      fit_obj$param_info$factors_alpha,
      fit_obj$param_info$factors
    ))
    fitted_all <- fitted_all[nzchar(fitted_all) & !is.na(fitted_all)]
    bad <- setdiff(factors_in_emm, fitted_all)
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "{.arg compare_specs} names factor{?s} not in the fit: {.val {bad}}.",
        "i" = "Fitted factors: {.val {fitted_all}}."
      ))
    }
  } else if (!is.null(dots$factors_in_emm)) {
    factors_in_emm <- dots$factors_in_emm
  }

  # Validate `at` once at the public boundary (single multi-value warning).
  .tmb_validate_at(fit_obj, at)

  # .tmb_compare_one_param() reads sdr$cov.fixed directly (not via vcov()),
  # once per requested param -- warn once here, before the loop, rather than
  # once per param (TICKET-063).
  .tmb_warn_if_hessian_not_pd(fit_obj)

  results_list <- stats::setNames(
    lapply(param, function(p) {
      .tmb_compare_one_param(
        fit_obj, p, factors_in_emm, contrast_type,
        adjust, at, ci_level, report_ratios, contrast_by
      )
    }),
    param
  )

  # Collect the per-parameter original -> effective contrast_by maps (attached
  # by .tmb_compare_one_param); strip the now-redundant slot from each block.
  contrast_by_map <- stats::setNames(
    lapply(results_list, function(b) {
      m <- attr(b, "contrast_by_map")
      if (is.null(m)) stats::setNames(character(0), character(0)) else m
    }),
    param
  )
  for (p in param) attr(results_list[[p]], "contrast_by_map") <- NULL

  class(results_list) <- "beezdemand_comparison"
  attr(results_list, "backend") <- "tmb"
  attr(results_list, "compare_specs_used") <- if (is.null(compare_specs)) {
    "all fitted factors"
  } else {
    deparse(compare_specs)
  }
  attr(results_list, "contrast_type_used") <- contrast_type
  # Report the user-requested original name(s) ONLY when by-grouping was
  # actually applied for at least one parameter; otherwise "NULL" so the
  # flattener/print do not synthesize an all-NA by-column (e.g.
  # redundant-by ignored for every parameter).
  any_by_applied <- any(vapply(contrast_by_map, length, integer(1)) > 0L)
  attr(results_list, "contrast_by_used") <- if (is.null(contrast_by) || !any_by_applied) {
    "NULL"
  } else {
    paste(contrast_by, collapse = ", ")
  }
  attr(results_list, "contrast_by_map") <- contrast_by_map
  attr(results_list, "adjustment_method") <- adjust
  results_list
}

# Build one parameter's nested comparison block for the TMB backend. Returns
# list(emmeans, contrasts_log10[, contrasts_ratio]); `contrasts_log10` carries
# an `std_labels` attribute of emmeans-style contrast labels (built from
# STRUCTURED ref-grid level values, never by regex-parsing native strings) that
# tidy.beezdemand_comparison() reads for the cross-backend `contrast` column.
.tmb_compare_one_param <- function(fit_obj, param, factors_in_emm,
                                   contrast_type, adjust, at, ci_level,
                                   report_ratios, contrast_by = NULL) {
  coefs <- fit_obj$model$coefficients
  sdr <- fit_obj$sdr
  target_name <- if (param == "Q0") "beta_q0" else "beta_alpha"
  beta <- unname(coefs[names(coefs) == target_name])

  vcov_mat <- NULL
  if (!is.null(sdr) && !is.null(sdr$cov.fixed)) {
    full_vcov <- as.matrix(sdr$cov.fixed)
    par_names <- names(fit_obj$opt$par)
    target_idx <- which(par_names == target_name)
    if (length(target_idx) == length(beta)) {
      vcov_mat <- full_vcov[target_idx, target_idx, drop = FALSE]
    }
  }
  if (is.null(vcov_mat)) {
    se_vals <- fit_obj$model$se[names(coefs) == target_name]
    vcov_mat <- diag(se_vals^2, nrow = length(se_vals))
  }

  grid <- .tmb_build_emm_ref_grid(
    fit_obj, param = param, at = at,
    factors_in_emm = factors_in_emm, validate = FALSE
  )
  z <- stats::qnorm((1 + ci_level) / 2)

  empty_log10 <- tibble::tibble(
    contrast = character(), estimate = numeric(), std.error = numeric(),
    statistic = numeric(), df = numeric(),
    conf.low = numeric(), conf.high = numeric(), p.value = numeric()
  )
  empty_ratio <- tibble::tibble(
    contrast = character(), ratio = numeric(),
    conf.low = numeric(), conf.high = numeric(), p.value = numeric()
  )
  finish_empty <- function(emm_block) {
    out <- list(emmeans = emm_block, contrasts_log10 = empty_log10)
    attr(out$contrasts_log10, "std_labels") <- character()
    if (report_ratios) out$contrasts_ratio <- empty_ratio
    out
  }

  if (isTRUE(grid$is_intercept_only)) {
    est <- beta[1L]
    se <- sqrt(vcov_mat[1L, 1L])
    emm_block <- tibble::tibble(
      level = "(Intercept)", estimate = exp(est), estimate_log = est,
      std.error = se, conf.low = exp(est - z * se), conf.high = exp(est + z * se)
    )
    return(finish_empty(emm_block))
  }

  use_factors <- grid$use_factors
  cov_names <- grid$cov_names
  level_combos <- grid$level_combos
  ref_X <- grid$ref_X
  n <- nrow(ref_X)

  cell_est <- as.numeric(ref_X %*% beta)
  cell_se <- sqrt(diag(ref_X %*% vcov_mat %*% t(ref_X)))

  # Label builders parametrized by the factor subset used for the label. For
  # by-grouped contrasts the by-vars are EXCLUDED from the contrast label (they
  # become separate by-columns), so the within-cell label matches the
  # `at = `-filtered route (the self-consistency anchor, TICKET-032 Decision 8).
  native_label_f <- function(i, fs) {
    if (length(fs) > 0L) {
      paste(vapply(fs, function(f)
        paste0(f, "=", as.character(level_combos[[f]][i])), character(1)),
        collapse = ", ")
    } else if (length(cov_names) > 0L) {
      paste(vapply(cov_names, function(cv)
        paste0(cv, "=", level_combos[[cv]][i]), character(1)), collapse = ", ")
    } else {
      "(Intercept)"
    }
  }
  std_label_f <- function(i, fs) {
    if (length(fs) > 0L) {
      paste(vapply(fs, function(f)
        as.character(level_combos[[f]][i]), character(1)), collapse = " ")
    } else {
      native_label_f(i, fs)
    }
  }
  native_label <- function(i) native_label_f(i, use_factors)
  std_label <- function(i) std_label_f(i, use_factors)

  emm_block <- tibble::tibble(
    level = vapply(seq_len(n), native_label, character(1)),
    estimate = exp(cell_est), estimate_log = cell_est, std.error = cell_se,
    conf.low = exp(cell_est - z * cell_se),
    conf.high = exp(cell_est + z * cell_se)
  )

  ln10 <- log(10)

  # ---- Resolve contrast_by for this parameter (collapse-aware, soft) --------
  # The boundary union-validation already ran (typos error in the public
  # function); here we map each requested by-var to THIS parameter's column
  # (direct hit, else the `<name>_<param>` collapse alias) and soft-skip names
  # absent from this parameter's design (collapse-induced asymmetry). `by_map`
  # records the original -> effective resolution actually used for by-grouping.
  effective_by <- character(0)
  by_map <- stats::setNames(character(0), character(0))
  if (n >= 2L && !is.null(contrast_by)) {
    param_factor_set <- if (param == "Q0") {
      fit_obj$param_info$factors_q0
    } else {
      fit_obj$param_info$factors_alpha
    }
    if (is.null(param_factor_set)) param_factor_set <- character(0)
    for (cb in contrast_by) {
      if (cb %in% param_factor_set) {
        effective_by <- c(effective_by, cb)
        by_map[cb] <- cb
      } else if (paste0(cb, "_", param) %in% param_factor_set) {
        mapped <- paste0(cb, "_", param)
        effective_by <- c(effective_by, mapped)
        by_map[cb] <- mapped
        message(
          "  Mapped contrast_by from '", cb, "' to '", mapped,
          "' for ", param, " due to collapse_levels."
        )
      }
      # else: silent per-param skip (collapse-induced asymmetry).
    }

    # Within-param collision: two requested by-vars -> same effective column.
    if (any(duplicated(effective_by))) {
      cli::cli_abort(c(
        "Two {.arg contrast_by} variables resolve to the same column for {param}.",
        "i" = "Resolved columns: {.val {effective_by}}."
      ))
    }

    # Pre-validation: every effective by-var must be in this parameter's
    # `compare_specs` retained set (the by-grid). Replaces the old NLME
    # silent-empty path with a loud, backend-consistent abort.
    if (length(effective_by) > 0L && !all(effective_by %in% use_factors)) {
      not_in <- setdiff(effective_by, use_factors)
      cli::cli_abort(c(
        "{cli::qty(not_in)}{.arg contrast_by} factor{?s} {.val {not_in}} {?is/are} not in {.arg compare_specs} for {param}.",
        "i" = "{cli::qty(use_factors)}{.arg compare_specs} factor{?s} for {param}: {.val {use_factors}}.",
        "x" = "Name the by-variable(s) in {.arg compare_specs} to condition contrasts on them."
      ))
    }

    # Redundant-by (NLME-exact): length-1 compare_specs equal to the by-set.
    if (length(effective_by) > 0L && length(use_factors) == 1L &&
        identical(sort(use_factors), sort(effective_by))) {
      message(
        "  `contrast_by` (", paste(effective_by, collapse = ", "),
        ") is redundant with `compare_specs` (~ ",
        paste(use_factors, collapse = " * "),
        ") for simple contrasts. Ignoring `contrast_by` for this parameter."
      )
      effective_by <- character(0)
    }

    # Keep `by_map` consistent with what is actually used for by-grouping.
    by_map <- by_map[unname(by_map) %in% effective_by]

    # Additive-model heads-up (GLOBAL model factor set, mirroring NLME).
    original_factors <- fit_obj$param_info$factors
    if (is.null(original_factors)) original_factors <- character(0)
    if (length(effective_by) > 0L && length(original_factors) > 1L &&
        !isTRUE(fit_obj$param_info$factor_interaction)) {
      message(
        "  Note: The original model fit for '", param,
        "' appears to be additive for factors: ",
        paste(original_factors, collapse = ", "),
        ". Contrasts using 'contrast_by = \"", paste(contrast_by, collapse = ", "),
        "\"' will likely show identical estimates across levels of '",
        paste(contrast_by, collapse = ", "), "'."
      )
    }
  }

  comparison_factors <- setdiff(use_factors, effective_by)

  # Row blocks: one per observed by-cell (factor-level order preserved from the
  # ref grid), or a single global block when no by-grouping is active. The
  # single-block path reproduces the pre-TICKET-032 output bit-for-bit.
  if (length(effective_by) > 0L) {
    by_key <- do.call(paste, c(
      lapply(effective_by, function(f) as.character(level_combos[[f]])),
      list(sep = "\r")
    ))
    blocks <- lapply(unique(by_key), function(k) which(by_key == k))
  } else {
    blocks <- list(seq_len(n))
  }

  # Compute pairwise / trt.vs.ctrl contrasts within one block of grid rows.
  # p-values are adjusted WITHIN the block (per by-cell), so a single-contrast
  # cell matches the `at = `-filtered route exactly.
  do_block <- function(rows) {
    m <- length(rows)
    if (m < 2L) return(NULL)
    if (contrast_type == "pairwise") {
      cmb <- utils::combn(m, 2L)
      lhs <- rows[cmb[1L, ]]
      rhs <- rows[cmb[2L, ]]
    } else {
      lhs <- rows[seq.int(2L, m)]
      rhs <- rep(rows[1L], m - 1L)
    }
    est_log <- numeric(length(lhs))
    se_log <- numeric(length(lhs))
    native <- character(length(lhs))
    stdlab <- character(length(lhs))
    for (k in seq_along(lhs)) {
      dx <- ref_X[lhs[k], ] - ref_X[rhs[k], ]
      est_log[k] <- sum(dx * beta)
      se_log[k] <- sqrt(as.numeric(t(dx) %*% vcov_mat %*% dx))
      native[k] <- paste(native_label_f(lhs[k], comparison_factors), "-",
                         native_label_f(rhs[k], comparison_factors))
      stdlab[k] <- paste(std_label_f(lhs[k], comparison_factors), "-",
                        std_label_f(rhs[k], comparison_factors))
    }
    zstat <- est_log / se_log
    p_adj <- stats::p.adjust(2 * stats::pnorm(-abs(zstat)), method = adjust)
    est_log10 <- est_log / ln10
    se_log10 <- se_log / ln10
    list(
      log10 = tibble::tibble(
        contrast = native, estimate = est_log10, std.error = se_log10,
        statistic = zstat, df = Inf,
        conf.low = est_log10 - z * se_log10,
        conf.high = est_log10 + z * se_log10, p.value = p_adj
      ),
      ratio = tibble::tibble(
        contrast = native, ratio = exp(est_log),
        conf.low = exp(est_log - z * se_log),
        conf.high = exp(est_log + z * se_log), p.value = p_adj
      ),
      std_labels = stdlab,
      first_row = rows[1L]
    )
  }

  # Build the by-column tibble (user-requested ORIGINAL names) for a block.
  by_cols_for <- function(first_row, nrows) {
    if (length(effective_by) == 0L) return(NULL)
    cols <- lapply(names(by_map), function(orig) {
      rep(as.character(level_combos[[by_map[[orig]]]][first_row]), nrows)
    })
    tibble::as_tibble(stats::setNames(cols, names(by_map)))
  }

  block_results <- Filter(Negate(is.null), lapply(blocks, do_block))

  if (length(block_results) == 0L) {
    # Empty but contrast_by was resolved: preserve the map so the metadata
    # contract holds even with no contrast rows.
    res <- finish_empty(emm_block)
    attr(res, "contrast_by_map") <- by_map
    return(res)
  }

  log10_parts <- lapply(block_results, function(r) {
    bc <- by_cols_for(r$first_row, nrow(r$log10))
    if (is.null(bc)) r$log10 else dplyr::bind_cols(bc, r$log10)
  })
  ratio_parts <- lapply(block_results, function(r) {
    bc <- by_cols_for(r$first_row, nrow(r$ratio))
    if (is.null(bc)) r$ratio else dplyr::bind_cols(bc, r$ratio)
  })

  contrasts_log10 <- dplyr::bind_rows(log10_parts)
  attr(contrasts_log10, "std_labels") <- unlist(
    lapply(block_results, function(r) r$std_labels), use.names = FALSE
  )

  out <- list(emmeans = emm_block, contrasts_log10 = contrasts_log10)
  if (report_ratios) {
    contrasts_ratio <- dplyr::bind_rows(ratio_parts)
    attr(contrasts_ratio, "std_labels") <- attr(contrasts_log10, "std_labels")
    out$contrasts_ratio <- contrasts_ratio
  }
  attr(out, "contrast_by_map") <- by_map
  out
}


# --- calc_group_metrics ---

#' Calculate Population-Level Demand Metrics for TMB Model
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param at Named list of factor-level filters or continuous-covariate
#'   value overrides (e.g. `list(condition = "C1", FTND_z = 0.5)`).
#'   When `NULL` (default), continuous covariates are evaluated at their
#'   training mean and factors are marginalized across observed levels
#'   (equal weights). When supplied, conditions the parameter EMMs to the
#'   specified factor levels and/or covariate values before deriving
#'   Pmax/Omax. Same shape as the `at` argument of
#'   \code{\link{get_demand_param_emms.beezdemand_tmb}} and
#'   \code{\link{get_demand_comparisons.beezdemand_tmb}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list with `Pmax`, `Omax`, `Qmax`, `elasticity_at_pmax`,
#'   `method`, and `conditioned_on` describing the reference point used.
#'   The `conditioned_on` field reports the actual conditioning applied
#'   (covariate values used, factor treatment per factor) so programmatic
#'   consumers do not have to re-derive it.
#'
#' @section Marginalization order:
#' For derived metrics (Pmax/Omax/Qmax) that depend nonlinearly on `Q0`
#' and `alpha` jointly, this function marginalizes parameters first then
#' derives metrics:
#' \enumerate{
#'   \item Compute log-Q0 and log-alpha EMMs at each cell of the reference
#'     grid produced by \code{.tmb_build_emm_ref_grid()}.
#'   \item Marginalize each parameter across factor cells with equal
#'     weights (matches the emmeans default).
#'   \item Derive Pmax/Omax/Qmax from the marginalized log-parameters at
#'     the user-supplied (or training-mean default) covariate point.
#' }
#' The result is "metrics evaluated at the average parameter values" rather
#' than "average metrics across cells". The two answers differ for nonlinear
#' transforms. The convention matches the parameter-level marginalization
#' used by \code{get_demand_param_emms()}.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' calc_group_metrics(fit)
#' # Conditioned at a specific covariate value:
#' # calc_group_metrics(fit_with_cov, at = list(FTND_z = 1))
#' }
#'
#' @seealso \code{\link{fit_demand_tmb}},
#'   \code{\link{get_demand_param_emms.beezdemand_tmb}}
#' @export
calc_group_metrics.beezdemand_tmb <- function(object, at = NULL, ...) {
  coefs <- object$model$coefficients
  has_k <- object$param_info$has_k

  beta_q0_idx <- which(names(coefs) == "beta_q0")
  beta_alpha_idx <- which(names(coefs) == "beta_alpha")
  beta_q0 <- unname(coefs[beta_q0_idx])
  beta_alpha <- unname(coefs[beta_alpha_idx])

  # Phase 5C: parameter-first marginalization. Compute log-Q0 and log-alpha
  # EMMs across the reference grid (continuous covariates at training
  # mean by default; factor levels marginal with equal weights), then
  # derive Pmax/Omax/Qmax from the marginalized parameters -- consistent
  # with the parameter-level convention used by `get_demand_param_emms()`.
  #
  # Validate `at` ONCE here so the two grid calls below (Q0 + alpha)
  # don't each emit a duplicate multi-value-continuous warning.
  .tmb_validate_at(object, at)

  grid_q0 <- .tmb_build_emm_ref_grid(
    object, param = "Q0", at = at, validate = FALSE
  )
  if (isTRUE(grid_q0$is_intercept_only)) {
    log_q0_marginal <- beta_q0[1L]
  } else {
    log_q0_emms <- as.numeric(grid_q0$ref_X %*% beta_q0)
    log_q0_marginal <- mean(log_q0_emms)
  }

  grid_alpha <- .tmb_build_emm_ref_grid(
    object, param = "alpha", at = at, validate = FALSE
  )
  if (isTRUE(grid_alpha$is_intercept_only)) {
    log_alpha_marginal <- beta_alpha[1L]
  } else {
    log_alpha_emms <- as.numeric(grid_alpha$ref_X %*% beta_alpha)
    log_alpha_marginal <- mean(log_alpha_emms)
  }

  Q0 <- exp(log_q0_marginal)
  alpha_val <- exp(log_alpha_marginal)

  # Build the conditioned_on field describing the actual conditioning.
  cov_names <- object$param_info$continuous_covariates
  if (is.null(cov_names)) cov_names <- character(0)
  fe_factors <- unique(c(
    object$param_info$factors,
    object$param_info$factors_q0,
    object$param_info$factors_alpha
  ))
  fe_factors <- fe_factors[nzchar(fe_factors) & !is.na(fe_factors)]
  conditioned_on <- list()
  if (length(cov_names) > 0L) {
    cov_values <- vapply(cov_names, function(cv) {
      if (!is.null(at) && cv %in% names(at)) {
        as.numeric(at[[cv]][1])
      } else {
        mean(object$data[[cv]], na.rm = TRUE)
      }
    }, numeric(1))
    names(cov_values) <- cov_names
    conditioned_on$covariates <- cov_values
  }
  if (length(fe_factors) > 0L) {
    factor_treatment <- vector("list", length(fe_factors))
    names(factor_treatment) <- fe_factors
    for (f in fe_factors) {
      if (!is.null(at) && f %in% names(at)) {
        factor_treatment[[f]] <- as.character(at[[f]])
      } else {
        factor_treatment[[f]] <- "marginal"
      }
    }
    conditioned_on$factors <- factor_treatment
  }
  if (length(conditioned_on) == 0L) conditioned_on <- NULL

  if (has_k) {
    k_val <- .tmb_get_k(object)
    result <- beezdemand_calc_pmax_omax(
      model_type = "hs",
      params = list(alpha = alpha_val, q0 = Q0, k = k_val),
      param_scales = list(alpha = "natural", q0 = "natural", k = "natural")
    )
  } else if (identical(object$param_info$equation, "zben")) {
    # zben has no SND closed form; route through the engine's numerical
    # fallback instead (GH #19), which needs a price domain to search over.
    result <- beezdemand_calc_pmax_omax(
      model_type = "zben",
      params = list(alpha = alpha_val, q0 = Q0),
      param_scales = list(alpha = "natural", q0 = "natural"),
      price_obs = object$data[[object$param_info$x_var]]
    )
  } else {
    result <- beezdemand_calc_pmax_omax(
      model_type = "snd",
      params = list(alpha = alpha_val, q0 = Q0),
      param_scales = list(alpha = "natural", q0 = "natural")
    )
  }

  list(
    Pmax = result$pmax_model,
    Omax = result$omax_model,
    Qmax = result$q_at_pmax_model,
    elasticity_at_pmax = result$elasticity_at_pmax_model,
    method = result$method_model,
    # GH #19 follow-up: TRUE only for zben fits
    # whose numerical Pmax search hit its domain-expansion cap without
    # finding the true (interior) maximum; FALSE for analytic (hs/snd)
    # fits, which never reach that path.
    pmax_at_bound = isTRUE(result$is_boundary_model),
    conditioned_on = conditioned_on
  )
}
