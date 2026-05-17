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
#'   One of `"internal"`, `"natural"`, `"log10"`.
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
  component[q0_idx] <- "consumption"
  component[alpha_idx] <- "consumption"
  component[nms == "log_k"] <- "consumption"
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

  coefficients <- coefficients |>
    dplyr::mutate(
      statistic = .data$estimate / .data$std.error,
      p.value = 2 * stats::pnorm(-abs(.data$statistic))
    )

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

  # Individual parameter summaries
  spars <- object$subject_pars
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

  for (b in seq_len(bmap$n_blocks)) {
    d_q0 <- bmap$block_q0_dim[b]
    d_alpha <- bmap$block_alpha_dim[b]
    d <- d_q0 + d_alpha
    if (d == 0L) next

    # Per-RE-column SDs.
    block_label_prefix <- if (bmap$n_blocks > 1L) sprintf("block%d ", b) else ""
    pdmat_label <- if (bmap$block_types[b] == 1L) "pdSymm" else "pdDiag"
    if (d_q0 > 0L) {
      for (j in seq_len(d_q0)) {
        nm <- if (d_q0 == 1L) "sigma_b (Q0 RE SD)" else
              sprintf("sigma_b[%s%d] (Q0 RE SD)", block_label_prefix, j)
        rows[[length(rows) + 1L]] <- data.frame(
          Component = nm,
          Estimate = exp(logsigma_full[sigma_offset + j]) / ln10,
          stringsAsFactors = FALSE
        )
      }
    }
    if (d_alpha > 0L) {
      for (j in seq_len(d_alpha)) {
        nm <- if (d_alpha == 1L) "sigma_c (alpha RE SD)" else
              sprintf("sigma_c[%s%d] (alpha RE SD)", block_label_prefix, j)
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
    # Codex round 5 caught the prior code reporting tanh(rho_raw)
    # directly as rho[j,k], which is a silent statistical wrong answer
    # for any pdSymm block of size > 2.
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

      cor_rows <- list()
      for (j in 2L:d) {
        for (k in seq_len(j - 1L)) {
          marginal_r <- R_corr[j, k]
          nm <- if (d_q0 == 1L && d_alpha == 1L && d == 2L) {
            "rho_bc (Q0-alpha correlation)"
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

#' Extract Coefficients from TMB Model
#'
#' Returns the optimizer's flat parameterization as a named numeric vector
#' (entries include `beta_q0`, `beta_alpha`, `logsigma_e`, and any random-
#' effect or covariance hyperparameters; intercepts are on log scale because
#' the optimizer works in unconstrained space).
#'
#' `type = "internal"` is the current and only supported value; it is exposed
#' as a forward-compatible alias for the per-subject tibble outputs planned
#' under TICKET-019 (where `coef(fit)` will default to a per-subject tibble
#' and `type = "internal"` will be preserved as the numeric-vector escape
#' hatch consumed by `car::deltaMethod`, `multcomp::glht`, and similar
#' tooling that expects a flat coefficient vector).
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param type Currently only `"internal"`. Reserved for the per-subject
#'   tibble outputs planned under TICKET-019.
#' @param ... Additional arguments (currently unused).
#'
#' @return Named numeric vector of fixed-effect coefficients on the
#'   optimizer's internal parameterization.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' coef(fit)
#' coef(fit, type = "internal")  # explicit equivalent
#' }
#'
#' @export
coef.beezdemand_tmb <- function(object, type = c("internal"), ...) {
  type <- match.arg(type)
  object$model$coefficients
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
  coef(object)
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
#'     \item `id` — subject identifier
#'     \item `b_i`, `c_i` (when present) — first-column convenience aliases
#'       for `q0_(Intercept)` and `alpha_(Intercept)`. Preserved for
#'       backward compatibility with older callers.
#'     \item `q0_<term>` — per-block random-effect coefficients for log-Q0,
#'       one column per random-effects design column from the parsed
#'       block structure. For factor-expanded or multi-block fits, these
#'       expose the per-condition slope REs that `b_i` / `c_i` alone do
#'       not surface.
#'     \item `alpha_<term>` — analogous columns for log-alpha.
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
#' @param ... Additional arguments.
#'
#' @return Depends on `type`:
#'   - `"response"`: tibble with .fitted column
#'   - `"parameters"`: tibble of subject-specific parameters
#'   - `"demand"`: tibble with price and .fitted columns
#'
#' @note Population-averaged (marginal) predictions integrating over the
#'   random effects distribution are not yet implemented for this model tier.
#'   The `type = "demand"` prediction uses RE = 0 (population fixed effects
#'   only). For marginal integration accounting for Jensen's inequality, use
#'   [predict.beezdemand_hurdle()] with `marginal = TRUE`.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#'
#' # Fitted values
#' head(predict(fit, type = "response"))
#'
#' # Population demand curve at specific prices
#' predict(fit, type = "demand", prices = c(0, 1, 5, 10, 20))
#'
#' # Subject-level parameters
#' head(predict(fit, type = "parameters"))
#' }
#'
#' @export
predict.beezdemand_tmb <- function(
  object,
  newdata = NULL,
  type = c("response", "parameters", "demand"),
  prices = NULL,
  scale = c("model", "natural"),
  correction = TRUE,
  ...
) {
  type <- match.arg(type)
  scale <- match.arg(scale)

  if (type == "parameters") {
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

  # type == "response": subject-specific fitted values (vectorized).
  # Rebuild fixed-effect linear predictors from newdata so that factor and
  # continuous-covariate values in newdata propagate into Q0 and alpha
  # (codex Bug 3 fix).
  if (is.null(newdata)) {
    newdata <- object$data
  }

  bp <- .tmb_build_predicted_pars(object, newdata)
  price_vec <- newdata[[x_var]]
  k_val <- if (has_k) .tmb_get_k(object) else NA

  fitted_vals <- .tmb_predict_equation(
    price_vec, bp$Q0, bp$alpha,
    k = k_val, log_q0 = bp$log_q0,
    equation = equation
  )

  ## Back-transform to natural scale if requested
  if (scale == "natural") {
    se <- exp(coefs[["logsigma_e"]])
    fitted_vals <- .tmb_backtransform(fitted_vals, equation, sigma_e = se,
                                      correction = correction)
  }

  out <- tibble::as_tibble(newdata)
  out$.fitted <- fitted_vals
  out
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
#' random-effect deviate (or zero for unknown subjects), and return
#' `Q0 = exp(eta_q0)` and `alpha = exp(eta_alpha)`. This is what makes
#' `predict()` respect factor and continuous-covariate values in `newdata`.
#'
#' @param object A `beezdemand_tmb` fit.
#' @param newdata A data frame with the modeling columns used at fit time
#'   (`id_var`, `x_var`, factor columns, continuous covariate columns).
#' @return A list with elements `Q0`, `alpha`, and `log_q0` (each of length
#'   `nrow(newdata)`).
#' @keywords internal
.tmb_build_predicted_pars <- function(object, newdata) {
  pinfo <- object$param_info
  spars <- object$subject_pars
  coefs <- object$model$coefficients

  beta_q0    <- unname(coefs[names(coefs) == "beta_q0"])
  beta_alpha <- unname(coefs[names(coefs) == "beta_alpha"])
  n_re       <- pinfo$n_random_effects

  # 1. Validate required columns are present. Phase 2 also requires
  # variables that appear only in the RE formula RHS (not in `factors`):
  # without them, .tmb_build_z_matrices() in step 4 below crashes with
  # cryptic `model.matrix()` errors. Codex round 6.
  re_parsed_pre <- pinfo$random_effects_parsed
  re_rhs_vars_pre <- character(0)
  if (!is.null(re_parsed_pre)) {
    for (b in re_parsed_pre$blocks) {
      rhs_form <- stats::as.formula(paste("~", deparse1(b$formula[[3]])))
      re_rhs_vars_pre <- c(re_rhs_vars_pre, all.vars(rhs_form))
    }
    re_rhs_vars_pre <- unique(re_rhs_vars_pre)
  }
  needed <- unique(c(pinfo$id_var, pinfo$x_var,
                     pinfo$factors_q0, pinfo$factors_alpha,
                     pinfo$continuous_covariates, re_rhs_vars_pre))
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
  # Phase 2 / Codex rounds 6 + 7.
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

  # 4. Add per-subject random-effect deviates (or zero for unknowns).
  subj_ids <- as.character(newdata[[pinfo$id_var]])
  subj_match <- match(subj_ids, spars$id)
  n_unknown <- sum(is.na(subj_match))
  if (n_unknown > 0) {
    cli::cli_warn(
      "{n_unknown} observation{?s} from unknown subject{?s}; using {.arg newdata} fixed effects with random effects = 0."
    )
  }
  # Phase 2 fix (Codex round 5): for factor-expanded RE specs the
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

  log_q0_total    <- log_q0_fix    + re_q0_contrib
  log_alpha_total <- log_alpha_fix + re_alpha_contrib
  list(
    Q0     = exp(log_q0_total),
    alpha  = exp(log_alpha_total),
    log_q0 = log_q0_total
  )
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

#' Get Subject-Specific Parameters from TMB Model
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param expanded Logical. When \code{FALSE} (default) returns the
#'   wide one-row-per-subject table. When \code{TRUE}, returns a long
#'   table with one row per (subject, within-subject-factor-level)
#'   combination, with model-derived per-cell \code{Q0}, \code{alpha},
#'   \code{Pmax}, and \code{Omax}. Use this for fits where a within-
#'   subject factor appears in \code{factors} or in
#'   \code{random_effects} (e.g. multi-block \code{pdBlocked} specs);
#'   the wide default returns \code{NA} in those columns because no
#'   single subject-level value is well-defined.
#' @param ... Additional arguments (currently unused).
#'
#' @return When \code{expanded = FALSE}: data frame with columns
#'   \code{id}, \code{b_i}, \code{c_i} (if 2 RE), \code{Q0},
#'   \code{alpha}, \code{Pmax}, \code{Omax}. When \code{expanded = TRUE}:
#'   data frame with the within-subject factor columns added, one row per
#'   (subject, factor-level) combination.
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
get_subject_pars.beezdemand_tmb <- function(object, expanded = FALSE, ...) {
  if (!isTRUE(expanded)) {
    return(object$subject_pars)
  }

  pinfo <- object$param_info
  data <- object$data
  spars <- object$subject_pars
  re_parsed <- pinfo$random_effects_parsed
  id_var <- pinfo$id_var

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
        "i" = "Pass {.code expanded = FALSE} (default) for the wide NA-fill, or pre-process the variable into a factor or numeric before fitting."
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
        cell_rows[[v]] <- mean(subj_rows[[v]], na.rm = TRUE)
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
    model_type <- "snd"
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
  out$Q0 <- Q0
  out$alpha <- alpha
  out$Pmax <- omax_pmax$pmax_model
  out$Omax <- omax_pmax$omax_model

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
#'   uses `"pseudo_log"` which handles zero values gracefully.
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
#' @param report_space Character. One of `"natural"`, `"log10"`, `"internal"`.
#' @param ... Additional arguments.
#'
#' @return A tibble of model coefficients.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' tidy(fit)
#' tidy(fit, report_space = "log10")
#' }
#'
#' @export
tidy.beezdemand_tmb <- function(
  x,
  report_space = c("natural", "log10", "internal"),
  ...
) {
  report_space <- match.arg(report_space)

  coefs <- x$model$coefficients
  se <- x$model$se
  nms <- names(coefs)

  # Create term names
  tn <- .tmb_build_term_names(x, nms)
  term <- tn$term
  q0_idx <- tn$q0_idx
  alpha_idx <- tn$alpha_idx

  # Determine component
  component <- character(length(nms))
  component[q0_idx] <- "consumption"
  component[alpha_idx] <- "consumption"
  component[nms == "log_k"] <- "consumption"
  # Match bare `logsigma` (Phase 2 vector parameter) as well as
  # `logsigma_e` and the legacy `logsigma_b` / `logsigma_c` names.
  component[grepl("^logsigma($|_)|^rho_", nms)] <- "variance"

  estimate_scale <- rep("log", length(nms))
  estimate_scale[grepl("^logsigma($|_)|^rho_", nms)] <- "natural"

  z_val <- coefs / se
  p_val <- 2 * stats::pnorm(-abs(z_val))

  out <- tibble::tibble(
    term = term,
    estimate = unname(coefs),
    std.error = unname(se),
    statistic = unname(z_val),
    p.value = unname(p_val),
    component = component,
    estimate_scale = estimate_scale,
    term_display = term
  )

  out <- beezdemand_transform_coef_table(
    coef_tbl = out,
    report_space = report_space,
    internal_space = "natural"
  )

  out <- out |>
    dplyr::mutate(
      statistic = .data$estimate / .data$std.error,
      p.value = 2 * stats::pnorm(-abs(.data$statistic))
    )

  if (isFALSE(x$hessian_pd)) {
    attr(out, "hessian_warning") <- paste0(
      "Hessian is not positive definite (pdHess = FALSE). ",
      "Standard errors, p-values, and confidence intervals may be unreliable."
    )
  }

  out
}


#' Glance at a beezdemand_tmb Model
#'
#' @param x A \code{beezdemand_tmb} object.
#' @param ... Additional arguments.
#'
#' @return A one-row tibble of model-level statistics.
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
    equation = x$param_info$equation,
    nobs = x$param_info$n_obs,
    n_subjects = x$param_info$n_subjects,
    n_random_effects = x$param_info$n_random_effects,
    converged = x$converged,
    logLik = x$loglik,
    AIC = x$AIC,
    BIC = x$BIC
  )
}


# Internal: compute fitted values and (response) residuals on a requested
# scale. Centralizes the scale convention shared by fitted(), residuals(),
# and augment() so the three accessors cannot drift apart.
.tmb_fitted_resid <- function(x,
                              scale = c("model", "natural"),
                              level = c("subject", "population"),
                              newdata = NULL) {
  scale <- match.arg(scale)
  level <- match.arg(level)
  # predict.beezdemand_tmb() honors `scale`; `level` is reserved for the
  # forthcoming TICKET-014 enhancement. The only supported value today is
  # "subject"; warn and proceed for "population".
  if (level == "population") {
    cli::cli_inform(
      "{.code level = \"population\"} not yet implemented; returning subject-level values (TICKET-014)."
    )
  }
  data_used <- if (is.null(newdata)) x$data else newdata
  pred <- predict(x, newdata = data_used, type = "response", scale = scale)
  equation <- x$param_info$equation
  y_var <- x$param_info$y_var
  y_obs <- data_used[[y_var]]
  if (scale == "model" && equation == "exponential") {
    # Model is on log scale; y_obs is natural. Zero rows are NA on log scale.
    y_on_scale <- ifelse(y_obs > 0, log(y_obs), NA_real_)
  } else {
    # exponentiated/simplified/zben on model scale (already natural), OR
    # any equation on the natural scale.
    y_on_scale <- y_obs
  }
  list(.fitted = pred$.fitted, .resid = y_on_scale - pred$.fitted)
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
#' @param level Reserved for TICKET-014. Currently `"subject"` only.
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
#' @param level Reserved for TICKET-014. Currently `"subject"` only.
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
#'   alone -- pass genuinely nested models. Rows of the multiple-fit table
#'   are ordered by ascending degrees of freedom, and the \code{Model}
#'   column labels them \code{Model1}, \code{Model2}, ... in that order.
#'
#' @seealso [anova.beezdemand_nlme()], [confint.beezdemand_tmb()].
#' @examples
#' \donttest{
#' data(apt_full)
#' fit <- fit_demand_tmb(apt_full, equation = "exponential",
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
#' @param ... Additional arguments.
#'
#' @return A tibble with term, estimate, conf.low, conf.high, level.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' confint(fit)
#' confint(fit, report_space = "natural")
#' }
#'
#' @export
confint.beezdemand_tmb <- function(
  object,
  parm = NULL,
  level = 0.95,
  report_space = c("internal", "natural"),
  ...
) {
  report_space <- match.arg(report_space)

  coefs <- object$model$coefficients
  se_vec <- object$model$se
  nms <- names(coefs)

  # Build display names first (before filtering) so parm can match either

  tn <- .tmb_build_term_names(object, nms)
  term <- tn$term

  if (!is.null(parm)) {
    # Match against display names first, then fall back to raw names
    keep <- term %in% parm | nms %in% parm
    coefs <- coefs[keep]
    se_vec <- se_vec[keep]
    nms <- nms[keep]
    term <- term[keep]
  }

  z <- stats::qnorm((1 + level) / 2)

  # Re-derive indices for the (possibly filtered) vector
  q0_idx <- which(nms == "beta_q0")
  alpha_idx <- which(nms == "beta_alpha")

  estimates <- coefs
  conf_low <- coefs - z * se_vec
  conf_high <- coefs + z * se_vec

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
#' `calc_group_metrics`) can validate ONCE at the top of the call —
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
#'   and factors_alpha — appropriate for callers that build BOTH grids
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
    use_factors <- intersect(use_factors, factors_in_emm)
  }

  # `at` validation: catch typos and bad values BEFORE grid construction.
  # Public-facing functions that call this helper twice (e.g.
  # calc_group_metrics() builds Q0 and alpha grids in one user call)
  # validate ONCE at their entry point and pass `validate = FALSE` so
  # the multi-value continuous warning fires only once per public call.
  if (isTRUE(validate)) {
    .tmb_validate_at(fit_obj, at, param_scope = param)
  }

  is_intercept_only <- length(use_factors) == 0L && length(cov_names) == 0L

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
  if (length(use_factors) > 0L) {
    level_combos <- unique(data_used[, use_factors, drop = FALSE])
  } else {
    level_combos <- data_used[1L, integer(0), drop = FALSE]
  }

  # Continuous covariates: hold at training mean unless overridden via `at`.
  # Multi-value `at` for continuous covariates emits a one-shot warning
  # (above) and uses the first value here — same convention emmeans uses
  # when its `at` argument supplies a vector.
  if (length(cov_names) > 0L) {
    for (cv in cov_names) {
      cv_value <- mean(data_used[[cv]], na.rm = TRUE)
      if (!is.null(at) && cv %in% names(at)) {
        cv_value <- as.numeric(at[[cv]][1])
      }
      level_combos[[cv]] <- cv_value
    }
  }

  ref_X <- stats::model.matrix(
    stats::as.formula(build_fixed_rhs(
      factors = use_factors,
      factor_interaction = fit_obj$param_info$factor_interaction,
      continuous_covariates = cov_names,
      data = data_used
    )),
    data = level_combos
  )

  # Apply factor-level `at` filter; covariate values were substituted above.
  if (!is.null(at) && length(use_factors) > 0L) {
    keep <- rep(TRUE, nrow(level_combos))
    for (nm in names(at)) {
      if (nm %in% use_factors) {
        keep <- keep & (as.character(level_combos[[nm]]) %in% as.character(at[[nm]]))
      }
    }
    level_combos <- level_combos[keep, , drop = FALSE]
    ref_X <- ref_X[keep, , drop = FALSE]
    if (nrow(level_combos) == 0L) {
      cli::cli_abort(c(
        "{.arg at} filter produced an empty reference grid.",
        "i" = "Check that the supplied factor levels exist in the data and are not mutually exclusive."
      ))
    }
  }

  list(
    level_combos = level_combos,
    ref_X = ref_X,
    use_factors = use_factors,
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
#' @param factors_in_emm Character vector of factors to include in the EMM
#'   reference grid. Must include *every* factor the model was fit on; any
#'   subset that drops a fitted factor is rejected with a clear error.
#'   Proper marginalization over omitted factors is planned for TICKET-011
#'   Phase 5. If `NULL` (default), all fitted factors are used.
#' @param at Named list specifying factor levels and continuous-covariate
#'   values for conditional EMMs. For continuous covariates, a single
#'   numeric value per covariate; multiple values produce a warning and
#'   only the first is used.
#' @param ci_level Numeric. Confidence level for intervals.
#' @param ... Additional arguments.
#'
#' @return A tibble with columns: level, estimate, std.error, conf.low, conf.high.
#'
#' @note TMB EMMs require `factors_in_emm` to include every fitted factor.
#'   Use `fit_demand_mixed()` (NLME backend) if you need to marginalize over
#'   a subset of factors while this gap is closed (see TICKET-011 Phase 5).
#'
#' @examples
#' \donttest{
#' data(apt_full)
#' dat <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
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

  # Dimension guard: the fitted beta spans the full design from `factors` +
  # `continuous_covariates`, so the reference grid must share that basis.
  # When `factors_in_emm` drops any fitted factor, `ref_X` has fewer columns
  # than `beta`, and downstream `sum(x_ref * beta)` would silently recycle
  # the shorter vector. Reject explicitly; proper marginalization over
  # omitted factors is planned for TICKET-011 Phase 5.
  if (ncol(ref_X) != length(beta)) {
    fitted_for_param <- if (param == "Q0") {
      fit_obj$param_info$factors_q0
    } else {
      fit_obj$param_info$factors_alpha
    }
    if (is.null(fitted_for_param)) fitted_for_param <- character(0)
    cli::cli_abort(c(
      "{.arg factors_in_emm} must include every fitted factor for {.field {param}}.",
      "i" = "Fitted factors: {.val {fitted_for_param}}.",
      "i" = "Requested: {.val {factors_in_emm}}.",
      "x" = "Marginalization over omitted factors is not yet supported for TMB fits (planned in TICKET-011 Phase 5)."
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
#' Computes pairwise contrasts between factor levels for demand parameters
#' from a `beezdemand_tmb` model.
#'
#' @param fit_obj A \code{beezdemand_tmb} object.
#' @param param Character. Which parameter: `"Q0"` or `"alpha"`.
#' @param contrast_type Character. Type of contrast: `"pairwise"` or `"trt.vs.ctrl"`.
#' @param p_adjust Character. P-value adjustment method (default `"holm"`).
#' @param ci_level Numeric. Confidence level.
#' @param ... Additional arguments.
#'
#' @return A tibble with contrast results.
#'
#' @examples
#' \donttest{
#' data(apt_full)
#' dat <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
#' fit <- fit_demand_tmb(dat, equation = "exponential",
#'                       factors = "gender", verbose = 0)
#' get_demand_comparisons(fit, param = "Q0")
#' }
#'
#' @export
get_demand_comparisons.beezdemand_tmb <- function(
  fit_obj,
  param = c("Q0", "alpha"),
  contrast_type = c("pairwise", "trt.vs.ctrl"),
  p_adjust = "holm",
  ci_level = 0.95,
  ...
) {
  param <- match.arg(param)
  contrast_type <- match.arg(contrast_type)

  # Forward `...` (notably `at` and `factors_in_emm`) so callers can
  # condition the contrast reference grid on specific factor levels or
  # covariate values. Before this, these args were silently dropped.
  emms <- get_demand_param_emms(
    fit_obj,
    param = param,
    ci_level = ci_level,
    ...
  )

  if (nrow(emms) < 2) {
    message("Fewer than 2 levels; no contrasts to compute.")
    return(tibble::tibble(
      contrast = character(),
      estimate = numeric(),
      std.error = numeric(),
      statistic = numeric(),
      p.value = numeric()
    ))
  }

  # Get beta and vcov
  coefs <- fit_obj$model$coefficients
  sdr <- fit_obj$sdr

  if (param == "Q0") {
    beta_idx <- which(names(coefs) == "beta_q0")
    beta <- coefs[beta_idx]
    use_factors <- fit_obj$param_info$factors_q0
  } else {
    beta_idx <- which(names(coefs) == "beta_alpha")
    beta <- coefs[beta_idx]
    use_factors <- fit_obj$param_info$factors_alpha
  }

  # vcov
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
    se_vals <- fit_obj$model$se[beta_idx]
    vcov_mat <- diag(se_vals^2, nrow = length(se_vals))
  }

  # Build the same conditioned reference grid the EMM call above used.
  # Re-extract `at` and `factors_in_emm` from `...` so the helper sees the
  # same conditioning that produced `emms` (TICKET-011 Phase 0.4 — Codex
  # rounds 2-4 flagged this drift as silent statistical corruption when
  # `at` filters factor levels: ref_X had more rows than emms, so the
  # pairwise loop produced off-grid contrasts and "NA" labels).
  # `at` was already validated by get_demand_param_emms() above; skip
  # re-validation so the multi-value warning fires exactly once.
  dots <- list(...)
  grid <- .tmb_build_emm_ref_grid(
    fit_obj,
    param = param,
    at = dots$at,
    factors_in_emm = dots$factors_in_emm,
    validate = FALSE
  )

  if (grid$is_intercept_only) {
    # Intercept-only fit: no factor levels to contrast.
    return(tibble::tibble(
      contrast = character(),
      estimate = numeric(),
      std.error = numeric(),
      statistic = numeric(),
      p.value = numeric()
    ))
  }

  level_combos <- grid$level_combos
  ref_X <- grid$ref_X
  cov_names <- grid$cov_names

  n_levels <- nrow(ref_X)
  z <- stats::qnorm((1 + ci_level) / 2)

  # Pairwise contrasts on log scale
  contrasts <- list()
  if (contrast_type == "pairwise") {
    for (i in seq_len(n_levels - 1)) {
      for (j in (i + 1):n_levels) {
        diff_x <- ref_X[i, ] - ref_X[j, ]
        est_diff <- sum(diff_x * beta)
        se_diff <- sqrt(as.numeric(t(diff_x) %*% vcov_mat %*% diff_x))
        z_stat <- est_diff / se_diff
        p_raw <- 2 * stats::pnorm(-abs(z_stat))

        label_i <- emms$level[i]
        label_j <- emms$level[j]

        contrasts[[length(contrasts) + 1]] <- tibble::tibble(
          contrast = paste(label_i, "-", label_j),
          estimate_log = est_diff,
          estimate_ratio = exp(est_diff),
          std.error = se_diff,
          statistic = z_stat,
          p.value.raw = p_raw
        )
      }
    }
  } else {
    # trt.vs.ctrl: compare all to first level
    for (j in 2:n_levels) {
      diff_x <- ref_X[j, ] - ref_X[1, ]
      est_diff <- sum(diff_x * beta)
      se_diff <- sqrt(as.numeric(t(diff_x) %*% vcov_mat %*% diff_x))
      z_stat <- est_diff / se_diff
      p_raw <- 2 * stats::pnorm(-abs(z_stat))

      contrasts[[length(contrasts) + 1]] <- tibble::tibble(
        contrast = paste(emms$level[j], "-", emms$level[1]),
        estimate_log = est_diff,
        estimate_ratio = exp(est_diff),
        std.error = se_diff,
        statistic = z_stat,
        p.value.raw = p_raw
      )
    }
  }

  result <- dplyr::bind_rows(contrasts)

  # P-value adjustment
  result$p.value <- stats::p.adjust(result$p.value.raw, method = p_adjust)
  result$conf.low <- exp(result$estimate_log - z * result$std.error)
  result$conf.high <- exp(result$estimate_log + z * result$std.error)

  result
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
#' This is "metrics evaluated at the average parameter values," NOT
#' "average metrics across cells" -- the two answers differ for nonlinear
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
    conditioned_on = conditioned_on
  )
}
