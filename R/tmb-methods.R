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
#'   from \code{beezdemand_summary}).
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
  component[grepl("^logsigma_|^rho_", nms)] <- "variance"

  estimate_scale <- rep("log", length(nms))
  estimate_scale[grepl("^logsigma_|^rho_", nms)] <- "natural"

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
  n_re <- object$param_info$n_random_effects

  # Build table from coefficients
  sigma_b <- exp(coefs[["logsigma_b"]])
  sigma_e <- exp(coefs[["logsigma_e"]])

  rows <- list(
    data.frame(
      Component = "sigma_b (Q0 RE SD)",
      Estimate = sigma_b,
      stringsAsFactors = FALSE
    ),
    data.frame(
      Component = "sigma_e (Residual SD)",
      Estimate = sigma_e,
      stringsAsFactors = FALSE
    )
  )

  correlations <- NULL

  if (n_re == 2) {
    sigma_c <- exp(coefs[["logsigma_c"]])
    rho_bc <- tanh(coefs[["rho_bc_raw"]])

    rows <- c(rows, list(
      data.frame(
        Component = "sigma_c (alpha RE SD)",
        Estimate = sigma_c,
        stringsAsFactors = FALSE
      )
    ))

    correlations <- data.frame(
      Component = "rho_bc (Q0-alpha correlation)",
      Estimate = rho_bc,
      stringsAsFactors = FALSE
    )
  }

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
#' @param object A \code{beezdemand_tmb} object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Named numeric vector of fixed effect coefficients.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' coef(fit)
#' }
#'
#' @export
coef.beezdemand_tmb <- function(object, ...) {
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
#' These are the Cholesky-transformed deviations (`b_i` for Q0, `c_i` for
#' alpha), not standardized scores. To obtain the standardized random effects
#' (`u` matrix), access `object$tmb_obj` directly.
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param ... Additional arguments.
#'
#' @return Data frame with subject-level random effects.
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
  re_cols <- intersect(c("b_i", "c_i"), names(spars))
  out <- spars[, c("id", re_cols), drop = FALSE]
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

    if (!is.null(object$param_info$factors) &&
        length(object$param_info$factors) > 0) {
      warning(
        "Demand curve reflects reference level only. ",
        "Use get_demand_param_emms() for per-group curves.",
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

  # type == "response": subject-specific fitted values (vectorized)
  if (is.null(newdata)) {
    newdata <- object$data
  }

  spars <- object$subject_pars

  # Map each observation to its subject's parameters
  subj_ids <- as.character(newdata[[id_var]])
  subj_match <- match(subj_ids, spars$id)

  # For unknown subjects, fall back to population reference-level parameters
  beta_q0_idx <- which(names(coefs) == "beta_q0")
  beta_alpha_idx <- which(names(coefs) == "beta_alpha")
  pop_Q0 <- exp(coefs[beta_q0_idx[1]])
  pop_alpha <- exp(coefs[beta_alpha_idx[1]])

  n_unknown <- sum(is.na(subj_match))
  if (n_unknown > 0) {
    warning(
      sprintf("%d observation(s) from unknown subject(s); using population reference-level parameters.", n_unknown),
      call. = FALSE
    )
  }

  Q0_vec <- ifelse(is.na(subj_match), pop_Q0, spars$Q0[subj_match])
  alpha_vec <- ifelse(is.na(subj_match), pop_alpha, spars$alpha[subj_match])
  price_vec <- newdata[[x_var]]
  k_val <- if (has_k) .tmb_get_k(object) else NA

  fitted_vals <- .tmb_predict_equation(
    price_vec, Q0_vec, alpha_vec,
    k = k_val, log_q0 = log(Q0_vec),
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
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame with columns: id, b_i, c_i (if 2 RE), Q0, alpha, Pmax, Omax.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' head(get_subject_pars(fit))
#' }
#'
#' @export
get_subject_pars.beezdemand_tmb <- function(object, ...) {
  object$subject_pars
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

    if (!is.null(ids)) {
      spars <- spars[spars$id %in% ids, , drop = FALSE]
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
  component[grepl("^logsigma_|^rho_", nms)] <- "variance"

  estimate_scale <- rep("log", length(nms))
  estimate_scale[grepl("^logsigma_|^rho_", nms)] <- "natural"

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

  out |>
    dplyr::mutate(
      statistic = .data$estimate / .data$std.error,
      p.value = 2 * stats::pnorm(-abs(.data$statistic))
    )
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
  pred <- predict(x, newdata = newdata, type = "response")
  equation <- x$param_info$equation
  y_var <- x$param_info$y_var

  data_used <- if (is.null(newdata)) x$data else newdata
  y_obs <- data_used[[y_var]]

  # Compute residuals on model scale (matching C++ likelihood)
  if (equation == "exponential") {
    # Model operates on log(Q); y_obs is natural consumption.
    # Zero observations were dropped during fitting, so log(0) = -Inf.
    # Set residuals to NA for these observations.
    log_y <- ifelse(y_obs > 0, log(y_obs), NA_real_)
    pred$.resid <- log_y - pred$.fitted
  } else {
    # exponentiated/simplified/zben: y and fitted on same scale
    pred$.resid <- y_obs - pred$.fitted
  }

  # Standardized Pearson residuals: (y - mu) / sigma_e
  sigma_e <- exp(x$model$coefficients[["logsigma_e"]])
  pred$.std_resid <- pred$.resid / sigma_e

  pred
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


# --- EMMs and comparisons ---

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
#' @param factors_in_emm Character vector of factors to marginalize over.
#'   If NULL, uses all factors in the model.
#' @param at Named list specifying factor levels for conditional EMMs.
#' @param ci_level Numeric. Confidence level for intervals.
#' @param ... Additional arguments.
#'
#' @return A tibble with columns: level, estimate, std.error, conf.low, conf.high.
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

  # Build reference grid
  # For each factor level combination, create a design vector
  factors <- fit_obj$param_info$factors
  if (is.null(factors) || length(factors) == 0) {
    # No factors: just return the intercept
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

  # Use the appropriate factors for this param
  if (param == "Q0") {
    use_factors <- fit_obj$param_info$factors_q0
  } else {
    use_factors <- fit_obj$param_info$factors_alpha
  }
  if (!is.null(factors_in_emm)) {
    use_factors <- intersect(use_factors, factors_in_emm)
  }

  # Get unique levels from the data
  data_used <- fit_obj$data
  level_combos <- unique(data_used[, use_factors, drop = FALSE])

  # Build reference design matrix rows
  ref_X <- stats::model.matrix(
    stats::as.formula(build_fixed_rhs(
      factors = use_factors,
      factor_interaction = fit_obj$param_info$factor_interaction,
      data = data_used
    )),
    data = level_combos
  )

  # Apply 'at' filter
  if (!is.null(at)) {
    keep <- rep(TRUE, nrow(level_combos))
    for (nm in names(at)) {
      if (nm %in% names(level_combos)) {
        keep <- keep & (level_combos[[nm]] %in% at[[nm]])
      }
    }
    level_combos <- level_combos[keep, , drop = FALSE]
    ref_X <- ref_X[keep, , drop = FALSE]
  }

  z <- stats::qnorm((1 + ci_level) / 2)

  # Compute EMMs
  results <- lapply(seq_len(nrow(ref_X)), function(i) {
    x_ref <- ref_X[i, ]
    est <- sum(x_ref * beta)
    se <- sqrt(as.numeric(t(x_ref) %*% vcov_mat %*% x_ref))

    level_label <- paste(
      vapply(use_factors, function(f) {
        paste0(f, "=", level_combos[[f]][i])
      }, character(1)),
      collapse = ", "
    )

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

  emms <- get_demand_param_emms(fit_obj, param = param, ci_level = ci_level)

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

  # Build reference grid
  data_used <- fit_obj$data
  level_combos <- unique(data_used[, use_factors, drop = FALSE])
  ref_X <- stats::model.matrix(
    stats::as.formula(build_fixed_rhs(
      factors = use_factors,
      factor_interaction = fit_obj$param_info$factor_interaction,
      data = data_used
    )),
    data = level_combos
  )

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
#' @param ... Additional arguments (currently unused).
#'
#' @return A list with Pmax, Omax, Qmax, elasticity_at_pmax, and method.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#' calc_group_metrics(fit)
#' }
#'
#' @export
calc_group_metrics.beezdemand_tmb <- function(object, ...) {
  coefs <- object$model$coefficients
  equation <- object$param_info$equation
  has_k <- object$param_info$has_k

  # Population-level parameters (intercepts only)
  beta_q0_idx <- which(names(coefs) == "beta_q0")
  beta_alpha_idx <- which(names(coefs) == "beta_alpha")

  Q0 <- exp(coefs[beta_q0_idx[1]])
  alpha_val <- exp(coefs[beta_alpha_idx[1]])

  if (has_k) {
    k_val <- .tmb_get_k(object)
    model_type <- "hs"

    result <- beezdemand_calc_pmax_omax(
      model_type = model_type,
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
    method = result$method_model
  )
}
