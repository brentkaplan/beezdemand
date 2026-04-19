#' Print Method for Hurdle Demand Model
#'
#' @param x An object of class \code{beezdemand_hurdle}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.beezdemand_hurdle <- function(x, ...) {
  cat("\nTwo-Part Mixed Effects Hurdle Demand Model\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat("Convergence:", ifelse(x$converged, "Yes", "No"), "\n")
  cat("Number of subjects:", x$param_info$n_subjects, "\n")
  cat("Number of observations:", x$param_info$n_obs, "\n")
  cat(
    "Random effects:",
    x$param_info$n_random_effects,
    paste0("(", paste(x$param_info$random_effects_spec, collapse = ", "), ")"),
    "\n"
  )
  cat("\nFixed Effects:\n")
  print(round(x$model$coefficients, 4))
  cat("\nUse summary() for full results.\n")

  invisible(x)
}


#' Summarize a Hurdle Demand Model Fit
#'
#' @description
#' Provides a summary of a fitted hurdle demand model, including fixed effects,
#' variance components, correlations, and fit statistics.
#'
#' @param object An object of class \code{beezdemand_hurdle} from
#'   \code{\link{fit_demand_hurdle}}.
#' @param report_space Character. Reporting space for core demand parameters.
#'   One of:
#'   - `"internal"`: report internal/fitting parameters (default internal naming)
#'   - `"natural"`: report natural-scale parameters when a natural mapping exists
#'   - `"log10"`: report `log10()`-scale parameters when a mapping exists
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{summary.beezdemand_hurdle} (also inherits
#'   from \code{beezdemand_summary}) containing:
#' \describe{
#'   \item{call}{The original function call}
#'   \item{model_class}{"beezdemand_hurdle"}
#'   \item{backend}{"TMB"}
#'   \item{coefficients}{Tibble of fixed effects with estimates, SEs, z-values, p-values}
#'   \item{coefficients_matrix}{Matrix form for printing (legacy compatibility)}
#'   \item{variance_components}{Matrix of variance/covariance estimates}
#'   \item{correlations}{Matrix of correlation estimates}
#'   \item{n_subjects}{Number of subjects}
#'   \item{nobs}{Number of observations}
#'   \item{converged}{Logical indicating convergence}
#'   \item{logLik}{Log-likelihood at convergence}
#'   \item{AIC}{Akaike Information Criterion}
#'   \item{BIC}{Bayesian Information Criterion}
#'   \item{group_metrics}{Group-level Pmax and Omax}
#'   \item{individual_metrics}{Summary of individual-level parameters}
#'   \item{notes}{Character vector of warnings/notes}
#' }
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' summary(fit)
#' }
#'
#' @export
summary.beezdemand_hurdle <- function(
  object,
  report_space = c("natural", "log10", "internal"),
  ...
) {
  report_space <- match.arg(report_space)
  # Normalize coefficient names (older versions used `logQ0`)
  coefs <- object$model$coefficients
  se_vec <- object$model$se
  if ("logQ0" %in% names(coefs) && !("log_q0" %in% names(coefs))) {
    names(coefs)[names(coefs) == "logQ0"] <- "log_q0"
  }
  if ("logQ0" %in% names(se_vec) && !("log_q0" %in% names(se_vec))) {
    names(se_vec)[names(se_vec) == "logQ0"] <- "log_q0"
  }

  # Guard against SE = 0 (boundary parameters, e.g. collapsed variance)
  boundary_mask <- se_vec == 0 & !is.na(se_vec)
  if (any(boundary_mask)) {
    boundary_params <- names(se_vec)[boundary_mask]
    cli::cli_warn(c(
      "Standard error is zero for {length(boundary_params)} parameter{?s}: {.val {boundary_params}}.",
      "i" = "This typically indicates a parameter estimated at its boundary (e.g., variance collapsed to zero).",
      "i" = "Consider simplifying the model or checking data quality."
    ))
  }
  # TMB uses Laplace approximation, so asymptotic z-tests are the right inference
  # (matches glmmTMB convention). The column is labelled "z value" (not the
  # historical "t value") to keep the label consistent with the pnorm-based
  # p-value computation below (TICKET-006).
  z_val <- ifelse(se_vec > 0, coefs / se_vec, NA_real_)

  # Build coefficient table (matrix form for printing)
  coef_matrix <- cbind(
    Estimate = coefs,
    `Std. Error` = se_vec,
    `z value` = z_val
  )

  # Build coefficient tibble (for contract compliance)
  coef_names <- names(coefs)
  p_val <- 2 * stats::pnorm(-abs(z_val))

  # Determine component for each coefficient
  component <- dplyr::case_when(
    coef_names %in% c("beta0", "beta1", "gamma0", "gamma1") ~ "zero_probability",
    coef_names %in% c("log_q0", "log_alpha", "log_k", "k", "alpha") ~ "consumption",
    grepl("^logsigma_|^rho_", coef_names) ~ "variance",
    TRUE ~ "fixed"
  )

  term <- dplyr::case_when(
    coef_names == "log_q0" ~ "Q0",
    coef_names == "log_alpha" ~ "alpha",
    coef_names == "log_k" ~ "k",
    TRUE ~ coef_names
  )

  coefficients <- tibble::tibble(
    term = term,
    estimate = unname(coefs),
    std.error = unname(se_vec),
    statistic = unname(z_val),
    p.value = unname(p_val),
    component = component,
	    estimate_scale = dplyr::case_when(
	      coef_names %in% c("beta0", "beta1", "gamma0", "gamma1") ~ "logit",
	      coef_names %in% c("log_q0", "log_alpha", "log_k", "alpha") ~ "log",
	      TRUE ~ "natural"
	    ),
	    term_display = term
	  )

  coefficients <- beezdemand_transform_coef_table(
    coef_tbl = coefficients,
    report_space = report_space,
    internal_space = "natural"
  )

  coefficients <- coefficients |>
    dplyr::mutate(
      statistic = ifelse(.data$std.error > 0, .data$estimate / .data$std.error, NA_real_),
      p.value = 2 * stats::pnorm(-abs(.data$statistic))
    )

  # Compute group-level demand metrics (Omax, Pmax) via unified engine
  group_metrics <- calc_group_metrics(object)

  # Conditional (Part-II only) metrics — long-standing meaning of $Pmax/$Omax.
  conditional_metrics <- tibble::tibble(
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

  # Unconditional metrics, integrating the Part-I zero-inflation logistic
  # into the expenditure curve (TICKET-003). Reported alongside conditional
  # metrics so users can see both at a glance.
  unconditional_metrics <- tibble::tibble(
    metric = c("pmax_unconditional", "omax_unconditional", "p_zero_at_pmax"),
    estimate = c(group_metrics$Pmax_unconditional,
                 group_metrics$Omax_unconditional,
                 group_metrics$p_zero_at_pmax),
    std.error = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    method = group_metrics$method_unconditional %||% "unknown",
    component = "unconditional",
    level = "population",
    id = NA_character_
  )

  derived_metrics <- dplyr::bind_rows(
    beezdemand_empty_derived_metrics(),
    conditional_metrics,
    unconditional_metrics
  )

  # Strategy B alpha* (normalized alpha; depends on alpha and k)
  notes <- character(0)
  if (isFALSE(object$hessian_pd)) {
    notes <- c(notes,
      "Warning: Hessian not positive definite \u2014 standard errors may be unreliable."
    )
  }
  part2 <- object$param_info$part2 %||% "zhao_exponential"
  if (!identical(part2, "simplified_exponential") &&
    all(c("log_alpha", "log_k") %in% names(coefs))) {
    vc <- NULL
    if (!is.null(object$sdr) && !is.null(object$sdr$cov.fixed)) {
      vc_try <- tryCatch(as.matrix(object$sdr$cov.fixed), error = function(e) NULL)
      if (!is.null(vc_try) &&
        all(c("log_alpha", "log_k") %in% colnames(vc_try))) {
        vc <- vc_try[c("log_alpha", "log_k"), c("log_alpha", "log_k"), drop = FALSE]
      }
    }

    if (is.null(vc)) {
      se_theta <- object$model$se
      vc <- c(log_alpha = se_theta[["log_alpha"]], log_k = se_theta[["log_k"]])
    }

    alpha_star_res <- .calc_alpha_star(
      params = list(log_alpha = unname(coefs[["log_alpha"]]), log_k = unname(coefs[["log_k"]])),
      param_scales = list(log_alpha = "log", log_k = "log"),
      vcov = vc,
      base = "e"
    )

    derived_metrics <- dplyr::bind_rows(
      derived_metrics,
      tibble::tibble(
        metric = "alpha_star",
        estimate = alpha_star_res$estimate,
        std.error = alpha_star_res$se,
        conf.low = NA_real_,
        conf.high = NA_real_,
        method = "delta_method",
        component = "consumption",
        level = "population",
        id = NA_character_
      )
    )

    if (!is.null(alpha_star_res$note) && nzchar(alpha_star_res$note)) {
      notes <- c(notes, paste0("alpha_star: ", alpha_star_res$note))
    }
  }
  
  # Add method metadata
  pmax_method_info <- list(
    method_model = group_metrics$method,
    is_boundary_model = group_metrics$is_boundary,
    unit_elasticity_pass_model = group_metrics$unit_elasticity_pass,
    note_model = group_metrics$note
  )

  # Compute summary of individual-level metrics
  individual_metrics <- list(
    Q0 = summary(object$subject_pars$Q0),
    alpha = summary(object$subject_pars$alpha),
    breakpoint = summary(object$subject_pars$breakpoint),
    Pmax = summary(object$subject_pars$Pmax[is.finite(
      object$subject_pars$Pmax
    )]),
    Omax = summary(object$subject_pars$Omax[is.finite(
      object$subject_pars$Omax
    )])
  )

  structure(
    list(
      call = object$call,
      model_class = "beezdemand_hurdle",
      backend = "TMB_hurdle",
      param_space = object$param_space %||% "natural",
      report_space = report_space,
      coefficients = coefficients,
      coefficients_matrix = coef_matrix,
      variance_components = object$model$variance_components,
      correlations = object$model$correlations,
      n_subjects = object$param_info$n_subjects,
      nobs = object$param_info$n_obs,
      n_random_effects = object$param_info$n_random_effects,
      random_effects_spec = object$param_info$random_effects_spec,
      converged = object$converged,
      logLik = object$loglik,
      AIC = object$AIC,
      BIC = object$BIC,
      group_metrics = group_metrics,
      derived_metrics = derived_metrics,
      individual_metrics = individual_metrics,
      pmax_method_info = pmax_method_info,
      notes = notes
    ),
    class = c("summary.beezdemand_hurdle", "beezdemand_summary")
  )
}


#' Print Summary of Hurdle Demand Model
#'
#' @param x An object of class \code{summary.beezdemand_hurdle}.
#' @param digits Number of significant digits to print. Default is 4.
#' @param n Number of rows to print for any tables (unused for this class).
#' @param ... Additional arguments passed to \code{print}.
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.summary.beezdemand_hurdle <- function(x, digits = 4, n = Inf, ...) {
  cat("\nTwo-Part Mixed Effects Hurdle Demand Model\n")
  cat("============================================\n\n")

  cat("Call:\n")
  print(x$call)
  cat("\n")

  cat("Convergence:", ifelse(x$converged, "Yes", "No"), "\n")
  cat("Number of subjects:", x$n_subjects, "\n")
  cat("Number of observations:", x$nobs, "\n")
  cat(
    "Random effects:",
    x$n_random_effects,
    paste0("(", paste(x$random_effects_spec, collapse = ", "), ")"),
    "\n\n"
  )

  cat("Fixed Effects:\n")
  cat("--------------\n")
  printCoefmat(x$coefficients_matrix, digits = digits, signif.stars = FALSE, ...)
  cat("\n")

  cat("Variance Components:\n")
  cat("--------------------\n")
  print(round(x$variance_components, digits), ...)
  cat("\n")

  cat("Correlations:\n")
  cat("-------------\n")
  print(round(x$correlations, digits), ...)
  cat("\n")

  cat("Model Fit:\n")
  cat("----------\n")
  cat(sprintf("  Log-likelihood: %.2f\n", x$logLik))
  cat(sprintf("  AIC: %.2f\n", x$AIC))
  cat(sprintf("  BIC: %.2f\n", x$BIC))
  cat("\n")

  cat("Demand Metrics (Group-Level):\n")
  cat("-----------------------------\n")
  dm <- x$derived_metrics
  pmax_val <- dm$estimate[dm$metric == "pmax_model"][1]
  omax_val <- dm$estimate[dm$metric == "omax_model"][1]
  q_at_pmax <- dm$estimate[dm$metric == "q_at_pmax_model"][1]
  elasticity <- dm$estimate[dm$metric == "elasticity_at_pmax_model"][1]
  method <- dm$method[dm$metric == "pmax_model"][1]
  
  if (!is.na(pmax_val)) {
    cat(sprintf(
      "  Pmax (price at max expenditure): %.4f\n",
      pmax_val
    ))
    cat(sprintf("  Omax (max expenditure): %.4f\n", omax_val))
    if (!is.na(q_at_pmax)) {
      cat(sprintf("  Q at Pmax: %.4f\n", q_at_pmax))
    }
    if (!is.na(elasticity)) {
      cat(sprintf("  Elasticity at Pmax: %.4f\n", elasticity))
    }
    cat(sprintf("  Method: %s\n", method %||% "unknown"))
  } else {
    cat("  Pmax/Omax: NA (k < e, no local maximum exists)\n")
    if (!is.null(x$group_metrics$note)) {
      cat(sprintf("  Note: %s\n", x$group_metrics$note))
    }
  }

  cat("\n")

  cat("Derived Parameters (Individual-Level Summary):\n")
  cat("----------------------------------------------\n")
  cat("  Q0 (Intensity):\n")
  print(x$individual_metrics$Q0)
  cat("  Alpha:\n")
  print(x$individual_metrics$alpha)
  cat("  Breakpoint:\n")
  print(x$individual_metrics$breakpoint)
  cat("  Pmax:\n")
  print(x$individual_metrics$Pmax)
  cat("  Omax:\n")
  print(x$individual_metrics$Omax)
  cat("\n")

  invisible(x)
}


#' Extract Coefficients from Hurdle Demand Model
#'
#' @param object An object of class \code{beezdemand_hurdle}.
#' @param report_space Character. One of `"natural"` (default), `"log10"`, or `"internal"`.
#'   Default is `"natural"` for consistency with `tidy()`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Named numeric vector of fixed effect coefficients.
#'
#' @export
coef.beezdemand_hurdle <- function(
  object,
  report_space = c("natural", "log10", "internal"),
  ...
) {
  report_space <- match.arg(report_space)
  coefs <- object$model$coefficients
  if ("logQ0" %in% names(coefs) && !("log_q0" %in% names(coefs))) {
    names(coefs)[names(coefs) == "logQ0"] <- "log_q0"
  }
  if (report_space == "internal") return(coefs)

  out <- coefs

  if ("log_q0" %in% names(out)) {
    if (report_space == "natural") {
      out[["Q0"]] <- exp(out[["log_q0"]])
    } else if (report_space == "log10") {
      out[["log10(Q0)"]] <- out[["log_q0"]] / log(10)
    }
    out <- out[names(out) != "log_q0"]
  }

  if ("log_alpha" %in% names(out)) {
    if (report_space == "natural") {
      out[["alpha"]] <- exp(out[["log_alpha"]])
    } else if (report_space == "log10") {
      out[["log10(alpha)"]] <- out[["log_alpha"]] / log(10)
    }
    out <- out[names(out) != "log_alpha"]
  }

  # Backwards compatibility: older objects stored log(alpha) under the name `alpha`.
  if (!("log_alpha" %in% names(coefs)) && "alpha" %in% names(out)) {
    if (report_space == "natural") {
      out[["alpha"]] <- exp(out[["alpha"]])
    } else if (report_space == "log10") {
      out[["log10(alpha)"]] <- out[["alpha"]] / log(10)
      out <- out[names(out) != "alpha"]
    }
  }

  if ("log_k" %in% names(out)) {
    if (report_space == "natural") {
      out[["k"]] <- exp(out[["log_k"]])
    } else if (report_space == "log10") {
      out[["log10(k)"]] <- out[["log_k"]] / log(10)
    }
    out <- out[names(out) != "log_k"]
  }

  # Backwards compatibility: older objects stored `k` on the natural scale.
  if ("k" %in% names(out) && report_space == "log10") {
    out[["log10(k)"]] <- log10(out[["k"]])
    out <- out[names(out) != "k"]
  }

  out
}


#' Extract Log-Likelihood from Hurdle Demand Model
#'
#' @description
#' Extracts the log-likelihood from a fitted hurdle demand model.
#' Useful for likelihood ratio tests comparing nested models.
#'
#' @param object An object of class \code{beezdemand_hurdle}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{logLik} with the log-likelihood value
#'   and attributes for degrees of freedom and number of observations.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' logLik(fit)
#' }
#'
#' @export
logLik.beezdemand_hurdle <- function(object, ...) {
  val <- object$loglik
  attr(val, "df") <- length(object$model$coefficients)
  attr(val, "nobs") <- object$param_info$n_obs
  class(val) <- "logLik"
  val
}


#' AIC for Hurdle Demand Model
#'
#' @param object A \code{beezdemand_hurdle} object.
#' @param ... Additional arguments (unused).
#' @param k Penalty per parameter. Default is 2 (standard AIC).
#'
#' @return A numeric AIC value.
#' @export
AIC.beezdemand_hurdle <- function(object, ..., k = 2) {
  ll <- logLik(object)
  -2 * as.numeric(ll) + k * attr(ll, "df")
}


#' BIC for Hurdle Demand Model
#'
#' @param object A \code{beezdemand_hurdle} object.
#' @param ... Additional arguments (unused).
#'
#' @return A numeric BIC value.
#' @export
BIC.beezdemand_hurdle <- function(object, ...) {
  ll <- logLik(object)
  -2 * as.numeric(ll) + log(attr(ll, "nobs")) * attr(ll, "df")
}


# ---- Marginal P(zero) integration helpers ----

#' Compute marginal (population-averaged) P(zero)
#'
#' Dispatches to the chosen integration method.
#'
#' @param object A `beezdemand_hurdle` object.
#' @param prices Numeric vector of prices.
#' @param method One of `"kde"`, `"normal"`, `"empirical"`.
#' @return Numeric vector of marginal P(zero) values.
#' @keywords internal
#' @noRd
.compute_marginal_pzero <- function(object, prices, method = "kde") {
  coefs <- object$model$coefficients
  beta0 <- unname(coefs[["beta0"]])
  beta1 <- unname(coefs[["beta1"]])
  epsilon <- object$param_info$epsilon

  re <- object$random_effects
  a_i <- re[, "a_i"]

  logsigma_a <- coefs[["logsigma_a"]]
  sigma_a <- exp(logsigma_a)

  if (method == "kde" && length(unique(a_i)) < 3) {
    cli::cli_warn(
      "Fewer than 3 unique random intercepts; falling back to 'normal' method.",
      call. = FALSE
    )
    method <- "normal"
  }

  switch(method,
    kde = .marginal_pzero_kde(a_i, beta0, beta1, prices, epsilon),
    normal = .marginal_pzero_normal(sigma_a, beta0, beta1, prices, epsilon),
    empirical = .marginal_pzero_empirical(a_i, beta0, beta1, prices, epsilon)
  )
}

#' Marginal P(zero) via kernel density estimation
#' @noRd
.marginal_pzero_kde <- function(a_i, beta0, beta1, prices, epsilon, n_grid = 512) {
  kde <- stats::density(a_i, n = n_grid)
  w <- kde$y / sum(kde$y)
  vapply(prices, function(p) {
    sum(w * stats::plogis(beta0 + kde$x + beta1 * log(p + epsilon)))
  }, numeric(1))
}

#' Marginal P(zero) via normal integration
#' @noRd
.marginal_pzero_normal <- function(sigma_a, beta0, beta1, prices, epsilon) {

  vapply(prices, function(p) {
    log_price_term <- beta1 * log(p + epsilon)
    stats::integrate(
      function(a) {
        stats::plogis(beta0 + a + log_price_term) * stats::dnorm(a, 0, sigma_a)
      },
      lower = -4 * sigma_a, upper = 4 * sigma_a
    )$value
  }, numeric(1))
}

#' Marginal P(zero) via empirical averaging over BLUPs
#' @noRd
.marginal_pzero_empirical <- function(a_i, beta0, beta1, prices, epsilon) {
  vapply(prices, function(p) {
    mean(stats::plogis(beta0 + a_i + beta1 * log(p + epsilon)))
  }, numeric(1))
}


#' Monte Carlo marginal demand prediction
#'
#' Integrates the demand function over the random effects distribution to
#' produce population-average predictions (accounting for Jensen's inequality).
#'
#' @param object A `beezdemand_hurdle` object.
#' @param prices Numeric vector of prices.
#' @param type One of `"response"` or `"demand"`.
#' @param correction Logical; apply lognormal retransformation correction.
#' @param n_draws Number of MC draws from the RE distribution.
#' @param seed Integer or NULL. RNG seed for reproducibility.
#'
#' @return A tibble with price, .fitted, and supporting columns.
#' @noRd
.compute_marginal_demand <- function(object, prices, type = "demand",
                                     correction = TRUE, n_draws = 1000L,
                                     seed = 42L) {
  coefs <- object$model$coefficients
  beta0 <- unname(coefs[["beta0"]])
  beta1 <- unname(coefs[["beta1"]])
  log_q0 <- unname(coefs[["log_q0"]] %||% coefs[["logQ0"]])
  log_alpha <- unname(coefs[["log_alpha"]] %||% coefs[["alpha"]])
  epsilon <- object$param_info$epsilon
  part2 <- object$param_info$part2 %||% "zhao_exponential"
  has_k <- !identical(part2, "simplified_exponential")
  log_k <- if (has_k) unname(coefs[["log_k"]] %||% log(coefs[["k"]])) else NA_real_
  k_val <- if (has_k) exp(log_k) else NA_real_

  sigma_a <- exp(coefs[["logsigma_a"]])
  sigma_b <- exp(coefs[["logsigma_b"]])
  sigma_e <- exp(coefs[["logsigma_e"]])
  n_re <- object$param_info$n_random_effects
  sigma_c <- if (n_re == 3) exp(coefs[["logsigma_c"]]) else 0

  cf <- if (isTRUE(correction)) exp(sigma_e^2 / 2) else 1

  # Build RE covariance matrix from estimated parameters
  rho_ab <- tanh(coefs[["rho_ab_raw"]])

  if (n_re == 3) {
    rho_ac <- tanh(coefs[["rho_ac_raw"]])
    rho_bc_partial <- tanh(coefs[["rho_bc_raw"]])
    rho_bc <- rho_ab * rho_ac +
      rho_bc_partial * sqrt((1 - rho_ab^2) * (1 - rho_ac^2))

    Sigma <- matrix(
      c(sigma_a^2, sigma_a * sigma_b * rho_ab, sigma_a * sigma_c * rho_ac,
        sigma_a * sigma_b * rho_ab, sigma_b^2, sigma_b * sigma_c * rho_bc,
        sigma_a * sigma_c * rho_ac, sigma_b * sigma_c * rho_bc, sigma_c^2),
      nrow = 3
    )
  } else {
    Sigma <- matrix(
      c(sigma_a^2, sigma_a * sigma_b * rho_ab,
        sigma_a * sigma_b * rho_ab, sigma_b^2),
      nrow = 2
    )
  }

  # Cholesky decomposition for correlated MVN draws
  L <- tryCatch(
    chol(Sigma),
    error = function(e) {
      # Fall back to diagonal if Sigma is not PD (shouldn't happen with
      # partial-correlation parametrization, but guard defensively)
      if (n_re == 3) {
        chol(diag(c(sigma_a^2, sigma_b^2, sigma_c^2)))
      } else {
        chol(diag(c(sigma_a^2, sigma_b^2)))
      }
    }
  )

  # Draw correlated random effects via Z %*% L where Z ~ iid N(0,1)
  draw_fn <- function() {
    Z <- matrix(stats::rnorm(n_draws * nrow(Sigma)), nrow = n_draws, ncol = nrow(Sigma))
    Z %*% L
  }

  if (!is.null(seed)) {
    old_seed <- get0(".Random.seed", envir = globalenv(), ifnotfound = NULL)
    on.exit({
      if (is.null(old_seed)) {
        rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", old_seed, envir = globalenv())
      }
    }, add = TRUE)
    set.seed(seed)
  }
  draws <- draw_fn()

  a_draws <- draws[, 1]
  b_draws <- draws[, 2]
  c_draws <- if (n_re == 3) draws[, 3] else rep(0, n_draws)

  # For each price, average over MC draws
  n_prices <- length(prices)
  avg_consumption <- numeric(n_prices)
  avg_prob_zero <- numeric(n_prices)
  avg_expected <- numeric(n_prices)

  for (ip in seq_len(n_prices)) {
    p <- prices[ip]

    # Part I: P(zero) for each draw
    eta <- beta0 + beta1 * log(p + epsilon) + a_draws
    pz <- stats::plogis(eta)

    # Part II: consumption for each draw
    alpha_i <- exp(log_alpha + c_draws)
    Q0_i <- exp(log_q0 + b_draws)

    mu <- if (identical(part2, "simplified_exponential")) {
      (log_q0 + b_draws) - alpha_i * Q0_i * p
    } else if (identical(part2, "exponential")) {
      (log_q0 + b_draws) + k_val * (exp(-alpha_i * Q0_i * p) - 1)
    } else {
      (log_q0 + b_draws) + k_val * (exp(-alpha_i * p) - 1)
    }

    consumption <- exp(mu) * cf

    avg_consumption[ip] <- mean(consumption)
    avg_prob_zero[ip] <- mean(pz)
    avg_expected[ip] <- mean(consumption * (1 - pz))
  }

  x_var <- object$param_info$x_var

  fitted_vals <- switch(type,
    response = avg_consumption,
    demand = avg_expected
  )

  tibble::tibble(
    !!x_var := prices,
    predicted_consumption = avg_consumption,
    prob_zero = avg_prob_zero,
    expected_consumption = avg_expected,
    .fitted = fitted_vals
  )
}


#' Predict Method for Hurdle Demand Models
#'
#' @description
#' Returns predictions from a fitted hurdle demand model.
#'
#' @param object An object of class \code{beezdemand_hurdle}.
#' @param newdata Optional data frame containing a price column matching the fitted
#'   object's `x_var`. If `newdata` includes the id column, subject-specific
#'   predictions are returned; otherwise population predictions are returned.
#'   If `newdata` is `NULL`, returns predictions for all subjects across a price grid.
#' @param type One of:
#'   \describe{
#'     \item{\code{"response"}}{Predicted consumption (part II)}
#'     \item{\code{"link"}}{Predicted log-consumption (linear predictor of part II)}
#'     \item{\code{"probability"}}{Predicted probability of zero consumption (part I)}
#'     \item{\code{"demand"}}{Predicted expected consumption = (1 - P0) * response}
#'     \item{\code{"parameters"}}{Subject-specific parameters (no `.fitted` column)}
#'   }
#' @param prices Optional numeric vector of prices used only when `newdata = NULL`.
#' @param marginal Logical; if `TRUE`, computes population-averaged (marginal)
#'   predictions by integrating over the random effects distribution.
#'   For `type = "probability"`, uses KDE/Normal/Empirical integration of the
#'   binary component. For `type = "response"` and `type = "demand"`, uses
#'   Monte Carlo integration over all random effects, producing the
#'   **population-average** demand curve (accounting for Jensen's inequality).
#'   Default is `FALSE`, which gives conditional (RE = 0) predictions
#'   representing a "typical" subject at the center of the RE distribution.
#' @param marginal_method Character. Method for marginal integration; one of
#'   `"kde"` (default, kernel density estimate of BLUPs), `"normal"` (integrate
#'   over the model-assumed N(0, sigma_a) distribution), or `"empirical"`
#'   (simple average over BLUPs). Ignored when `marginal = FALSE`.
#' @param correction Logical; if `TRUE` (default), applies the lognormal
#'   retransformation correction `exp(sigma_e^2 / 2)` when back-transforming
#'   from the log scale to the natural consumption scale. This produces
#'   the **arithmetic mean** (conditional on Q > 0). Set to `FALSE` to obtain
#'   the **median** (geometric mean), which is useful for individual-level
#'   "most likely" predictions. Only applies to `type = "response"` and
#'   `type = "demand"`.
#' @param seed Integer or `NULL`. Random seed for Monte Carlo marginal
#'   predictions (default `42L`). Set to `NULL` to use current RNG state.
#'   The global RNG state is preserved and restored after the call.
#' @param se.fit Logical; if `TRUE`, includes a `.se.fit` column (delta-method via
#'   `sdreport` when available).
#' @param interval One of `"none"` (default) or `"confidence"`.
#' @param level Confidence level when `interval = "confidence"`.
#' @param ... Unused.
#'
#' @details
#' ## Retransformation correction
#'
#' The hurdle model specifies Gaussian errors on log-consumption (Part II):
#' `log(Q) ~ N(mu, sigma_e^2)`. The conditional distribution of Q given
#' Q > 0 is therefore lognormal. The arithmetic mean of a lognormal is
#' `exp(mu + sigma_e^2/2)`, not `exp(mu)`. Using `exp(mu)` returns the
#' **median** (geometric mean), which systematically underestimates the
#' arithmetic mean by a factor of `exp(sigma_e^2/2)`. This correction is
#' applied by default when `type = "response"` or `type = "demand"`. Set
#' `correction = FALSE` to obtain the median instead.
#'
#' This is a parametric correction assuming normality of log-scale residuals
#' (Duan, 1983). Under the model's normality assumption, this is equivalent to
#' Duan's nonparametric smearing estimator.
#'
#' ## Marginal P(zero)
#'
#' The conditional P(zero) curve (when `marginal = FALSE`) sets the random
#' intercept to zero, which produces a near step-function that misrepresents
#' the observed fraction of zero responses. The marginal curve integrates over
#' the random effect distribution, answering "what fraction of the population
#' has stopped buying at this price?"
#'
#' The `"kde"` and `"empirical"` methods integrate over empirical Bayes
#' estimates (BLUPs) of the random intercepts. BLUPs are shrunk toward zero
#' compared to the true random effects, so these methods slightly
#' underestimate the RE variance. In practice, this shrinkage bias is often
#' smaller than the bias from assuming normality when the true RE distribution
#' is non-normal. The `"normal"` method integrates over the model-assumed
#' N(0, sigma_a) distribution, which is correct under the model but may be
#' wrong if the normality assumption is violated. Use [plot_qq()] to assess
#' RE normality.
#'
#' ## Conditional vs. marginal demand predictions
#'
#' Population-level demand predictions (when no subject ID is provided) can be
#' computed in two ways:
#'
#' - **Conditional (default, `marginal = FALSE`):** Sets all random effects to
#'   zero and evaluates the demand function at the fixed-effect (population)
#'   parameters. For nonlinear models, this corresponds to the **conditional
#'   mode**, not the population-average mean.
#'
#' - **Marginal (`marginal = TRUE`):** Integrates the prediction over the
#'   estimated random-effects distribution via Monte Carlo sampling. This gives
#'   the **population-average** demand curve. Due to Jensen's inequality, this
#'   curve lies above the conditional curve when the demand function is convex
#'   in the random effects (which it is for exponential demand with log-normal
#'   Q0 and alpha).
#'
#' The conditional prediction is appropriate for characterizing a "typical"
#' subject. The marginal prediction is appropriate for predicting aggregate
#' consumption in a population.
#'
#' @return For `type = "parameters"`, a tibble of subject-level parameters.
#'   For `type = "probability"` with `marginal = TRUE`, a tibble with columns
#'   for price, `prob_zero`, and `.fitted` (no subject column).
#'   Otherwise, a tibble containing the `newdata` columns plus `.fitted` and
#'   helper columns `predicted_log_consumption`, `predicted_consumption`,
#'   `prob_zero`, and `expected_consumption`. When requested, `.se.fit` and
#'   `.lower`/`.upper` are included.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#'
#' # Get subject-specific parameters
#' pars <- predict(fit, type = "parameters")
#'
#' # Predict demand at specific prices
#' demand <- predict(fit, type = "demand", prices = c(0, 0.5, 1, 2, 5))
#' }
#'
#' @export
predict.beezdemand_hurdle <- function(
  object,
  newdata = NULL,
  type = c("response", "link", "parameters", "probability", "demand"),
  prices = NULL,
  marginal = FALSE,
  marginal_method = c("kde", "normal", "empirical"),
  correction = TRUE,
  seed = 42L,
  se.fit = FALSE,
  interval = c("none", "confidence"),
  level = 0.95,
  ...
) {
  newdata_user <- newdata
  type <- match.arg(type)
  interval <- match.arg(interval)
  if (!is.null(level) && (!is.numeric(level) || length(level) != 1 || is.na(level) ||
    level <= 0 || level >= 1)) {
    cli::cli_abort("'level' must be a single number between 0 and 1.")
  }
  id_var <- object$param_info$id_var
  x_var <- object$param_info$x_var
  epsilon <- object$param_info$epsilon
  part2 <- object$param_info$part2 %||% "zhao_exponential"

  # For type = "parameters", just return subject-specific parameters
  if (type == "parameters") {
    return(tibble::as_tibble(object$subject_pars))
  }

  marginal_method <- match.arg(marginal_method)

  if (isTRUE(marginal) && !type %in% c("probability", "response", "demand")) {
    cli::cli_warn("'marginal' only applies to type = 'probability', 'response', or 'demand'; ignoring.",
            call. = FALSE)
    marginal <- FALSE
  }

  if (isTRUE(marginal)) {
    # Build price vector without subject expansion
    if (!is.null(newdata)) {
      if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
      if (!(x_var %in% names(newdata))) {
        cli::cli_abort("{.arg newdata} must include the price column {.field {x_var}}.")
      }
      if (id_var %in% names(newdata)) {
        cli::cli_warn(
          "{.code marginal = TRUE} produces population-level predictions; ignoring {.field {id_var}} column."
        )
      }
      price_vec <- newdata[[x_var]]
    } else if (!is.null(prices)) {
      price_vec <- prices
    } else {
      price_vec <- sort(unique(object$data[[x_var]]))
    }

    if (type == "probability") {
      # Existing marginal P(zero) integration
      prob_marginal <- .compute_marginal_pzero(
        object, price_vec, method = marginal_method
      )

      out <- tibble::tibble(
        !!x_var := price_vec,
        prob_zero = prob_marginal,
        .fitted = prob_marginal
      )
      attr(out, "marginal_method") <- marginal_method

      if (isTRUE(se.fit) || interval != "none") {
        cli::cli_warn("Standard errors not yet supported for marginal predictions; returning NA.",
                call. = FALSE)
        out$.se.fit <- NA_real_
        if (interval != "none") {
          out$.lower <- NA_real_
          out$.upper <- NA_real_
        }
      }
      return(out)
    }

    # Marginal integration for type = "response" or "demand":
    # Monte Carlo integration over the random effects distribution
    out <- .compute_marginal_demand(
      object, price_vec, type = type, correction = correction,
      n_draws = 1000L, seed = seed
    )

    attr(out, "marginal_method") <- "monte_carlo"

    if (isTRUE(se.fit) || interval != "none") {
      cli::cli_warn("Standard errors not yet supported for marginal predictions; returning NA.",
              call. = FALSE)
      out$.se.fit <- NA_real_
      if (interval != "none") {
        out$.lower <- NA_real_
        out$.upper <- NA_real_
      }
    }
    return(out)
  }

  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
    if (!(x_var %in% names(newdata))) {
      cli::cli_abort("{.arg newdata} must include the price column {.field {x_var}}.")
    }
  } else {
    if (!is.null(prices)) {
      newdata <- data.frame(prices, stringsAsFactors = FALSE)
      names(newdata) <- x_var
    } else {
      newdata <- data.frame(sort(unique(object$data[[x_var]])), stringsAsFactors = FALSE)
      names(newdata) <- x_var
    }
  }

  # Extract coefficients
  coefs <- object$model$coefficients
  beta0 <- unname(coefs[["beta0"]])
  beta1 <- unname(coefs[["beta1"]])
  log_q0 <- unname(coefs[["log_q0"]] %||% coefs[["logQ0"]])
  log_alpha <- unname(coefs[["log_alpha"]] %||% coefs[["alpha"]])
  has_k <- !identical(part2, "simplified_exponential")
  log_k <- if (has_k) unname(coefs[["log_k"]] %||% log(coefs[["k"]])) else NA_real_

  # Build row-wise prediction design:
  # - if user provided id, compute subject-specific predictions
  # - otherwise compute population predictions
  subjects <- as.character(object$param_info$subject_levels)

  if (is.null(newdata_user)) {
    # Legacy behavior: return predictions for all subjects across the price grid.
    x_vals <- newdata[[x_var]]
    newdata <- expand.grid(
      id = subjects,
      x = x_vals,
      stringsAsFactors = FALSE
    )
    names(newdata)[1] <- id_var
    names(newdata)[2] <- x_var
  }

  has_id <- id_var %in% names(newdata)
  if (has_id) {
    newdata[[id_var]] <- as.character(newdata[[id_var]])
  }

  if (is.null(newdata) || !has_id) {
    # Population prediction: random effects set to 0
    a_row <- 0
    b_row <- 0
    c_row <- 0
  } else {
    id_match <- match(newdata[[id_var]], subjects)
    if (anyNA(id_match)) {
      missing_ids <- unique(newdata[[id_var]][is.na(id_match)])
      cli::cli_abort(
        "Unknown id values in `newdata`: ",
        paste(missing_ids, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    re <- object$random_effects
    a_row <- re[id_match, "a_i"]
    b_row <- re[id_match, "b_i"]

    n_re <- object$param_info$n_random_effects
    c_row <- if (n_re == 3) re[id_match, "c_i"] else 0
  }

  x <- newdata[[x_var]]
  eta <- beta0 + beta1 * log(x + epsilon) + a_row
  prob_zero <- stats::plogis(eta)

  alpha_i <- exp(log_alpha + c_row)
  Q0_i <- exp(log_q0 + b_row)
  k_val <- if (has_k) exp(log_k) else NA_real_

  mu <- if (identical(part2, "simplified_exponential")) {
    (log_q0 + b_row) - alpha_i * Q0_i * x
  } else if (identical(part2, "exponential")) {
    (log_q0 + b_row) + k_val * (exp(-alpha_i * Q0_i * x) - 1)
  } else {
    (log_q0 + b_row) + k_val * (exp(-alpha_i * x) - 1)
  }

  predicted_log_consumption <- as.numeric(mu)

  # Lognormal retransformation correction (Duan, 1983):

  # log(Q) ~ N(mu, sigma_e^2) implies E[Q|Q>0] = exp(mu + sigma_e^2/2),
  # not exp(mu) which is the median. The bias factor exp(sigma_e^2/2) > 1.
  if (isTRUE(correction) && type %in% c("response", "demand")) {
    sigma_e <- exp(object$model$coefficients[["logsigma_e"]])
    correction_factor <- exp(sigma_e^2 / 2)
  } else {
    correction_factor <- 1
  }
  predicted_consumption <- exp(predicted_log_consumption) * correction_factor
  expected_consumption <- predicted_consumption * (1 - prob_zero)

  out <- tibble::as_tibble(newdata)
  out$predicted_log_consumption <- predicted_log_consumption
  out$predicted_consumption <- predicted_consumption
  out$prob_zero <- prob_zero
  out$expected_consumption <- expected_consumption

  out$.fitted <- switch(
    type,
    response = predicted_consumption,
    link = predicted_log_consumption,
    probability = prob_zero,
    demand = expected_consumption
  )

  want_se <- isTRUE(se.fit) || interval != "none"
  if (want_se) {
    if (is.null(object$sdr) || is.null(object$sdr$cov.fixed)) {
      cli::cli_warn("vcov is unavailable; returning NA for uncertainty columns.")
      out$.se.fit <- NA_real_
      if (interval != "none") {
        out$.lower <- NA_real_
        out$.upper <- NA_real_
      }
      return(out)
    }

    vcov_full <- tryCatch(as.matrix(object$sdr$cov.fixed), error = function(e) NULL)
    if (is.null(vcov_full)) {
      cli::cli_warn("vcov is unavailable; returning NA for uncertainty columns.")
      out$.se.fit <- NA_real_
      if (interval != "none") {
        out$.lower <- NA_real_
        out$.upper <- NA_real_
      }
      return(out)
    }

    theta0 <- object$model$coefficients
    param_names <- intersect(names(theta0), colnames(vcov_full))
    theta0 <- theta0[param_names]
    vcov <- vcov_full[param_names, param_names, drop = FALSE]

    eval_fitted <- function(theta) {
      beta0_t <- unname(theta[["beta0"]])
      beta1_t <- unname(theta[["beta1"]])
      log_q0_t <- unname(theta[["log_q0"]] %||% theta[["logQ0"]])
      log_alpha_t <- unname(theta[["log_alpha"]] %||% theta[["alpha"]])
      log_k_t <- if (has_k) unname(theta[["log_k"]] %||% log(theta[["k"]])) else NA_real_
      k_t <- if (has_k) exp(log_k_t) else NA_real_
      logsigma_e_t <- unname(theta[["logsigma_e"]])
      sigma_e_t <- exp(logsigma_e_t)

      eta_t <- beta0_t + beta1_t * log(x + epsilon) + a_row
      prob0_t <- stats::plogis(eta_t)
      alpha_t <- exp(log_alpha_t + c_row)
      q0_t <- exp(log_q0_t + b_row)

      mu_t <- if (identical(part2, "simplified_exponential")) {
        (log_q0_t + b_row) - alpha_t * q0_t * x
      } else if (identical(part2, "exponential")) {
        (log_q0_t + b_row) + k_t * (exp(-alpha_t * q0_t * x) - 1)
      } else {
        (log_q0_t + b_row) + k_t * (exp(-alpha_t * x) - 1)
      }

      # Apply retransformation correction if enabled
      cf_t <- if (isTRUE(correction) && type %in% c("response", "demand")) {
        exp(sigma_e_t^2 / 2)
      } else {
        1
      }
      resp_t <- exp(mu_t) * cf_t
      dem_t <- resp_t * (1 - prob0_t)

      switch(
        type,
        response = as.numeric(resp_t),
        link = as.numeric(mu_t),
        probability = as.numeric(prob0_t),
        demand = as.numeric(dem_t)
      )
    }

    base_fit <- eval_fitted(theta0)
    grad <- matrix(0, nrow = length(base_fit), ncol = length(theta0))
    colnames(grad) <- names(theta0)

    for (j in seq_along(theta0)) {
      step <- 1e-6 * max(1, abs(theta0[[j]]))
      theta_p <- theta0
      theta_p[[j]] <- theta_p[[j]] + step
      grad[, j] <- (eval_fitted(theta_p) - base_fit) / step
    }

    v <- grad %*% vcov
    se_vec <- sqrt(rowSums(v * grad))
    out$.se.fit <- as.numeric(se_vec)

    if (interval != "none") {
      z <- stats::qnorm((1 + level) / 2)
      if (type %in% c("response", "demand")) {
        # Construct CIs on the log scale then back-transform via exp().
        # This guarantees asymmetric, always-positive intervals for
        # consumption quantities (Meeker & Escobar, 1995).
        log_fitted <- log(out$.fitted)
        # SE on the log scale via delta method: se(log(f)) = se(f) / f
        se_log <- out$.se.fit / out$.fitted
        out$.lower <- exp(log_fitted - z * se_log)
        out$.upper <- exp(log_fitted + z * se_log)
      } else {
        # For link and probability types, symmetric Wald CIs are appropriate
        out$.lower <- out$.fitted - z * out$.se.fit
        out$.upper <- out$.fitted + z * out$.se.fit
      }
    }
  }

  out
}


#' Plot Demand Curves from Hurdle Demand Model
#'
#' @description
#' Creates visualizations of fitted demand curves from a hurdle demand model.
#'
#' @param x An object of class \code{beezdemand_hurdle}.
#' @param type Character string specifying the plot type:
#'   \describe{
#'     \item{\code{"demand"}}{Predicted demand curves (default)}
#'     \item{\code{"population"}}{Alias for \code{"demand"}}
#'     \item{\code{"probability"}}{Probability of zero consumption}
#'     \item{\code{"parameters"}}{Distribution of subject-specific parameters}
#'     \item{\code{"individual"}}{Individual demand curves for selected subjects}
#'   }
#' @param ids Optional vector of subject IDs to plot (alias of \code{subjects}).
#' @param parameters Character vector specifying which parameters to plot when
#'   \code{type = "parameters"}. Options are: \code{"Q0"}, \code{"alpha"},
#'   \code{"breakpoint"}, \code{"Pmax"}, \code{"Omax"}. Default is all five.
#' @param prices Numeric vector of prices for plotting. If \code{NULL},
#'   uses a sequence from 0 to max observed price.
#' @param subjects Character or numeric vector of subject IDs to plot for
#'   \code{type = "individual"}. If \code{NULL}, plots first 9 subjects.
#' @param show_population Logical; if \code{TRUE}, overlay population-level
#'   curve on individual plots. Default is \code{TRUE}.
#' @param show_observed Logical; if \code{TRUE}, overlay observed data points.
#' @param x_trans Character. Transformation for x-axis. Default "log".
#' @param y_trans Character. Transformation for y-axis. Default "log".
#' @param free_trans Value used to display free (x = 0) on log scales. Use NULL
#'   to drop x <= 0 values instead.
#' @param facet Faceting specification (TRUE for \code{~id} or a formula).
#' @param x_limits Optional numeric vector of length 2 for x-axis limits.
#' @param y_limits Optional numeric vector of length 2 for y-axis limits.
#' @param x_lab Optional x-axis label.
#' @param y_lab Optional y-axis label.
#' @param xlab Deprecated alias for \code{x_lab}.
#' @param ylab Deprecated alias for \code{y_lab}.
#' @param style Plot styling, passed to \code{theme_beezdemand()}.
#' @param show_pred Which prediction layers to plot: "population", "individual",
#'   or "both".
#' @param observed_point_alpha Alpha for observed points.
#' @param observed_point_size Size for observed points.
#' @param pop_line_alpha Alpha for population curve.
#' @param pop_line_size Line size for population curve.
#' @param ind_line_alpha Alpha for individual curves.
#' @param ind_line_size Line size for individual curves.
#' @param marginal Logical; if `TRUE` (default) and `type = "probability"`,
#'   the population curve shows the marginal (population-averaged) P(zero)
#'   instead of the conditional (RE = 0) curve. Set to `FALSE` for the old
#'   conditional behavior.
#' @param marginal_method Character. Method for marginal integration when
#'   `marginal = TRUE`. One of `"kde"` (default), `"normal"`, or
#'   `"empirical"`. See [predict.beezdemand_hurdle()] for details.
#' @param par_trans Named list of transformations for parameter distribution
#'   plots (when `type = "parameters"`). Names are parameter names (e.g.,
#'   `"alpha"`), values are transformation names (e.g., `"log10"`). Default
#'   applies `log10` to alpha.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#'
#' # Plot mean demand curve
#' plot(fit)
#'
#' # Plot parameter distributions
#' plot(fit, type = "parameters")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_histogram facet_wrap labs
#'   theme_minimal scale_y_log10
#' @importFrom rlang .data
#' @export
plot.beezdemand_hurdle <- function(
  x,
  type = c("demand", "population", "probability", "parameters", "individual", "both"),
  ids = NULL,
  subjects = NULL,
  parameters = c("Q0", "alpha", "breakpoint", "Pmax", "Omax"),
  prices = NULL,
  show_population = TRUE,
  show_pred = NULL,
  show_observed = TRUE,
  x_trans = c("log10", "log", "linear", "pseudo_log"),
  y_trans = NULL,
  free_trans = 0.01,
  facet = NULL,
  x_limits = NULL,
  y_limits = NULL,
  x_lab = NULL,
  y_lab = NULL,
  xlab = NULL,
  ylab = NULL,
  style = c("modern", "apa"),
  observed_point_alpha = 0.5,
  observed_point_size = 1.8,
  pop_line_alpha = 0.9,
  pop_line_size = 1.0,
  ind_line_alpha = 0.35,
  ind_line_size = 0.7,
  marginal = TRUE,
  marginal_method = c("kde", "normal", "empirical"),
  par_trans = NULL,
  ...
) {
  y_trans_missing <- is.null(y_trans)
  type <- match.arg(type)
  if (type == "population") {
    type <- "demand"
  }
  if (type == "both") {
    type <- "individual"
    show_pred <- "both"
  }
  x_trans <- match.arg(x_trans)
  type_for_trans <- if (type == "individual") "demand" else type
  if (y_trans_missing) {
    y_trans <- beezdemand_default_y_trans(type = type_for_trans)
  }
  y_trans <- match.arg(y_trans, c("log10", "log", "linear", "pseudo_log"))
  style <- match.arg(style)
  x_var <- x$param_info$x_var
  id_var <- x$param_info$id_var
  epsilon <- x$param_info$epsilon

  labels <- beezdemand_normalize_plot_labels(x_lab, y_lab, xlab, ylab)
  x_lab <- labels$x_lab %||% "Price"
  y_lab <- labels$y_lab %||% "Consumption"

  if (!is.null(subjects)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "plot(subjects = )",
      "plot(ids = )"
    )
    if (is.null(ids)) {
      ids <- subjects
    }
  }

  if (!is.null(ids)) {
    subjects <- ids
  }

  subtitle_note <- FALSE
  free_trans_used <- FALSE

  # Set up price sequence
  if (is.null(prices)) {
    max_price <- max(x$data[[x_var]], na.rm = TRUE)
    prices <- seq(0, max_price, length.out = 100)
  }

  # Extract population parameters
  coefs <- x$model$coefficients
  part2 <- x$param_info$part2 %||% "zhao_exponential"
  beta0 <- coefs["beta0"]
  beta1 <- coefs["beta1"]
  log_q0 <- if ("log_q0" %in% names(coefs)) coefs["log_q0"] else coefs["logQ0"]
  alpha <- exp(coefs["log_alpha"])
  Q0 <- exp(log_q0)
  k <- if (!identical(part2, "simplified_exponential")) {
    if ("log_k" %in% names(coefs)) exp(coefs["log_k"]) else coefs["k"]
  } else {
    NA_real_
  }

  if (type == "demand") {
    # Population demand curve
    pop_data <- data.frame(
      price = prices,
      log_consumption = if (identical(part2, "simplified_exponential")) {
        log_q0 - alpha * Q0 * prices
      } else if (identical(part2, "exponential")) {
        log_q0 + k * (exp(-alpha * Q0 * prices) - 1)
      } else {
        log_q0 + k * (exp(-alpha * prices) - 1)
      }
    )
    pop_data$consumption <- exp(pop_data$log_consumption)

    pop_df <- data.frame(x = pop_data$price, y = pop_data$consumption)
    free_pop <- beezdemand_apply_free_trans(pop_df, "x", x_trans, free_trans)
    pop_df <- free_pop$data
    free_trans_used <- free_trans_used || free_pop$replaced

    pop_y <- beezdemand_drop_nonpositive_y(pop_df, "y", y_trans)
    pop_df <- pop_y$data
    subtitle_note <- subtitle_note || pop_y$dropped

    p <- ggplot(pop_df, aes(x = .data$x, y = .data$y)) +
      geom_line(
        linewidth = pop_line_size,
        alpha = pop_line_alpha,
        color = beezdemand_style_color(style, "primary")
      )

    if (show_observed) {
      obs_df <- x$data[, c(x_var, x$param_info$y_var)]
      names(obs_df) <- c("x", "y")
      free_obs <- beezdemand_apply_free_trans(obs_df, "x", x_trans, free_trans)
      obs_df <- free_obs$data
      free_trans_used <- free_trans_used || free_obs$replaced

      obs_y <- beezdemand_drop_nonpositive_y(obs_df, "y", y_trans)
      obs_df <- obs_y$data
      subtitle_note <- subtitle_note || obs_y$dropped

      p <- p +
        geom_point(
          data = obs_df,
          aes(x = .data$x, y = .data$y),
          alpha = observed_point_alpha,
          size = observed_point_size
        )
    }

    subtitle <- NULL
    if (isTRUE(subtitle_note)) {
      subtitle <- "Zeros omitted on log scale."
    }
    beezdemand_warn_free_trans(free_trans_used, free_trans)

    x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
    y_limits <- beezdemand_resolve_limits(y_limits, y_trans, axis = "y")

    p <- p +
      labs(
        title = "Population Demand Curve",
        subtitle = subtitle,
        x = x_lab,
        y = y_lab
      ) +
      scale_x_continuous(
        trans = beezdemand_get_trans(x_trans),
        limits = x_limits,
        labels = beezdemand_axis_labels()
      ) +
      scale_y_continuous(
        trans = beezdemand_get_trans(y_trans),
        limits = y_limits,
        labels = beezdemand_axis_labels()
      ) +
      theme_beezdemand(style = style)

    if (x_trans == "log10") {
      p <- p + ggplot2::annotation_logticks(sides = "b")
    }
    if (y_trans == "log10") {
      p <- p + ggplot2::annotation_logticks(sides = "l")
    }

    return(p)
  }

  if (type == "probability") {
    if (is.null(show_pred)) {
      show_pred <- if (is.null(subjects)) {
        "population"
      } else if (isTRUE(show_population)) {
        "both"
      } else {
        "individual"
      }
    }
    show_pred <- beezdemand_normalize_show_pred(show_pred)

    p <- ggplot2::ggplot()

    if (any(show_pred %in% "individual")) {
      if (is.null(subjects)) {
        subjects <- utils::head(x$param_info$subject_levels, 9)
      }
      pred <- predict(x, type = "probability", prices = prices)
      pred <- pred[pred[[id_var]] %in% subjects, , drop = FALSE]

      pred_df <- data.frame(
        id = as.character(pred[[id_var]]),
        x = pred[[x_var]],
        y = pred$prob_zero
      )
      free_pred <- beezdemand_apply_free_trans(pred_df, "x", x_trans, free_trans)
      pred_df <- free_pred$data
      free_trans_used <- free_trans_used || free_pred$replaced

      pred_y <- beezdemand_drop_nonpositive_y(pred_df, "y", y_trans)
      pred_df <- pred_y$data
      subtitle_note <- subtitle_note || pred_y$dropped

      p <- p +
        ggplot2::geom_line(
          data = pred_df,
          ggplot2::aes(x = .data$x, y = .data$y, color = .data$id),
          linewidth = ind_line_size,
          alpha = ind_line_alpha
        )
    }

    if (any(show_pred %in% "population")) {
      marginal_method <- match.arg(marginal_method)
      if (isTRUE(marginal)) {
        pop_pzero <- .compute_marginal_pzero(x, prices, method = marginal_method)
        pop_data <- data.frame(price = prices, prob_zero = pop_pzero)
      } else {
        pop_data <- data.frame(
          price = prices,
          eta = beta0 + beta1 * log(prices + epsilon)
        )
        pop_data$prob_zero <- stats::plogis(pop_data$eta)
      }

      pop_df <- data.frame(x = pop_data$price, y = pop_data$prob_zero)
      free_pop <- beezdemand_apply_free_trans(pop_df, "x", x_trans, free_trans)
      pop_df <- free_pop$data
      free_trans_used <- free_trans_used || free_pop$replaced

      pop_y <- beezdemand_drop_nonpositive_y(pop_df, "y", y_trans)
      pop_df <- pop_y$data
      subtitle_note <- subtitle_note || pop_y$dropped

      p <- p +
        ggplot2::geom_line(
          data = pop_df,
          ggplot2::aes(x = .data$x, y = .data$y),
          linewidth = pop_line_size,
          alpha = pop_line_alpha,
          color = beezdemand_style_color(style, "secondary"),
          linetype = if (any(show_pred %in% "individual")) "dashed" else "solid"
        )
    }

    x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
    y_limits <- beezdemand_resolve_limits(y_limits, y_trans, axis = "y")

    if (!is.null(facet)) {
      if (any(show_pred %in% "individual")) {
        if (isTRUE(facet)) {
          p <- p + ggplot2::facet_wrap(~id)
        } else if (is.character(facet)) {
          p <- p + ggplot2::facet_wrap(stats::as.formula(facet))
        } else if (inherits(facet, "formula")) {
          p <- p + ggplot2::facet_wrap(facet)
        }
      }
    }

    p <- p +
      ggplot2::labs(
        title = "Probability of Zero Consumption",
        subtitle = if (isTRUE(subtitle_note)) "Zeros omitted on log scale." else NULL,
        x = x_lab,
        y = "P(Consumption = 0)"
      ) +
      ggplot2::scale_x_continuous(
        trans = beezdemand_get_trans(x_trans),
        limits = x_limits,
        labels = beezdemand_axis_labels()
      ) +
      ggplot2::scale_y_continuous(
        trans = beezdemand_get_trans(y_trans),
        limits = if (is.null(y_limits)) c(0, 1) else y_limits,
        labels = beezdemand_axis_labels()
      ) +
      theme_beezdemand(style = style)

    beezdemand_warn_free_trans(free_trans_used, free_trans)

    return(p)
  }

  if (type == "parameters") {
    # Distribution of subject-specific parameters
    pars <- x$subject_pars

    # Validate parameters argument
    valid_params <- c("Q0", "alpha", "breakpoint", "Pmax", "Omax")
    parameters <- match.arg(parameters, valid_params, several.ok = TRUE)

    # Default transforms: log10 for alpha (always extremely right-skewed)
    default_par_trans <- list(alpha = "log10")
    if (!is.null(par_trans)) {
      default_par_trans[names(par_trans)] <- par_trans
    }
    par_trans_resolved <- default_par_trans

    # Build mapping from parameter names to display names
    param_map <- list(
      Q0 = "Q0 (Intensity)",
      alpha = "Alpha",
      breakpoint = "Breakpoint",
      Pmax = "Pmax",
      Omax = "Omax"
    )

    # Create long format data for selected parameters
    values_list <- lapply(parameters, function(p) {
      vals <- pars[[p]]
      vals <- vals[is.finite(vals)] # Remove Inf/NA
      # Apply per-parameter transform if specified
      tfun_name <- par_trans_resolved[[p]]
      if (!is.null(tfun_name) && !identical(tfun_name, "identity")) {
        tfun <- switch(tfun_name,
          log10 = log10, log = log, sqrt = sqrt,
          cli::cli_abort("Unknown transform: {.val {tfun_name}}.")
        )
        vals <- vals[vals > 0]
        vals <- tfun(vals)
      }
      vals
    })
    labels_list <- lapply(parameters, function(p) {
      base_label <- param_map[[p]]
      tfun_name <- par_trans_resolved[[p]]
      if (!is.null(tfun_name) && !identical(tfun_name, "identity")) {
        paste0(tfun_name, "(", base_label, ")")
      } else {
        base_label
      }
    })

    pars_long <- data.frame(
      parameter = factor(
        rep(unlist(labels_list), vapply(values_list, length, integer(1))),
        levels = unlist(labels_list)
      ),
      value = unlist(values_list)
    )

    # Create plot
    pars_df <- pars_long
    names(pars_df) <- c("parameter", "value")
    free_pars <- beezdemand_apply_free_trans(pars_df, "value", x_trans, free_trans)
    pars_df <- free_pars$data
    free_trans_used <- free_trans_used || free_pars$replaced

    x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
    p <- ggplot(pars_df, aes(x = .data$value)) +
      geom_histogram(
        bins = 30,
        fill = beezdemand_style_color(style, "accent"),
        color = "white",
        alpha = 0.7
      ) +
      facet_wrap(
        ~parameter,
        scales = "free",
        nrow = ceiling(length(parameters) / 3)
      ) +
      labs(
        title = "Distribution of Subject-Specific Parameters",
        x = "Parameter Value",
        y = "Count"
      ) +
      scale_x_continuous(
        trans = beezdemand_get_trans(x_trans),
        limits = x_limits,
        labels = beezdemand_axis_labels()
      ) +
      theme_beezdemand(style = style)

    beezdemand_warn_free_trans(free_trans_used, free_trans)

    return(p)
  }

  if (type == "individual") {
    if (is.null(show_pred)) {
      show_pred <- if (show_population) c("individual", "population") else "individual"
    }
    show_pred <- beezdemand_normalize_show_pred(show_pred)

    # Select subjects
    if (is.null(subjects)) {
      subjects <- utils::head(x$param_info$subject_levels, 9)
    }

    # Get predictions
    pred <- predict(x, type = "demand", prices = prices)
    pred <- pred[pred[[id_var]] %in% subjects, ]

    # Population curve for reference
    pop_data <- data.frame(
      price = prices,
      consumption = if (identical(part2, "simplified_exponential")) {
        exp(log_q0 - alpha * Q0 * prices)
      } else if (identical(part2, "exponential")) {
        exp(log_q0 + k * (exp(-alpha * Q0 * prices) - 1))
      } else {
        exp(log_q0 + k * (exp(-alpha * prices) - 1))
      }
    )

    pred_df <- NULL
    if (any(show_pred %in% "individual")) {
      pred_df <- data.frame(
        id = as.character(pred[[id_var]]),
        x = pred[[x_var]],
        y = pred$predicted_consumption
      )
      free_pred <- beezdemand_apply_free_trans(pred_df, "x", x_trans, free_trans)
      pred_df <- free_pred$data
      free_trans_used <- free_trans_used || free_pred$replaced

      pred_y <- beezdemand_drop_nonpositive_y(pred_df, "y", y_trans)
      pred_df <- pred_y$data
      subtitle_note <- subtitle_note || pred_y$dropped
    }

    p <- ggplot()
    if (!is.null(pred_df)) {
      p <- p +
        geom_line(
          data = pred_df,
          aes(x = .data$x, y = .data$y, color = .data$id),
          linewidth = ind_line_size,
          alpha = ind_line_alpha
        )
    }

    if (any(show_pred %in% "population")) {
      pop_df <- data.frame(x = prices, y = pop_data$consumption)
      free_pop <- beezdemand_apply_free_trans(pop_df, "x", x_trans, free_trans)
      pop_df <- free_pop$data
      free_trans_used <- free_trans_used || free_pop$replaced

      pop_y <- beezdemand_drop_nonpositive_y(pop_df, "y", y_trans)
      pop_df <- pop_y$data
      subtitle_note <- subtitle_note || pop_y$dropped

      p <- p +
        geom_line(
          data = pop_df,
          aes(x = .data$x, y = .data$y),
          linetype = "dashed",
          linewidth = pop_line_size,
          alpha = pop_line_alpha,
          color = beezdemand_style_color(style, "dark")
        )
    }

    if (show_observed) {
      obs <- x$data[x$data[[id_var]] %in% subjects, ]
      obs_df <- data.frame(
        id = as.character(obs[[id_var]]),
        x = obs[[x_var]],
        y = obs[[x$param_info$y_var]]
      )
      free_obs <- beezdemand_apply_free_trans(obs_df, "x", x_trans, free_trans)
      obs_df <- free_obs$data
      free_trans_used <- free_trans_used || free_obs$replaced

      obs_y <- beezdemand_drop_nonpositive_y(obs_df, "y", y_trans)
      obs_df <- obs_y$data
      subtitle_note <- subtitle_note || obs_y$dropped

      p <- p +
        geom_point(
          data = obs_df,
          aes(x = .data$x, y = .data$y, color = .data$id),
          alpha = observed_point_alpha,
          size = observed_point_size
        )
    }

    p <- p +
      facet_wrap(~id) +
      labs(
        title = "Individual Demand Curves",
        subtitle = if (show_population) {
          "Dashed line = population average"
        } else {
          NULL
        },
        x = "Price",
        y = "Predicted Consumption"
      ) +
      theme_beezdemand(style = style) +
      ggplot2::theme(legend.position = "none") +
      scale_x_continuous(
        trans = beezdemand_get_trans(x_trans),
        limits = x_limits,
        labels = beezdemand_axis_labels()
      ) +
      scale_y_continuous(
        trans = beezdemand_get_trans(y_trans),
        limits = y_limits,
        labels = beezdemand_axis_labels()
      )

    p <- beezdemand_apply_color_scale(p, style, pred_df, "id")

    if (x_trans == "log10") {
      p <- p + ggplot2::annotation_logticks(sides = "b")
    }
    if (y_trans == "log10") {
      p <- p + ggplot2::annotation_logticks(sides = "l")
    }

    if (isTRUE(subtitle_note)) {
      p <- p + ggplot2::labs(subtitle = "Zeros omitted on log scale.")
    }
    beezdemand_warn_free_trans(free_trans_used, free_trans)

    return(p)
  }
}


#' Plot Demand Curve for a Single Subject
#'
#' @description
#' Creates a demand curve plot for a single subject with optional observed data
#' and population reference curve.
#'
#' @param object An object of class \code{beezdemand_hurdle}.
#' @param subject_id The ID of the subject to plot.
#' @param prices Numeric vector of prices for plotting. If \code{NULL},
#'   uses a sequence from 0 to max observed price.
#' @param show_data Logical; if \code{TRUE}, overlay observed data points.
#'   Default is \code{TRUE}.
#' @param show_population Logical; if \code{TRUE}, show population curve.
#'   Default is \code{TRUE}.
#' @param style Plot styling, passed to \code{theme_beezdemand()}.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' plot_subject(fit, subject_id = "19")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal
#' @importFrom rlang .data
#' @export
plot_subject <- function(
  object,
  subject_id,
  prices = NULL,
  show_data = TRUE,
  show_population = TRUE,
  style = c("modern", "apa")
) {
  if (!inherits(object, "beezdemand_hurdle")) {
    cli::cli_abort("object must be of class 'beezdemand_hurdle'")
  }
  style <- match.arg(style)

  id_var <- object$param_info$id_var
  x_var <- object$param_info$x_var
  y_var <- object$param_info$y_var

  if (!subject_id %in% object$param_info$subject_levels) {
    cli::cli_abort("subject_id not found in model")
  }

  # Set up prices
  if (is.null(prices)) {
    max_price <- max(object$data[[x_var]], na.rm = TRUE)
    prices <- seq(0, max_price, length.out = 100)
  }

  # Get predictions for this subject
  pred <- predict(object, type = "demand", prices = prices)
  pred <- pred[pred[[id_var]] == subject_id, ]

  # Get observed data
  obs <- object$data[object$data[[id_var]] == subject_id, ]

  # Population curve
  coefs <- object$model$coefficients
  part2 <- object$param_info$part2 %||% "zhao_exponential"
  log_q0 <- if ("log_q0" %in% names(coefs)) coefs["log_q0"] else coefs["logQ0"]
  k <- if (!identical(part2, "simplified_exponential")) {
    if ("log_k" %in% names(coefs)) exp(coefs["log_k"]) else coefs["k"]
  } else {
    NA_real_
  }
  alpha <- exp(coefs["log_alpha"])

  Q0 <- exp(log_q0)
  pop_data <- data.frame(
    price = prices,
    consumption = if (identical(part2, "simplified_exponential")) {
      exp(log_q0 - alpha * Q0 * prices)
    } else if (identical(part2, "exponential")) {
      exp(log_q0 + k * (exp(-alpha * Q0 * prices) - 1))
    } else {
      exp(log_q0 + k * (exp(-alpha * prices) - 1))
    }
  )

  # Build plot
  p <- ggplot(pred, aes(x = .data[[x_var]], y = .data$predicted_consumption)) +
    geom_line(linewidth = 1.2, color = "#2E86AB")

  if (show_population) {
    p <- p +
      geom_line(
        data = pop_data,
        aes(x = .data$price, y = .data$consumption),
        linetype = "dashed",
        color = "gray50"
      )
  }

  if (show_data) {
    p <- p +
      geom_point(
        data = obs,
        aes(x = .data[[x_var]], y = .data[[y_var]]),
        color = "#E94F37",
        size = 2
      )
  }

  p <- p +
    labs(
      title = paste("Demand Curve - Subject", subject_id),
      x = "Price",
      y = "Consumption"
    ) +
    theme_beezdemand(style = style)

  return(p)
}


#' Tidy a beezdemand_hurdle Model
#'
#' @description
#' Returns a tibble of model coefficients in tidy format.
#'
#' @param x An object of class \code{beezdemand_hurdle}.
#' @param report_space Character. Reporting space for core demand parameters.
#'   One of `"internal"`, `"natural"`, or `"log10"`.
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{term}{Parameter name}
#'     \item{estimate}{Point estimate}
#'     \item{std.error}{Standard error}
#'     \item{statistic}{z-value}
#'     \item{p.value}{P-value}
#'     \item{component}{One of "zero_probability", "consumption", "variance", or "fixed"}
#'   }
#'
#' @export
tidy.beezdemand_hurdle <- function(
  x,
  report_space = c("natural", "log10", "internal"),
  ...
) {
  report_space <- match.arg(report_space)
  coefs <- x$model$coefficients
  se <- x$model$se
  if ("logQ0" %in% names(coefs) && !("log_q0" %in% names(coefs))) {
    names(coefs)[names(coefs) == "logQ0"] <- "log_q0"
  }
  if ("logQ0" %in% names(se) && !("log_q0" %in% names(se))) {
    names(se)[names(se) == "logQ0"] <- "log_q0"
  }
  coef_names <- names(coefs)
  z_val <- coefs / se
  p_val <- 2 * stats::pnorm(-abs(z_val))

  # Determine component for each coefficient
  component <- dplyr::case_when(
    coef_names %in% c("beta0", "beta1", "gamma0", "gamma1") ~ "zero_probability",
    coef_names %in% c("log_q0", "log_alpha", "log_k", "k", "alpha") ~ "consumption",
    grepl("^logsigma_|^rho_|^sigma_", coef_names) ~ "variance",
    TRUE ~ "fixed"
  )

  term <- dplyr::case_when(
    coef_names == "log_q0" ~ "Q0",
    coef_names == "log_alpha" ~ "alpha",
    coef_names == "log_k" ~ "k",
    TRUE ~ coef_names
  )

  out <- tibble::tibble(
    term = term,
    estimate = unname(coefs),
    std.error = unname(se),
    statistic = unname(z_val),
    p.value = unname(p_val),
    component = component,
	    estimate_scale = dplyr::case_when(
	      coef_names %in% c("beta0", "beta1", "gamma0", "gamma1") ~ "logit",
	      coef_names %in% c("log_q0", "log_alpha", "log_k", "alpha") ~ "log",
	      TRUE ~ "natural"
	    ),
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


#' Glance at a beezdemand_hurdle Model
#'
#' @description
#' Returns a one-row tibble of model-level statistics.
#'
#' @param x An object of class \code{beezdemand_hurdle}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A one-row tibble with columns:
#'   \describe{
#'     \item{model_class}{"beezdemand_hurdle"}
#'     \item{backend}{"TMB"}
#'     \item{nobs}{Number of observations}
#'     \item{n_subjects}{Number of subjects}
#'     \item{n_random_effects}{Number of random effects}
#'     \item{converged}{Convergence status}
#'     \item{logLik}{Log-likelihood}
#'     \item{AIC}{Akaike Information Criterion}
#'     \item{BIC}{Bayesian Information Criterion}
#'   }
#'
#' @export
glance.beezdemand_hurdle <- function(x, ...) {
  tibble::tibble(
    model_class = "beezdemand_hurdle",
    backend = "TMB_hurdle",
    nobs = x$param_info$n_obs,
    n_subjects = x$param_info$n_subjects,
    n_random_effects = x$param_info$n_random_effects,
    converged = x$converged,
    logLik = x$loglik,
    AIC = x$AIC,
    BIC = x$BIC
  )
}

#' Confidence Intervals for Hurdle Demand Model Parameters
#'
#' Computes confidence intervals for fixed effect parameters from a TMB-based
#' hurdle demand model using the asymptotic normal approximation.
#'
#' @param object A `beezdemand_hurdle` object from [fit_demand_hurdle()].
#' @param parm Character vector of parameter names to compute CIs for.
#'   Default includes all fixed effect parameters.
#' @param level Confidence level (default 0.95).
#' @param report_space Character. Reporting space for parameters:
#'   - `"internal"`: parameters on internal/fitting scale (log for Q0, alpha)
#'   - `"natural"`: back-transformed to natural scale
#' @param ... Additional arguments (ignored).
#'
#' @return A tibble with columns: `term`, `estimate`, `conf.low`, `conf.high`,
#'   `level`, `component`, `estimate_scale`.
#'
#' @details
#' Confidence intervals are computed using the asymptotic normal approximation
#' based on standard errors from `TMB::sdreport()`. For parameters estimated
#' on the log scale (Q0, alpha, k), intervals can be back-transformed to the
#' natural scale using `report_space = "natural"`.
#'
#' The transformation uses:
#' - For log-scale parameters: exp(estimate +/- z * SE)
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' confint(fit)
#' }
#'
#' @importFrom stats qnorm
#' @export
confint.beezdemand_hurdle <- function(
  object,
  parm = NULL,
  level = 0.95,
  report_space = c("internal", "natural"),
  ...
) {
  report_space <- match.arg(report_space)

  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    cli::cli_abort("`level` must be a single number between 0 and 1.")
  }

  coefs <- object$model$coefficients
  se_vec <- object$model$se

  if (is.null(coefs) || length(coefs) == 0) {
    return(tibble::tibble(
      term = character(),
      estimate = numeric(),
      conf.low = numeric(),
      conf.high = numeric(),
      level = numeric(),
      component = character(),
      estimate_scale = character()
    ))
  }

  # Normalize coefficient names
  if ("logQ0" %in% names(coefs) && !("log_q0" %in% names(coefs))) {
    names(coefs)[names(coefs) == "logQ0"] <- "log_q0"
  }
  if ("logQ0" %in% names(se_vec) && !("log_q0" %in% names(se_vec))) {
    names(se_vec)[names(se_vec) == "logQ0"] <- "log_q0"
  }

  # Filter parameters if parm is specified
  if (!is.null(parm)) {
    keep <- names(coefs) %in% parm
    coefs <- coefs[keep]
    se_vec <- se_vec[keep]
  }

  if (length(coefs) == 0) {
    cli::cli_warn("No requested parameters found in model.")
    return(tibble::tibble(
      term = character(),
      estimate = numeric(),
      conf.low = numeric(),
      conf.high = numeric(),
      level = numeric(),
      component = character(),
      estimate_scale = character()
    ))
  }

  z <- stats::qnorm((1 + level) / 2)

  # Determine component and scale for each parameter
  coef_names <- names(coefs)
  component <- dplyr::case_when(
    coef_names %in% c("beta0", "beta1", "gamma0", "gamma1") ~ "zero_probability",
    coef_names %in% c("log_q0", "log_alpha", "log_k", "k", "alpha") ~ "consumption",
    grepl("^logsigma_|^rho_", coef_names) ~ "variance",
    TRUE ~ "fixed"
  )

  # Internal scale for each parameter
  estimate_scale <- dplyr::case_when(
    coef_names %in% c("beta0", "beta1", "gamma0", "gamma1") ~ "logit",
    coef_names %in% c("log_q0", "log_alpha", "log_k") ~ "log",
    TRUE ~ "natural"
  )

  # Compute CIs on internal scale
  conf_low <- coefs - z * se_vec
  conf_high <- coefs + z * se_vec
  estimates <- coefs

  # Transform to natural scale if requested
  if (report_space == "natural") {
    log_params <- coef_names %in% c("log_q0", "log_alpha", "log_k")
    if (any(log_params)) {
      estimates[log_params] <- exp(coefs[log_params])
      conf_low[log_params] <- exp(conf_low[log_params])
      conf_high[log_params] <- exp(conf_high[log_params])
      estimate_scale[log_params] <- "natural"
    }
  }

  # Build display names
  term_display <- dplyr::case_when(
    coef_names == "log_q0" & report_space == "natural" ~ "Q0",
    coef_names == "log_alpha" & report_space == "natural" ~ "alpha",
    coef_names == "log_k" & report_space == "natural" ~ "k",
    coef_names == "log_q0" ~ "log(Q0)",
    coef_names == "log_alpha" ~ "log(alpha)",
    coef_names == "log_k" ~ "log(k)",
    TRUE ~ coef_names
  )

  tibble::tibble(
    term = term_display,
    estimate = unname(estimates),
    conf.low = unname(conf_low),
    conf.high = unname(conf_high),
    level = level,
    component = component,
    estimate_scale = estimate_scale
  )
}


#' Augment a beezdemand_hurdle Model with Fitted Values and Residuals
#'
#' @description
#' Returns the original data with fitted values, residuals, and predictions
#' from a hurdle demand model. This enables easy model diagnostics and
#' visualization with the tidyverse.
#'
#' @param x An object of class \code{beezdemand_hurdle}.
#' @param newdata Optional data frame of new data for prediction. If NULL,
#'   uses the original data from the model.
#' @param ... Additional arguments (currently unused).
#'
#' @param component Character. Which residuals to compute:
#'   \describe{
#'     \item{`"combined"`}{(Default) Randomized quantile residuals (Dunn &
#'       Smyth, 1996) that assess both the binary and continuous components
#'       simultaneously. If the model is correctly specified, these are exactly
#'       N(0,1).}
#'     \item{`"continuous"`}{Log-scale residuals `log(y) - mu` for positive
#'       observations only (zeros are NA). Assesses Part II specification.}
#'     \item{`"binary"`}{Not returned as residuals; use the `.fitted_prob`
#'       column and observed binary indicators for calibration diagnostics.}
#'   }
#'
#' @return A tibble containing the original data plus:
#'   \describe{
#'     \item{.fitted}{Fitted demand values (natural scale)}
#'     \item{.fitted_link}{Fitted values on log scale (Part II mean)}
#'     \item{.fitted_prob}{Predicted probability of consumption (1 - P(zero))}
#'     \item{.resid}{Residuals (type depends on `component`; see above)}
#'     \item{.resid_response}{Residuals on response scale (y - .fitted)}
#'   }
#'
#' @details
#' ## Residual types for hurdle models
#'
#' The hurdle model has two components, each requiring different diagnostic
#' approaches:
#'
#' - **Continuous residuals** (`component = "continuous"`): Standard log-scale
#'   residuals `log(y) - mu` for observations where y > 0. Zeros are excluded
#'   (NA). Assesses whether the lognormal conditional distribution is well-
#'   specified.
#'
#' - **Randomized quantile residuals** (`component = "combined"`, default):
#'   Following Dunn & Smyth (1996), maps each observation through the fitted
#'   hurdle CDF and then the standard normal quantile function. If the model is
#'   correctly specified, these residuals are exactly N(0,1) regardless of which
#'   component generated the observation. For zeros, a uniform random variate
#'   within `[0, P(zero)]` breaks ties that would otherwise create a spike in
#'   the QQ plot.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' augmented <- augment(fit)
#'
#' # Plot residuals
#' library(ggplot2)
#' ggplot(augmented, aes(x = .fitted, y = .resid)) +
#'   geom_point(alpha = 0.5) +
#'   geom_hline(yintercept = 0, linetype = "dashed")
#' }
#'
#' @importFrom tibble as_tibble
#' @export
augment.beezdemand_hurdle <- function(x, newdata = NULL,
                                      component = c("combined", "continuous"),
                                      ...) {
  component <- match.arg(component)

  if (is.null(newdata)) {
    data <- x$data
  } else {
    data <- newdata
  }

  if (is.null(data)) {
    cli::cli_abort("No data available. Provide 'newdata' or ensure model contains data.",
         call. = FALSE)
  }

  # Get variable names
  y_var <- x$param_info$y_var
  x_var <- x$param_info$x_var
  id_var <- x$param_info$id_var

  # Get predictions - demand type returns all columns we need
  fitted_demand <- predict(x, newdata = data, type = "demand")

  # Build output tibble
  out <- tibble::as_tibble(data)

  # Extract fitted values and probability from demand prediction
  out$.fitted <- fitted_demand$.fitted
  out$.fitted_prob <- 1 - fitted_demand$prob_zero  # P(consumption > 0)

  # Log-scale fitted values (for Part II residuals)
  out$.fitted_link <- fitted_demand$predicted_log_consumption

  y_obs <- data[[y_var]]

  if (component == "continuous") {
    # Part II only: log(y) - fitted_link for positive y, NA for zeros
    out$.resid <- ifelse(
      y_obs > 0,
      log(y_obs) - out$.fitted_link,
      NA_real_
    )
  } else {
    # Randomized quantile residuals (Dunn & Smyth, 1996)
    # Maps each obs through the hurdle CDF, then Phi^{-1}
    sigma_e <- exp(x$model$coefficients[["logsigma_e"]])
    prob_zero <- fitted_demand$prob_zero
    mu <- fitted_demand$predicted_log_consumption

    out$.resid <- .hurdle_quantile_residuals(
      y_obs, prob_zero, mu, sigma_e
    )
  }

  out$.resid_response <- y_obs - out$.fitted

  out
}


#' Randomized quantile residuals for hurdle models
#'
#' @param y Observed consumption values.
#' @param prob_zero Predicted P(zero) for each observation.
#' @param mu Predicted log-consumption mean (Part II) for each observation.
#' @param sigma_e Residual SD on log scale.
#'
#' @return Numeric vector of quantile residuals ~ N(0,1) under correct model.
#' @noRd
.hurdle_quantile_residuals <- function(y, prob_zero, mu, sigma_e) {
  n <- length(y)
  qresid <- numeric(n)

  for (i in seq_len(n)) {
    if (is.na(y[i])) {
      qresid[i] <- NA_real_
    } else if (y[i] == 0) {
      # Zero observation: CDF value is P(zero)
      # Randomize within [0, P(zero)] to break discrete mass
      u <- stats::runif(1, min = 0, max = prob_zero[i])
      # Clamp to avoid Inf from qnorm(0) or qnorm(1)
      u <- max(u, .Machine$double.eps)
      u <- min(u, 1 - .Machine$double.eps)
      qresid[i] <- stats::qnorm(u)
    } else {
      # Positive observation: CDF = P(zero) + P(positive) * F_lognormal(y)
      # F_lognormal(y) = Phi((log(y) - mu) / sigma_e)
      z <- (log(y[i]) - mu[i]) / sigma_e
      F_cond <- stats::pnorm(z)  # CDF of lognormal for the continuous part
      F_hurdle <- prob_zero[i] + (1 - prob_zero[i]) * F_cond
      # Clamp to avoid Inf
      F_hurdle <- max(F_hurdle, .Machine$double.eps)
      F_hurdle <- min(F_hurdle, 1 - .Machine$double.eps)
      qresid[i] <- stats::qnorm(F_hurdle)
    }
  }

  qresid
}
