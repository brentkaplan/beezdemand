#' Print Method for Hurdle Demand Model
#'
#' @param x An object of class \code{beezdemand_hurdle}.
#' @param ... Additional arguments (currently unused).
#'
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
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
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

  # Build coefficient table (matrix form for printing)
  coef_matrix <- cbind(
    Estimate = coefs,
    `Std. Error` = se_vec,
    `t value` = coefs / se_vec
  )

  # Build coefficient tibble (for contract compliance)
  coef_names <- names(coefs)
  z_val <- coefs / se_vec
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
      statistic = .data$estimate / .data$std.error,
      p.value = 2 * stats::pnorm(-abs(.data$statistic))
    )

  # Compute group-level demand metrics (Omax, Pmax) via unified engine
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

  # Strategy B alpha* (normalized alpha; depends on alpha and k)
  notes <- character(0)
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
      backend = "TMB",
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
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
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
#' @export
BIC.beezdemand_hurdle <- function(object, ...) {
  ll <- logLik(object)
  -2 * as.numeric(ll) + log(attr(ll, "nobs")) * attr(ll, "df")
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
#' @param se.fit Logical; if `TRUE`, includes a `.se.fit` column (delta-method via
#'   `sdreport` when available).
#' @param interval One of `"none"` (default) or `"confidence"`.
#' @param level Confidence level when `interval = "confidence"`.
#' @param ... Unused.
#'
#' @return For `type = "parameters"`, a tibble of subject-level parameters.
#'   Otherwise, a tibble containing the `newdata` columns plus `.fitted` and
#'   helper columns `predicted_log_consumption`, `predicted_consumption`,
#'   `prob_zero`, and `expected_consumption`. When requested, `.se.fit` and
#'   `.lower`/`.upper` are included.
#'
#' @examples
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
#'
#' # Get subject-specific parameters
#' pars <- predict(fit, type = "parameters")
#'
#' # Predict demand at specific prices
#' demand <- predict(fit, type = "demand", prices = c(0, 0.5, 1, 2, 5))
#'
#' # Predict probability of zero consumption
#' probs <- predict(fit, type = "probability", prices = seq(0, 10, by = 0.5))
#' }
#'
#' @export
predict.beezdemand_hurdle <- function(
  object,
  newdata = NULL,
  type = c("response", "link", "parameters", "probability", "demand"),
  prices = NULL,
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
    stop("'level' must be a single number between 0 and 1.", call. = FALSE)
  }
  id_var <- object$param_info$id_var
  x_var <- object$param_info$x_var
  epsilon <- object$param_info$epsilon
  part2 <- object$param_info$part2 %||% "zhao_exponential"

  # For type = "parameters", just return subject-specific parameters
  if (type == "parameters") {
    return(tibble::as_tibble(object$subject_pars))
  }

  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
    if (!(x_var %in% names(newdata))) {
      stop("`newdata` must include the price column `", x_var, "`.", call. = FALSE)
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
      stop(
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
  predicted_consumption <- exp(predicted_log_consumption)
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
      warning("vcov is unavailable; returning NA for uncertainty columns.", call. = FALSE)
      out$.se.fit <- NA_real_
      if (interval != "none") {
        out$.lower <- NA_real_
        out$.upper <- NA_real_
      }
      return(out)
    }

    vcov_full <- tryCatch(as.matrix(object$sdr$cov.fixed), error = function(e) NULL)
    if (is.null(vcov_full)) {
      warning("vcov is unavailable; returning NA for uncertainty columns.", call. = FALSE)
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

      resp_t <- exp(mu_t)
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
      out$.lower <- out$.fitted - z * out$.se.fit
      out$.upper <- out$.fitted + z * out$.se.fit
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
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
#'
#' # Plot mean demand curve
#' plot(fit)
#'
#' # Plot probability curves
#' plot(fit, type = "probability")
#'
#' # Plot subject-specific probability curves (with population overlay)
#' plot(fit, type = "probability", ids = c("1", "2", "3"))
#' plot(fit, type = "probability", ids = c("1", "2", "3"), facet = TRUE)
#'
#' # Plot parameter distributions
#' plot(fit, type = "parameters")
#'
#' # Plot individual curves
#' plot(fit, type = "individual", subjects = c("1", "2", "3"))
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
      "plot(x, subjects = )",
      "plot(x, ids = )"
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
      pop_data <- data.frame(
        price = prices,
        eta = beta0 + beta1 * log(prices + epsilon)
      )
      pop_data$prob_zero <- stats::plogis(pop_data$eta)

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
      vals[is.finite(vals)] # Remove Inf/NA
    })
    labels_list <- lapply(parameters, function(p) param_map[[p]])

    pars_long <- data.frame(
      parameter = factor(
        rep(unlist(labels_list), sapply(values_list, length)),
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
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
#' plot_subject(fit, subject_id = "1")
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
    stop("object must be of class 'beezdemand_hurdle'")
  }
  style <- match.arg(style)

  id_var <- object$param_info$id_var
  x_var <- object$param_info$x_var
  y_var <- object$param_info$y_var

  if (!subject_id %in% object$param_info$subject_levels) {
    stop("subject_id not found in model")
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

  out |>
    dplyr::mutate(
      statistic = .data$estimate / .data$std.error,
      p.value = 2 * stats::pnorm(-abs(.data$statistic))
    )
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
    backend = "TMB",
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
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
#' confint(fit)
#' confint(fit, level = 0.90)
#' confint(fit, report_space = "natural")
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
    stop("`level` must be a single number between 0 and 1.", call. = FALSE)
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
    warning("No requested parameters found in model.", call. = FALSE)
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
#' @return A tibble containing the original data plus:
#'   \describe{
#'     \item{.fitted}{Fitted demand values (natural scale)}
#'     \item{.fitted_link}{Fitted values on log scale (Part II mean)}
#'     \item{.fitted_prob}{Predicted probability of consumption (1 - P(zero))}
#'     \item{.resid}{Residuals on log scale for positive observations, NA for zeros}
#'     \item{.resid_response}{Residuals on response scale (y - .fitted)}
#'   }
#'
#' @details
#' For two-part hurdle models:
#' - `.fitted` gives predicted demand on the natural consumption scale
#' - `.fitted_prob` gives the predicted probability of positive consumption
#' - `.resid` is defined only for positive observations as log(y) - .fitted_link
#' - Observations with zero consumption have `.resid = NA` since they are
#'   explained by Part I (the zero-probability component), not Part II
#'
#' @examples
#' \dontrun{
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
augment.beezdemand_hurdle <- function(x, newdata = NULL, ...) {
  if (is.null(newdata)) {
    data <- x$data
  } else {
    data <- newdata
  }

  if (is.null(data)) {
    stop("No data available. Provide 'newdata' or ensure model contains data.",
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

  # Residuals: log(y) - fitted_link for positive y, NA for zeros
  y_obs <- data[[y_var]]
  out$.resid <- ifelse(
    y_obs > 0,
    log(y_obs) - out$.fitted_link,
    NA_real_
  )
  out$.resid_response <- y_obs - out$.fitted

  out
}
