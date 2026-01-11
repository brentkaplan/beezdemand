#' @title S3 Methods for Joint Hurdle Cross-Price Models
#' @description Print, summary, predict, and plot methods for objects of class
#'   \code{beezdemand_joint_hurdle}.
#' @name joint-hurdle-methods
NULL

# Suppress R CMD check notes for ggplot2 aesthetics
utils::globalVariables(c(
  "stream_name",
  "price_T",
  "y_pred",
  "prob_zero",
  "Value",
  "y"
))

#' @export
print.beezdemand_joint_hurdle <- function(x, ...) {
  joint_type <- if (!is.null(x$joint_type)) x$joint_type else "saturated"

  cat("\nJoint Hurdle Cross-Price Model\n")
  cat("===============================\n\n")

  cat("Variant:", joint_type, "\n")

  cat("Call:\n")
  print(x$call)

  cat("\nObservations:", x$n_obs, "from", x$n_subjects, "subjects\n")
  cat("Streams:\n")
  cat("  alone.target:", x$stream_counts["alone.target"], "\n")
  cat("  own.target:  ", x$stream_counts["own.target"], "\n")
  cat("  own.alt:     ", x$stream_counts["own.alt"], "\n")

  cat("\nConvergence:", if (x$convergence == 0) "Yes" else "No", "\n")
  cat("Log-likelihood:", round(x$logLik, 2), "\n")
  cat("AIC:", round(x$AIC, 2), "\n")

  if (length(x$warnings) > 0) {
    cat("\nWarnings:\n")
    for (w in x$warnings) {
      cat("  -", w, "\n")
    }
  }

  invisible(x)
}


#' Summary method for beezdemand_joint_hurdle
#'
#' Returns a structured summary object containing model coefficients,
#' fit statistics, variance components, and model-specific details.
#'
#' @param object A beezdemand_joint_hurdle object
#' @param ... Additional arguments (ignored)
#' @return A `summary.beezdemand_joint_hurdle` object (inherits from
#'   `beezdemand_summary`) with fields including:
#'   - `call`: The original function call
#'   - `model_class`: "beezdemand_joint_hurdle"
#'   - `backend`: "TMB"
#'   - `joint_type`: "saturated" or "latent"
#'   - `k_fixed`, `k_value`: Whether k was fixed and its value
#'   - `coefficients`: Combined tibble of all coefficients
#'   - `coefficients_hurdle`: Part I coefficients
#'   - `coefficients_by_stream`: List of tibbles by stream
#'   - `variance_components`: Random effect variances
#'   - `correlations`: Random effect correlations
#' @export
summary.beezdemand_joint_hurdle <- function(object, ...) {
  joint_type <- if (!is.null(object$joint_type)) {
    object$joint_type
  } else {
    "saturated"
  }

  coef_nat <- object$coefficients_natural
  coef_raw <- object$coefficients

  # Get standard errors if available
  se <- rep(NA_real_, length(coef_nat))
  names(se) <- names(coef_nat)
  if (!is.null(object$vcov)) {
    vcov_names <- rownames(object$vcov)
    for (nm in names(coef_nat)) {
      if (nm %in% vcov_names) {
        se[nm] <- sqrt(object$vcov[nm, nm])
      }
    }
  }

  # Part I (hurdle) coefficients
  hurdle_params <- c("gamma0", "gamma_own_target", "gamma_own_alt", "gamma1")
  coefficients_hurdle <- tibble::tibble(
    term = hurdle_params,
    estimate = unname(coef_nat[hurdle_params]),
    std.error = unname(se[hurdle_params]),
    statistic = coef_nat[hurdle_params] / se[hurdle_params],
    p.value = 2 * stats::pnorm(-abs(coef_nat[hurdle_params] / se[hurdle_params])),
    component = "zero_probability",
    estimate_scale = "logit",
    term_display = hurdle_params
  )

  # Stream-specific coefficients depend on joint_type
  if (joint_type == "saturated") {
    # alone.target stream
    at_params <- c("logQ0_AT", "alpha_AT")
    coefficients_alone_target <- tibble::tibble(
      term = at_params,
      estimate = unname(coef_nat[at_params]),
      std.error = unname(se[at_params]),
      statistic = coef_nat[at_params] / se[at_params],
      p.value = 2 * stats::pnorm(-abs(coef_nat[at_params] / se[at_params])),
      component = "stream:alone.target",
      estimate_scale = dplyr::case_when(grepl("^log", at_params) ~ "log", TRUE ~ "natural"),
      term_display = at_params
    )

    # own.target stream
    ot_params <- c("logQ0_OT", "alpha_OT")
    coefficients_own_target <- tibble::tibble(
      term = ot_params,
      estimate = unname(coef_nat[ot_params]),
      std.error = unname(se[ot_params]),
      statistic = coef_nat[ot_params] / se[ot_params],
      p.value = 2 * stats::pnorm(-abs(coef_nat[ot_params] / se[ot_params])),
      component = "stream:own.target",
      estimate_scale = dplyr::case_when(grepl("^log", ot_params) ~ "log", TRUE ~ "natural"),
      term_display = ot_params
    )

    # own.alt stream
    oa_params <- c("logQalone_OA", "I", "beta")
    coefficients_own_alt <- tibble::tibble(
      term = oa_params,
      estimate = unname(coef_nat[oa_params]),
      std.error = unname(se[oa_params]),
      statistic = coef_nat[oa_params] / se[oa_params],
      p.value = 2 * stats::pnorm(-abs(coef_nat[oa_params] / se[oa_params])),
      component = "stream:own.alt",
      estimate_scale = dplyr::case_when(grepl("^log", oa_params) ~ "log", TRUE ~ "natural"),
      term_display = oa_params
    )

    # Derived parameters
    derived_params <- tibble::tibble(
      term = c("Q0_AT", "Q0_OT", "Qalone_OA"),
      estimate = c(
        exp(coef_nat["logQ0_AT"]),
        exp(coef_nat["logQ0_OT"]),
        exp(coef_nat["logQalone_OA"])
      ),
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      component = "consumption"
    )

    derived_metrics <- dplyr::bind_rows(
      beezdemand_empty_derived_metrics(),
      tibble::tibble(
        metric = c("Q0_AT", "Q0_OT", "Qalone_OA"),
        estimate = derived_params$estimate,
        std.error = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        method = "none",
        component = "consumption",
        level = "population",
        id = NA_character_
      )
    )

    coefficients_by_stream <- list(
      alone.target = coefficients_alone_target,
      own.target = coefficients_own_target,
      own.alt = coefficients_own_alt
    )

    # Variance components
    variance_components <- tibble::tibble(
      term = c("sigma_a", "sigma_b_AT", "sigma_b_OT", "sigma_b_OA", "sigma_e"),
      estimate = c(
        exp(coef_raw["logsigma_a"]),
        exp(coef_raw["logsigma_b_AT"]),
        exp(coef_raw["logsigma_b_OT"]),
        exp(coef_raw["logsigma_b_OA"]),
        exp(coef_raw["logsigma_e"])
      ),
      description = c("zeros RE", "alone.target RE", "own.target RE",
                      "own.alt RE", "residual")
    )

    # Correlations
    correlations <- tibble::tibble(
      term = c("rho(AT, OT)", "rho(AT, OA)", "rho(OT, OA)"),
      estimate = c(
        tanh(coef_raw["rho_AT_OT_raw"]),
        tanh(coef_raw["rho_AT_OA_raw"]),
        tanh(coef_raw["rho_OT_OA_raw"])
      )
    )

    latent_loadings <- NULL
    latent_sds <- NULL
  } else {
    # Latent model - population means
    theta_params <- c("theta_Q0_AT", "theta_alpha_AT", "theta_Q0_OT",
                      "theta_alpha_OT", "theta_Qalone_OA", "I", "beta")
    coefficients_theta <- tibble::tibble(
      term = theta_params,
      estimate = unname(coef_nat[theta_params]),
      std.error = unname(se[theta_params]),
      statistic = coef_nat[theta_params] / se[theta_params],
      p.value = 2 * stats::pnorm(-abs(coef_nat[theta_params] / se[theta_params])),
      component = "shared",
      estimate_scale = dplyr::case_when(grepl("^log", theta_params) ~ "log", TRUE ~ "natural"),
      term_display = theta_params
    )

    # Latent trait loadings
    lambda_params <- c("lambda_sub_q0", "lambda_sub_alpha", "lambda_sub_alt")
    latent_loadings <- tibble::tibble(
      term = lambda_params,
      estimate = unname(coef_nat[lambda_params]),
      std.error = unname(se[lambda_params]),
      statistic = coef_nat[lambda_params] / se[lambda_params],
      p.value = 2 * stats::pnorm(-abs(coef_nat[lambda_params] / se[lambda_params])),
      component = "shared",
      estimate_scale = "natural",
      term_display = lambda_params
    )

    coefficients_by_stream <- list(
      population_means = coefficients_theta
    )

    derived_params <- tibble::tibble(
      term = character(0), estimate = numeric(0), std.error = numeric(0),
      statistic = numeric(0), p.value = numeric(0), component = character(0)
    )
    derived_metrics <- beezdemand_empty_derived_metrics()

    # Latent trait SDs
    latent_sds <- tibble::tibble(
      term = c("sigma_buy", "sigma_val", "sigma_sens", "sigma_sub", "sigma_e"),
      estimate = c(
        exp(coef_raw["logsigma_buy"]),
        exp(coef_raw["logsigma_val"]),
        exp(coef_raw["logsigma_sens"]),
        exp(coef_raw["logsigma_sub"]),
        exp(coef_raw["logsigma_e"])
      ),
      description = c("buying propensity", "valuation", "price sensitivity",
                      "substitutability", "residual")
    )

    variance_components <- latent_sds

    # Latent trait correlations
    correlations <- tibble::tibble(
      term = c("rho(buy, val)", "rho(buy, sens)", "rho(buy, sub)",
               "rho(val, sens)", "rho(val, sub)", "rho(sens, sub)"),
      estimate = c(
        tanh(coef_raw["rho_buy_val_raw"]),
        tanh(coef_raw["rho_buy_sens_raw"]),
        tanh(coef_raw["rho_buy_sub_raw"]),
        tanh(coef_raw["rho_val_sens_raw"]),
        tanh(coef_raw["rho_val_sub_raw"]),
        tanh(coef_raw["rho_sens_sub_raw"])
      )
    )
  }

  # Shared k parameter
  k_param <- tibble::tibble(
    term = "k",
    estimate = object$k_value,
    std.error = if (!object$k_fixed && !is.null(se["k"])) se["k"] else NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    component = "shared",
    estimate_scale = "natural",
    term_display = "k"
  )

  # Combine all coefficients
  coefficients <- dplyr::bind_rows(
    coefficients_hurdle,
    do.call(dplyr::bind_rows, coefficients_by_stream),
    k_param
  )

  if (joint_type == "latent" && !is.null(latent_loadings)) {
    coefficients <- dplyr::bind_rows(coefficients, latent_loadings)
  }

  structure(
    list(
      call = object$call,
      model_class = "beezdemand_joint_hurdle",
      backend = "TMB",
      joint_type = joint_type,
      nobs = object$n_obs,
      n_subjects = object$n_subjects,
      stream_counts = object$stream_counts,
      k_fixed = object$k_fixed,
      k_value = object$k_value,
      epsilon = object$epsilon,
      converged = object$convergence == 0,
      logLik = object$logLik,
      AIC = object$AIC,
      BIC = object$BIC,
      coefficients = coefficients,
      derived_metrics = derived_metrics,
      coefficients_hurdle = coefficients_hurdle,
      coefficients_by_stream = coefficients_by_stream,
      derived_params = derived_params,
      variance_components = variance_components,
      correlations = correlations,
      latent_loadings = latent_loadings,
      latent_sds = latent_sds,
      notes = object$warnings
    ),
    class = c("summary.beezdemand_joint_hurdle", "beezdemand_summary")
  )
}

#' Print method for summary.beezdemand_joint_hurdle
#'
#' @param x A summary.beezdemand_joint_hurdle object
#' @param digits Number of significant digits to print
#' @param n Number of rows to print for any tables (unused for this class).
#' @param ... Additional arguments (ignored)
#' @export
print.summary.beezdemand_joint_hurdle <- function(x, digits = 4, n = Inf, ...) {
  cat("\nJoint Hurdle Cross-Price Model Summary\n")
  cat("=======================================\n\n")

  # Model info
  cat("Variant:", x$joint_type, "\n")
  cat("Observations:", x$nobs, "\n")
  cat("Subjects:", x$n_subjects, "\n")
  cat("k fixed:", x$k_fixed, "(value:", x$k_value, ")\n")
  cat("Epsilon:", x$epsilon, "\n\n")

  # Stream counts
  cat("Stream Counts:\n")
  cat("  alone.target:", x$stream_counts["alone.target"], "\n")
  cat("  own.target:  ", x$stream_counts["own.target"], "\n")
  cat("  own.alt:     ", x$stream_counts["own.alt"], "\n\n")

  # Fixed effects
  cat("Fixed Effects:\n")
  cat("--------------\n")

  # Part I
  cat("\nPart I (Hurdle - Shared Zeros Model):\n")
  hurdle_df <- as.data.frame(x$coefficients_hurdle[, c("term", "estimate")])
  for (i in seq_len(nrow(hurdle_df))) {
    cat(sprintf("  %-24s %.4f\n", paste0(hurdle_df$term[i], ":"), hurdle_df$estimate[i]))
  }

  if (x$joint_type == "saturated") {
    # Saturated model output
    at <- x$coefficients_by_stream$alone.target
    ot <- x$coefficients_by_stream$own.target
    oa <- x$coefficients_by_stream$own.alt

    cat("\nPart II - Target Demand (alone.target):\n")
    for (i in seq_len(nrow(at))) {
      cat(sprintf("  %-24s %.4f\n", paste0(at$term[i], ":"), at$estimate[i]))
    }
    q0_at <- x$derived_metrics$estimate[x$derived_metrics$metric == "Q0_AT"][1]
    cat(sprintf("  %-24s %.4f\n", "Q0_AT:", q0_at))

    cat("\nPart II - Target Demand (own.target):\n")
    for (i in seq_len(nrow(ot))) {
      cat(sprintf("  %-24s %.4f\n", paste0(ot$term[i], ":"), ot$estimate[i]))
    }
    q0_ot <- x$derived_metrics$estimate[x$derived_metrics$metric == "Q0_OT"][1]
    cat(sprintf("  %-24s %.4f\n", "Q0_OT:", q0_ot))

    cat("\nPart II - Cross-Price (own.alt):\n")
    for (i in seq_len(nrow(oa))) {
      cat(sprintf("  %-24s %.4f\n", paste0(oa$term[i], ":"), oa$estimate[i]))
    }
    qalone_oa <- x$derived_metrics$estimate[x$derived_metrics$metric == "Qalone_OA"][1]
    cat(sprintf("  %-24s %.4f\n", "Qalone_OA:", qalone_oa))

    # Shared k
    cat("\nShared Parameter:\n")
    cat(sprintf("  %-24s %.4f%s\n", "k:",
                x$k_value, if (x$k_fixed) " (fixed)" else " (estimated)"))

    # Variance components
    cat("\nVariance Components:\n")
    cat("--------------------
")
    for (i in seq_len(nrow(x$variance_components))) {
      cat(sprintf("  %-24s %.4f\n",
                  paste0(x$variance_components$term[i], " (",
                         x$variance_components$description[i], "): "),
                  x$variance_components$estimate[i]))
    }

    # Correlations
    cat("\nIntensity RE Correlations:\n")
    for (i in seq_len(nrow(x$correlations))) {
      cat(sprintf("  %-24s %.4f\n",
                  paste0(x$correlations$term[i], ":"),
                  x$correlations$estimate[i]))
    }
  } else {
    # Latent model output
    pm <- x$coefficients_by_stream$population_means

    cat("\nPart II - Population Means (theta):\n")
    for (i in seq_len(nrow(pm))) {
      cat(sprintf("  %-24s %.4f\n", paste0(pm$term[i], ":"), pm$estimate[i]))
    }

    # Shared k
    cat("\nShared Parameter:\n")
    cat(sprintf("  %-24s %.4f%s\n", "k:",
                x$k_value, if (x$k_fixed) " (fixed)" else " (estimated)"))

    # Latent trait loadings
    if (!is.null(x$latent_loadings) && nrow(x$latent_loadings) > 0) {
      cat("\nLatent Trait Loadings:\n")
      for (i in seq_len(nrow(x$latent_loadings))) {
        cat(sprintf("  %-24s %.4f\n",
                    paste0(x$latent_loadings$term[i], ":"),
                    x$latent_loadings$estimate[i]))
      }
    }

    # Latent trait SDs
    cat("\nLatent Trait Standard Deviations:\n")
    for (i in seq_len(nrow(x$latent_sds))) {
      cat(sprintf("  %-24s %.4f\n",
                  paste0(x$latent_sds$term[i], ":"),
                  x$latent_sds$estimate[i]))
    }

    # Latent trait correlations
    cat("\nLatent Trait Correlations:\n")
    for (i in seq_len(nrow(x$correlations))) {
      cat(sprintf("  %-24s %.4f\n",
                  paste0(x$correlations$term[i], ":"),
                  x$correlations$estimate[i]))
    }
  }

  # Model fit
  cat("\nModel Fit:\n")
  cat("----------\n")
  cat("  Log-likelihood:        ", sprintf("%.2f", x$logLik), "\n")
  cat("  AIC:                   ", sprintf("%.2f", x$AIC), "\n")
  cat("  BIC:                   ", sprintf("%.2f", x$BIC), "\n")
  cat("  Convergence:           ", if (x$converged) "Yes" else "No", "\n")

  if (length(x$notes) > 0) {
    cat("\nWarnings:\n")
    for (w in x$notes) {
      cat("  - ", w, "\n")
    }
  }

  invisible(x)
}


#' @export
coef.beezdemand_joint_hurdle <- function(
  object,
  type = c("all", "fixed", "hurdle", "demand", "crossprice", "latent"),
  ...
) {
  type <- match.arg(type)
  joint_type <- if (!is.null(object$joint_type)) {
    object$joint_type
  } else {
    "saturated"
  }

  coef <- object$coefficients_natural

  if (type == "all") {
    return(coef)
  }

  if (type == "fixed") {
    return(coef[!grepl("^logsigma|^rho_", names(coef))])
  }

  if (type == "hurdle") {
    return(coef[c("gamma0", "gamma_own_target", "gamma_own_alt", "gamma1")])
  }

  if (type == "demand") {
    if (joint_type == "saturated") {
      return(coef[c("logQ0_AT", "alpha_AT", "logQ0_OT", "alpha_OT", "k")])
    } else {
      return(coef[c(
        "theta_Q0_AT",
        "theta_alpha_AT",
        "theta_Q0_OT",
        "theta_alpha_OT",
        "k"
      )])
    }
  }

  if (type == "crossprice") {
    if (joint_type == "saturated") {
      return(coef[c("logQalone_OA", "I", "beta")])
    } else {
      return(coef[c("theta_Qalone_OA", "I", "beta")])
    }
  }

  if (type == "latent") {
    if (joint_type == "latent") {
      return(coef[c("lambda_sub_q0", "lambda_sub_alpha", "lambda_sub_alt")])
    } else {
      warning("Latent loadings only available for joint_type = 'latent'")
      return(NULL)
    }
  }
}


#' @export
logLik.beezdemand_joint_hurdle <- function(object, ...) {
  ll <- object$logLik
  attr(ll, "df") <- length(object$coefficients)
  attr(ll, "nobs") <- object$n_obs
  class(ll) <- "logLik"
  ll
}


#' @export
AIC.beezdemand_joint_hurdle <- function(object, ..., k = 2) {
  -2 * object$logLik + k * length(object$coefficients)
}


#' @export
BIC.beezdemand_joint_hurdle <- function(object, ...) {
  -2 * object$logLik + log(object$n_obs) * length(object$coefficients)
}


#' Predict from Joint Hurdle Model
#'
#' @param object A \code{beezdemand_joint_hurdle} object.
#' @param newdata Data frame with price_T values. If NULL, uses original data.
#' @param type One of `"response"` (predicted consumption), `"link"` (linear predictor),
#'   `"probability"` (P(y=0)), or `"parameters"` (subject-level parameters).
#' @param stream Which stream(s) to predict for: "all", "alone.target",
#'   "own.target", "own.alt".
#' @param level "population" or "subject". Currently, only population predictions are
#'   implemented; this argument is reserved for future subject-level prediction support.
#' @param se.fit Logical; if `TRUE`, includes a `.se.fit` column (currently `NA`
#'   because standard errors are not implemented for `beezdemand_joint_hurdle` predictions).
#' @param interval One of `"none"` (default) or `"confidence"`. When requested,
#'   `.lower`/`.upper` are returned as `NA`.
#' @param interval_level Confidence level when `interval = "confidence"`. Currently
#'   used only for validation.
#' @param ... Additional arguments (unused).
#'
#' @return Predictions as a data frame or matrix.
#' @export
predict.beezdemand_joint_hurdle <- function(
  object,
  newdata = NULL,
  type = c("response", "link", "probability", "parameters"),
  stream = c("all", "alone.target", "own.target", "own.alt"),
  level = c("population", "subject"),
  se.fit = FALSE,
  interval = c("none", "confidence"),
  interval_level = 0.95,
  ...
) {
  type <- match.arg(type)
  stream <- match.arg(stream)
  level <- match.arg(level)
  interval <- match.arg(interval)
  if (!is.null(interval_level) && (!is.numeric(interval_level) || length(interval_level) != 1 ||
    is.na(interval_level) || interval_level <= 0 || interval_level >= 1)) {
    stop("'interval_level' must be a single number between 0 and 1.", call. = FALSE)
  }

  coef <- object$coefficients_natural

  if (type == "parameters") {
    # Return subject-level parameters
    re <- object$random_effects

    # Compute subject-level parameters
    subject_pars <- data.frame(
      id = rownames(re),
      # Zeros
      a_i = re[, "a_i"],
      # alone.target
      logQ0_AT_i = coef["logQ0_AT"] + re[, "b_AT_i"],
      Q0_AT_i = exp(coef["logQ0_AT"] + re[, "b_AT_i"]),
      # own.target
      logQ0_OT_i = coef["logQ0_OT"] + re[, "b_OT_i"],
      Q0_OT_i = exp(coef["logQ0_OT"] + re[, "b_OT_i"]),
      # own.alt
      logQalone_OA_i = coef["logQalone_OA"] + re[, "b_OA_i"],
      Qalone_OA_i = exp(coef["logQalone_OA"] + re[, "b_OA_i"]),
      stringsAsFactors = FALSE
    )
    rownames(subject_pars) <- NULL
    return(tibble::as_tibble(subject_pars))
  }

  # Get price values
  if (is.null(newdata)) {
    prices <- sort(unique(object$data$price_T))
  } else {
    if ("price_T" %in% names(newdata)) {
      prices <- newdata$price_T
    } else if ("x" %in% names(newdata)) {
      prices <- newdata$x
    } else {
      stop("newdata must contain 'price_T' or 'x' column")
    }
  }

  epsilon <- object$epsilon
  k_val <- object$k_value

  # Define streams to predict
  streams <- if (stream == "all") {
    c("alone.target", "own.target", "own.alt")
  } else {
    stream
  }

  results <- list()

  for (s in streams) {
    if (type == "probability") {
      # P(y = 0)
      gamma_s <- switch(
        s,
        "alone.target" = 0,
        "own.target" = coef["gamma_own_target"],
        "own.alt" = coef["gamma_own_alt"]
      )
      eta <- coef["gamma0"] + gamma_s + coef["gamma1"] * log(prices + epsilon)
      prob_zero <- exp(eta) / (1 + exp(eta))

      results[[s]] <- data.frame(
        stream = s,
        price_T = prices,
        prob_zero = prob_zero,
        .fitted = prob_zero,
        stringsAsFactors = FALSE
      )
    } else {
      # type == "response" or "link"
      if (s %in% c("alone.target", "own.target")) {
        # Zhao demand model
        logQ0 <- if (s == "alone.target") coef["logQ0_AT"] else coef["logQ0_OT"]
        alpha <- if (s == "alone.target") coef["alpha_AT"] else coef["alpha_OT"]

        mu <- logQ0 + k_val * (exp(-alpha * prices) - 1)
        y_pred <- exp(mu)
      } else {
        # Cross-price model
        mu <- coef["logQalone_OA"] + coef["I"] * exp(-coef["beta"] * prices)
        y_pred <- exp(mu)
      }

      fitted <- if (type == "link") as.numeric(mu) else as.numeric(y_pred)
      results[[s]] <- data.frame(
        stream = s,
        price_T = prices,
        y_pred = as.numeric(y_pred),
        mu = as.numeric(mu),
        .fitted = fitted,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- tibble::as_tibble(do.call(rbind, results))

  if (isTRUE(se.fit) || interval != "none") {
    warning(
      "Standard errors/intervals are not implemented for `beezdemand_joint_hurdle` predictions; returning NA.",
      call. = FALSE
    )
    out$.se.fit <- NA_real_
    if (interval != "none") {
      out$.lower <- NA_real_
      out$.upper <- NA_real_
    }
  }

  out
}


#' Plot Joint Hurdle Model
#'
#' @param x A \code{beezdemand_joint_hurdle} object.
#' @param type One of "demand", "population", "probability", "individual", "parameters".
#' @param stream Which stream(s) to plot.
#' @param n_points Number of points for smooth curves (default 200).
#' @param x_trans Character. Transformation for x-axis. Default "log".
#' @param y_trans Character. Transformation for y-axis. Default "log".
#' @param free_trans Value used to display free (x = 0) on log scales. Use NULL
#'   to drop x <= 0 values instead.
#' @param x_limits Optional numeric vector of length 2 for x-axis limits.
#' @param y_limits Optional numeric vector of length 2 for y-axis limits.
#' @param x_lab Optional x-axis label.
#' @param y_lab Optional y-axis label.
#' @param xlab Deprecated alias for \code{x_lab}.
#' @param ylab Deprecated alias for \code{y_lab}.
#' @param style Plot styling, passed to \code{theme_beezdemand()}.
#' @param observed_point_alpha Alpha for observed points.
#' @param observed_point_size Size for observed points.
#' @param pop_line_alpha Alpha for population curve.
#' @param pop_line_size Line size for population curve.
#' @param ... Additional arguments passed to ggplot.
#'
#' @return A ggplot2 object.
#' @export
plot.beezdemand_joint_hurdle <- function(
  x,
  type = c("demand", "population", "probability", "individual", "parameters"),
  stream = c("all", "alone.target", "own.target", "own.alt"),
  n_points = 200,
  x_trans = c("log10", "log", "linear", "pseudo_log"),
  y_trans = NULL,
  free_trans = 0.01,
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
  ...
) {
  y_trans_missing <- is.null(y_trans)
  type <- match.arg(type)
  if (type == "population") {
    type <- "demand"
  }
  stream <- match.arg(stream)
  style <- match.arg(style)
  x_trans <- match.arg(x_trans)
  if (y_trans_missing) {
    y_trans <- beezdemand_default_y_trans(type = type)
  }
  y_trans <- match.arg(y_trans, c("log10", "log", "linear", "pseudo_log"))

  labels <- beezdemand_normalize_plot_labels(x_lab, y_lab, xlab, ylab)
  x_lab <- labels$x_lab %||% "Price"
  y_lab <- labels$y_lab %||% "Consumption"

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  free_trans_used <- FALSE
  subtitle_note <- FALSE

  x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
  y_limits <- beezdemand_resolve_limits(y_limits, y_trans, axis = "y")

  if (type == "demand") {
    # Population demand curves
    prices <- seq(min(x$data$price_T), max(x$data$price_T), length.out = n_points)
    preds <- predict(
      x,
      newdata = data.frame(price_T = prices),
      type = "response",
      stream = stream
    )

    preds$stream_name <- factor(
      preds$stream,
      levels = c("alone.target", "own.target", "own.alt"),
      labels = c("Target (Alone)", "Target (Own)", "Alternative")
    )

    # Original data
    dat <- x$data
    dat$stream_name <- factor(
      c("alone.target", "own.target", "own.alt")[dat$stream + 1],
      levels = c("alone.target", "own.target", "own.alt"),
      labels = c("Target (Alone)", "Target (Own)", "Alternative")
    )

    if (stream != "all") {
      dat <- dat[dat$stream_name == preds$stream_name[1], ]
    }

    pred_df <- data.frame(
      x = preds$price_T,
      y = preds$y_pred,
      stream_name = preds$stream_name
    )
    free_pred <- beezdemand_apply_free_trans(pred_df, "x", x_trans, free_trans)
    pred_df <- free_pred$data
    free_trans_used <- free_trans_used || free_pred$replaced

    pred_y <- beezdemand_drop_nonpositive_y(pred_df, "y", y_trans)
    pred_df <- pred_y$data
    subtitle_note <- subtitle_note || pred_y$dropped

    obs_df <- data.frame(
      x = dat$price_T,
      y = dat$y,
      stream_name = dat$stream_name
    )
    free_obs <- beezdemand_apply_free_trans(obs_df, "x", x_trans, free_trans)
    obs_df <- free_obs$data
    free_trans_used <- free_trans_used || free_obs$replaced

    obs_y <- beezdemand_drop_nonpositive_y(obs_df, "y", y_trans)
    obs_df <- obs_y$data
    subtitle_note <- subtitle_note || obs_y$dropped

    p <- ggplot2::ggplot()

    if (nrow(obs_df) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = obs_df,
          ggplot2::aes(x = x, y = y, color = stream_name),
          alpha = observed_point_alpha,
          size = observed_point_size
        )
    }

    if (nrow(pred_df) > 0) {
      p <- p +
        ggplot2::geom_line(
          data = pred_df,
          ggplot2::aes(x = x, y = y, color = stream_name),
          linewidth = pop_line_size,
          alpha = pop_line_alpha
        )
    }

    p <- p +
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
      ggplot2::labs(
        x = x_lab,
        y = y_lab,
        color = "Stream",
        title = "Joint Model: Demand Curves"
      ) +
      theme_beezdemand(style = style)

    p <- beezdemand_apply_color_scale(p, style, pred_df, "stream_name")
  } else if (type == "probability") {
    # P(y = 0) curves
    prices <- seq(min(x$data$price_T), max(x$data$price_T), length.out = n_points)
    preds <- predict(
      x,
      newdata = data.frame(price_T = prices),
      type = "probability",
      stream = stream
    )

    preds$stream_name <- factor(
      preds$stream,
      levels = c("alone.target", "own.target", "own.alt"),
      labels = c("Target (Alone)", "Target (Own)", "Alternative")
    )

    pred_df <- data.frame(
      x = preds$price_T,
      y = preds$prob_zero,
      stream_name = preds$stream_name
    )
    free_pred <- beezdemand_apply_free_trans(pred_df, "x", x_trans, free_trans)
    pred_df <- free_pred$data
    free_trans_used <- free_trans_used || free_pred$replaced

    p <- ggplot2::ggplot(
      pred_df,
      ggplot2::aes(x = x, y = y, color = stream_name)
    )

    if (nrow(pred_df) > 0) {
      p <- p +
        ggplot2::geom_line(
          linewidth = pop_line_size,
          alpha = pop_line_alpha
        )
    }

    p <- p +
      ggplot2::scale_x_continuous(
        trans = beezdemand_get_trans(x_trans),
        limits = x_limits,
        labels = beezdemand_axis_labels()
      ) +
      ggplot2::scale_y_continuous(
        trans = beezdemand_get_trans(y_trans),
        limits = y_limits,
        labels = beezdemand_axis_labels()
      ) +
      ggplot2::labs(
        x = x_lab,
        y = "P(Consumption = 0)",
        color = "Stream",
        title = "Joint Model: Zero Probability"
      ) +
      theme_beezdemand(style = style)

    p <- beezdemand_apply_color_scale(p, style, pred_df, "stream_name")
  } else if (type == "parameters") {
    # Subject parameter distributions
    pars <- predict(x, type = "parameters")

    # Reshape for plotting
    par_long <- data.frame(
      Parameter = rep(
        c("Q0 (Alone)", "Q0 (Own)", "Qalone (Alt)"),
        each = nrow(pars)
      ),
      Value = c(pars$Q0_AT_i, pars$Q0_OT_i, pars$Qalone_OA_i),
      stringsAsFactors = FALSE
    )

    p <- ggplot2::ggplot(par_long, ggplot2::aes(x = Value)) +
      ggplot2::geom_histogram(
        bins = 20,
        fill = beezdemand_style_color(style, "accent"),
        color = "white",
        alpha = 0.7
      ) +
      ggplot2::facet_wrap(~Parameter, scales = "free") +
      ggplot2::labs(
        x = "Parameter Value",
        y = "Count",
        title = "Subject-Level Parameter Distributions"
      ) +
      ggplot2::scale_x_continuous(
        trans = beezdemand_get_trans(x_trans),
        limits = x_limits,
        labels = beezdemand_axis_labels()
      ) +
      theme_beezdemand(style = style)
  } else {
    # Individual curves
    stop("Individual curves not yet implemented for joint model")
  }

  if (isTRUE(subtitle_note)) {
    p <- p + ggplot2::labs(subtitle = "Zeros omitted on log scale.")
  }
  if (type %in% c("demand", "probability")) {
    if (x_trans == "log10") {
      p <- p + ggplot2::annotation_logticks(sides = "b")
    }
    if (y_trans == "log10") {
      p <- p + ggplot2::annotation_logticks(sides = "l")
    }
  }
  beezdemand_warn_free_trans(free_trans_used, free_trans)

  p
}


#' Tidy Method for Joint Hurdle Model
#'
#' @param x A \code{beezdemand_joint_hurdle} object.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with coefficient information including:
#'   - `term`: Parameter name
#'   - `estimate`: Point estimate
#'   - `std.error`: Standard error (if available)
#'   - `statistic`: z-value
#'   - `p.value`: P-value
#'   - `component`: One of "hurdle", "alone.target", "own.target",
#'     "own.alt", "variance", "latent_loadings", "shared"
#' @export
tidy.beezdemand_joint_hurdle <- function(x, ...) {
  coef <- x$coefficients_natural
  joint_type <- if (!is.null(x$joint_type)) x$joint_type else "saturated"

  # Get standard errors if available
  se <- rep(NA_real_, length(coef))
  names(se) <- names(coef)
  if (!is.null(x$vcov)) {
    vcov_names <- rownames(x$vcov)
    for (nm in names(coef)) {
      if (nm %in% vcov_names) {
        se[nm] <- sqrt(x$vcov[nm, nm])
      }
    }
  }

  # Determine component based on parameter name
  component <- rep("variance", length(coef))
  component[grepl("^gamma", names(coef))] <- "hurdle"
  component[names(coef) == "k"] <- "shared"

  if (joint_type == "saturated") {
    component[grepl("_AT$", names(coef))] <- "alone.target"
    component[grepl("_OT$", names(coef))] <- "own.target"
    component[grepl("_OA$", names(coef)) | names(coef) %in% c("I", "beta")] <- "own.alt"
  } else {
    # Latent model
    component[grepl("^theta_", names(coef))] <- "population_means"
    component[grepl("^lambda_", names(coef))] <- "latent_loadings"
    component[names(coef) %in% c("I", "beta")] <- "population_means"
  }

  # Calculate z-values and p-values
  z_val <- coef / se
  p_val <- 2 * stats::pnorm(-abs(z_val))

  out <- tibble::tibble(
    term = names(coef),
    estimate = unname(coef),
    std.error = unname(se),
    statistic = unname(z_val),
    p.value = unname(p_val),
    component = component,
    estimate_scale = dplyr::case_when(
      grepl("^log|^logsigma_", names(coef)) ~ "log",
      TRUE ~ "natural"
    ),
    term_display = names(coef)
  )

  beezdemand_transform_coef_table(
    coef_tbl = out,
    report_space = "natural",
    internal_space = "natural"
  )
}


#' Glance Method for Joint Hurdle Model
#'
#' @param x A \code{beezdemand_joint_hurdle} object.
#' @param ... Additional arguments (unused).
#'
#' @return A one-row tibble with model-level statistics including:
#'   - `model_class`: "beezdemand_joint_hurdle"
#'   - `backend`: "TMB"
#'   - `joint_type`: "saturated" or "latent"
#'   - `nobs`: Number of observations
#'   - `n_subjects`: Number of subjects
#'   - `converged`: Convergence status
#'   - `logLik`, `AIC`, `BIC`: Model fit statistics
#'   - `k_fixed`, `k_value`: Whether k was fixed and its value
#' @export
glance.beezdemand_joint_hurdle <- function(x, ...) {
  joint_type <- if (!is.null(x$joint_type)) x$joint_type else "saturated"

  tibble::tibble(
    model_class = "beezdemand_joint_hurdle",
    backend = "TMB",
    joint_type = joint_type,
    nobs = x$n_obs,
    n_subjects = x$n_subjects,
    n_alone_target = unname(x$stream_counts["alone.target"]),
    n_own_target = unname(x$stream_counts["own.target"]),
    n_own_alt = unname(x$stream_counts["own.alt"]),
    converged = x$convergence == 0,
    logLik = x$logLik,
    AIC = x$AIC,
    BIC = x$BIC,
    k_fixed = x$k_fixed,
    k_value = x$k_value
  )
}
