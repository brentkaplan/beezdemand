# ==============================================================================
# S3 Methods for beezdemand_tmb Objects
# ==============================================================================

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
  q0_idx <- which(nms == "beta_q0")
  alpha_idx <- which(nms == "beta_alpha")
  other_idx <- which(!nms %in% c("beta_q0", "beta_alpha"))

  # Get design matrix column names for readable term labels
  q0_colnames <- colnames(object$formula_details$X_q0)
  alpha_colnames <- colnames(object$formula_details$X_alpha)

  term <- character(length(nms))
  term[q0_idx] <- paste0("Q0:", q0_colnames)
  term[alpha_idx] <- paste0("alpha:", alpha_colnames)
  term[other_idx] <- nms[other_idx]

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

  result <- structure(
    list(
      call = object$call,
      model_class = "beezdemand_tmb",
      backend = "TMB",
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
  object$AIC
}

#' @export
BIC.beezdemand_tmb <- function(object, ...) {
  object$BIC
}


# --- fixef / ranef ---

#' Extract Fixed Effects from TMB Model
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param ... Additional arguments.
#'
#' @return Named numeric vector of fixed effects.
#' @export
fixef.beezdemand_tmb <- function(object, ...) {
  coef(object)
}

#' Extract Random Effects from TMB Model
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param ... Additional arguments.
#'
#' @return Data frame with subject-level random effects.
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
#' @param ... Additional arguments.
#'
#' @return Depends on `type`:
#'   - `"response"`: tibble with .fitted column
#'   - `"parameters"`: tibble of subject-specific parameters
#'   - `"demand"`: tibble with price and .fitted columns
#'
#' @export
predict.beezdemand_tmb <- function(
  object,
  newdata = NULL,
  type = c("response", "parameters", "demand"),
  prices = NULL,
  ...
) {
  type <- match.arg(type)

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

    # Get population intercepts
    beta_q0_idx <- which(names(coefs) == "beta_q0")
    beta_alpha_idx <- which(names(coefs) == "beta_alpha")
    log_q0 <- coefs[beta_q0_idx[1]]
    log_alpha <- coefs[beta_alpha_idx[1]]
    Q0 <- exp(log_q0)
    alpha_val <- exp(log_alpha)

    fitted <- .tmb_predict_equation(
      prices, Q0, alpha_val,
      k = if (has_k) .tmb_get_k(object) else NA,
      log_q0 = log_q0,
      equation = equation
    )

    return(tibble::tibble(
      price = prices,
      .fitted = fitted
    ))
  }

  # type == "response": subject-specific fitted values
  if (is.null(newdata)) {
    newdata <- object$data
  }

  subjects <- as.character(object$param_info$subject_levels)
  spars <- object$subject_pars

  # Build predictions per observation
  n <- nrow(newdata)
  fitted_vals <- numeric(n)

  for (i in seq_len(n)) {
    subj <- as.character(newdata[[id_var]][i])
    price_i <- newdata[[x_var]][i]

    row_idx <- which(spars$id == subj)
    if (length(row_idx) == 0) {
      # Unknown subject: use population parameters
      beta_q0_idx <- which(names(coefs) == "beta_q0")
      beta_alpha_idx <- which(names(coefs) == "beta_alpha")
      Q0_i <- exp(coefs[beta_q0_idx[1]])
      alpha_i <- exp(coefs[beta_alpha_idx[1]])
    } else {
      Q0_i <- spars$Q0[row_idx[1]]
      alpha_i <- spars$alpha[row_idx[1]]
    }

    beta_q0_idx <- which(names(coefs) == "beta_q0")
    log_q0_i <- log(Q0_i)

    fitted_vals[i] <- .tmb_predict_equation(
      price_i, Q0_i, alpha_i,
      k = if (has_k) .tmb_get_k(object) else NA,
      log_q0 = log_q0_i,
      equation = equation
    )
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
      # Returns log(Q)
      log_q0 + k * (exp(-alpha * Q0 * price) - 1)
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
      rate <- (alpha / Q0_log10) * Q0
      Q0_log10 * exp(-rate * price)
    }
  )
}


# --- get_subject_pars ---

#' Get Subject-Specific Parameters from TMB Model
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame with columns: id, b_i, c_i (if 2 RE), Q0, alpha, Pmax, Omax.
#' @export
get_subject_pars.beezdemand_tmb <- function(object, ...) {
  object$subject_pars
}


# --- plot ---

#' Plot TMB Mixed-Effects Demand Model
#'
#' @param x A \code{beezdemand_tmb} object.
#' @param type Character. One of `"demand"` (population curve), `"individual"`
#'   (per-subject curves), `"parameters"` (parameter distributions).
#' @param ids Character vector of subject IDs to plot (for individual type).
#' @param prices Optional numeric vector of prices for curve generation.
#' @param show_population Logical. Show population curve overlay.
#' @param show_observed Logical. Show observed data points.
#' @param x_trans Character. X-axis transformation.
#' @param y_trans Character. Y-axis transformation.
#' @param inv_fun Optional function to back-transform y-axis (e.g., `ll4_inv`).
#' @param x_lab Character. X-axis label.
#' @param y_lab Character. Y-axis label.
#' @param style Character. Plot style: "modern" or "apa".
#' @param ... Additional arguments.
#'
#' @return A ggplot2 object.
#' @export
plot.beezdemand_tmb <- function(
  x,
  type = c("demand", "individual", "parameters"),
  ids = NULL,
  prices = NULL,
  show_population = TRUE,
  show_observed = TRUE,
  x_trans = c("log10", "log", "linear", "pseudo_log"),
  y_trans = NULL,
  inv_fun = NULL,
  x_lab = NULL,
  y_lab = NULL,
  style = c("modern", "apa"),
  ...
) {
  type <- match.arg(type)
  x_trans <- match.arg(x_trans)
  y_trans_missing <- is.null(y_trans)
  if (y_trans_missing) {
    y_trans <- if (x$param_info$equation == "exponential") "linear" else "pseudo_log"
  }
  y_trans <- match.arg(y_trans, c("log10", "log", "linear", "pseudo_log"))
  style <- match.arg(style)

  equation <- x$param_info$equation
  x_var <- x$param_info$x_var
  id_var <- x$param_info$id_var
  x_lab <- x_lab %||% "Price"
  y_lab <- y_lab %||% switch(equation,
    exponential = "log(Consumption)",
    zben = "LL4(Consumption)",
    "Consumption"
  )

  # Price sequence
  if (is.null(prices)) {
    max_price <- max(x$data[[x_var]], na.rm = TRUE)
    prices <- seq(0, max_price, length.out = 200)
  }

  if (type == "parameters") {
    return(.tmb_plot_parameters(x, style = style))
  }

  # Population prediction
  pop_pred <- predict(x, type = "demand", prices = prices)

  if (!is.null(inv_fun)) {
    pop_pred$.fitted <- inv_fun(pop_pred$.fitted)
  }

  p <- ggplot2::ggplot()

  # Observed data
  if (show_observed && type != "demand") {
    obs_data <- x$data
    y_obs <- obs_data[[x$param_info$y_var]]
    if (!is.null(inv_fun)) y_obs <- inv_fun(y_obs)
    obs_df <- data.frame(
      price = obs_data[[x_var]],
      consumption = y_obs,
      id = obs_data[[id_var]]
    )
    p <- p + ggplot2::geom_point(
      data = obs_df,
      ggplot2::aes(x = .data$price, y = .data$consumption,
                   group = .data$id),
      alpha = 0.3, size = 1.5
    )
  }

  if (type == "individual" && show_observed) {
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
      alpha = 0.3, linewidth = 0.5
    )
  }

  if (show_population) {
    p <- p + ggplot2::geom_line(
      data = pop_pred,
      ggplot2::aes(x = .data$price, y = .data$.fitted),
      color = "blue", linewidth = 1.2
    )
  }

  # Axis transforms
  if (x_trans == "log10") {
    p <- p + ggplot2::scale_x_log10()
  } else if (x_trans == "pseudo_log") {
    p <- p + ggplot2::scale_x_continuous(trans = scales::pseudo_log_trans())
  }

  if (y_trans == "log10") {
    p <- p + ggplot2::scale_y_log10()
  } else if (y_trans == "pseudo_log") {
    p <- p + ggplot2::scale_y_continuous(trans = scales::pseudo_log_trans())
  }

  p <- p +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::theme_minimal()

  if (style == "apa") {
    p <- p + theme_apa()
  }

  p
}


#' @keywords internal
.tmb_plot_parameters <- function(x, style = "modern") {
  spars <- x$subject_pars
  plot_data <- tidyr::pivot_longer(
    spars,
    cols = c("Q0", "alpha", "Pmax", "Omax"),
    names_to = "parameter",
    values_to = "value"
  )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
    ggplot2::facet_wrap(~ parameter, scales = "free") +
    ggplot2::labs(x = "Value", y = "Count") +
    ggplot2::theme_minimal()

  if (style == "apa") p <- p + theme_apa()
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
  q0_idx <- which(nms == "beta_q0")
  alpha_idx <- which(nms == "beta_alpha")

  q0_colnames <- colnames(x$formula_details$X_q0)
  alpha_colnames <- colnames(x$formula_details$X_alpha)

  term <- character(length(nms))
  term[q0_idx] <- paste0("Q0:", q0_colnames)
  term[alpha_idx] <- paste0("alpha:", alpha_colnames)
  other_idx <- which(!nms %in% c("beta_q0", "beta_alpha"))
  term[other_idx] <- nms[other_idx]

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
#' @export
glance.beezdemand_tmb <- function(x, ...) {
  tibble::tibble(
    model_class = "beezdemand_tmb",
    backend = "TMB",
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
#' @return A tibble with original data plus .fitted and .resid columns.
#' @export
augment.beezdemand_tmb <- function(x, newdata = NULL, ...) {
  pred <- predict(x, newdata = newdata, type = "response")
  equation <- x$param_info$equation
  y_var <- x$param_info$y_var

  data_used <- if (is.null(newdata)) x$data else newdata
  y_obs <- data_used[[y_var]]

  pred$.resid <- y_obs - pred$.fitted
  pred
}


# --- confint ---

#' Confidence Intervals for TMB Model Parameters
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param parm Character vector of parameter names.
#' @param level Confidence level (default 0.95).
#' @param report_space Character. `"internal"` or `"natural"`.
#' @param ... Additional arguments.
#'
#' @return A tibble with term, estimate, conf.low, conf.high, level.
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

  if (!is.null(parm)) {
    keep <- nms %in% parm
    coefs <- coefs[keep]
    se_vec <- se_vec[keep]
    nms <- nms[keep]
  }

  z <- stats::qnorm((1 + level) / 2)

  # Create readable term names
  q0_idx <- which(nms == "beta_q0")
  alpha_idx <- which(nms == "beta_alpha")
  q0_colnames <- colnames(object$formula_details$X_q0)
  alpha_colnames <- colnames(object$formula_details$X_alpha)

  term <- character(length(nms))
  term[q0_idx] <- paste0("Q0:", q0_colnames)
  term[alpha_idx] <- paste0("alpha:", alpha_colnames)
  other_idx <- which(!nms %in% c("beta_q0", "beta_alpha"))
  term[other_idx] <- nms[other_idx]

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
