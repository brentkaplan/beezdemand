#' @title S3 Methods for beezdemand_cp_hurdle Objects
#' @name beezdemand_cp_hurdle-methods
#' @description Methods for cross-price hurdle demand model objects.
NULL

# Suppress R CMD check notes for ggplot2 aesthetics
utils::globalVariables(c("Qalone", "I_individual", "prob"))

#' Print method for beezdemand_cp_hurdle
#' @param x A beezdemand_cp_hurdle object
#' @param ... Additional arguments (ignored)
#' @export
print.beezdemand_cp_hurdle <- function(x, ...) {
  cat("Two-Part Mixed Effects Hurdle Cross-Price Demand Model\n")
  cat("======================================================\n\n")

  n_re <- x$param_info$n_random_effects
  cat(
    "Model: ",
    n_re,
    "-RE (",
    paste(x$param_info$random_effects_spec, collapse = ", "),
    ")\n",
    sep = ""
  )
  cat("Subjects:", x$param_info$n_subjects, "\n")
  cat("Observations:", x$param_info$n_obs, "\n")
  cat("Converged:", x$converged, "\n\n")

  cat("Fixed Effects:\n")
  coefs <- x$model$coefficients
  # Display beta instead of log_beta
  coefs_display <- coefs[names(coefs) != "log_beta"]
  coefs_display <- c(coefs_display, beta = x$model$beta)
  print(round(coefs_display, 4))

  cat("\nLog-likelihood:", round(x$loglik, 2), "\n")
  cat("AIC:", round(x$AIC, 2), "\n")
  cat("BIC:", round(x$BIC, 2), "\n")

  invisible(x)
}

#' Summary method for beezdemand_cp_hurdle
#'
#' Returns a structured summary object containing model coefficients,
#' fit statistics, variance components, and interpretation.
#'
#' @param object A beezdemand_cp_hurdle object
#' @param ... Additional arguments (ignored)
#' @return A `summary.beezdemand_cp_hurdle` object (also inherits from
#'   `beezdemand_summary`) with fields:
#'   - `call`: The original function call
#'   - `model_class`: "beezdemand_cp_hurdle"
#'   - `backend`: "TMB"
#'   - `nobs`, `n_subjects`: Data dimensions
#'   - `coefficients`: Tibble of all coefficients with std.error, statistic, p.value
#'   - `coefficients_part1`: Part I (probability) coefficients
#'   - `coefficients_part2`: Part II (consumption) coefficients
#'   - `transformed_params`: Derived parameters (Qalone, beta)
#'   - `variance_components`: Random effect variances
#'   - `correlations`: Random effect correlations (if 3RE)
#'   - `interpretation`: Character describing substitute/complement relationship
#'   - `logLik`, `AIC`, `BIC`: Model fit statistics
#' @export
summary.beezdemand_cp_hurdle <- function(object, ...) {
  n_re <- object$param_info$n_random_effects
  coefs <- object$model$coefficients
  se <- object$model$se


  # Part I coefficients (probability model)
  part1_params <- c("beta0", "beta1")
  part1_coefs <- coefs[part1_params]
  part1_se <- se[part1_params]
  part1_z <- part1_coefs / part1_se
  part1_p <- 2 * stats::pnorm(-abs(part1_z))

  coefficients_part1 <- tibble::tibble(
    term = part1_params,
    estimate = unname(part1_coefs),
    std.error = unname(part1_se),
    statistic = unname(part1_z),
    p.value = unname(part1_p),
    component = "part1_probability"
  )

  # Part II coefficients (consumption model)
  part2_params <- c("logQalone", "I", "log_beta")
  part2_coefs <- coefs[part2_params]
  part2_se <- se[part2_params]
  part2_z <- part2_coefs / part2_se
  part2_p <- 2 * stats::pnorm(-abs(part2_z))

  coefficients_part2 <- tibble::tibble(
    term = part2_params,
    estimate = unname(part2_coefs),
    std.error = unname(part2_se),
    statistic = unname(part2_z),
    p.value = unname(part2_p),
    component = "part2_consumption"
  )

  # Combined coefficients tibble
  coefficients <- dplyr::bind_rows(coefficients_part1, coefficients_part2)

  # Transformed parameters
  transformed_params <- tibble::tibble(
    term = c("Qalone", "beta"),
    estimate = c(exp(coefs["logQalone"]), object$model$beta),
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    component = "derived"
  )

  # Variance components
  vc <- object$model$variance_components
  if (!is.null(vc)) {
    vc_terms <- c("sigma_a", "sigma_b")
    vc_values <- c(vc$sigma_a, vc$sigma_b)
    vc_desc <- c("zeros RE", "Qalone RE")
    if (n_re == 3 && !is.null(vc$sigma_c)) {
      vc_terms <- c(vc_terms, "sigma_c")
      vc_values <- c(vc_values, vc$sigma_c)
      vc_desc <- c(vc_desc, "I RE")
    }
    vc_terms <- c(vc_terms, "sigma_e")
    vc_values <- c(vc_values, vc$sigma_e)
    vc_desc <- c(vc_desc, "residual")

    variance_components <- tibble::tibble(
      term = vc_terms,
      estimate = vc_values,
      description = vc_desc
    )
  } else {
    variance_components <- tibble::tibble(
      term = character(0),
      estimate = numeric(0),
      description = character(0)
    )
  }

  # Correlations
  if (n_re >= 2 && !is.null(vc)) {
    corr_terms <- "rho_ab"
    corr_values <- vc$rho_ab
    if (n_re == 3) {
      if (!is.null(vc$rho_ac)) {
        corr_terms <- c(corr_terms, "rho_ac")
        corr_values <- c(corr_values, vc$rho_ac)
      }
      if (!is.null(vc$rho_bc)) {
        corr_terms <- c(corr_terms, "rho_bc")
        corr_values <- c(corr_values, vc$rho_bc)
      }
    }
    correlations <- tibble::tibble(
      term = corr_terms,
      estimate = corr_values
    )
  } else {
    correlations <- NULL
  }

  # Compute percent zeros
  pct_zeros <- 100 * mean(object$data$delta == 1)

  # Interpretation
  I_val <- coefs["I"]
  interpretation <- if (I_val < 0) "substitutes" else "complements"

  structure(
    list(
      call = object$call,
      model_class = "beezdemand_cp_hurdle",
      backend = "TMB",
      nobs = object$param_info$n_obs,
      n_subjects = object$param_info$n_subjects,
      pct_zeros = pct_zeros,
      n_random_effects = n_re,
      random_effects_spec = object$param_info$random_effects_spec,
      epsilon = object$param_info$epsilon,
      converged = object$converged,
      logLik = object$loglik,
      AIC = object$AIC,
      BIC = object$BIC,
      coefficients = coefficients,
      coefficients_part1 = coefficients_part1,
      coefficients_part2 = coefficients_part2,
      transformed_params = transformed_params,
      variance_components = variance_components,
      correlations = correlations,
      interpretation = interpretation,
      notes = character(0)
    ),
    class = c("summary.beezdemand_cp_hurdle", "beezdemand_summary")
  )
}

#' Print method for summary.beezdemand_cp_hurdle
#'
#' @param x A summary.beezdemand_cp_hurdle object
#' @param digits Number of significant digits to print
#' @param ... Additional arguments (ignored)
#' @export
print.summary.beezdemand_cp_hurdle <- function(x, digits = 4, ...) {
  cat("Two-Part Mixed Effects Hurdle Cross-Price Demand Model\n")
  cat("======================================================\n\n")

  # Model info
  cat("Model Specification:\n")
  cat(
    "  Random effects:",
    paste(x$random_effects_spec, collapse = ", "),
    "\n"
  )
  cat("  Epsilon:", x$epsilon, "\n\n")

  cat("Data Summary:\n")
  cat("  Subjects:", x$n_subjects, "\n")
  cat("  Observations:", x$nobs, "\n")
  cat("  Zero consumption:", sprintf("%.1f%%", x$pct_zeros), "\n\n")

  # Part I coefficients
  cat("Part I (Probability of Zero) - Logistic Regression:\n")
  cat("  logit(pi) = beta0 + beta1 * log(x + epsilon) + a_i\n\n")

  part1_print <- as.data.frame(x$coefficients_part1[, c("term", "estimate", "std.error", "statistic", "p.value")])
  rownames(part1_print) <- part1_print$term
  part1_print$term <- NULL
  names(part1_print) <- c("Estimate", "Std.Error", "z value", "Pr(>|z|)")
  print(round(part1_print, digits))
  cat("\n")

  # Part II coefficients
  cat("Part II (Log Consumption | Positive) - Cross-Price Model:\n")
  if (x$n_random_effects == 3) {
    cat("  log(Q) = (logQalone + b_i) + (I + c_i) * exp(-beta * x) + e\n\n")
  } else {
    cat("  log(Q) = (logQalone + b_i) + I * exp(-beta * x) + e\n\n")
  }

  part2_print <- as.data.frame(x$coefficients_part2[, c("term", "estimate", "std.error", "statistic", "p.value")])
  rownames(part2_print) <- part2_print$term
  part2_print$term <- NULL
  names(part2_print) <- c("Estimate", "Std.Error", "z value", "Pr(>|z|)")
  print(round(part2_print, digits))

  # Transformed parameters
  tp <- x$transformed_params
  cat("\n  Transformed: beta =", round(tp$estimate[tp$term == "beta"], digits), "\n")
  cat("               Qalone =", round(tp$estimate[tp$term == "Qalone"], digits), "\n\n")

  # Variance components
  cat("Variance Components:\n")
  if (nrow(x$variance_components) > 0) {
    for (i in seq_len(nrow(x$variance_components))) {
      cat(
        "  ", x$variance_components$term[i],
        " (", x$variance_components$description[i], "): ",
        round(x$variance_components$estimate[i], digits), "\n",
        sep = ""
      )
    }
  }

  # Correlations
  if (!is.null(x$correlations) && nrow(x$correlations) > 0) {
    for (i in seq_len(nrow(x$correlations))) {
      cat("  ", x$correlations$term[i], ": ",
          round(x$correlations$estimate[i], digits), "\n", sep = "")
    }
  }
  cat("\n")

  # Model fit
  cat("Model Fit:\n")
  cat("  Log-likelihood:", round(x$logLik, 2), "\n")
  cat("  AIC:", round(x$AIC, 2), "\n")
  cat("  BIC:", round(x$BIC, 2), "\n")
  cat("  Converged:", x$converged, "\n\n")

  # Interpretation
  cat("Interpretation:\n")
  if (x$interpretation == "substitutes") {
    cat("  I < 0: Products are SUBSTITUTES (alternative reduces consumption)\n")
  } else {
    cat("  I > 0: Products are COMPLEMENTS (alternative increases consumption)\n")
  }

  invisible(x)
}

#' Coefficients method for beezdemand_cp_hurdle
#' @param object A beezdemand_cp_hurdle object
#' @param type Which coefficients: "all" (default), "fixed", "random", or "derived"
#' @param ... Additional arguments (ignored)
#' @export
coef.beezdemand_cp_hurdle <- function(
  object,
  type = c("all", "fixed", "random", "derived"),
  ...
) {
  type <- match.arg(type)

  switch(
    type,
    fixed = object$model$coefficients,
    random = object$random_effects,
    derived = c(
      beta = unname(object$model$beta),
      Qalone = unname(exp(object$model$coefficients["logQalone"]))
    ),
    all = c(
      object$model$coefficients,
      beta = unname(object$model$beta),
      Qalone = unname(exp(object$model$coefficients["logQalone"]))
    )
  )
}

#' Log-likelihood method for beezdemand_cp_hurdle
#' @param object A beezdemand_cp_hurdle object
#' @param ... Additional arguments (ignored)
#' @export
logLik.beezdemand_cp_hurdle <- function(object, ...) {
  ll <- object$loglik
  attr(ll, "df") <- length(object$opt$par)
  attr(ll, "nobs") <- object$param_info$n_obs
  class(ll) <- "logLik"
  ll
}

#' AIC method for beezdemand_cp_hurdle
#' @param object A beezdemand_cp_hurdle object
#' @param ... Additional arguments (ignored)
#' @param k Penalty per parameter (default 2 for AIC)
#' @export
AIC.beezdemand_cp_hurdle <- function(object, ..., k = 2) {
  k * length(object$opt$par) - 2 * object$loglik
}

#' BIC method for beezdemand_cp_hurdle
#' @param object A beezdemand_cp_hurdle object
#' @param ... Additional arguments (ignored)
#' @export
BIC.beezdemand_cp_hurdle <- function(object, ...) {
  log(object$param_info$n_obs) * length(object$opt$par) - 2 * object$loglik
}

#' Predict method for beezdemand_cp_hurdle
#'
#' @param object A beezdemand_cp_hurdle object
#' @param newdata Optional data frame with columns for x (price) and id
#' @param type Type of prediction: "demand" (log consumption), "response" (consumption),
#'   "probability" (P(zero)), or "parameters" (subject-specific parameters)
#' @param level Prediction level: "population" or "individual"
#' @param ... Additional arguments (ignored)
#' @return Predictions based on type
#' @export
predict.beezdemand_cp_hurdle <- function(
  object,
  newdata = NULL,
  type = c("demand", "response", "probability", "parameters"),
  level = c("population", "individual"),
  ...
) {
  type <- match.arg(type)
  level <- match.arg(level)

  if (type == "parameters") {
    return(object$subject_pars)
  }

  coefs <- object$model$coefficients
  beta_param <- object$model$beta

  if (is.null(newdata)) {
    newdata <- object$data
  }

  x <- newdata$x
  epsilon <- object$param_info$epsilon

  if (level == "population") {
    # Population-level predictions
    if (type == "probability") {
      # P(zero consumption)
      eta <- coefs["beta0"] + coefs["beta1"] * log(x + epsilon)
      prob <- exp(eta) / (1 + exp(eta))
      return(prob)
    } else {
      # Log consumption (Part II)
      logQ <- coefs["logQalone"] + coefs["I"] * exp(-beta_param * x)
      if (type == "response") {
        return(exp(logQ))
      }
      return(logQ)
    }
  } else {
    # Individual-level predictions
    if (!"id" %in% names(newdata)) {
      stop("newdata must contain 'id' column for individual-level predictions")
    }

    ids <- newdata$id
    unique_ids <- unique(object$subject_pars$id)

    # Map IDs to subject parameters
    id_match <- match(ids, unique_ids)
    if (any(is.na(id_match))) {
      warning("Some IDs in newdata not found in fitted model")
    }

    re <- object$random_effects
    n_re <- object$param_info$n_random_effects

    if (type == "probability") {
      a_i <- re[id_match, "a_i"]
      eta <- coefs["beta0"] + coefs["beta1"] * log(x + epsilon) + a_i
      prob <- exp(eta) / (1 + exp(eta))
      return(prob)
    } else {
      b_i <- re[id_match, "b_i"]
      if (n_re == 3) {
        c_i <- re[id_match, "c_i"]
        I_ind <- coefs["I"] + c_i
      } else {
        I_ind <- rep(coefs["I"], length(x))
      }
      logQ <- (coefs["logQalone"] + b_i) + I_ind * exp(-beta_param * x)
      if (type == "response") {
        return(exp(logQ))
      }
      return(logQ)
    }
  }
}

#' Plot method for beezdemand_cp_hurdle
#'
#' @param x A beezdemand_cp_hurdle object
#' @param type Type of plot: "demand", "population", "probability", "individual", or "parameters"
#' @param ids Optional vector of subject IDs to plot (type = "individual").
#' @param n_points Number of points for smooth curves
#' @param show_observed Whether to show observed data points
#' @param show_pred Which prediction layers to plot: "population", "individual",
#'   or "both".
#' @param show_data Deprecated alias for \code{show_observed}.
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
#' @param ind_line_alpha Alpha for individual curves.
#' @param ind_line_size Line size for individual curves.
#' @param ... Additional arguments passed to ggplot
#' @return A ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_bw scale_color_viridis_d
plot.beezdemand_cp_hurdle <- function(
  x,
  type = c("demand", "population", "probability", "individual", "parameters", "both"),
  ids = NULL,
  n_points = 200,
  show_observed = TRUE,
  show_pred = NULL,
  show_data = NULL,
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
  style <- match.arg(style)
  x_trans <- match.arg(x_trans)
  if (y_trans_missing) {
    y_trans <- beezdemand_default_y_trans(type = type)
  }
  y_trans <- match.arg(y_trans, c("log10", "log", "linear", "pseudo_log"))
  object <- x

  labels <- beezdemand_normalize_plot_labels(x_lab, y_lab, xlab, ylab)
  x_lab <- labels$x_lab %||% "Price"
  y_lab <- labels$y_lab %||% "Consumption"

  if (!is.null(show_data)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "plot(x, show_data = )",
      "plot(x, show_observed = )"
    )
    show_observed <- show_data
  }

  subtitle_note <- FALSE
  free_trans_used <- FALSE

  coefs <- object$model$coefficients
  beta_param <- object$model$beta
  epsilon <- object$param_info$epsilon

  x_range <- range(object$data$x)
  x_seq <- seq(x_range[1], x_range[2], length.out = n_points)

  if (type == "demand") {
    show_pred <- "population"
    # Population demand curve
    logQ <- coefs["logQalone"] + coefs["I"] * exp(-beta_param * x_seq)
    Q <- exp(logQ)

    pred_df <- data.frame(x = x_seq, y = Q)
    free_pred <- beezdemand_apply_free_trans(pred_df, "x", x_trans, free_trans)
    pred_df <- free_pred$data
    free_trans_used <- free_trans_used || free_pred$replaced

    pred_y <- beezdemand_drop_nonpositive_y(pred_df, "y", y_trans)
    pred_df <- pred_y$data
    subtitle_note <- subtitle_note || pred_y$dropped

    x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
    y_limits <- beezdemand_resolve_limits(y_limits, y_trans, axis = "y")

    p <- ggplot2::ggplot(pred_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(
        linewidth = pop_line_size,
        alpha = pop_line_alpha,
        color = beezdemand_style_color(style, "primary")
      ) +
      ggplot2::labs(
        x = x_lab,
        y = y_lab,
        title = "Population Cross-Price Demand Curve"
      ) +
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
      theme_beezdemand(style = style)

    if (show_observed) {
      obs_df <- object$data[object$data$delta == 0, ]
      obs_df <- data.frame(x = obs_df$x, y = obs_df$y)
      free_obs <- beezdemand_apply_free_trans(obs_df, "x", x_trans, free_trans)
      obs_df <- free_obs$data
      free_trans_used <- free_trans_used || free_obs$replaced

      obs_y <- beezdemand_drop_nonpositive_y(obs_df, "y", y_trans)
      obs_df <- obs_y$data
      subtitle_note <- subtitle_note || obs_y$dropped

      p <- p +
        ggplot2::geom_point(
          data = obs_df,
          ggplot2::aes(x = x, y = y),
          alpha = observed_point_alpha,
          size = observed_point_size
        )
    }

    if (x_trans == "log10") {
      p <- p + ggplot2::annotation_logticks(sides = "b")
    }
    if (y_trans == "log10") {
      p <- p + ggplot2::annotation_logticks(sides = "l")
    }
  } else if (type == "probability") {
    # Probability of zero curve
    eta <- coefs["beta0"] + coefs["beta1"] * log(x_seq + epsilon)
    prob <- exp(eta) / (1 + exp(eta))

    pred_df <- data.frame(x = x_seq, prob = prob)

    pred_df <- data.frame(x = x_seq, prob = prob)
    free_pred <- beezdemand_apply_free_trans(pred_df, "x", x_trans, free_trans)
    pred_df <- free_pred$data
    free_trans_used <- free_trans_used || free_pred$replaced

    x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
    y_limits <- beezdemand_resolve_limits(y_limits, y_trans, axis = "y")

    p <- ggplot2::ggplot(pred_df, ggplot2::aes(x = x, y = prob)) +
      ggplot2::geom_line(
        linewidth = pop_line_size,
        alpha = pop_line_alpha,
        color = beezdemand_style_color(style, "secondary")
      ) +
      ggplot2::labs(
        x = x_lab,
        y = "P(Zero Consumption)",
        title = "Probability of Zero Consumption"
      ) +
      ggplot2::ylim(0, 1) +
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
      theme_beezdemand(style = style)
  } else if (type == "individual") {
    if (is.null(show_pred)) {
      show_pred <- "individual"
    }
    show_pred <- beezdemand_normalize_show_pred(show_pred)

    # Individual demand curves
    n_subj <- min(20, object$param_info$n_subjects)
    subj_ids <- object$subject_pars$id
    if (!is.null(ids)) {
      subj_ids <- subj_ids[as.character(subj_ids) %in% as.character(ids)]
    }
    subj_ids <- utils::head(subj_ids, n_subj)
    if (length(subj_ids) == 0) {
      stop("No matching ids found in fit results.")
    }

    ind_preds <- lapply(subj_ids, function(sid) {
      newdata <- data.frame(x = x_seq, id = sid)
      data.frame(
        x = x_seq,
        y = predict(object, newdata, type = "response", level = "individual"),
        id = as.character(sid)
      )
    })
    ind_df <- do.call(rbind, ind_preds)

    if (any(show_pred %in% "population")) {
      logQ <- coefs["logQalone"] + coefs["I"] * exp(-beta_param * x_seq)
      Q <- exp(logQ)
      pop_pred <- data.frame(x = x_seq, y = Q)
      free_pop <- beezdemand_apply_free_trans(pop_pred, "x", x_trans, free_trans)
      pop_pred <- free_pop$data
      free_trans_used <- free_trans_used || free_pop$replaced

      pop_y <- beezdemand_drop_nonpositive_y(pop_pred, "y", y_trans)
      pop_pred <- pop_y$data
      subtitle_note <- subtitle_note || pop_y$dropped

      p <- ggplot2::ggplot(pop_pred, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(
          linewidth = pop_line_size,
          alpha = pop_line_alpha,
          color = beezdemand_style_color(style, "primary"),
          linetype = "dashed"
        )
    } else {
      p <- ggplot2::ggplot()
    }

    free_ind <- beezdemand_apply_free_trans(ind_df, "x", x_trans, free_trans)
    ind_df <- free_ind$data
    free_trans_used <- free_trans_used || free_ind$replaced

    ind_y <- beezdemand_drop_nonpositive_y(ind_df, "y", y_trans)
    ind_df <- ind_y$data
    subtitle_note <- subtitle_note || ind_y$dropped

    x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
    y_limits <- beezdemand_resolve_limits(y_limits, y_trans, axis = "y")

    p <- p +
      ggplot2::geom_line(
        data = ind_df,
        ggplot2::aes(x = x, y = y, color = id, group = id),
        alpha = ind_line_alpha,
        linewidth = ind_line_size
      ) +
      ggplot2::labs(
        x = x_lab,
        y = y_lab,
        title = "Individual Cross-Price Demand Curves",
        color = "Subject"
      ) +
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
      theme_beezdemand(style = style) +
      ggplot2::theme(legend.position = "none")

    p <- beezdemand_apply_color_scale(p, style, ind_df, "id")

    if (x_trans == "log10") {
      p <- p + ggplot2::annotation_logticks(sides = "b")
    }
    if (y_trans == "log10") {
      p <- p + ggplot2::annotation_logticks(sides = "l")
    }
  } else if (type == "parameters") {
    # Parameter distributions
    sp <- object$subject_pars

    x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
    p1 <- ggplot2::ggplot(sp, ggplot2::aes(x = Qalone)) +
      ggplot2::geom_histogram(
        bins = 20,
        fill = beezdemand_style_color(style, "accent"),
        color = "white"
      ) +
      ggplot2::labs(x = "Q_alone", title = "Distribution of Q_alone") +
      ggplot2::scale_x_continuous(
        trans = beezdemand_get_trans(x_trans),
        limits = x_limits,
        labels = beezdemand_axis_labels()
      ) +
      theme_beezdemand(style = style)

    p2 <- ggplot2::ggplot(sp, ggplot2::aes(x = I_individual)) +
      ggplot2::geom_histogram(
        bins = 20,
        fill = beezdemand_style_color(style, "secondary"),
        color = "white"
      ) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::labs(
        x = "I (individual)",
        title = "Distribution of Interaction Parameter"
      ) +
      ggplot2::scale_x_continuous(
        trans = beezdemand_get_trans(x_trans),
        limits = x_limits,
        labels = beezdemand_axis_labels()
      ) +
      theme_beezdemand(style = style)

    if (requireNamespace("patchwork", quietly = TRUE)) {
      p <- p1 + p2
    } else {
      p <- p1
      message("Install 'patchwork' package for combined parameter plots")
    }
  }

  if (isTRUE(subtitle_note) && !is.null(p)) {
    p <- p + ggplot2::labs(subtitle = "Zeros omitted on log scale.")
  }
  beezdemand_warn_free_trans(free_trans_used, free_trans)

  return(p)
}

#' Tidy method for beezdemand_cp_hurdle
#'
#' @param x A beezdemand_cp_hurdle object
#' @param ... Additional arguments (ignored)
#' @return A tibble of model coefficients with columns:
#'   - `term`: Parameter name
#'   - `estimate`: Point estimate
#'   - `std.error`: Standard error
#'   - `statistic`: z-value
#'   - `p.value`: P-value
#'   - `component`: One of "part1_probability", "part2_consumption", "derived"
#' @export
tidy.beezdemand_cp_hurdle <- function(x, ...) {
  coefs <- x$model$coefficients
  se <- x$model$se

  z_val <- coefs / se
  p_val <- 2 * stats::pnorm(-abs(z_val))

  # Determine components for each parameter (include variance/correlation terms)
  component <- rep("fixed", length(coefs))
  names(component) <- names(coefs)
  component[names(coefs) %in% c("beta0", "beta1")] <- "part1_probability"
  component[names(coefs) %in% c("logQalone", "I", "log_beta")] <- "part2_consumption"
  component[grepl("^logsigma_|^rho_|^sigma_|^var_|^cov_", names(coefs))] <- "variance"

  # Build tibble for fixed effects
  fixed_tidy <- tibble::tibble(
    term = names(coefs),
    estimate = unname(coefs),
    std.error = unname(se),
    statistic = unname(z_val),
    p.value = unname(p_val),
    component = unname(component)
  )

  # Add derived parameters (beta and Qalone on natural scale)
  derived_tidy <- tibble::tibble(
    term = c("beta", "Qalone"),
    estimate = c(x$model$beta, exp(coefs["logQalone"])),
    std.error = c(
      # Delta method approximation for beta
      se["log_beta"] * x$model$beta,
      # Delta method approximation for Qalone
      se["logQalone"] * exp(coefs["logQalone"])
    ),
    statistic = NA_real_,
    p.value = NA_real_,
    component = "derived"
  )

  dplyr::bind_rows(fixed_tidy, derived_tidy)
}

#' Glance method for beezdemand_cp_hurdle
#'
#' @param x A beezdemand_cp_hurdle object
#' @param ... Additional arguments (ignored)
#' @return A one-row tibble of model statistics with columns:
#'   - `model_class`: "beezdemand_cp_hurdle"
#'   - `backend`: "TMB"
#'   - `nobs`: Number of observations
#'   - `n_subjects`: Number of subjects
#'   - `n_random_effects`: Number of random effects
#'   - `converged`: Convergence status
#'   - `logLik`: Log-likelihood
#'   - `AIC`, `BIC`: Information criteria
#' @export
glance.beezdemand_cp_hurdle <- function(x, ...) {
  tibble::tibble(
    model_class = "beezdemand_cp_hurdle",
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
