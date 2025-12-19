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
#' @param object A beezdemand_cp_hurdle object
#' @param ... Additional arguments (ignored)
#' @export
summary.beezdemand_cp_hurdle <- function(object, ...) {
  cat("Two-Part Mixed Effects Hurdle Cross-Price Demand Model\n")
  cat("======================================================\n\n")

  # Model info
  n_re <- object$param_info$n_random_effects
  cat("Model Specification:\n")
  cat(
    "  Random effects:",
    paste(object$param_info$random_effects_spec, collapse = ", "),
    "\n"
  )
  cat("  Epsilon:", object$param_info$epsilon, "\n\n")

  cat("Data Summary:\n")
  cat("  Subjects:", object$param_info$n_subjects, "\n")
  cat("  Observations:", object$param_info$n_obs, "\n")
  zero_pct <- 100 * mean(object$data$delta == 1)
  cat("  Zero consumption:", sprintf("%.1f%%", zero_pct), "\n\n")

  # Part I coefficients
  cat("Part I (Probability of Zero) - Logistic Regression:\n")
  cat("  logit(pi) = beta0 + beta1 * log(x + epsilon) + a_i\n\n")

  coefs <- object$model$coefficients
  se <- object$model$se

  part1_params <- c("beta0", "beta1")
  part1_coefs <- coefs[part1_params]
  part1_se <- se[part1_params]
  part1_z <- part1_coefs / part1_se
  part1_p <- 2 * pnorm(-abs(part1_z))

  part1_table <- data.frame(
    Estimate = part1_coefs,
    `Std.Error` = part1_se,
    `z value` = part1_z,
    `Pr(>|z|)` = part1_p,
    check.names = FALSE
  )
  print(round(part1_table, 4))
  cat("\n")

  # Part II coefficients
  cat("Part II (Log Consumption | Positive) - Cross-Price Model:\n")
  if (n_re == 3) {
    cat("  log(Q) = (logQalone + b_i) + (I + c_i) * exp(-beta * x) + e\n\n")
  } else {
    cat("  log(Q) = (logQalone + b_i) + I * exp(-beta * x) + e\n\n")
  }

  part2_params <- c("logQalone", "I", "log_beta")
  part2_coefs <- coefs[part2_params]
  part2_se <- se[part2_params]
  part2_z <- part2_coefs / part2_se
  part2_p <- 2 * pnorm(-abs(part2_z))

  part2_table <- data.frame(
    Estimate = part2_coefs,
    `Std.Error` = part2_se,
    `z value` = part2_z,
    `Pr(>|z|)` = part2_p,
    check.names = FALSE
  )
  rownames(part2_table)[3] <- "log_beta"
  print(round(part2_table, 4))

  cat("\n  Transformed: beta =", round(object$model$beta, 4), "\n")
  cat("               Qalone =", round(exp(coefs["logQalone"]), 4), "\n\n")

  # Variance components
  cat("Variance Components:\n")
  vc <- object$model$variance_components
  if (!is.null(vc)) {
    cat("  sigma_a (zeros RE):", round(vc$sigma_a, 4), "\n")
    cat("  sigma_b (Qalone RE):", round(vc$sigma_b, 4), "\n")
    if (n_re == 3 && !is.null(vc$sigma_c)) {
      cat("  sigma_c (I RE):", round(vc$sigma_c, 4), "\n")
    }
    cat("  sigma_e (residual):", round(vc$sigma_e, 4), "\n")
    cat("  rho_ab:", round(vc$rho_ab, 4), "\n")
    if (n_re == 3) {
      if (!is.null(vc$rho_ac)) {
        cat("  rho_ac:", round(vc$rho_ac, 4), "\n")
      }
      if (!is.null(vc$rho_bc)) cat("  rho_bc:", round(vc$rho_bc, 4), "\n")
    }
  }
  cat("\n")

  # Model fit
  cat("Model Fit:\n")
  cat("  Log-likelihood:", round(object$loglik, 2), "\n")
  cat("  AIC:", round(object$AIC, 2), "\n")
  cat("  BIC:", round(object$BIC, 2), "\n")
  cat("  Converged:", object$converged, "\n\n")

  # Interpretation
  I_val <- coefs["I"]
  cat("Interpretation:\n")
  if (I_val < 0) {
    cat("  I < 0: Products are SUBSTITUTES (alternative reduces consumption)\n")
  } else {
    cat(
      "  I > 0: Products are COMPLEMENTS (alternative increases consumption)\n"
    )
  }

  invisible(object)
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
#' @param type Type of plot: "demand", "probability", "individual", or "parameters"
#' @param n_points Number of points for smooth curves
#' @param show_data Whether to show observed data points
#' @param ... Additional arguments passed to ggplot
#' @return A ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_bw scale_color_viridis_d
plot.beezdemand_cp_hurdle <- function(
  x,
  type = c("demand", "probability", "individual", "parameters"),
  n_points = 100,
  show_data = TRUE,
  ...
) {
  type <- match.arg(type)
  object <- x

  coefs <- object$model$coefficients
  beta_param <- object$model$beta
  epsilon <- object$param_info$epsilon

  x_range <- range(object$data$x)
  x_seq <- seq(x_range[1], x_range[2], length.out = n_points)

  if (type == "demand") {
    # Population demand curve
    logQ <- coefs["logQalone"] + coefs["I"] * exp(-beta_param * x_seq)
    Q <- exp(logQ)

    pred_df <- data.frame(x = x_seq, y = Q)

    p <- ggplot2::ggplot(pred_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(linewidth = 1, color = "blue") +
      ggplot2::labs(
        x = "Alternative Price",
        y = "Predicted Consumption",
        title = "Population Cross-Price Demand Curve"
      ) +
      ggplot2::theme_bw()

    if (show_data) {
      obs_df <- object$data[object$data$delta == 0, ]
      p <- p +
        ggplot2::geom_point(
          data = obs_df,
          ggplot2::aes(x = x, y = exp(logQ)),
          alpha = 0.3
        )
    }
  } else if (type == "probability") {
    # Probability of zero curve
    eta <- coefs["beta0"] + coefs["beta1"] * log(x_seq + epsilon)
    prob <- exp(eta) / (1 + exp(eta))

    pred_df <- data.frame(x = x_seq, prob = prob)

    p <- ggplot2::ggplot(pred_df, ggplot2::aes(x = x, y = prob)) +
      ggplot2::geom_line(linewidth = 1, color = "red") +
      ggplot2::labs(
        x = "Alternative Price",
        y = "P(Zero Consumption)",
        title = "Probability of Zero Consumption"
      ) +
      ggplot2::ylim(0, 1) +
      ggplot2::theme_bw()
  } else if (type == "individual") {
    # Individual demand curves
    n_subj <- min(20, object$param_info$n_subjects)
    subj_ids <- object$subject_pars$id[1:n_subj]

    ind_preds <- lapply(subj_ids, function(sid) {
      newdata <- data.frame(x = x_seq, id = sid)
      data.frame(
        x = x_seq,
        y = predict(object, newdata, type = "response", level = "individual"),
        id = as.character(sid)
      )
    })
    ind_df <- do.call(rbind, ind_preds)

    p <- ggplot2::ggplot(
      ind_df,
      ggplot2::aes(x = x, y = y, color = id, group = id)
    ) +
      ggplot2::geom_line(alpha = 0.7) +
      ggplot2::labs(
        x = "Alternative Price",
        y = "Predicted Consumption",
        title = "Individual Cross-Price Demand Curves",
        color = "Subject"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
  } else if (type == "parameters") {
    # Parameter distributions
    sp <- object$subject_pars

    p1 <- ggplot2::ggplot(sp, ggplot2::aes(x = Qalone)) +
      ggplot2::geom_histogram(bins = 20, fill = "steelblue", color = "white") +
      ggplot2::labs(x = "Q_alone", title = "Distribution of Q_alone") +
      ggplot2::theme_bw()

    p2 <- ggplot2::ggplot(sp, ggplot2::aes(x = I_individual)) +
      ggplot2::geom_histogram(bins = 20, fill = "coral", color = "white") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::labs(
        x = "I (individual)",
        title = "Distribution of Interaction Parameter"
      ) +
      ggplot2::theme_bw()

    if (requireNamespace("patchwork", quietly = TRUE)) {
      p <- p1 + p2
    } else {
      p <- p1
      message("Install 'patchwork' package for combined parameter plots")
    }
  }

  return(p)
}

#' Tidy method for beezdemand_cp_hurdle
#' @param x A beezdemand_cp_hurdle object
#' @param ... Additional arguments (ignored)
#' @return A data frame of model coefficients
#' @export
tidy.beezdemand_cp_hurdle <- function(x, ...) {
  coefs <- x$model$coefficients
  se <- x$model$se

  # Replace log_beta with beta
  idx <- which(names(coefs) == "log_beta")
  if (length(idx) > 0) {
    names(coefs)[idx] <- "beta"
    coefs[idx] <- x$model$beta
    # Approximate SE for beta using delta method
    se[idx] <- se[idx] * x$model$beta
    names(se)[idx] <- "beta"
  }

  z_val <- coefs / se
  p_val <- 2 * pnorm(-abs(z_val))

  data.frame(
    term = names(coefs),
    estimate = unname(coefs),
    std.error = unname(se),
    statistic = unname(z_val),
    p.value = unname(p_val),
    stringsAsFactors = FALSE
  )
}

#' Glance method for beezdemand_cp_hurdle
#' @param x A beezdemand_cp_hurdle object
#' @param ... Additional arguments (ignored)
#' @return A one-row data frame of model statistics
#' @export
glance.beezdemand_cp_hurdle <- function(x, ...) {
  data.frame(
    n_subjects = x$param_info$n_subjects,
    n_obs = x$param_info$n_obs,
    n_random_effects = x$param_info$n_random_effects,
    logLik = x$loglik,
    AIC = x$AIC,
    BIC = x$BIC,
    converged = x$converged,
    stringsAsFactors = FALSE
  )
}
