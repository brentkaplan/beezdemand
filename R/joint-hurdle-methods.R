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
  cat("\nJoint Hurdle Cross-Price Model\n")
  cat("===============================\n\n")

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


#' @export
summary.beezdemand_joint_hurdle <- function(object, ...) {
  cat("\nJoint Hurdle Cross-Price Model Summary\n")
  cat("=======================================\n\n")

  # Model info
  cat("Observations:", object$n_obs, "\n")
  cat("Subjects:", object$n_subjects, "\n")
  cat("k fixed:", object$k_fixed, "(value:", object$k_value, ")\n")
  cat("Epsilon:", object$epsilon, "\n\n")

  # Stream counts
  cat("Stream Counts:\n")
  cat("  alone.target:", object$stream_counts["alone.target"], "\n")
  cat("  own.target:  ", object$stream_counts["own.target"], "\n")
  cat("  own.alt:     ", object$stream_counts["own.alt"], "\n\n")

  # Fixed effects
  cat("Fixed Effects:\n")
  cat("--------------\n")

  # Part I
  cat("\nPart I (Hurdle - Shared Zeros Model):\n")
  coef <- object$coefficients_natural
  cat("  gamma0 (intercept):    ", sprintf("%.4f", coef["gamma0"]), "\n")
  cat(
    "  gamma_own_target:      ",
    sprintf("%.4f", coef["gamma_own_target"]),
    "\n"
  )
  cat("  gamma_own_alt:         ", sprintf("%.4f", coef["gamma_own_alt"]), "\n")
  cat("  gamma1 (price slope):  ", sprintf("%.4f", coef["gamma1"]), "\n")

  # Part II - Target Demand
  cat("\nPart II - Target Demand (alone.target):\n")
  cat("  logQ0_AT:              ", sprintf("%.4f", coef["logQ0_AT"]), "\n")
  cat("  Q0_AT:                 ", sprintf("%.4f", exp(coef["logQ0_AT"])), "\n")
  cat("  alpha_AT:              ", sprintf("%.6f", coef["alpha_AT"]), "\n")

  cat("\nPart II - Target Demand (own.target):\n")
  cat("  logQ0_OT:              ", sprintf("%.4f", coef["logQ0_OT"]), "\n")
  cat("  Q0_OT:                 ", sprintf("%.4f", exp(coef["logQ0_OT"])), "\n")
  cat("  alpha_OT:              ", sprintf("%.6f", coef["alpha_OT"]), "\n")

  cat("\nPart II - Cross-Price (own.alt):\n")
  cat("  logQalone_OA:          ", sprintf("%.4f", coef["logQalone_OA"]), "\n")
  cat(
    "  Qalone_OA:             ",
    sprintf("%.4f", exp(coef["logQalone_OA"])),
    "\n"
  )
  cat("  I:                     ", sprintf("%.4f", coef["I"]), "\n")
  cat("  beta:                  ", sprintf("%.4f", coef["beta"]), "\n")

  # Shared k
  cat("\nShared Parameter:\n")
  cat(
    "  k:                     ",
    sprintf("%.4f", object$k_value),
    if (object$k_fixed) " (fixed)" else " (estimated)",
    "\n"
  )

  # Variance components
  cat("\nVariance Components:\n")
  cat("--------------------\n")
  sigma_a <- exp(object$coefficients["logsigma_a"])
  sigma_b_AT <- exp(object$coefficients["logsigma_b_AT"])
  sigma_b_OT <- exp(object$coefficients["logsigma_b_OT"])
  sigma_b_OA <- exp(object$coefficients["logsigma_b_OA"])
  sigma_e <- exp(object$coefficients["logsigma_e"])

  cat("  sigma_a (zeros RE):    ", sprintf("%.4f", sigma_a), "\n")
  cat("  sigma_b_AT:            ", sprintf("%.4f", sigma_b_AT), "\n")
  cat("  sigma_b_OT:            ", sprintf("%.4f", sigma_b_OT), "\n")
  cat("  sigma_b_OA:            ", sprintf("%.4f", sigma_b_OA), "\n")
  cat("  sigma_e (residual):    ", sprintf("%.4f", sigma_e), "\n")

  # Correlations
  rho_AT_OT <- tanh(object$coefficients["rho_AT_OT_raw"])
  rho_AT_OA <- tanh(object$coefficients["rho_AT_OA_raw"])
  rho_OT_OA <- tanh(object$coefficients["rho_OT_OA_raw"])

  cat("\nIntensity RE Correlations:\n")
  cat("  rho(AT, OT):           ", sprintf("%.4f", rho_AT_OT), "\n")
  cat("  rho(AT, OA):           ", sprintf("%.4f", rho_AT_OA), "\n")
  cat("  rho(OT, OA):           ", sprintf("%.4f", rho_OT_OA), "\n")

  # Model fit
  cat("\nModel Fit:\n")
  cat("----------\n")
  cat("  Log-likelihood:        ", sprintf("%.2f", object$logLik), "\n")
  cat("  AIC:                   ", sprintf("%.2f", object$AIC), "\n")
  cat("  BIC:                   ", sprintf("%.2f", object$BIC), "\n")
  cat(
    "  Convergence:           ",
    if (object$convergence == 0) "Yes" else "No",
    "\n"
  )

  if (length(object$warnings) > 0) {
    cat("\nWarnings:\n")
    for (w in object$warnings) {
      cat("  - ", w, "\n")
    }
  }

  invisible(object)
}


#' @export
coef.beezdemand_joint_hurdle <- function(
  object,
  type = c("all", "fixed", "hurdle", "demand", "crossprice"),
  ...
) {
  type <- match.arg(type)

  coef <- object$coefficients_natural

  switch(
    type,
    all = coef,
    fixed = coef[!grepl("^logsigma|^rho_", names(coef))],
    hurdle = coef[c("gamma0", "gamma_own_target", "gamma_own_alt", "gamma1")],
    demand = coef[c("logQ0_AT", "alpha_AT", "logQ0_OT", "alpha_OT", "k")],
    crossprice = coef[c("logQalone_OA", "I", "beta")]
  )
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
#' @param type One of "response" (predicted consumption), "probability" (P(y=0)),
#'   or "parameters" (subject-level parameters).
#' @param stream Which stream(s) to predict for: "all", "alone.target",
#'   "own.target", "own.alt".
#' @param level "population" or "subject".
#' @param ... Additional arguments (unused).
#'
#' @return Predictions as a data frame or matrix.
#' @export
predict.beezdemand_joint_hurdle <- function(
  object,
  newdata = NULL,
  type = c("response", "probability", "parameters"),
  stream = c("all", "alone.target", "own.target", "own.alt"),
  level = c("population", "subject"),
  ...
) {
  type <- match.arg(type)
  stream <- match.arg(stream)
  level <- match.arg(level)

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
    return(subject_pars)
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
        stringsAsFactors = FALSE
      )
    } else {
      # type == "response"
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

      results[[s]] <- data.frame(
        stream = s,
        price_T = prices,
        y_pred = y_pred,
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, results)
}


#' Plot Joint Hurdle Model
#'
#' @param x A \code{beezdemand_joint_hurdle} object.
#' @param type One of "demand", "probability", "individual", "parameters".
#' @param stream Which stream(s) to plot.
#' @param ... Additional arguments passed to ggplot.
#'
#' @return A ggplot2 object.
#' @export
plot.beezdemand_joint_hurdle <- function(
  x,
  type = c("demand", "probability", "individual", "parameters"),
  stream = c("all", "alone.target", "own.target", "own.alt"),
  ...
) {
  type <- match.arg(type)
  stream <- match.arg(stream)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  if (type == "demand") {
    # Population demand curves
    prices <- seq(min(x$data$price_T), max(x$data$price_T), length.out = 100)
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

    p <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = dat,
        ggplot2::aes(x = price_T, y = y, color = stream_name),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        data = preds,
        ggplot2::aes(x = price_T, y = y_pred, color = stream_name),
        linewidth = 1
      ) +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10() +
      ggplot2::labs(
        x = "Target Price",
        y = "Consumption",
        color = "Stream",
        title = "Joint Model: Demand Curves"
      ) +
      ggplot2::theme_bw()
  } else if (type == "probability") {
    # P(y = 0) curves
    prices <- seq(min(x$data$price_T), max(x$data$price_T), length.out = 100)
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

    p <- ggplot2::ggplot(
      preds,
      ggplot2::aes(x = price_T, y = prob_zero, color = stream_name)
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::scale_x_log10() +
      ggplot2::labs(
        x = "Target Price",
        y = "P(Consumption = 0)",
        color = "Stream",
        title = "Joint Model: Zero Probability"
      ) +
      ggplot2::ylim(0, 1) +
      ggplot2::theme_bw()
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
      ggplot2::geom_histogram(bins = 20, fill = "steelblue", color = "white") +
      ggplot2::facet_wrap(~Parameter, scales = "free") +
      ggplot2::labs(
        x = "Parameter Value",
        y = "Count",
        title = "Subject-Level Parameter Distributions"
      ) +
      ggplot2::theme_bw()
  } else {
    # Individual curves
    stop("Individual curves not yet implemented for joint model")
  }

  p
}


#' Tidy Method for Joint Hurdle Model
#'
#' @param x A \code{beezdemand_joint_hurdle} object.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with coefficient information.
#' @export
tidy.beezdemand_joint_hurdle <- function(x, ...) {
  coef <- x$coefficients_natural

  # Get standard errors if available
  se <- rep(NA_real_, length(coef))
  if (!is.null(x$vcov)) {
    # Match names
    vcov_names <- rownames(x$vcov)
    for (i in seq_along(coef)) {
      nm <- names(coef)[i]
      if (nm %in% vcov_names) {
        se[i] <- sqrt(x$vcov[nm, nm])
      }
    }
  }

  # Determine component
  component <- rep("variance", length(coef))
  component[grepl("^gamma", names(coef))] <- "hurdle"
  component[grepl("AT$", names(coef)) | names(coef) == "k"] <- "alone.target"
  component[grepl("OT$", names(coef))] <- "own.target"
  component[
    grepl("OA$", names(coef)) | names(coef) %in% c("I", "beta")
  ] <- "own.alt"

  tibble::tibble(
    term = names(coef),
    estimate = unname(coef),
    std.error = se,
    component = component
  )
}


#' Glance Method for Joint Hurdle Model
#'
#' @param x A \code{beezdemand_joint_hurdle} object.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with model-level statistics.
#' @export
glance.beezdemand_joint_hurdle <- function(x, ...) {
  tibble::tibble(
    logLik = x$logLik,
    AIC = x$AIC,
    BIC = x$BIC,
    nobs = x$n_obs,
    nsubjects = x$n_subjects,
    n_alone_target = x$stream_counts["alone.target"],
    n_own_target = x$stream_counts["own.target"],
    n_own_alt = x$stream_counts["own.alt"],
    converged = x$convergence == 0,
    k_fixed = x$k_fixed,
    k_value = x$k_value
  )
}
