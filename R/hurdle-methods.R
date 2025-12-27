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
summary.beezdemand_hurdle <- function(object, ...) {
  # Build coefficient table (matrix form for printing)
  coef_matrix <- cbind(
    Estimate = object$model$coefficients,
    `Std. Error` = object$model$se,
    `t value` = object$model$coefficients / object$model$se
  )

  # Build coefficient tibble (for contract compliance)
  coef_names <- names(object$model$coefficients)
  z_val <- object$model$coefficients / object$model$se
  p_val <- 2 * stats::pnorm(-abs(z_val))

  # Determine component for each coefficient
  component <- dplyr::case_when(
    coef_names %in% c("gamma0", "gamma1") ~ "probability",
    coef_names %in% c("logQ0", "log_alpha") ~ "consumption",
    grepl("^logsigma_|^rho_", coef_names) ~ "variance",
    TRUE ~ "fixed"
  )

  coefficients <- tibble::tibble(
    term = coef_names,
    estimate = unname(object$model$coefficients),
    std.error = unname(object$model$se),
    statistic = unname(z_val),
    p.value = unname(p_val),
    component = component
  )

  # Compute group-level demand metrics (Omax, Pmax)
  group_metrics <- calc_group_metrics(object)

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
      individual_metrics = individual_metrics,
      notes = character(0)
    ),
    class = c("summary.beezdemand_hurdle", "beezdemand_summary")
  )
}


#' Print Summary of Hurdle Demand Model
#'
#' @param x An object of class \code{summary.beezdemand_hurdle}.
#' @param digits Number of significant digits to print. Default is 4.
#' @param ... Additional arguments passed to \code{print}.
#'
#' @export
print.summary.beezdemand_hurdle <- function(x, digits = 4, ...) {
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
  if (!is.na(x$group_metrics$Pmax)) {
    cat(sprintf(
      "  Pmax (price at max expenditure): %.4f\n",
      x$group_metrics$Pmax
    ))
    cat(sprintf("  Omax (max expenditure): %.4f\n", x$group_metrics$Omax))
  } else {
    cat("  Pmax/Omax: NA (k < e, no local maximum exists)\n")
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
#' @param ... Additional arguments (currently unused).
#'
#' @return Named numeric vector of fixed effect coefficients.
#'
#' @export
coef.beezdemand_hurdle <- function(object, ...) {
  object$model$coefficients
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
#' Extracts subject-specific predictions from a fitted hurdle demand model,
#' or generates predictions for new price values.
#'
#' @param object An object of class \code{beezdemand_hurdle}.
#' @param newdata Optional data frame with new data for prediction. Currently
#'   not fully implemented - predictions use subjects from the fitted model.
#' @param type Character string specifying the type of prediction:
#'   \describe{
#'     \item{\code{"parameters"}}{Subject-specific demand parameters (default)}
#'     \item{\code{"demand"}}{Predicted consumption at specified prices}
#'     \item{\code{"probability"}}{Predicted probability of zero consumption}
#'   }
#' @param prices Numeric vector of prices for prediction when \code{type = "demand"}
#'   or \code{type = "probability"}. If \code{NULL}, uses unique prices from
#'   the original data.
#' @param ... Additional arguments (currently unused).
#'
#' @return Depends on \code{type}:
#' \describe{
#'   \item{\code{"parameters"}}{Data frame with subject-specific parameters}
#'   \item{\code{"demand"}}{Data frame with columns: id, x, predicted_consumption,
#'     predicted_log_consumption, prob_zero, expected_consumption}
#'   \item{\code{"probability"}}{Data frame with columns: id, x, prob_zero}
#' }
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
  type = c("parameters", "demand", "probability"),
  prices = NULL,
  ...
) {
  type <- match.arg(type)
  id_var <- object$param_info$id_var
  x_var <- object$param_info$x_var
  epsilon <- object$param_info$epsilon

  # For type = "parameters", just return subject-specific parameters
  if (type == "parameters") {
    return(object$subject_pars)
  }

  # Get prices for prediction
  if (is.null(prices)) {
    prices <- sort(unique(object$data[[x_var]]))
  }

  # Extract coefficients
  coefs <- object$model$coefficients
  beta0 <- coefs["beta0"]
  beta1 <- coefs["beta1"]
  logQ0 <- coefs["logQ0"]
  k <- coefs["k"]
  alpha_fixed <- coefs["alpha"]

  # Get random effects
  a_i <- object$random_effects[, "a_i"]
  b_i <- object$random_effects[, "b_i"]

  # c_i only exists for 3 random effect models
  n_re <- object$param_info$n_random_effects
  if (n_re == 3) {
    c_i <- object$random_effects[, "c_i"]
  } else {
    c_i <- rep(0, length(a_i)) # No random effect on alpha
  }

  subjects <- object$param_info$subject_levels
  n_subjects <- length(subjects)

  if (type == "probability") {
    show_pred <- "population"
    # Predict P(zero consumption) for each subject and price
    results <- expand.grid(
      id = subjects,
      x = prices,
      stringsAsFactors = FALSE
    )

    results$prob_zero <- NA_real_
    for (i in seq_len(n_subjects)) {
      idx <- results$id == subjects[i]
      eta <- beta0 + beta1 * log(results$x[idx] + epsilon) + a_i[i]
      results$prob_zero[idx] <- exp(eta) / (1 + exp(eta))
    }

    names(results)[1] <- id_var
    names(results)[2] <- x_var
    return(results)
  }

  if (type == "demand") {
    show_pred <- "population"
    # Predict consumption for each subject and price
    results <- expand.grid(
      id = subjects,
      x = prices,
      stringsAsFactors = FALSE
    )

    results$predicted_log_consumption <- NA_real_
    results$predicted_consumption <- NA_real_
    results$prob_zero <- NA_real_

    for (i in seq_len(n_subjects)) {
      idx <- results$id == subjects[i]
      p <- results$x[idx]

      # Log consumption (Part II)
      # For 2 RE model, c_i[i] = 0, so alpha is fixed
      alpha_i <- alpha_fixed + c_i[i]
      mu_ij <- (logQ0 + b_i[i]) + k * (exp(-alpha_i * p) - 1)
      results$predicted_log_consumption[idx] <- mu_ij
      results$predicted_consumption[idx] <- exp(mu_ij)

      # Probability of zero (Part I)
      eta <- beta0 + beta1 * log(p + epsilon) + a_i[i]
      results$prob_zero[idx] <- exp(eta) / (1 + exp(eta))
    }

    # Expected consumption accounting for probability of zero
    results$expected_consumption <- results$predicted_consumption *
      (1 - results$prob_zero)

    names(results)[1] <- id_var
    names(results)[2] <- x_var
    return(results)
  }
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
#' @param x_limits Optional numeric vector of length 2 for x-axis limits.
#' @param y_limits Optional numeric vector of length 2 for y-axis limits.
#' @param x_lab Optional x-axis label.
#' @param y_lab Optional y-axis label.
#' @param xlab Deprecated alias for \code{x_lab}.
#' @param ylab Deprecated alias for \code{y_lab}.
#' @param style Plot styling, passed to \code{theme_beezdemand()}.
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
  beta0 <- coefs["beta0"]
  beta1 <- coefs["beta1"]
  logQ0 <- coefs["logQ0"]
  k <- coefs["k"]
  alpha <- coefs["alpha"]

  if (type == "demand") {
    # Population demand curve
    pop_data <- data.frame(
      price = prices,
      log_consumption = logQ0 + k * (exp(-alpha * prices) - 1)
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
    # Population probability curve
    pop_data <- data.frame(
      price = prices,
      eta = beta0 + beta1 * log(prices + epsilon)
    )
    pop_data$prob_zero <- exp(pop_data$eta) / (1 + exp(pop_data$eta))

    pop_df <- data.frame(x = pop_data$price, y = pop_data$prob_zero)
    free_pop <- beezdemand_apply_free_trans(pop_df, "x", x_trans, free_trans)
    pop_df <- free_pop$data
    free_trans_used <- free_trans_used || free_pop$replaced

    x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
    y_limits <- beezdemand_resolve_limits(y_limits, y_trans, axis = "y")

    p <- ggplot(pop_df, aes(x = .data$x, y = .data$y)) +
      geom_line(
        linewidth = pop_line_size,
        alpha = pop_line_alpha,
        color = beezdemand_style_color(style, "secondary")
      ) +
      labs(
        title = "Probability of Zero Consumption",
        x = x_lab,
        y = "P(Consumption = 0)"
      ) +
      ggplot2::ylim(0, 1) +
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
      consumption = exp(logQ0 + k * (exp(-alpha * prices) - 1))
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
  logQ0 <- coefs["logQ0"]
  k <- coefs["k"]
  alpha <- coefs["alpha"]

  pop_data <- data.frame(
    price = prices,
    consumption = exp(logQ0 + k * (exp(-alpha * prices) - 1))
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
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{term}{Parameter name}
#'     \item{estimate}{Point estimate}
#'     \item{std.error}{Standard error}
#'     \item{statistic}{z-value}
#'     \item{p.value}{P-value}
#'     \item{component}{One of "probability", "consumption", or "variance"}
#'   }
#'
#' @export
tidy.beezdemand_hurdle <- function(x, ...) {
  coef_names <- names(x$model$coefficients)
  coefs <- x$model$coefficients
  se <- x$model$se
  z_val <- coefs / se
  p_val <- 2 * stats::pnorm(-abs(z_val))

  # Determine component for each coefficient
  component <- dplyr::case_when(
    coef_names %in% c("gamma0", "gamma1") ~ "probability",
    coef_names %in% c("logQ0", "log_alpha") ~ "consumption",
    coef_names %in% c("beta0", "beta1") ~ "probability",
    grepl("^logsigma_|^rho_|^sigma_", coef_names) ~ "variance",
    TRUE ~ "fixed"
  )

  tibble::tibble(
    term = coef_names,
    estimate = unname(coefs),
    std.error = unname(se),
    statistic = unname(z_val),
    p.value = unname(p_val),
    component = component
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
