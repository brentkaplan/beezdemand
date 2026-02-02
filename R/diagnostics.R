# Model Diagnostics Suite
# Provides diagnostic functions for beezdemand model objects

#' Check Demand Model Diagnostics
#'
#' @description
#' Performs diagnostic checks on fitted demand models, returning information
#' about convergence, boundary conditions, and residual patterns.
#'
#' @param object A fitted model object of class `beezdemand_hurdle`,
#'   `beezdemand_nlme`, or `beezdemand_fixed`.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class `beezdemand_diagnostics` containing:
#'   \describe{
#'     \item{convergence}{List with convergence status and messages}
#'     \item{boundary}{List with boundary condition warnings}
#'     \item{residuals}{Summary statistics for residuals}
#'     \item{random_effects}{Summary of random effects (if applicable)}
#'     \item{issues}{Character vector of identified issues}
#'     \item{recommendations}{Character vector of recommendations}
#'   }
#'
#' @details
#' The function checks for:
#' \itemize{
#'   \item Convergence status and optimization messages
#'   \item Parameters at or near boundaries
#'   \item Residual patterns (heteroscedasticity, outliers)
#'   \item Random effect variance estimates near zero
#'   \item Correlation matrices near singularity
#' }
#'
#' @note
#' This function is named `check_demand_model()` to avoid potential conflicts
#' with `performance::check_model()` from the performance package.
#'
#' @examples
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
#' diagnostics <- check_demand_model(fit)
#' print(diagnostics)
#' }
#'
#' @seealso [plot_residuals()], [plot_qq()]
#' @export
check_demand_model <- function(object, ...) {
  UseMethod("check_demand_model")
}


#' @rdname check_demand_model
#' @export
check_demand_model.beezdemand_hurdle <- function(object, ...) {
  issues <- character(0)
  recommendations <- character(0)

 # 1. Check convergence
  convergence <- .check_hurdle_convergence(object)
  if (!convergence$converged) {
    issues <- c(issues, "Model did not converge")
    recommendations <- c(recommendations, "Try different starting values or increase iterations")
  }

  # 2. Check boundary conditions
  boundary <- .check_hurdle_boundaries(object)
  if (length(boundary$at_boundary) > 0) {
    issues <- c(issues, paste("Parameters at boundary:", paste(boundary$at_boundary, collapse = ", ")))
    recommendations <- c(recommendations, "Consider simplifying model or checking data quality")
  }

  # 3. Check random effects
  random_effects <- .check_hurdle_random_effects(object)
  if (any(random_effects$near_zero)) {
    near_zero_re <- names(random_effects$variances)[random_effects$near_zero]
    issues <- c(issues, paste("Random effect variance near zero:", paste(near_zero_re, collapse = ", ")))
    recommendations <- c(recommendations, "Consider removing these random effects")
  }

  # 4. Check residuals
  residuals <- .check_hurdle_residuals(object)
  if (residuals$has_outliers) {
    issues <- c(issues, sprintf("Detected %d potential outliers (|resid| > 3)", residuals$n_outliers))
    recommendations <- c(recommendations, "Investigate outlying observations")
  }

  structure(
    list(
      model_class = "beezdemand_hurdle",
      convergence = convergence,
      boundary = boundary,
      residuals = residuals,
      random_effects = random_effects,
      issues = issues,
      recommendations = recommendations,
      n_issues = length(issues)
    ),
    class = "beezdemand_diagnostics"
  )
}


#' @rdname check_demand_model
#' @export
check_demand_model.beezdemand_nlme <- function(object, ...) {
  issues <- character(0)
  recommendations <- character(0)

  # 1. Check convergence
  convergence <- .check_nlme_convergence(object)
  if (!convergence$converged) {
    issues <- c(issues, "Model did not converge")
    recommendations <- c(recommendations, convergence$message)
  }

  # 2. Check random effects
  random_effects <- .check_nlme_random_effects(object)
  if (any(random_effects$near_zero)) {
    near_zero_re <- names(random_effects$variances)[random_effects$near_zero]
    issues <- c(issues, paste("Random effect variance near zero:", paste(near_zero_re, collapse = ", ")))
    recommendations <- c(recommendations, "Consider removing these random effects from the model")
  }

  # 3. Check correlation matrix
  if (!is.null(random_effects$correlation) && !is.na(random_effects$near_singular)) {
    if (random_effects$near_singular) {
      issues <- c(issues, "Random effects correlation matrix is near singular")
      recommendations <- c(recommendations, "Consider using diagonal covariance structure (pdDiag)")
    }
  }

  # 4. Check residuals
  residuals <- .check_nlme_residuals(object)
  if (residuals$has_outliers) {
    issues <- c(issues, sprintf("Detected %d potential outliers (|resid| > 3 SD)", residuals$n_outliers))
    recommendations <- c(recommendations, "Investigate outlying observations")
  }

  structure(
    list(
      model_class = "beezdemand_nlme",
      convergence = convergence,
      boundary = list(at_boundary = character(0)),  # NLME doesn't have explicit boundaries
      residuals = residuals,
      random_effects = random_effects,
      issues = issues,
      recommendations = recommendations,
      n_issues = length(issues)
    ),
    class = "beezdemand_diagnostics"
  )
}


#' @rdname check_demand_model
#' @export
check_demand_model.beezdemand_fixed <- function(object, ...) {
  issues <- character(0)
  recommendations <- character(0)

  # 1. Check convergence (per subject)
  convergence <- .check_fixed_convergence(object)
  if (convergence$n_failed > 0) {
    pct_failed <- round(100 * convergence$n_failed / convergence$n_total, 1)
    issues <- c(issues, sprintf("%d of %d subjects failed to converge (%.1f%%)",
                                convergence$n_failed, convergence$n_total, pct_failed))
    if (pct_failed > 20) {
      recommendations <- c(recommendations, "High failure rate suggests data quality issues or inappropriate model")
    }
  }

  # 2. Check parameter estimates
  params <- .check_fixed_parameters(object)
  if (length(params$extreme_values) > 0) {
    issues <- c(issues, paste("Extreme parameter estimates detected for subjects:",
                              paste(utils::head(params$extreme_values, 5), collapse = ", "),
                              if (length(params$extreme_values) > 5) "..." else ""))
    recommendations <- c(recommendations, "Review data for these subjects")
  }

  # 3. Check residuals (aggregate)
  residuals <- .check_fixed_residuals(object)
  if (residuals$has_outliers) {
    issues <- c(issues, sprintf("Detected %d potential outliers across subjects", residuals$n_outliers))
  }

  structure(
    list(
      model_class = "beezdemand_fixed",
      convergence = convergence,
      boundary = list(at_boundary = character(0)),
      residuals = residuals,
      random_effects = NULL,  # Not applicable
      issues = issues,
      recommendations = recommendations,
      n_issues = length(issues)
    ),
    class = "beezdemand_diagnostics"
  )
}


#' Print Method for Model Diagnostics
#'
#' @param x A `beezdemand_diagnostics` object.
#' @param ... Additional arguments (ignored).
#' @export
print.beezdemand_diagnostics <- function(x, ...) {
  cat("\nModel Diagnostics\n")
  cat(strrep("=", 50), "\n")
  cat("Model class:", x$model_class, "\n\n")

  # Convergence
  cat("Convergence:\n")
  if (x$convergence$converged) {
    cat("  Status: Converged\n")
  } else {
    cat("  Status: NOT CONVERGED\n")
    if (!is.null(x$convergence$message)) {
      cat("  Message:", x$convergence$message, "\n")
    }
  }

  # Random effects (if applicable)
  if (!is.null(x$random_effects)) {
    cat("\nRandom Effects:\n")
    vars <- x$random_effects$variances
    if (!is.null(vars) && length(vars) > 0) {
      for (nm in names(vars)) {
        status <- if (x$random_effects$near_zero[nm]) " [NEAR ZERO]" else ""
        cat(sprintf("  %s variance: %.4g%s\n", nm, vars[nm], status))
      }
    }
  }

  # Residuals
  cat("\nResiduals:\n")
  cat(sprintf("  Mean: %.4g\n", x$residuals$mean))
  cat(sprintf("  SD: %.4g\n", x$residuals$sd))
  cat(sprintf("  Range: [%.4g, %.4g]\n", x$residuals$min, x$residuals$max))
  if (x$residuals$has_outliers) {
    cat(sprintf("  Outliers: %d observations\n", x$residuals$n_outliers))
  }

  # Issues summary
  if (x$n_issues > 0) {
    cat("\n", strrep("-", 50), "\n", sep = "")
    cat("Issues Detected (", x$n_issues, "):\n", sep = "")
    for (i in seq_along(x$issues)) {
      cat("  ", i, ". ", x$issues[i], "\n", sep = "")
    }

    if (length(x$recommendations) > 0) {
      cat("\nRecommendations:\n")
      for (rec in x$recommendations) {
        cat("  - ", rec, "\n", sep = "")
      }
    }
  } else {
    cat("\n", strrep("-", 50), "\n", sep = "")
    cat("No issues detected.\n")
  }

  invisible(x)
}


#' Plot Residual Diagnostics
#'
#' @description
#' Creates diagnostic plots for model residuals including residuals vs fitted,
#' scale-location, and histogram of residuals.
#'
#' @param object A fitted model object.
#' @param type Character; type of residual plot. One of:
#'   - `"fitted"`: Residuals vs fitted values
#'   - `"histogram"`: Histogram of residuals
#'   - `"qq"`: Q-Q plot of residuals
#'   - `"all"`: All plots combined (default)
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return A ggplot2 object or list of ggplot2 objects.
#'
#' @examples
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
#' plot_residuals(fit)
#' plot_residuals(fit, type = "qq")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_histogram geom_qq
#'   geom_qq_line labs theme_minimal
#' @export
plot_residuals <- function(object, type = c("all", "fitted", "histogram", "qq"), ...) {
  type <- match.arg(type)

  # Get augmented data with residuals
  aug <- tryCatch(
    augment(object),
    error = function(e) {
      stop("Cannot compute residuals for this model: ", e$message, call. = FALSE)
    }
  )

  if (!".resid" %in% names(aug)) {
    stop("Model does not provide residuals.", call. = FALSE)
  }

  # Filter out NA residuals (e.g., zeros in hurdle models)
  aug <- aug[!is.na(aug$.resid), ]

  plots <- list()

  # Residuals vs Fitted
  if (type %in% c("all", "fitted")) {
    p_fitted <- ggplot2::ggplot(aug, ggplot2::aes(x = .data$.fitted, y = .data$.resid)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      ggplot2::geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 0.8) +
      ggplot2::labs(
        title = "Residuals vs Fitted",
        x = "Fitted values",
        y = "Residuals"
      ) +
      ggplot2::theme_minimal()
    plots$fitted <- p_fitted
  }

  # Histogram
  if (type %in% c("all", "histogram")) {
    p_hist <- ggplot2::ggplot(aug, ggplot2::aes(x = .data$.resid)) +
      ggplot2::geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
      ggplot2::labs(
        title = "Distribution of Residuals",
        x = "Residuals",
        y = "Count"
      ) +
      ggplot2::theme_minimal()
    plots$histogram <- p_hist
  }

  # Q-Q plot
  if (type %in% c("all", "qq")) {
    p_qq <- ggplot2::ggplot(aug, ggplot2::aes(sample = .data$.resid)) +
      ggplot2::geom_qq(alpha = 0.5) +
      ggplot2::geom_qq_line(color = "red", linetype = "dashed") +
      ggplot2::labs(
        title = "Normal Q-Q Plot",
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      ggplot2::theme_minimal()
    plots$qq <- p_qq
  }

  if (type == "all") {
    # Return list of plots
    class(plots) <- c("beezdemand_diagnostic_plots", "list")
    return(plots)
  } else {
    return(plots[[1]])
  }
}


#' Plot Random Effects Q-Q
#'
#' @description
#' Creates Q-Q plots for random effects to assess normality assumptions.
#'
#' @param object A fitted model object with random effects (`beezdemand_hurdle`
#'   or `beezdemand_nlme`).
#' @param which Character vector; which random effects to plot. Default is all.
#' @param ... Additional arguments (ignored).
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
#' plot_qq(fit)
#' plot_qq(fit, which = "Q0")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_qq geom_qq_line facet_wrap labs theme_minimal
#' @export
plot_qq <- function(object, which = NULL, ...) {
  UseMethod("plot_qq")
}


#' @rdname plot_qq
#' @export
plot_qq.beezdemand_hurdle <- function(object, which = NULL, ...) {
  # Get subject parameters (includes random effects)
  subj_pars <- object$subject_pars

  if (is.null(subj_pars)) {
    stop("No subject-level parameters available.", call. = FALSE)
  }

  # Identify random effect columns
  # Hurdle models use a_i (zeros), b_i (Q0), c_i (alpha)
  re_col_map <- c(
    a_i = "zeros",
    b_i = "Q0",
    c_i = "alpha"
  )
  available_cols <- names(re_col_map)[names(re_col_map) %in% names(subj_pars)]

  if (length(available_cols) == 0) {
    stop("No random effects found in model.", call. = FALSE)
  }

  if (!is.null(which)) {
    # Filter to requested effects
    re_col_map_rev <- stats::setNames(names(re_col_map), re_col_map)
    which_cols <- re_col_map_rev[which]
    which_cols <- which_cols[!is.na(which_cols)]
    available_cols <- intersect(available_cols, which_cols)
  }

  if (length(available_cols) == 0) {
    stop("Specified random effects not found in model.", call. = FALSE)
  }

  # Reshape for plotting
  re_data <- tidyr::pivot_longer(
    subj_pars[, c("id", available_cols), drop = FALSE],
    cols = tidyr::all_of(available_cols),
    names_to = "effect",
    values_to = "value"
  )

  # Clean up names for display
  re_data$effect <- re_col_map[re_data$effect]

  p <- ggplot2::ggplot(re_data, ggplot2::aes(sample = .data$value)) +
    ggplot2::geom_qq(alpha = 0.6) +
    ggplot2::geom_qq_line(color = "red", linetype = "dashed") +
    ggplot2::facet_wrap(~effect, scales = "free") +
    ggplot2::labs(
      title = "Q-Q Plot of Random Effects",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggplot2::theme_minimal()

  p
}


#' @rdname plot_qq
#' @export
plot_qq.beezdemand_nlme <- function(object, which = NULL, ...) {
  # Get random effects from nlme model
  if (is.null(object$model)) {
    stop("No model object available.", call. = FALSE)
  }

  re <- tryCatch(
    nlme::ranef(object$model),
    error = function(e) NULL
  )

  if (is.null(re)) {
    stop("Cannot extract random effects from model.", call. = FALSE)
  }

  re_cols <- names(re)

  if (!is.null(which)) {
    re_cols <- intersect(re_cols, which)
  }

  if (length(re_cols) == 0) {
    stop("Specified random effects not found in model.", call. = FALSE)
  }

  re$id <- rownames(re)

  # Reshape for plotting
  re_data <- tidyr::pivot_longer(
    re[, c("id", re_cols), drop = FALSE],
    cols = all_of(re_cols),
    names_to = "effect",
    values_to = "value"
  )

  p <- ggplot2::ggplot(re_data, ggplot2::aes(sample = .data$value)) +
    ggplot2::geom_qq(alpha = 0.6) +
    ggplot2::geom_qq_line(color = "red", linetype = "dashed") +
    ggplot2::facet_wrap(~effect, scales = "free") +
    ggplot2::labs(
      title = "Q-Q Plot of Random Effects",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggplot2::theme_minimal()

  p
}


# =============================================================================
# Internal helper functions
# =============================================================================

.check_hurdle_convergence <- function(object) {
  converged <- TRUE
  message <- NULL

  if (!is.null(object$model)) {
    opt <- object$model
    if (!is.null(opt$convergence)) {
      converged <- opt$convergence == 0
      if (!converged && !is.null(opt$message)) {
        message <- opt$message
      }
    }
  }

  list(
    converged = converged,
    message = message
  )
}


.check_hurdle_boundaries <- function(object) {
  at_boundary <- character(0)

  # Check variance parameters (should be > 0)
  if (!is.null(object$random_effects_sd)) {
    sds <- object$random_effects_sd
    for (nm in names(sds)) {
      if (!is.na(sds[nm]) && sds[nm] < 1e-6) {
        at_boundary <- c(at_boundary, paste0("SD_", nm))
      }
    }
  }

  list(at_boundary = at_boundary)
}


.check_hurdle_random_effects <- function(object) {
  variances <- NULL
  near_zero <- NULL

  if (!is.null(object$random_effects_sd)) {
    sds <- object$random_effects_sd
    variances <- sds^2
    names(variances) <- names(sds)
    near_zero <- variances < 1e-6
    names(near_zero) <- names(variances)
  }

  list(
    variances = variances,
    near_zero = near_zero
  )
}


.check_hurdle_residuals <- function(object) {
  aug <- tryCatch(augment(object), error = function(e) NULL)

  if (is.null(aug) || !".resid" %in% names(aug)) {
    return(list(
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_,
      has_outliers = FALSE,
      n_outliers = 0
    ))
  }

  resid <- aug$.resid[!is.na(aug$.resid)]

  if (length(resid) == 0) {
    return(list(
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_,
      has_outliers = FALSE,
      n_outliers = 0
    ))
  }

  sd_resid <- stats::sd(resid)
  outliers <- abs(resid) > 3 * sd_resid

  list(
    mean = mean(resid),
    sd = sd_resid,
    min = min(resid),
    max = max(resid),
    has_outliers = any(outliers),
    n_outliers = sum(outliers)
  )
}


.check_nlme_convergence <- function(object) {
  converged <- TRUE
  message <- NULL

  if (is.null(object$model)) {
    return(list(converged = FALSE, message = "Model fitting failed"))
  }

  # Check if model converged based on nlme diagnostics
  # nlme doesn't have a simple convergence flag, but we can check for warnings
  model <- object$model

  # Check apVar for convergence issues
  if (is.character(model$apVar)) {
    converged <- FALSE
    message <- "Hessian is not positive definite; variance estimates may be unreliable"
  }

  list(
    converged = converged,
    message = message
  )
}


.check_nlme_random_effects <- function(object) {
  variances <- NULL
  near_zero <- NULL
  correlation <- NULL
  near_singular <- NA

  if (is.null(object$model)) {
    return(list(
      variances = variances,
      near_zero = near_zero,
      correlation = correlation,
      near_singular = near_singular
    ))
  }

  model <- object$model

  # Get variance-covariance of random effects
  vc <- tryCatch(
    nlme::VarCorr(model),
    error = function(e) NULL
  )

  if (!is.null(vc)) {
    # Extract variance estimates
    var_cols <- c("Variance", "StdDev")
    var_col <- intersect(colnames(vc), var_cols)[1]

    if (!is.na(var_col)) {
      # Get row names (parameter names)
      param_names <- rownames(vc)
      param_names <- param_names[param_names != "Residual"]

      if (length(param_names) > 0) {
        # Get variance values
        var_vals <- as.numeric(vc[param_names, var_col])
        names(var_vals) <- param_names
        variances <- var_vals
        near_zero <- var_vals < 1e-6
        names(near_zero) <- param_names
      }
    }

    # Check for near-singular correlation
    if ("Corr" %in% colnames(vc)) {
      corr_vals <- as.numeric(vc[, "Corr"])
      corr_vals <- corr_vals[!is.na(corr_vals)]
      if (length(corr_vals) > 0 && any(abs(corr_vals) > 0.99)) {
        near_singular <- TRUE
      } else {
        near_singular <- FALSE
      }
    }
  }

  list(
    variances = variances,
    near_zero = near_zero,
    correlation = correlation,
    near_singular = near_singular
  )
}


.check_nlme_residuals <- function(object) {
  if (is.null(object$model)) {
    return(list(
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_,
      has_outliers = FALSE,
      n_outliers = 0
    ))
  }

  resid <- tryCatch(
    stats::residuals(object$model, type = "normalized"),
    error = function(e) stats::residuals(object$model)
  )

  if (is.null(resid) || length(resid) == 0) {
    return(list(
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_,
      has_outliers = FALSE,
      n_outliers = 0
    ))
  }

  sd_resid <- stats::sd(resid)
  outliers <- abs(resid) > 3

  list(
    mean = mean(resid),
    sd = sd_resid,
    min = min(resid),
    max = max(resid),
    has_outliers = any(outliers),
    n_outliers = sum(outliers)
  )
}


.check_fixed_convergence <- function(object) {
  n_total <- object$n_total %||% 0
  n_success <- object$n_success %||% 0
  n_failed <- object$n_fail %||% (n_total - n_success)

  list(
    converged = n_failed == 0,
    n_total = n_total,
    n_success = n_success,
    n_failed = n_failed,
    message = if (n_failed > 0) sprintf("%d subjects failed to converge", n_failed) else NULL
  )
}


.check_fixed_parameters <- function(object) {
  extreme_values <- character(0)

  if (!is.null(object$results) && is.data.frame(object$results)) {
    results <- object$results

    # Check for extreme Q0 values
    if ("Q0d" %in% names(results)) {
      q0 <- results$Q0d
      q0_extreme <- which(q0 < 0 | q0 > 1e6 | is.na(q0))
      if (length(q0_extreme) > 0) {
        ids <- if ("id" %in% names(results)) results$id[q0_extreme] else q0_extreme
        extreme_values <- c(extreme_values, as.character(ids))
      }
    }

    # Check for extreme alpha values
    if ("Alpha" %in% names(results)) {
      alpha <- results$Alpha
      alpha_extreme <- which(alpha < 0 | alpha > 1 | is.na(alpha))
      if (length(alpha_extreme) > 0) {
        ids <- if ("id" %in% names(results)) results$id[alpha_extreme] else alpha_extreme
        extreme_values <- c(extreme_values, as.character(ids))
      }
    }

    extreme_values <- unique(extreme_values)
  }

  list(extreme_values = extreme_values)
}


.check_fixed_residuals <- function(object) {
  aug <- tryCatch(augment(object), error = function(e) NULL)

  if (is.null(aug) || !".resid" %in% names(aug)) {
    return(list(
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_,
      has_outliers = FALSE,
      n_outliers = 0
    ))
  }

  resid <- aug$.resid[!is.na(aug$.resid)]

  if (length(resid) == 0) {
    return(list(
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_,
      has_outliers = FALSE,
      n_outliers = 0
    ))
  }

  sd_resid <- stats::sd(resid)
  outliers <- abs(resid) > 3 * sd_resid

  list(
    mean = mean(resid),
    sd = sd_resid,
    min = min(resid),
    max = max(resid),
    has_outliers = any(outliers),
    n_outliers = sum(outliers)
  )
}
