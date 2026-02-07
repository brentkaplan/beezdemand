#' Utility Functions for Cross-Price Hurdle Models
#' @name cp-hurdle-utils
#' @description Helper functions for working with cross-price hurdle demand models.
NULL

#' Compare Two Cross-Price Hurdle Models
#'
#' Performs a likelihood ratio test to compare two nested cross-price hurdle models.
#' Typically used to compare 2-RE and 3-RE models.
#'
#' @param model_full The full model (more parameters, e.g., 3-RE)
#' @param model_reduced The reduced model (fewer parameters, e.g., 2-RE)
#' @return A list with class "cp_hurdle_comparison" containing test results
#' @keywords internal
#' @examples
#' \dontrun{
#' fit2 <- fit_cp_hurdle(data, "y", "x", "id", random_effects = c("zeros", "qalone"))
#' fit3 <- fit_cp_hurdle(data, "y", "x", "id", random_effects = c("zeros", "qalone", "I"))
#' compare_cp_hurdle_models(fit3, fit2)
#' }
compare_cp_hurdle_models <- function(model_full, model_reduced) {
  if (
    !inherits(model_full, "beezdemand_cp_hurdle") ||
      !inherits(model_reduced, "beezdemand_cp_hurdle")
  ) {
    stop("Both arguments must be beezdemand_cp_hurdle objects")
  }

  # Get log-likelihoods
  ll_full <- model_full$loglik
  ll_reduced <- model_reduced$loglik

  # Check nesting (full should have more parameters)
  df_full <- length(model_full$opt$par)
  df_reduced <- length(model_reduced$opt$par)

  if (df_full <= df_reduced) {
    stop("model_full should have more parameters than model_reduced")
  }

  # Likelihood ratio test
  lr_stat <- 2 * (ll_full - ll_reduced)
  df_diff <- df_full - df_reduced
  p_value <- pchisq(lr_stat, df = df_diff, lower.tail = FALSE)

  result <- list(
    test = "Likelihood Ratio Test",
    model_full = list(
      n_re = model_full$param_info$n_random_effects,
      loglik = ll_full,
      df = df_full,
      AIC = model_full$AIC,
      BIC = model_full$BIC
    ),
    model_reduced = list(
      n_re = model_reduced$param_info$n_random_effects,
      loglik = ll_reduced,
      df = df_reduced,
      AIC = model_reduced$AIC,
      BIC = model_reduced$BIC
    ),
    lr_statistic = lr_stat,
    df = df_diff,
    p_value = p_value
  )

  class(result) <- "cp_hurdle_comparison"
  return(result)
}

#' Print method for cp_hurdle_comparison
#' @param x A cp_hurdle_comparison object
#' @param ... Additional arguments (ignored)
#' @keywords internal
#' @export
print.cp_hurdle_comparison <- function(x, ...) {
  cat("Likelihood Ratio Test for Cross-Price Hurdle Models\n")
  cat("====================================================\n\n")

  cat("Full model (", x$model_full$n_re, "-RE):\n", sep = "")
  cat("  Log-likelihood:", round(x$model_full$loglik, 2), "\n")
  cat("  Parameters:", x$model_full$df, "\n")
  cat("  AIC:", round(x$model_full$AIC, 2), "\n")
  cat("  BIC:", round(x$model_full$BIC, 2), "\n\n")

  cat("Reduced model (", x$model_reduced$n_re, "-RE):\n", sep = "")
  cat("  Log-likelihood:", round(x$model_reduced$loglik, 2), "\n")
  cat("  Parameters:", x$model_reduced$df, "\n")
  cat("  AIC:", round(x$model_reduced$AIC, 2), "\n")
  cat("  BIC:", round(x$model_reduced$BIC, 2), "\n\n")

  cat("Test Results:\n")
  cat("  LR statistic:", round(x$lr_statistic, 4), "\n")
  cat("  df:", x$df, "\n")
  cat("  p-value:", format.pval(x$p_value, digits = 4), "\n\n")

  # Recommendation
  if (x$p_value < 0.05) {
    cat("Conclusion: Full model significantly better (p < 0.05)\n")
  } else {
    cat("Conclusion: No significant difference; prefer simpler model\n")
  }

  # AIC/BIC comparison
  cat("\nInformation Criteria Comparison:\n")
  aic_diff <- x$model_full$AIC - x$model_reduced$AIC
  bic_diff <- x$model_full$BIC - x$model_reduced$BIC
  cat(
    "  AIC favors:",
    ifelse(aic_diff < 0, "Full model", "Reduced model"),
    "(diff =",
    round(abs(aic_diff), 2),
    ")\n"
  )
  cat(
    "  BIC favors:",
    ifelse(bic_diff < 0, "Full model", "Reduced model"),
    "(diff =",
    round(abs(bic_diff), 2),
    ")\n"
  )

  invisible(x)
}

#' Get Parameter Summary for Cross-Price Hurdle Model
#'
#' Computes summary statistics for subject-specific parameters.
#'
#' @param fit_obj A beezdemand_cp_hurdle object
#' @param ci_level Confidence interval level (default 0.95)
#' @return A data frame with parameter summary statistics
#' @keywords internal
get_cp_hurdle_param_summary <- function(fit_obj, ci_level = 0.95) {
  if (!inherits(fit_obj, "beezdemand_cp_hurdle")) {
    stop("fit_obj must be a beezdemand_cp_hurdle object")
  }

  sp <- fit_obj$subject_pars
  alpha <- 1 - ci_level

  # Function to compute summary stats
  param_stats <- function(x, name) {
    q_lower <- quantile(x, alpha / 2, na.rm = TRUE)
    q_upper <- quantile(x, 1 - alpha / 2, na.rm = TRUE)
    data.frame(
      parameter = name,
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      ci_lower = q_lower,
      ci_upper = q_upper,
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  # Compute stats for each parameter
  results <- rbind(
    param_stats(sp$Qalone, "Qalone"),
    param_stats(sp$I_individual, "I_individual"),
    param_stats(sp$breakpoint, "breakpoint")
  )

  rownames(results) <- NULL
  return(results)
}

#' Interpret Cross-Price Interaction Parameter
#'
#' Provides interpretation of the I parameter value.
#'
#' @param I_value The I parameter value (scalar or vector)
#' @return Character vector with interpretation
#' @keywords internal
interpret_cp_interaction <- function(I_value) {
  sapply(I_value, function(I) {
    if (is.na(I)) {
      return("Unknown (NA)")
    } else if (I < -0.5) {
      return("Strong substitute")
    } else if (I < 0) {
      return("Weak substitute")
    } else if (I < 0.5) {
      return("Weak complement")
    } else {
      return("Strong complement")
    }
  })
}

#' Extract Subject-Specific Predictions
#'
#' Creates a data frame with subject-specific predictions across a price range.
#'
#' @param fit_obj A beezdemand_cp_hurdle object
#' @param x_range Price range (default uses observed range)
#' @param n_points Number of prediction points
#' @return A data frame with columns: id, x, logQ, Q, prob_zero
#' @keywords internal
get_cp_hurdle_predictions <- function(fit_obj, x_range = NULL, n_points = 50) {
  if (!inherits(fit_obj, "beezdemand_cp_hurdle")) {
    stop("fit_obj must be a beezdemand_cp_hurdle object")
  }

  if (is.null(x_range)) {
    x_range <- range(fit_obj$data$x)
  }

  x_seq <- seq(x_range[1], x_range[2], length.out = n_points)
  ids <- unique(fit_obj$subject_pars$id)

  # Generate predictions for each subject
  preds <- lapply(ids, function(sid) {
    newdata <- data.frame(x = x_seq, id = sid)
    data.frame(
      id = sid,
      x = x_seq,
      logQ = predict(fit_obj, newdata, type = "demand", level = "individual"),
      Q = predict(fit_obj, newdata, type = "response", level = "individual"),
      prob_zero = predict(
        fit_obj,
        newdata,
        type = "probability",
        level = "individual"
      )
    )
  })

  do.call(rbind, preds)
}
