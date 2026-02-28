# Model Comparison Framework
# Provides unified model comparison across beezdemand model classes

#' Compare Demand Models
#'
#' @description
#' Compare multiple demand models using information criteria and likelihood
#' ratio tests (when applicable). Works with all beezdemand model classes.
#'
#' @param ... Two or more model objects of class `beezdemand_hurdle`,
#'   `beezdemand_nlme`, or `beezdemand_fixed`.
#' @param test Character; type of statistical test. One of:
#'   - `"auto"` (default): Use LRT if models are nested and comparable, otherwise IC-only.
#'   - `"lrt"`: Force likelihood ratio test (requires nested models from same backend).
#'   - `"none"`: Only report information criteria, no p-value.
#'
#' @return An object of class `beezdemand_model_comparison` containing:
#'   \describe{
#'     \item{comparison}{Data frame with model fit statistics}
#'     \item{test_type}{Type of test performed}
#'     \item{lrt_results}{LRT results if performed (NULL otherwise)}
#'     \item{best_model}{Index of best model by BIC}
#'     \item{notes}{Character vector of notes/warnings}
#'     \item{nesting_verified}{Logical; always FALSE since nesting is not
#'       automatically verified. Users must ensure models are properly nested
#'       for valid LRT interpretation.}
#'   }
#'
#' @details
#' Models are compared using AIC and BIC. For models from the same statistical
#' backend (e.g., two hurdle models or two NLME models), likelihood ratio tests
#' can be performed if the models are nested.
#'
#' When comparing models from different backends (e.g., hurdle vs NLME), only
#' information criteria comparisons are possible since the likelihoods are not
#' directly comparable for LRT purposes.
#'
#' ## Backend Compatibility
#'
#' | Backend 1 | Backend 2 | LRT Possible? |
#' |-----------|-----------|---------------|
#' | hurdle    | hurdle    | Yes (if nested) |
#' | nlme      | nlme      | Yes (if nested) |
#' | fixed     | fixed     | No (no likelihood) |
#' | hurdle    | nlme      | No |
#' | hurdle    | fixed     | No |
#' | nlme      | fixed     | No |
#'
#' @section Statistical Notes:
#' The likelihood ratio test (LRT) assumes that:
#' \enumerate{
#'   \item The models are **nested** (the reduced model is a special case of
#'     the full model obtained by constraining parameters).
#'   \item Both models are fit to **identical data**.
#'   \item Under the null hypothesis, the LR statistic follows a chi-square

#'     distribution with degrees of freedom equal to the difference in
#'     the number of parameters.
#' }
#'
#' **Important caveat for mixed-effects models:** When variance components
#' are tested at the boundary (e.g., testing whether a random effect variance
#' is zero), the standard chi-square distribution is not appropriate. The
#' correct null distribution is a mixture of chi-squares (Stram & Lee, 1994).
#' The p-values reported here use the standard chi-square approximation,
#' which is conservative (p-values are too large) for boundary tests.
#'
#' This function does **not** automatically verify that models are nested.
#' Users should ensure models are properly nested before interpreting LRT
#' p-values.
#'
#' @references
#' Stram, D. O., & Lee, J. W. (1994). Variance components testing in the
#' longitudinal mixed effects model. *Biometrics*, 50(4), 1171-1177.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit2 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
#'                           random_effects = c("zeros", "q0"))
#' fit3 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
#'                           random_effects = c("zeros", "q0", "alpha"))
#' compare_models(fit2, fit3)
#' }
#'
#' @seealso [compare_hurdle_models()] for the legacy hurdle-specific comparison
#' @export
compare_models <- function(..., test = c("auto", "lrt", "none")) {
  test <- match.arg(test)
  models <- list(...)

  if (length(models) < 2) {
    stop("At least two models must be provided for comparison.", call. = FALSE)
  }

  # Validate model classes
  valid_classes <- c("beezdemand_hurdle", "beezdemand_nlme", "beezdemand_fixed")

  for (i in seq_along(models)) {
    if (!inherits(models[[i]], valid_classes)) {
      stop(sprintf(
        "Model %d is not a recognized beezdemand model class. Got: %s",
        i, paste(class(models[[i]]), collapse = ", ")
      ), call. = FALSE)
    }
  }

  # Get model info
  model_info <- lapply(seq_along(models), function(i) {
    .get_model_info(models[[i]], name = paste0("Model_", i))
  })

  # Build comparison table
  comparison <- do.call(rbind, lapply(model_info, function(info) {
    data.frame(
      Model = info$name,
      Class = info$class,
      Backend = info$backend,
      nobs = info$nobs,
      df = info$df,
      logLik = info$logLik,
      AIC = info$AIC,
      BIC = info$BIC,
      stringsAsFactors = FALSE
    )
  }))

  notes <- character(0)
  lrt_results <- NULL

  # Check backend compatibility for LRT
  backends <- vapply(model_info, `[[`, character(1), "backend")
  same_backend <- length(unique(backends)) == 1

  if (!same_backend && test == "lrt") {
    warning(
      "Models have different backends (", paste(unique(backends), collapse = ", "),
      "). LRT cannot be performed; using IC-only comparison.",
      call. = FALSE
    )
    test <- "none"
    notes <- c(notes, "Models have different backends; LRT not applicable.")
  }

  # Check if LRT is possible (same backend with valid likelihoods)
  can_lrt <- same_backend &&
    all(vapply(model_info, function(x) !is.na(x$logLik), logical(1))) &&
    backends[1] != "legacy"

  if (test == "auto") {
    test <- if (can_lrt) "lrt" else "none"
  }

  if (test == "lrt") {
    if (!can_lrt) {
      if (backends[1] == "legacy") {
        warning(
          "LRT not available for legacy fixed-effect models (no likelihood).",
          call. = FALSE
        )
        notes <- c(notes, "Legacy fixed-effect models do not provide likelihood values.")
      } else {
        warning(
          "LRT cannot be performed: some models have missing likelihoods.",
          call. = FALSE
        )
      }
      test <- "none"
    } else {
      # Check sample size consistency across models (only relevant when doing LRT)
      nobs_values <- vapply(model_info, `[[`, numeric(1), "nobs")
      if (length(unique(stats::na.omit(nobs_values))) > 1) {
        warning(
          "Models appear to use different sample sizes. ",
          "LRT requires identical data for valid comparison.",
          call. = FALSE
        )
        notes <- c(notes, "Sample sizes differ across models.")
      }

      # Note about nesting assumption
      message(
        "Note: LRT assumes models are nested (reduced model is a special case ",
        "of the full model). Nesting is not automatically verified. ",
        "See ?compare_models for guidance on valid model comparisons."
      )
      notes <- c(notes, "LRT nesting assumption not verified.")

      # Perform pairwise LRT between adjacent models (sorted by df)
      ord <- order(comparison$df)
      comparison <- comparison[ord, ]
      model_info <- model_info[ord]
      models <- models[ord]

      lrt_list <- list()
      for (i in seq_len(nrow(comparison) - 1)) {
        reduced_idx <- i
        full_idx <- i + 1

        ll_reduced <- comparison$logLik[reduced_idx]
        ll_full <- comparison$logLik[full_idx]
        df_reduced <- comparison$df[reduced_idx]
        df_full <- comparison$df[full_idx]

        lr_stat <- 2 * (ll_full - ll_reduced)
        df_diff <- df_full - df_reduced

        # Check for negative LR statistic
        if (lr_stat < 0) {
          warning(sprintf(
            "Negative LR statistic (%.3f) for %s vs %s. This may indicate optimization issues or non-nested models.",
            lr_stat,
            comparison$Model[reduced_idx],
            comparison$Model[full_idx]
          ), call. = FALSE)
          p_value <- NA_real_
        } else if (df_diff <= 0) {
          warning(sprintf(
            "%s does not have more parameters than %s (df diff = %d).",
            comparison$Model[full_idx],
            comparison$Model[reduced_idx],
            df_diff
          ), call. = FALSE)
          p_value <- NA_real_
        } else {
          p_value <- stats::pchisq(lr_stat, df = df_diff, lower.tail = FALSE)
        }

        lrt_list[[i]] <- data.frame(
          Comparison = sprintf("%s vs %s",
                               comparison$Model[reduced_idx],
                               comparison$Model[full_idx]),
          LR_stat = lr_stat,
          df = df_diff,
          p_value = p_value,
          stringsAsFactors = FALSE
        )
      }

      lrt_results <- do.call(rbind, lrt_list)
    }
  }

  # Determine best model by IC (prefer BIC, fallback to AIC)
  best_idx <- NA_integer_
  if (any(is.finite(comparison$BIC))) {
    best_idx <- which.min(comparison$BIC)
  } else if (any(is.finite(comparison$AIC))) {
    best_idx <- which.min(comparison$AIC)
  }

  # Add delta columns
  if (any(is.finite(comparison$AIC))) {
    comparison$delta_AIC <- comparison$AIC - min(comparison$AIC, na.rm = TRUE)
  } else {
    comparison$delta_AIC <- rep(NA_real_, nrow(comparison))
  }
  if (any(is.finite(comparison$BIC))) {
    comparison$delta_BIC <- comparison$BIC - min(comparison$BIC, na.rm = TRUE)
  } else {
    comparison$delta_BIC <- rep(NA_real_, nrow(comparison))
  }

  result <- structure(
    list(
      comparison = comparison,
      test_type = test,
      lrt_results = lrt_results,
      best_model = best_idx,
      notes = notes,
      nesting_verified = FALSE  # Nesting is never automatically verified
    ),
    class = "beezdemand_model_comparison"
  )

  result
}


#' Print Method for Model Comparison
#'
#' @param x A `beezdemand_model_comparison` object.
#' @param digits Number of significant digits to print.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.beezdemand_model_comparison <- function(x, digits = 4, ...) {
  cat("\nModel Comparison\n")
  cat(strrep("=", 50), "\n\n")

  # Format comparison table
  comp <- x$comparison
  comp$logLik <- round(comp$logLik, digits)
  comp$AIC <- round(comp$AIC, digits)
  comp$BIC <- round(comp$BIC, digits)
  comp$delta_AIC <- round(comp$delta_AIC, digits)
  comp$delta_BIC <- round(comp$delta_BIC, digits)

  print(comp, row.names = FALSE)

  if (is.na(x$best_model)) {
    cat("\nBest model: NA (no comparable information criteria available)\n")
  } else {
    cat("\nBest model by BIC:", x$comparison$Model[x$best_model], "\n")
  }

  if (!is.null(x$lrt_results) && nrow(x$lrt_results) > 0) {
    cat("\nLikelihood Ratio Tests:\n")
    cat(strrep("-", 40), "\n")
    lrt <- x$lrt_results
    lrt$LR_stat <- round(lrt$LR_stat, digits)
    lrt$p_value <- format.pval(lrt$p_value, digits = 3)
    print(lrt, row.names = FALSE)
  }

  if (length(x$notes) > 0) {
    cat("\nNotes:\n")
    for (note in x$notes) {
      cat("  -", note, "\n")
    }
  }

  invisible(x)
}


#' ANOVA Method for Hurdle Demand Models
#'
#' @description
#' Compare nested hurdle demand models using likelihood ratio tests.
#'
#' @param object A `beezdemand_hurdle` model.
#' @param ... Additional `beezdemand_hurdle` models to compare.
#'
#' @return An object of class `anova.beezdemand` containing:
#'   \describe{
#'     \item{table}{Data frame with model comparison statistics}
#'     \item{lrt}{Likelihood ratio test results}
#'   }
#'
#' @details
#' All models must be fit to the same data. Models are ordered by degrees of
#' freedom, and sequential likelihood ratio tests are performed.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit2 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
#'                           random_effects = c("zeros", "q0"))
#' fit3 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
#'                           random_effects = c("zeros", "q0", "alpha"))
#' anova(fit2, fit3)
#' }
#'
#' @importFrom stats pchisq
#' @export
anova.beezdemand_hurdle <- function(object, ...) {
  models <- list(object, ...)

  if (length(models) < 2) {
    stop("At least two models must be provided for anova().", call. = FALSE)
  }

  # Validate all are hurdle models
  for (i in seq_along(models)) {
    if (!inherits(models[[i]], "beezdemand_hurdle")) {
      stop(sprintf(
        "Model %d is not a beezdemand_hurdle object. Use compare_models() for mixed backends.",
        i
      ), call. = FALSE)
    }
  }

  # Extract model info
  model_info <- lapply(seq_along(models), function(i) {
    m <- models[[i]]
    list(
      name = paste0("Model_", i),
      logLik = m$loglik,
      df = length(m$model$coefficients),
      AIC = m$AIC,
      BIC = m$BIC,
      n_re = m$param_info$n_random_effects
    )
  })

  # Build table
  table <- do.call(rbind, lapply(model_info, function(info) {
    data.frame(
      Model = info$name,
      n_RE = info$n_re,
      df = info$df,
      logLik = info$logLik,
      AIC = info$AIC,
      BIC = info$BIC,
      stringsAsFactors = FALSE
    )
  }))

  # Sort by df
  ord <- order(table$df)
  table <- table[ord, ]
  model_info <- model_info[ord]

  # Perform sequential LRT
  lrt_list <- list()
  for (i in seq_len(nrow(table) - 1)) {
    ll_reduced <- table$logLik[i]
    ll_full <- table$logLik[i + 1]
    df_reduced <- table$df[i]
    df_full <- table$df[i + 1]

    lr_stat <- 2 * (ll_full - ll_reduced)
    df_diff <- df_full - df_reduced

    p_value <- if (df_diff > 0) {
      stats::pchisq(lr_stat, df = df_diff, lower.tail = FALSE)
    } else {
      NA_real_
    }

    lrt_list[[i]] <- data.frame(
      Comparison = sprintf("%s vs %s", table$Model[i], table$Model[i + 1]),
      LR_stat = lr_stat,
      df = df_diff,
      Pr_Chisq = p_value,
      stringsAsFactors = FALSE
    )
  }

  lrt <- if (length(lrt_list) > 0) do.call(rbind, lrt_list) else NULL

  result <- structure(
    list(
      table = table,
      lrt = lrt
    ),
    class = c("anova.beezdemand_hurdle", "anova.beezdemand")
  )

  result
}


#' ANOVA Method for NLME Demand Models
#'
#' @description
#' Compare nested NLME demand models using likelihood ratio tests.
#'
#' @param object A `beezdemand_nlme` model.
#' @param ... Additional `beezdemand_nlme` models to compare.
#'
#' @return An object of class `anova.beezdemand` containing model comparison statistics.
#'
#' @details
#' For NLME models, this method delegates to `nlme::anova.lme()` on the
#' underlying model objects when possible.
#'
#' @examples
#' \donttest{
#' data(ko)
#' fit1 <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'                          id_var = "monkey", equation_form = "zben",
#'                          random_effects = Q0 ~ 1)
#' fit2 <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'                          id_var = "monkey", equation_form = "zben",
#'                          random_effects = Q0 + alpha ~ 1)
#' anova(fit1, fit2)
#' }
#'
#' @export
anova.beezdemand_nlme <- function(object, ...) {
  models <- list(object, ...)

  if (length(models) < 2) {
    stop("At least two models must be provided for anova().", call. = FALSE)
  }

  # Validate all are NLME models
  for (i in seq_along(models)) {
    if (!inherits(models[[i]], "beezdemand_nlme")) {
      stop(sprintf(
        "Model %d is not a beezdemand_nlme object. Use compare_models() for mixed backends.",
        i
      ), call. = FALSE)
    }
  }

  # Extract underlying nlme objects
  nlme_models <- lapply(models, function(m) m$model)

  # Delegate to nlme::anova.lme
  result <- do.call(stats::anova, nlme_models)

  # Add class for consistent handling
  class(result) <- c("anova.beezdemand_nlme", "anova.beezdemand", class(result))

  result
}


#' Print Method for ANOVA Comparisons
#'
#' @param x An `anova.beezdemand` object.
#' @param digits Number of significant digits to print.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.anova.beezdemand_hurdle <- function(x, digits = 4, ...) {
  cat("\nAnalysis of Variance Table\n")
  cat(strrep("=", 50), "\n\n")

  table <- x$table
  table$logLik <- round(table$logLik, digits)
  table$AIC <- round(table$AIC, digits)
  table$BIC <- round(table$BIC, digits)

  print(table, row.names = FALSE)

  if (!is.null(x$lrt) && nrow(x$lrt) > 0) {
    cat("\nLikelihood Ratio Tests:\n")
    cat(strrep("-", 40), "\n")
    lrt <- x$lrt
    lrt$LR_stat <- round(lrt$LR_stat, digits)
    lrt$Pr_Chisq <- format.pval(lrt$Pr_Chisq, digits = 3)
    print(lrt, row.names = FALSE)
  }

  invisible(x)
}


# Internal: Extract model info for comparison
.get_model_info <- function(model, name = "Model") {
  class_name <- class(model)[1]

  # Default values
  info <- list(
    name = name,
    class = class_name,
    backend = "unknown",
    nobs = NA_integer_,
    df = NA_integer_,
    logLik = NA_real_,
    AIC = NA_real_,
    BIC = NA_real_
  )

  if (inherits(model, "beezdemand_hurdle")) {
    info$backend <- "TMB"
    info$nobs <- nrow(model$data)
    info$df <- length(model$model$coefficients)
    info$logLik <- model$loglik
    info$AIC <- model$AIC
    info$BIC <- model$BIC

  } else if (inherits(model, "beezdemand_nlme")) {
    info$backend <- "nlme"
    info$nobs <- tryCatch(nrow(model$model$data), error = function(e) NA_integer_)
    info$df <- tryCatch(attr(stats::logLik(model$model), "df"), error = function(e) NA_integer_)
    info$logLik <- tryCatch(as.numeric(stats::logLik(model$model)), error = function(e) NA_real_)
    info$AIC <- tryCatch(stats::AIC(model$model), error = function(e) NA_real_)
    info$BIC <- tryCatch(stats::BIC(model$model), error = function(e) NA_real_)

  } else if (inherits(model, "beezdemand_fixed")) {
    info$backend <- "legacy"
    info$nobs <- if (!is.null(model$data_used)) {
      sum(vapply(model$data_used, nrow, integer(1)))
    } else {
      NA_integer_
    }
    info$df <- model$n_success * 3  # Approximate: each subject has Q0, alpha, k
    # Legacy fixed models don't have true likelihood values
    info$logLik <- NA_real_
    info$AIC <- NA_real_
    info$BIC <- NA_real_

  }

  info
}
