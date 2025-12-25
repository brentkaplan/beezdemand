#' @title Fixed-Effect Demand Curve Fitting
#' @description Modern wrapper for fitting individual demand curves via nonlinear
#'   least squares. Returns a structured S3 object with standard methods.
#' @name fixed-demand
NULL

#' Fit Fixed-Effect Demand Curves
#'
#' Modern interface for fitting individual demand curves via nonlinear
#' least squares. Returns a structured S3 object with standard methods
#' including `summary()`, `tidy()`, and `glance()`.
#'
#' @param data Data frame in long format with columns: `id`, `x` (price), `y` (consumption).
#' @param equation Character. Equation type: `"hs"` (Hursh & Silberberg, 2008),
#'   `"koff"` (Koffarnus et al., 2015), or `"linear"`. Default `"hs"`.
#' @param k Scaling constant. Numeric value (fixed), `"ind"` (individual),
#'   `"fit"` (free parameter), or `"range"` (data-driven). Default `2`.
#' @param agg Character. Aggregation method: `"Mean"`, `"Pooled"`, or `NULL`
#'   for individual fits. Default `NULL`.
#' @param x_var Character. Name of the price column. Default `"x"`.
#' @param y_var Character. Name of the consumption column. Default `"y"`.
#' @param id_var Character. Name of the subject identifier column. Default `"id"`.
#' @param ... Additional arguments passed to the underlying `FitCurves()` engine.
#'
#' @return An object of class `beezdemand_fixed` with components:
#'   \describe{
#'     \item{results}{Data frame of fitted parameters for each subject}
#'     \item{fits}{List of model fit objects (if `detailed = TRUE` internally)}
#'     \item{predictions}{List of prediction data frames}
#'     \item{data_used}{List of data frames used for each fit}
#'     \item{call}{The original function call}
#'     \item{equation}{The equation form used}
#'     \item{k_spec}{Description of k specification}
#'     \item{agg}{Aggregation method used}
#'     \item{n_total}{Total number of subjects/fits attempted}
#'     \item{n_success}{Number of successful fits}
#'     \item{n_fail}{Number of failed fits}
#'   }
#'
#' @details
#' This function is a modern wrapper around the legacy `FitCurves()` function.
#' It provides the same fitting capabilities but returns a structured S3 object
#' with standardized methods for model interrogation.
#'
#' @examples
#' \dontrun{
#' data(apt)
#' fit <- fit_demand_fixed(apt, equation = "hs", k = 2)
#' print(fit)
#' summary(fit)
#' tidy(fit)
#' glance(fit)
#' }
#'
#' @export
fit_demand_fixed <- function(data,
                             equation = c("hs", "koff", "linear"),
                             k = 2,
                             agg = NULL,
                             x_var = "x",
                             y_var = "y",
                             id_var = "id",
                             ...) {
  equation <- match.arg(equation)
  call <- match.call()

  # Call legacy engine with detailed = TRUE to get all outputs
  legacy_result <- FitCurves(
    dat = data,
    equation = equation,
    k = k,
    agg = agg,
    detailed = TRUE,
    xcol = x_var,
    ycol = y_var,
    idcol = id_var,
    ...
  )

  # Determine k specification mode for display
  k_spec <- if (is.numeric(k)) {
    paste0("fixed (", k, ")")
  } else {
    k
  }

  # Extract results - handle both list and data.frame returns
  if (is.list(legacy_result) && "dfres" %in% names(legacy_result)) {
    results <- legacy_result$dfres
    fits <- legacy_result$fits
    predictions <- legacy_result$newdats
    data_used <- legacy_result$adfs
  } else {
    # Simple data frame return
    results <- legacy_result
    fits <- NULL
    predictions <- NULL
    data_used <- NULL
  }

  # Count successes/failures
  if (is.data.frame(results) && nrow(results) > 0) {
    n_total <- nrow(results)
    success_flag <- NULL
    if ("Alpha" %in% names(results)) {
      success_flag <- !is.na(results$Alpha)
    } else if (all(c("L", "b", "a") %in% names(results))) {
      success_flag <- !is.na(results$L) & !is.na(results$b) & !is.na(results$a)
    } else if ("R2" %in% names(results)) {
      success_flag <- !is.na(results$R2)
    } else {
      success_flag <- rep(TRUE, n_total)
    }

    n_success <- sum(success_flag)
    n_fail <- n_total - n_success
  } else {
    n_total <- n_success <- n_fail <- NA_integer_
  }

  structure(
    list(
      results = results,
      fits = fits,
      predictions = predictions,
      data_used = data_used,
      call = call,
      equation = equation,
      k_spec = k_spec,
      k_value = if (is.numeric(k)) k else NA_real_,
      agg = agg,
      x_var = x_var,
      y_var = y_var,
      id_var = id_var,
      n_total = n_total,
      n_success = n_success,
      n_fail = n_fail
    ),
    class = c("beezdemand_fixed", "list")
  )
}


#' Print Method for beezdemand_fixed
#'
#' @param x A beezdemand_fixed object
#' @param ... Additional arguments (ignored)
#' @export
print.beezdemand_fixed <- function(x, ...) {
  cat("\nFixed-Effect Demand Model\n")
  cat("==========================\n\n")

  cat("Call:\n")
  print(x$call)
  cat("\n")

  cat("Equation:", x$equation, "\n")
  cat("k:", x$k_spec, "\n")
  if (!is.null(x$agg)) {
    cat("Aggregation:", x$agg, "\n")
  }
  cat("Subjects:", x$n_total, "(", x$n_success, "converged,", x$n_fail, "failed)\n")
  cat("\nUse summary() for parameter summaries, tidy() for tidy output.\n")

  invisible(x)
}


#' Summary Method for beezdemand_fixed
#'
#' @param object A beezdemand_fixed object
#' @param ... Additional arguments (ignored)
#' @return A summary.beezdemand_fixed object (inherits from beezdemand_summary)
#' @export
summary.beezdemand_fixed <- function(object, ...) {
  # Build coefficients tibble from results if available
  if (!is.null(object$results) && is.data.frame(object$results) &&
      nrow(object$results) > 0) {
    results <- object$results
    id_values <- if ("id" %in% names(results)) as.character(results$id) else rep(NA_character_, nrow(results))

    param_specs <- list(
      Q0 = list(estimate = "Q0d", se = "Q0se"),
      alpha = list(estimate = "Alpha", se = "Alphase"),
      k = list(estimate = "K", se = NA_character_),
      L = list(estimate = "L", se = "Lse"),
      b = list(estimate = "b", se = "bse"),
      a = list(estimate = "a", se = "ase")
    )

    param_specs <- Filter(function(spec) spec$estimate %in% names(results), param_specs)

    coefficients_list <- lapply(names(param_specs), function(term_name) {
      spec <- param_specs[[term_name]]
      tibble::tibble(
        id = id_values,
        term = term_name,
        estimate = results[[spec$estimate]],
        std.error = if (!is.na(spec$se) && spec$se %in% names(results)) results[[spec$se]] else NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        component = "fixed"
      )
    })
    coefficients <- dplyr::bind_rows(coefficients_list)

    param_summary <- lapply(names(param_specs), function(term_name) {
      vals <- results[[param_specs[[term_name]]$estimate]]
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) summary(vals) else NULL
    })
    names(param_summary) <- names(param_specs)

    # Count observations if data_used is available
    nobs <- if (!is.null(object$data_used)) {
      sum(vapply(object$data_used, nrow, integer(1)))
    } else {
      NA_integer_
    }
  } else {
    coefficients <- tibble::tibble(
      id = character(),
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      statistic = numeric(),
      p.value = numeric(),
      component = character()
    )
    param_summary <- list()
    nobs <- NA_integer_
  }

  structure(
    list(
      call = object$call,
      model_class = "beezdemand_fixed",
      backend = "legacy",
      equation = object$equation,
      k_spec = object$k_spec,
      agg = object$agg,
      nobs = nobs,
      n_subjects = object$n_total,
      n_success = object$n_success,
      n_fail = object$n_fail,
      converged = NA,
      logLik = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      coefficients = coefficients,
      param_summary = param_summary,
      results = object$results,
      notes = character(0)
    ),
    class = c("summary.beezdemand_fixed", "beezdemand_summary")
  )
}


#' Print Method for summary.beezdemand_fixed
#'
#' @param x A summary.beezdemand_fixed object
#' @param digits Number of significant digits to print
#' @param ... Additional arguments (ignored)
#' @export
print.summary.beezdemand_fixed <- function(x, digits = 4, ...) {
  cat("\n")
  cat("Fixed-Effect Demand Model Summary\n")
  cat(strrep("=", 50), "\n\n")

  cat("Equation:", x$equation, "\n")
  cat("k:", x$k_spec, "\n")
  if (!is.null(x$agg)) {
    cat("Aggregation:", x$agg, "\n")
  }
  cat("\n")

  cat("Fit Summary:\n")
  cat("  Total subjects:", x$n_subjects, "\n")
  cat("  Converged:", x$n_success, "\n")
  cat("  Failed:", x$n_fail, "\n")
  if (!is.na(x$nobs)) {
    cat("  Total observations:", x$nobs, "\n")
  }
  cat("\n")

  if (length(x$param_summary) > 0) {
    cat("Parameter Summary (across subjects):\n")

    if (!is.null(x$param_summary$Q0)) {
      q0_sum <- x$param_summary$Q0
      cat("  Q0:\n")
      cat("    Median:", round(q0_sum["Median"], digits), "\n")
      cat("    Range: [", round(q0_sum["Min."], digits), ",",
          round(q0_sum["Max."], digits), "]\n")
    }

    if (!is.null(x$param_summary$alpha)) {
      alpha_sum <- x$param_summary$alpha
      cat("  alpha:\n")
      cat("    Median:", round(alpha_sum["Median"], 6), "\n")
      cat("    Range: [", round(alpha_sum["Min."], 6), ",",
          round(alpha_sum["Max."], 6), "]\n")
    }
  }

  if (length(x$notes) > 0) {
    cat("\nNotes:\n")
    for (note in x$notes) {
      cat("  -", note, "\n")
    }
  }

  invisible(x)
}


#' Tidy Method for beezdemand_fixed
#'
#' @param x A beezdemand_fixed object
#' @param ... Additional arguments (ignored)
#' @return A tibble of model coefficients with columns: id, term, estimate,
#'   std.error, statistic, p.value, component
#' @export
tidy.beezdemand_fixed <- function(x, ...) {
  if (is.null(x$results) || !is.data.frame(x$results) || nrow(x$results) == 0) {
    return(tibble::tibble(
      id = character(),
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      statistic = numeric(),
      p.value = numeric(),
      component = character()
    ))
  }
  results <- x$results
  id_values <- if ("id" %in% names(results)) as.character(results$id) else rep(NA_character_, nrow(results))

  param_specs <- list(
    Q0 = list(estimate = "Q0d", se = "Q0se"),
    alpha = list(estimate = "Alpha", se = "Alphase"),
    k = list(estimate = "K", se = NA_character_),
    L = list(estimate = "L", se = "Lse"),
    b = list(estimate = "b", se = "bse"),
    a = list(estimate = "a", se = "ase")
  )

  param_specs <- Filter(function(spec) spec$estimate %in% names(results), param_specs)
  coefficient_rows <- lapply(names(param_specs), function(term_name) {
    spec <- param_specs[[term_name]]
    tibble::tibble(
      id = id_values,
      term = term_name,
      estimate = results[[spec$estimate]],
      std.error = if (!is.na(spec$se) && spec$se %in% names(results)) results[[spec$se]] else NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      component = "fixed"
    )
  })

  dplyr::bind_rows(coefficient_rows)
}


#' Glance Method for beezdemand_fixed
#'
#' @param x A beezdemand_fixed object
#' @param ... Additional arguments (ignored)
#' @return A one-row tibble of model statistics
#' @export
glance.beezdemand_fixed <- function(x, ...) {
  # Count observations if data_used is available
  nobs <- if (!is.null(x$data_used)) {
    sum(vapply(x$data_used, nrow, integer(1)))
  } else {
    NA_integer_
  }

  tibble::tibble(
    model_class = "beezdemand_fixed",
    backend = "legacy",
    equation = x$equation,
    k_spec = x$k_spec,
    nobs = nobs,
    n_subjects = x$n_total,
    n_success = x$n_success,
    n_fail = x$n_fail,
    converged = NA,
    logLik = NA_real_,
    AIC = NA_real_,
    BIC = NA_real_
  )
}
