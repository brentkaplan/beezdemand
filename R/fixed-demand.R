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
#' @param param_space Character. Parameterization used for fitting. One of:
#'   - `"natural"`: fit `Q0`, `alpha` (and `k` if `k = "fit"`) on their natural scale
#'   - `"log10"`: fit `log10(Q0)`, `log10(alpha)` (and `log10(k)` if `k = "fit"`)
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
                             param_space = c("natural", "log10"),
                             ...) {
  equation <- match.arg(equation)
  param_space <- match.arg(param_space)
  call <- match.call()

  # Call legacy engine with detailed = TRUE to get all outputs
  legacy_warnings <- character(0)
  legacy_result <- withCallingHandlers(
    FitCurves(
      dat = data,
      equation = equation,
      k = k,
      agg = agg,
      detailed = TRUE,
      xcol = x_var,
      ycol = y_var,
      idcol = id_var,
      param_space = param_space,
      ...
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      legacy_warnings <<- c(legacy_warnings, msg)
      # Legacy FitCurves can emit high-frequency data warnings; capture them
      # but avoid spamming downstream consumers/tests.
      if (grepl("Zeros found in data not compatible with equation", msg)) {
        invokeRestart("muffleWarning")
      }
    }
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
      legacy_warnings = unique(legacy_warnings),
      call = call,
      equation = equation,
      k_spec = k_spec,
      k_value = if (is.numeric(k)) k else NA_real_,
      agg = agg,
      x_var = x_var,
      y_var = y_var,
      id_var = id_var,
      param_space = param_space,
      param_space_details = beezdemand_param_space_details_core(
        internal_names = list(Q0 = "q0", alpha = "alpha", k = "k"),
        internal_spaces = list(
          Q0 = if (param_space == "log10") "log10" else "natural",
          alpha = if (param_space == "log10") "log10" else "natural",
          k = if (is.character(k) && identical(k, "fit") && param_space == "log10") "log10" else "natural"
        )
      ),
      n_total = n_total,
      n_success = n_success,
      n_fail = n_fail
    ),
    class = c("beezdemand_fixed", "list")
  )
}
