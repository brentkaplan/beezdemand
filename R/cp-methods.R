utils::globalVariables(c(
  "target",
  "y_mean",
  "y_sd",
  "unsys",
  "fit",
  "y_pred"
))

#-------------------------------------------------------------------------------
# Summarize Nonlinear Model (cp_model_nls)
#' Summarize a Cross-Price Demand Model (Nonlinear)
#'
#' @param object A cross-price model object from fit_cp_nls with return_all=TRUE.
#' @param inverse_fun Optional function to inverse-transform predictions (e.g., ll4_inv).
#' @param ... Additional arguments (unused).
#' @return A list containing model summary information.
#' @importFrom nlstools confint2
#' @importFrom stats residuals AIC BIC
#' @export
summary.cp_model_nls <- function(object, inverse_fun = NULL, ...) {
  model <- object$model
  equation <- object$equation
  method <- object$method

  # Check if model is NULL and return an informative message
  if (is.null(model)) {
    result <- list(
      call = NA,
      coefficients = data.frame(
        Estimate = numeric(0),
        "Std. Error" = numeric(0),
        "t value" = numeric(0),
        "Pr(>|t|)" = numeric(0)
      ),
      conf_int = NULL,
      equation = equation,
      equation_text = switch(
        equation,
        exponential = "y ~ log10(qalone) + I * exp(-beta * x)",
        exponentiated = "y ~ qalone * 10^(I * exp(-beta * x))",
        additive = "y ~ qalone + I * exp(-beta * x)",
        "Unknown equation type"
      ),
      method = method,
      method_description = switch(
        method,
        nls_multstart = "Multiple starting values optimization with nls.multstart",
        nlsLM = "Levenberg-Marquardt nonlinear least-squares algorithm",
        wrapnlsr = "Nonlinear least squares with 'nlsr' package",
        "Unknown model type"
      ),
      r_squared = NA,
      aic = NA,
      bic = NA,
      transform = "none",
      residuals = numeric(0),
      derived_metrics = NULL,
      data = object$data,
      error_message = "Model fitting failed - no valid model available"
    )

    class(result) <- "summary.cp_model_nls"
    return(result)
  }

  # Use provided data or attempt to extract from the model environment.
  data <- if (!is.null(object$data)) object$data else {
    model_env <- if (inherits(model, "nls")) model$m$getEnv() else
      environment(model)
    data.frame(x = model_env$x, y = model_env$y)
  }

  # Get coefficient summary
  if (inherits(model, c("nls", "nlsLM"))) {
    coef_summary <- summary(model)$coefficients
  } else if (inherits(model, "nlsr")) {
    coef_summary <- summary(model)$coefficients
    colnames(coef_summary) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  } else {
    coef_summary <- data.frame(
      Estimate = coef(model),
      `Std. Error` = NA,
      `t value` = NA,
      `Pr(>|t|)` = NA
    )
  }

  # Calculate R2: If transformation was applied, compute on the original scale.
  if (!is.null(inverse_fun) && equation == "exponential") {
    y_orig <- inverse_fun(data$y)
    y_pred <- fitted(model)
    y_pred_orig <- inverse_fun(y_pred)
    r_squared <- 1 -
      (sum((y_orig - y_pred_orig)^2) / sum((y_orig - mean(y_orig))^2))
    transform_info <- deparse(substitute(inverse_fun))
  } else {
    r_squared <- 1 - (sum(residuals(model)^2) / sum((data$y - mean(data$y))^2))
    transform_info <- "none"
  }

  # Fit statistics
  aic <- tryCatch(AIC(model), error = function(e) NA)
  bic <- tryCatch(BIC(model), error = function(e) NA)

  model_type_info <- switch(
    method,
    nls_multstart = "Multiple starting values optimization with nls.multstart",
    nlsLM = "Levenberg-Marquardt nonlinear least-squares algorithm",
    wrapnlsr = "Nonlinear least squares with 'nlsr' package",
    "Unknown model type"
  )

  equation_text <- switch(
    equation,
    exponential = "y ~ log10(qalone) + I * exp(-beta * x)",
    exponentiated = "y ~ qalone * 10^(I * exp(-beta * x))",
    additive = "y ~ qalone + I * exp(-beta * x)",
    "Unknown equation type"
  )

  conf_int <- tryCatch(
    {
      if (requireNamespace("nlstools", quietly = TRUE)) {
        ci <- nlstools::confint2(model)
        as.data.frame(ci)
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )

  if (inherits(object, "cp_model_nls") && !is.null(data)) {
    coefs <- coef(model)
    derived_metrics <- list(
      beta = coefs["beta"],
      I = coefs["I"],
      qalone = coefs["qalone"]
    )
  } else {
    derived_metrics <- NULL
  }

  result <- list(
    call = if (is.null(model$call)) NA else model$call,
    coefficients = coef_summary,
    conf_int = conf_int,
    equation = equation,
    equation_text = equation_text,
    method = method,
    method_description = model_type_info,
    r_squared = r_squared,
    aic = aic,
    bic = bic,
    transform = transform_info,
    residuals = residuals(model),
    derived_metrics = derived_metrics,
    data = data
  )

  class(result) <- "summary.cp_model_nls"
  return(result)
}

#' Print method for summary.cp_model_nls objects
#' @param x A `summary.*` object
#' @param ... Unused
#' @export
print.summary.cp_model_nls <- function(x, ...) {
  cat("Cross-Price Demand Model Summary\n")
  cat("================================\n\n")

  # Check for error message and display it prominently if present
  if (!is.null(x$error_message)) {
    cat("ERROR: ", x$error_message, "\n\n")
  }

  cat("Model Specification:\n")
  cat("Equation type:", x$equation, "\n")
  cat("Functional form:", x$equation_text, "\n")
  cat("Fitting method:", x$method, "\n")
  cat("Method details:", x$method_description, "\n")

  if (!is.null(x$error_message)) {
    cat("\nNote: No valid model was fit. Parameter estimates unavailable.\n")
    invisible(x)
    return()
  }

  if (x$transform != "none") {
    cat("Transformation applied:", x$transform, "\n")
  }
  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients)

  if (!is.null(x$conf_int)) {
    cat("\nConfidence Intervals:\n")
    print(x$conf_int, digits = 4)
  }
  cat("\nFit Statistics:\n")
  cat("R-squared:", format(x$r_squared, digits = 4), "\n")
  if (!is.na(x$aic)) cat("AIC:", format(x$aic, digits = 4), "\n")
  if (!is.na(x$bic)) cat("BIC:", format(x$bic, digits = 4), "\n")

  if (!is.null(x$derived_metrics)) {
    cat("\nParameter Interpretation:\n")
    cat(
      "qalone:",
      format(x$derived_metrics$qalone, digits = 4),
      " - consumption at zero alternative price\n"
    )
    cat(
      "I:",
      format(x$derived_metrics$I, digits = 4),
      " - interaction parameter (substitution direction)\n"
    )
    cat(
      "beta:",
      format(x$derived_metrics$beta, digits = 4),
      " - decay parameter (speed of cross-price effect decay)\n"
    )
  }
  invisible(x)
}


#' Plot a Cross-Price Demand Model (Nonlinear)
#'
#' @param x A cross-price model object from fit_cp_nls with return_all=TRUE.
#' @param data Optional data frame with x and y; if NULL, uses object$data.
#' @param inverse_fun Optional function to inverse-transform predictions.
#' @param n_points Number of points used for prediction curve.
#' @param title Optional plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param x_trans Transformation for x-axis: "identity", "log10", or "pseudo_log".
#' @param y_trans Transformation for y-axis: "identity", "log10", or "pseudo_log".
#' @param point_size Size of data points.
#' @param ... Additional arguments (passed to predict).
#' @return A ggplot2 object.
#' @importFrom scales log10_trans pseudo_log_trans identity_trans
#' @export
plot.cp_model_nls <- function(
  x,
  data = NULL,
  inverse_fun = NULL,
  n_points = 100,
  title = NULL,
  xlab = "Price",
  ylab = "Consumption",
  x_trans = "identity",
  y_trans = "identity",
  point_size = 3,
  ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")
  if (!requireNamespace("scales", quietly = TRUE))
    stop("Package 'scales' is required.")

  # Use provided data or fallback
  if (is.null(data)) {
    if (!is.null(x$data)) {
      data <- x$data
    } else {
      stop(
        "No data provided and no data found in model object. Please provide data."
      )
    }
  }

  # Defensive: ensure data has x and y
  if (!all(c("x", "y") %in% names(data)))
    stop("Data must contain columns 'x' and 'y'")

  # If model is NULL, plot only the data points and warn
  if (is.null(x$model)) {
    warning("Model fitting failed; plotting data points only.")
    p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(
        shape = 21,
        size = point_size,
        fill = "white"
      ) +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = if (is.null(title)) "No model fit: data only" else title
      ) +
      ggplot2::theme_bw()
    return(p)
  }

  # --- existing code for valid model below ---
  if ("target" %in% names(data)) {
    data <- data[data$target == "alt", ]
    if (nrow(data) == 0)
      stop("No data with target = 'alt' found in provided data")
  }

  allowed_trans <- c("identity", "log10", "pseudo_log")
  x_trans <- match.arg(x_trans, allowed_trans)
  y_trans <- match.arg(y_trans, allowed_trans)

  get_trans <- function(trans_name) {
    switch(
      trans_name,
      log10 = scales::log10_trans(),
      pseudo_log = scales::pseudo_log_trans(),
      identity = scales::identity_trans()
    )
  }
  x_trans_obj <- get_trans(x_trans)
  y_trans_obj <- get_trans(y_trans)

  if (x_trans == "log10" && any(data$x <= 0, na.rm = TRUE)) {
    data <- data[data$x > 0, ]
    warning("Filtered out non-positive x values for log10 transformation")
    if (nrow(data) == 0)
      stop("No positive x values left after filtering for log10 transformation")
  }

  x_range <- range(data$x, na.rm = TRUE)
  if (!all(is.finite(x_range)))
    stop("Cannot determine a valid x range from the provided data")

  if (x_trans == "log10") {
    min_x <- max(0.001, x_range[1])
    pred_x <- exp(seq(log(min_x), log(x_range[2]), length.out = n_points))
  } else {
    pred_x <- seq(x_range[1], x_range[2], length.out = n_points)
  }

  new_x <- data.frame(x = pred_x)
  preds <- predict(x, newdata = new_x, inverse_fun = inverse_fun, ...)

  y_col <- if (
    !is.null(inverse_fun) && "y_pred_untransformed" %in% names(preds)
  )
    "y_pred_untransformed" else "y_pred"

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = preds,
      ggplot2::aes(x = x, y = .data[[y_col]]),
      color = "blue",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(x = x, y = y),
      shape = 21,
      size = point_size,
      fill = "white"
    ) +
    ggplot2::scale_x_continuous(trans = x_trans_obj) +
    ggplot2::scale_y_continuous(trans = y_trans_obj)

  if (x_trans == "log10") p <- p + ggplot2::annotation_logticks(sides = "b")
  if (y_trans == "log10") p <- p + ggplot2::annotation_logticks(sides = "l")

  p <- p +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme_bw()
  return(p)
}


#' Predict from a Cross-Price Demand Model (Nonlinear)
#'
#' @param object A cross-price model object from fit_cp_nls with return_all=TRUE.
#' @param newdata A data frame containing an 'x' column.
#' @param inverse_fun Optional inverse transformation function.
#' @param ... Additional arguments.
#' @return A data frame with x values and predicted y values.
#' @export
predict.cp_model_nls <- function(
  object,
  newdata = NULL,
  inverse_fun = NULL,
  ...
) {
  if (!inherits(object, "cp_model_nls"))
    stop("Object must be of class 'cp_model_nls'")
  if (is.null(newdata))
    stop("'newdata' must be provided as a data frame with an 'x' column")
  if (!("x" %in% names(newdata)))
    stop("'newdata' must contain a column named 'x'")

  equation <- object$equation
  model <- object$model
  coefs <- tryCatch(coef(model), error = function(e) {
    stop("Could not extract coefficients from model: ", e$message)
  })

  if (!all(c("qalone", "I", "beta") %in% names(coefs)))
    stop("Missing required coefficients: qalone, I, or beta")

  qalone <- coefs["qalone"]
  I_param <- coefs["I"]
  beta <- coefs["beta"]

  x_vals <- newdata$x
  y_pred <- switch(
    equation,
    exponentiated = qalone * 10^(I_param * exp(-beta * x_vals)),
    exponential = log10(qalone) + I_param * exp(-beta * x_vals),
    additive = qalone + I_param * exp(-beta * x_vals),
    stop("Unsupported equation type: ", equation)
  )

  result <- data.frame(x = x_vals, y_pred = y_pred)
  if (!is.null(inverse_fun) && equation == "exponential") {
    tryCatch(
      {
        result$y_pred_untransformed <- inverse_fun(y_pred)
      },
      error = function(e) {
        warning("Failed to apply inverse transformation: ", e$message)
      }
    )
  }
  return(result)
}

#-------------------------------------------------------------------------------
# Predict Methods for Linear Models
#' Predict method for cp_model_lm objects.
#'
#' @param object A cp_model_lm object.
#' @param newdata Data frame containing new x values.
#' @param ... Additional arguments.
#' @return Data frame with predictions.
#' @export
predict.cp_model_lm <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) stop("newdata must be provided")
  predictions <- predict(object$model, newdata = newdata, ...)
  data.frame(x = newdata$x, y_pred = predictions)
}

#' Predict from a Mixed-Effects Cross-Price Demand Model
#'
#' Generates predictions from a mixed-effects cross-price demand model (of class
#' \code{cp_model_lmer}). The function supports two modes:
#'
#' \describe{
#'   \item{\code{"fixed"}}{Returns predictions based solely on the fixed-effects component
#'       (using \code{re.form = NA}).}
#'   \item{\code{"random"}}{Returns subject-specific predictions (fixed plus random effects)
#'       (using \code{re.form = NULL}).}
#' }
#'
#' @param object A \code{cp_model_lmer} object (as returned by \code{fit_cp_linear(type = "mixed", ...)}).
#' @param newdata A data frame containing at least an \code{x} column. For \code{pred_type = "random"},
#'   an \code{id} column is required. If absent, the function extracts unique ids from \code{object$data}
#'   and expands the grid accordingly. If no ids are available, a default id of 1 is used (with a warning).
#' @param pred_type Character string specifying the type of prediction: either \code{"fixed"} (population-level)
#'   or \code{"random"} (subject-specific). The default is \code{"fixed"}.
#' @param ... Additional arguments passed to the underlying \code{predict} function.
#'
#' @return A data frame containing all columns of \code{newdata} plus a column \code{y_pred}
#'   with the corresponding predictions.
#'
#' @examples
#' \dontrun{
#' # Population-level predictions:
#' predict(fit_out_3, newdata = ex_sub, pred_type = "fixed")
#'
#' # Subject-specific predictions:
#' predict(fit_out_3, newdata = ex_sub, pred_type = "random")
#' }
#'
#' @export
predict.cp_model_lmer <- function(
  object,
  newdata = NULL,
  pred_type = c("fixed", "random"),
  ...
) {
  if (is.null(newdata)) {
    stop("newdata must be provided")
  }
  if (!("x" %in% names(newdata))) {
    stop("newdata must contain a column named 'x'")
  }

  pred_type <- match.arg(pred_type)

  # If subject-specific predictions are requested, ensure newdata has an 'id' column.
  if (pred_type == "random" && !("id" %in% names(newdata))) {
    if (!is.null(object$data) && "id" %in% names(object$data)) {
      ids <- unique(object$data$id)
      # Expand grid: for every x value, create a row for each id.
      newdata <- expand.grid(x = newdata$x, id = ids)
    } else {
      warning(
        "newdata does not contain 'id' and object$data does not provide ids; assigning default id = 1"
      )
      newdata$id <- 1
    }
  }

  if (pred_type == "fixed") {
    preds <- predict(object$model, newdata = newdata, re.form = NA, ...)
  } else if (pred_type == "random") {
    preds <- predict(object$model, newdata = newdata, re.form = NULL, ...)
  }

  out <- as.data.frame(newdata)
  out$y_pred <- preds
  return(out)
}


#-------------------------------------------------------------------------------
# Summary Methods for Linear Models
#' Summary method for cp_model_lm objects.
#'
#' @param object A cp_model_lm object.
#' @param ... Additional arguments.
#' @return A list summarizing the linear model.
#' @export
summary.cp_model_lm <- function(object, ...) {
  model_summary <- summary(object$model)
  result <- list(
    call = object$model$call,
    coefficients = model_summary$coefficients,
    formula = object$formula,
    method = object$method,
    r_squared = model_summary$r.squared,
    adj_r_squared = model_summary$adj.r.squared,
    residuals = residuals(object$model)
  )
  class(result) <- "summary.cp_model_lm"
  return(result)
}

#' Summary method for cp_model_lmer objects.
#'
#' @param object A cp_model_lmer object.
#' @param ... Additional arguments.
#' @return A list summarizing the mixed-effects model.
#' @importFrom lme4 VarCorr
#' @importFrom performance r2_nakagawa
#' @importFrom stats AIC BIC
#' @export
summary.cp_model_lmer <- function(object, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required")
  }
  model_summary <- summary(object$model)
  fixed_effects <- model_summary$coefficients

  # Reformat the random effects table for clarity.
  var_corr <- lme4::VarCorr(object$model)
  rand_effects <- as.data.frame(var_corr)
  # Optionally, rename columns to make them more intuitive.
  names(rand_effects) <- c("Group", "Term", "Variance", "Std.Dev", "NA")

  r2 <- tryCatch(
    {
      if (requireNamespace("performance", quietly = TRUE)) {
        performance::r2_nakagawa(object$model)
      } else {
        list(R2_conditional = NA, R2_marginal = NA)
      }
    },
    error = function(e) list(R2_conditional = NA, R2_marginal = NA)
  )

  aic <- AIC(object$model)
  bic <- BIC(object$model)

  result <- list(
    call = object$model@call,
    coefficients = fixed_effects,
    random_effects = rand_effects,
    formula = object$formula,
    method = object$method,
    r2_marginal = r2$R2_marginal,
    r2_conditional = r2$R2_conditional,
    AIC = aic,
    BIC = bic,
    residuals = residuals(object$model)
  )
  class(result) <- "summary.cp_model_lmer"
  return(result)
}

#-------------------------------------------------------------------------------
# Broom Methods
#' Convert a cross-price model to a tidy data frame of coefficients
#'
#' This function extracts model coefficients from a cross-price demand model
#' into a tidy data frame format, following the conventions of the broom package.
#' It handles cases where model fitting failed gracefully, returning an empty
#' data frame with the expected structure.
#'
#' @param x A model object from fit_cp_nls or fit_cp_linear
#' @param ... Additional arguments (unused)
#'
#' @return A data frame with one row per coefficient, containing columns:
#'   \item{term}{The name of the model parameter}
#'   \item{estimate}{The estimated coefficient value}
#'   \item{std.error}{The standard error of the coefficient}
#'   \item{statistic}{The t-statistic for the coefficient}
#'   \item{p.value}{The p-value for the coefficient}
#'
#' @examples
#' \dontrun{
#' # Fit a cross-price demand model
#' model <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE)
#'
#' # Get coefficients in tidy format
#' tidy(model)
#' }
#'
#' @importFrom tibble rownames_to_column
#' @export
tidy.cp_model_nls <- function(x, ...) {
  if (is.null(x$model)) {
    return(data.frame(
      term = character(0),
      estimate = numeric(0),
      std.error = numeric(0),
      statistic = numeric(0),
      p.value = numeric(0)
    ))
  }

  summ <- summary(x)
  if (!is.null(summ$coefficients)) {
    stats::setNames(
      as.data.frame(summ$coefficients),
      c("estimate", "std.error", "statistic", "p.value")
    ) |>
      tibble::rownames_to_column("term")
  } else {
    # Fallback if summary doesn't contain coefficient matrix
    coeffs <- coef(x)
    data.frame(
      term = names(coeffs),
      estimate = unname(coeffs),
      std.error = NA,
      statistic = NA,
      p.value = NA,
      stringsAsFactors = FALSE
    )
  }
}

#' Get model summaries from a cross-price model
#'
#' This function extracts model summary statistics from a cross-price demand model
#' into a single-row data frame, following the conventions of the broom package.
#' It returns goodness-of-fit measures and other model information.
#'
#' @param x A model object from fit_cp_nls or fit_cp_linear
#' @param ... Additional arguments (unused)
#'
#' @return A one-row data frame with model summary statistics:
#'   \item{r.squared}{R-squared value indicating model fit}
#'   \item{aic}{Akaike Information Criterion}
#'   \item{bic}{Bayesian Information Criterion}
#'   \item{equation}{The equation type used in the model}
#'   \item{method}{The method used to fit the model}
#'   \item{transform}{The transformation applied to the data, if any}
#'
#' @examples
#' \dontrun{
#' # Fit a cross-price demand model
#' model <- fit_cp_nls(data, equation = "exponentiated", return_all = TRUE)
#'
#' # Get model summary statistics
#' glance(model)
#' }
#'
#' @export
glance.cp_model_nls <- function(x, ...) {
  summ <- summary(x)
  data.frame(
    r.squared = summ$r_squared,
    aic = summ$aic,
    bic = summ$bic,
    equation = summ$equation,
    method = summ$method,
    transform = summ$transform,
    stringsAsFactors = FALSE
  )
}


#-------------------------------------------------------------------------------
# Print Methods
#' Print method for summary.cp_model_lm objects.
#' @param x A `summary.*` object
#' @param ... Unused
#' @export
print.summary.cp_model_lm <- function(x, ...) {
  cat("Linear Cross-Price Demand Model Summary\n")
  cat("=======================================\n\n")
  cat("Formula:", deparse(x$formula), "\n")
  cat("Method:", x$method, "\n\n")
  cat("Coefficients:\n")
  printCoefmat(x$coefficients)
  cat(
    "\nR-squared:",
    format(x$r_squared, digits = 4),
    "  Adjusted R-squared:",
    format(x$adj_r_squared, digits = 4),
    "\n"
  )
  invisible(x)
}

#' Print method for summary.cp_model_lmer objects.
#' @param x A `summary.*` object
#' @param ... Unused
#' @export
print.summary.cp_model_lmer <- function(x, ...) {
  cat("Mixed-Effects Linear Cross-Price Demand Model Summary\n")
  cat("====================================================\n\n")
  cat("Formula:", deparse(x$formula), "\n")
  cat("Method:", x$method, "\n\n")
  cat("Fixed Effects:\n")
  printCoefmat(x$coefficients)
  cat("\nRandom Effects:\n")
  print(x$random_effects, row.names = FALSE)
  cat("\nModel Fit:\n")
  if (!is.na(x$r2_marginal))
    cat(
      "R2 (marginal):",
      format(x$r2_marginal, digits = 4),
      "  [Fixed effects only]\n"
    )
  if (!is.na(x$r2_conditional))
    cat(
      "R2 (conditional):",
      format(x$r2_conditional, digits = 4),
      "  [Fixed + random effects]\n"
    )
  cat("AIC:", format(x$AIC, digits = 4), "\n")
  cat("BIC:", format(x$BIC, digits = 4), "\n")
  cat("\nNote: R2 values for mixed models are approximate.\n")
  invisible(x)
}

#-------------------------------------------------------------------------------
# Plot Methods for Linear Models

#' Plot Method for Linear Cross-Price Demand Models
#'
#' Creates a ggplot2 visualization of a fitted linear cross-price demand model
#' (of class \code{cp_model_lm}). The plot overlays a prediction line over the
#' observed data points. Axis transformations (e.g., \code{"log10"}) can be applied.
#' If the model includes group effects, separate lines will be drawn for each group.
#'
#' @param x A \code{cp_model_lm} object (as returned by \code{fit_cp_linear(type = "fixed", ...)}).
#' @param data Optional data frame containing columns \code{x} and \code{y} to plot.
#'   If not provided, the function uses \code{object$data} if available.
#' @param x_trans Transformation for the x-axis; one of \code{"identity"}, \code{"log10"}, or \code{"pseudo_log"}.
#'   Default is \code{"identity"}.
#' @param y_trans Transformation for the y-axis; one of \code{"identity"}, \code{"log10"}, or \code{"pseudo_log"}.
#'   Default is \code{"identity"}.
#' @param n_points Number of points to create in the prediction grid. Default is \code{100}.
#' @param xlab Label for the x-axis. Default is \code{"Price"}.
#' @param ylab Label for the y-axis. Default is \code{"Consumption"}.
#' @param title Optional title for the plot; default is \code{NULL}.
#' @param point_size Size of the data points in the plot. Default is \code{3}.
#' @param ... Additional arguments passed to the generic \code{predict} method.
#'
#' @return A ggplot2 object displaying the fitted model predictions and observed data.
#'
#' @export
plot.cp_model_lm <- function(
  x,
  data = NULL,
  x_trans = "identity",
  y_trans = "identity",
  n_points = 100,
  xlab = "Price",
  ylab = "Consumption",
  title = NULL,
  point_size = 3,
  ...
) {
  object <- x
  # Use provided data, or fallback on object$data if available.
  if (is.null(data)) {
    if (!is.null(object$data)) {
      data <- object$data
    } else {
      stop(
        "No data provided and no data found in model object. Please supply a data frame."
      )
    }
  }

  # Filter for target "alt" if that column exists.
  if ("target" %in% names(data)) {
    data <- data[data$target == "alt", ]
  }
  if (!all(c("x", "y") %in% names(data))) {
    stop("Data must contain columns 'x' and 'y'.")
  }

  # Determine if the model has group effects
  has_group_effects <- !is.null(object$group_effects) &&
    (isTRUE(object$group_effects) ||
      object$group_effects %in% c("intercept", "interaction"))

  # Get the group variable if present in the model formula
  if (has_group_effects && !("group" %in% names(data))) {
    stop("Model includes group effects but 'group' variable not found in data.")
  }

  # Determine the x-range and create a prediction grid.
  x_range <- range(data$x, na.rm = TRUE)
  if (x_trans == "log10") {
    if (any(data$x <= 0, na.rm = TRUE)) {
      data <- data[data$x > 0, ]
      warning("Filtered out non-positive x values for log10 transformation")
      x_range <- range(data$x, na.rm = TRUE)
    }
    min_x <- max(0.001, x_range[1])
    pred_x <- exp(seq(log(min_x), log(x_range[2]), length.out = n_points))
  } else {
    pred_x <- seq(x_range[1], x_range[2], length.out = n_points)
  }

  # Create prediction grid - now handling group if present
  if (has_group_effects) {
    # Get unique groups
    groups <- unique(data$group)
    # Create a grid with all combinations of x and group
    newdata <- expand.grid(x = pred_x, group = groups)
    # Ensure group is a factor if original was a factor
    if (is.factor(data$group))
      newdata$group <- factor(newdata$group, levels = levels(data$group))
  } else {
    newdata <- data.frame(x = pred_x)
  }

  # Generate predictions (your S3 predict method will return a data frame with a y_pred column)
  preds <- predict(object, newdata = newdata, ...)
  if (has_group_effects) {
    preds <- data.frame(
      x = newdata$x,
      group = newdata$group,
      y_pred = preds$y_pred
    )
  } else {
    preds <- data.frame(x = newdata$x, y_pred = preds$y_pred)
  }

  # Build the ggplot
  p <- ggplot2::ggplot()

  # Add line(s) - different handling for group vs no group
  if (has_group_effects) {
    p <- p +
      ggplot2::geom_line(
        data = preds,
        ggplot2::aes(x = x, y = y_pred, color = group, group = group),
        linewidth = 1
      )

    # Add points with color by group
    p <- p +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(x = x, y = y, fill = group),
        shape = 21,
        size = point_size,
        color = "black",
        stroke = 0.5
      )
  } else {
    p <- p +
      ggplot2::geom_line(
        data = preds,
        ggplot2::aes(x = x, y = y_pred),
        color = "blue",
        linewidth = 1
      )

    # Add points
    p <- p +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(x = x, y = y),
        shape = 21,
        size = point_size,
        fill = "white"
      )
  }

  # Add labels and theme
  p <- p +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme_bw()

  # Apply axis transformations
  if (x_trans == "log10") {
    p <- p +
      ggplot2::scale_x_continuous(trans = scales::log10_trans()) +
      ggplot2::annotation_logticks(sides = "b")
  }
  if (y_trans == "log10") {
    p <- p +
      ggplot2::scale_y_continuous(trans = scales::log10_trans()) +
      ggplot2::annotation_logticks(sides = "l")
  }

  return(p)
}


#' Plot Method for Mixed-Effects Cross-Price Demand Models
#'
#' Creates a ggplot2 visualization of a fitted mixed-effects cross-price demand model
#' (of class \code{cp_model_lmer}). This function allows you to plot:
#'
#' \describe{
#'   \item{\code{"fixed"}}{Only the population-level (fixed-effects) prediction.}
#'   \item{\code{"random"}}{Only the subject-specific predictions.}
#'   \item{\code{"all"}}{Both: the fixed-effects and the subject-specific predictions.}
#' }
#'
#' If the model includes group effects, separate lines will be drawn for each group.
#'
#' @param x A \code{cp_model_lmer} object (as returned by
#'   \code{fit_cp_linear(type = "mixed", ...)}).
#' @param data Optional data frame containing columns \code{x} and \code{y} to be plotted.
#'   If not provided, \code{object$data} is used.
#' @param x_trans Transformation for the x-axis; one of \code{"identity"}, \code{"log10"}, or
#'   \code{"pseudo_log"}. Default is \code{"identity"}.
#' @param y_trans Transformation for the y-axis; one of \code{"identity"}, \code{"log10"}, or
#'   \code{"pseudo_log"}. Default is \code{"identity"}.
#' @param n_points Number of points to use in creating the prediction grid. Default is \code{100}.
#' @param xlab Label for the x-axis. Default is \code{"Price"}.
#' @param ylab Label for the y-axis. Default is \code{"Consumption"}.
#' @param title Optional title for the plot; default is \code{NULL}.
#' @param point_size Size of the observed data points. Default is \code{3}.
#' @param pred_type Character string specifying which prediction components to plot:
#'   \describe{
#'     \item{\code{"fixed"}}{Plot only the fixed-effects (population) prediction.}
#'     \item{\code{"random"}}{Plot only the subject-specific predictions.}
#'     \item{\code{"all"}}{Plot both the fixed-effects and the subject-specific predictions.}
#'   }
#'   The default is \code{"fixed"}.
#' @param ... Additional arguments passed to \code{\link{predict.cp_model_lmer}}.
#'
#' @return A ggplot2 object displaying the observed data points along with the prediction curves.
#'
#' @export
plot.cp_model_lmer <- function(
  x,
  data = NULL,
  x_trans = "identity",
  y_trans = "identity",
  n_points = 100,
  xlab = "Price",
  ylab = "Consumption",
  title = NULL,
  point_size = 3,
  pred_type = c("fixed", "random", "all"),
  ...
) {
  object <- x # Rename for clarity (x is the formal parameter name for plot methods)
  pred_type <- match.arg(pred_type)

  # Use provided data or fall back on object$data if available.
  if (is.null(data)) {
    if (!is.null(object$data)) {
      data <- object$data
    } else {
      stop(
        "No data provided and no data found in model object. Please supply a data frame."
      )
    }
  }

  # Filter data for target == 'alt' if present.
  if ("target" %in% names(data)) {
    data <- data[data$target == "alt", ]
  }
  if (!all(c("x", "y") %in% names(data))) {
    stop("Data must contain columns 'x' and 'y'.")
  }

  # Determine if the model has group effects
  has_group_effects <- !is.null(object$group_effects) &&
    (isTRUE(object$group_effects) ||
      object$group_effects %in% c("intercept", "interaction"))

  # Check for group column when needed
  if (has_group_effects && !("group" %in% names(data))) {
    stop("Model includes group effects but 'group' variable not found in data.")
  }

  # If x_trans is "log10", filter out non-positive x values.
  x_range <- range(data$x, na.rm = TRUE)
  if (x_trans == "log10") {
    if (any(data$x <= 0, na.rm = TRUE)) {
      data <- data[data$x > 0, ]
      warning("Filtered out non-positive x values for log10 transformation")
      x_range <- range(data$x, na.rm = TRUE)
    }
  }

  # Create a prediction grid for x values.
  if (x_trans == "log10") {
    min_x <- max(1e-3, x_range[1])
    pred_x <- exp(seq(log(min_x), log(x_range[2]), length.out = n_points))
  } else {
    pred_x <- seq(x_range[1], x_range[2], length.out = n_points)
  }

  # Initialize plot
  p <- ggplot2::ggplot() +
    # Plot observed data - with group coloring if applicable
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(x = x, y = y, fill = if (has_group_effects) group else NULL),
      shape = 21,
      size = point_size,
      color = "black",
      stroke = 0.5
    ) +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme_bw()

  # Population (fixed) predictions.
  if (pred_type %in% c("fixed", "all")) {
    # Create prediction grid for fixed effects
    if (has_group_effects) {
      # Get unique groups
      groups <- unique(data$group)
      # Create grid with all x and group combinations
      pop_newdata <- expand.grid(x = pred_x, group = groups)
      # Ensure group is a factor if original was
      if (is.factor(data$group)) {
        pop_newdata$group <- factor(
          pop_newdata$group,
          levels = levels(data$group)
        )
      }
    } else {
      pop_newdata <- data.frame(x = pred_x)
    }

    # Get fixed-effects predictions
    preds_fixed <- predict(
      object,
      newdata = pop_newdata,
      pred_type = "fixed",
      ...
    )

    # Add to plot - different handling for groups
    if (has_group_effects) {
      p <- p +
        ggplot2::geom_line(
          data = preds_fixed,
          ggplot2::aes(x = x, y = y_pred, color = group, group = group),
          linewidth = 1.2
        )
    } else {
      p <- p +
        ggplot2::geom_line(
          data = preds_fixed,
          ggplot2::aes(x = x, y = y_pred, group = 1),
          color = "black",
          linewidth = 1.2
        )
    }
  }

  # Subject-specific (random) predictions.
  if (pred_type %in% c("random", "all")) {
    # Get unique IDs
    if ("id" %in% names(data)) {
      ids <- unique(data$id)

      # Create prediction grid for random effects
      if (has_group_effects) {
        # With groups: need x, id and group combinations
        groups <- unique(data$group)
        # For random effects, we need ID-specific predictions for each group
        rand_newdata <- expand.grid(x = pred_x, id = ids, group = groups)
        # Filter to match id-group combinations that exist in the data
        id_group_pairs <- unique(data[, c("id", "group")])
        rand_newdata <- merge(rand_newdata, id_group_pairs)
        # Ensure group is a factor if original was
        if (is.factor(data$group)) {
          rand_newdata$group <- factor(
            rand_newdata$group,
            levels = levels(data$group)
          )
        }
      } else {
        # Without groups: just need x and id combinations
        rand_newdata <- expand.grid(x = pred_x, id = ids)
      }

      # Get subject-specific predictions
      preds_random <- predict(
        object,
        newdata = rand_newdata,
        pred_type = "random",
        ...
      )

      # Add to plot - different handling for groups
      if (has_group_effects) {
        p <- p +
          ggplot2::geom_line(
            data = preds_random,
            ggplot2::aes(
              x = x,
              y = y_pred,
              group = interaction(id, group),
              color = group
            ),
            linewidth = 0.6,
            alpha = 0.5
          )
      } else {
        p <- p +
          ggplot2::geom_line(
            data = preds_random,
            ggplot2::aes(x = x, y = y_pred, group = id),
            color = "blue",
            linewidth = 0.6,
            alpha = 0.5
          )
      }
    } else {
      warning(
        "No 'id' column found in data. Cannot plot subject-specific predictions."
      )
    }
  }

  # Apply axis transformations.
  if (x_trans == "log10") {
    p <- p +
      ggplot2::scale_x_continuous(trans = scales::log10_trans()) +
      ggplot2::annotation_logticks(sides = "b")
  }
  if (y_trans == "log10") {
    p <- p +
      ggplot2::scale_y_continuous(trans = scales::log10_trans()) +
      ggplot2::annotation_logticks(sides = "l")
  }

  # Add appropriate legend titles if needed
  if (has_group_effects) {
    p <- p +
      ggplot2::guides(
        color = ggplot2::guide_legend(title = "Group"),
        fill = ggplot2::guide_legend(title = "Group")
      )
  }

  return(p)
}


#' Extract Coefficients from Cross-Price Demand Models
#'
#' @description Methods to extract coefficients from various cross-price demand model objects.
#'
#' @name coef-methods
#' @rdname coef-methods
NULL

#' @describeIn coef-methods Extract coefficients from a nonlinear cross-price model
#' @param object A cp_model_nls object
#' @param ... Additional arguments (not used).
#' @return Named vector of coefficients
#' @export
coef.cp_model_nls <- function(object, ...) {
  if (!inherits(object, "cp_model_nls")) {
    stop("Object must be of class 'cp_model_nls'")
  }

  # Simply extract the coefficients from the underlying model
  coef(object$model)
}

#' @describeIn coef-methods Extract coefficients from a linear cross-price model
#' @param object A cp_model_lm object
#' @param ... Additional arguments (not used).
#' @export
coef.cp_model_lm <- function(object, ...) {
  if (!inherits(object, "cp_model_lm")) {
    stop("Object must be of class 'cp_model_lm'")
  }

  # Simply extract the coefficients from the underlying model
  coef(object$model)
}

#' @describeIn coef-methods Extract coefficients from a mixed-effects cross-price model
#' @param fixed_only Logical; if TRUE, returns only fixed effects. Default is FALSE.
#' @param combine Logical; if TRUE and fixed_only=FALSE, returns fixed + random effects combined. Default is TRUE.
#' @importFrom lme4 fixef
#' @export
coef.cp_model_lmer <- function(
  object,
  fixed_only = FALSE,
  combine = TRUE,
  ...
) {
  if (!inherits(object, "cp_model_lmer")) {
    stop("Object must be of class 'cp_model_lmer'")
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop(
      "Package 'lme4' is required for extracting coefficients from mixed-effects models"
    )
  }

  # If only fixed effects are needed
  if (fixed_only) {
    return(lme4::fixef(object$model))
  }

  # Get both fixed and random effects
  if (combine) {
    # Don't use lme4::coef() - just use coef() directly
    return(coef(object$model)) # This uses S3 dispatch to find the right method
  } else {
    # Return a list with separate fixed and random effects
    list(
      fixed = lme4::fixef(object$model),
      random = lme4::ranef(object$model)
    )
  }
}


#' Extract Random Effects from Mixed-Effects Cross-Price Model
#'
#' @param object A cp_model_lmer object
#' @param ... Additional arguments passed to ranef
#' @return List of random effects
#' @importFrom lme4 ranef
#' @export
ranef.cp_model_lmer <- function(object, ...) {
  if (!inherits(object, "cp_model_lmer")) {
    stop("Object must be of class 'cp_model_lmer'")
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for extracting random effects")
  }

  lme4::ranef(object$model, ...)
}

#' Extract Fixed Effects from Mixed-Effects Cross-Price Model
#'
#' @param object A cp_model_lmer object
#' @param ... Additional arguments passed to fixef
#' @return Named vector of fixed effects
#' @importFrom lme4 fixef
#' @export
fixef.cp_model_lmer <- function(object, ...) {
  if (!inherits(object, "cp_model_lmer")) {
    stop("Object must be of class 'cp_model_lmer'")
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for extracting fixed effects")
  }

  lme4::fixef(object$model, ...)
}

#' Extract All Coefficient Types from Cross-Price Demand Models
#'
#' @description
#' A convenience function to extract coefficients from any type of cross-price
#' demand model in a unified format. For mixed effects models, returns a list
#' with different coefficient types.
#'
#' @param object A cross-price demand model object (cp_model_nls, cp_model_lm, or cp_model_lmer)
#' @param ... Additional arguments passed to the appropriate coef method
#' @return For cp_model_nls and cp_model_lm, returns the model coefficients.
#'   For cp_model_lmer, returns a list with fixed, random, and combined coefficients.
#' @importFrom lme4 fixef ranef
#' @export
extract_coefficients <- function(object, ...) {
  if (inherits(object, "cp_model_nls") || inherits(object, "cp_model_lm")) {
    return(coef(object, ...))
  } else if (inherits(object, "cp_model_lmer")) {
    return(list(
      fixed = fixef(object, ...),
      random = ranef(object, ...),
      combined = coef(object, fixed_only = FALSE, combine = TRUE, ...)
    ))
  } else {
    stop(
      "Unsupported model class. Must be one of: cp_model_nls, cp_model_lm, cp_model_lmer"
    )
  }
}

#-------------------------------------------------------------------------------
# Posthoc Methods for Linear Models
#' Test for significant interaction in a cross-price demand model
#'
#' @param object A cp_model_lmer object from fit_cp_linear
#' @param alpha Significance level for testing (default: 0.05)
#' @return Logical indicating whether interaction is significant
#' @keywords internal
has_significant_interaction <- function(object, alpha = 0.05) {
  if (!inherits(object, "cp_model_lmer")) {
    stop("Object must be a cp_model_lmer object")
  }

  # Get coefficient summary
  coefs <- coef(summary(object$model))

  # Look for interaction terms
  interaction_rows <- grep(":", rownames(coefs))

  if (length(interaction_rows) == 0) {
    return(FALSE) # No interaction terms found
  }

  # For mixed models, calculate p-values from t-values
  # Column indices may differ between model types
  if ("t value" %in% colnames(coefs)) {
    t_col <- "t value"
  } else if ("t.value" %in% colnames(coefs)) {
    t_col <- "t.value"
  } else {
    # If t-values are in column 3 (typical for lmer models)
    t_col <- 3
  }

  # Extract t-values for interaction terms
  t_values <- coefs[interaction_rows, t_col]

  # Calculate approximate p-values (two-tailed)
  # For mixed models, degrees of freedom are not straightforward
  # We'll use a conservative approach with normal approximation
  p_values <- 2 * (1 - pnorm(abs(t_values)))

  # Check if any interaction is significant
  any(p_values < alpha, na.rm = TRUE)
}

#' Run pairwise slope comparisons for cross-price demand model
#'
#' This function performs pairwise comparisons of slopes between groups in a
#' cross-price demand model, but only when a significant interaction is present.
#' The emmeans table showing estimated marginal means for slopes is always returned.
#'
#' @param object A cp_model_lmer object from fit_cp_linear
#' @param alpha Significance level for testing (default: 0.05)
#' @param adjust Method for p-value adjustment; see emmeans::contrast (default: "tukey")
#' @param ... Additional arguments passed to emmeans
#' @return List containing the emmeans table and optionally pairwise comparisons if interaction is significant
#' @importFrom emmeans emmeans emtrends contrast
#' @export
cp_posthoc_slopes <- function(object, alpha = 0.05, adjust = "tukey", ...) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Package 'emmeans' is required for pairwise comparisons")
  }

  if (!inherits(object, "cp_model_lmer")) {
    stop("Object must be a cp_model_lmer object")
  }

  # Check if there's a significant interaction
  has_interaction <- has_significant_interaction(object, alpha)

  # Calculate the emmeans for slopes regardless of significance
  if (isTRUE(object$log10x)) {
    # The formula has log10(x), so identify x as the variable
    trends <- emmeans::emtrends(
      object$model,
      ~group,
      var = "x",
      tran = "log10",
      ...
    )
  } else {
    # For regular x, just use emtrends directly
    trends <- emmeans::emtrends(object$model, ~group, var = "x", ...)
  }

  # Always include the emmeans table in the result
  result <- list(
    emmeans = as.data.frame(summary(trends)),
    significant_interaction = has_interaction
  )

  # Only compute contrasts if there's a significant interaction
  if (has_interaction) {
    # Compute pairwise differences of slopes
    contrasts <- emmeans::contrast(
      trends,
      method = "pairwise",
      adjust = adjust,
      ...
    )

    # Convert to a clean data frame with standardized column names
    contrast_df <- as.data.frame(summary(contrasts))

    # Add significance indicators
    if ("p.value" %in% names(contrast_df)) {
      contrast_df$significance <- ""
      contrast_df$significance[contrast_df$p.value < alpha] <- "*"
      contrast_df$significance[contrast_df$p.value < alpha / 5] <- "**"
      contrast_df$significance[contrast_df$p.value < alpha / 20] <- "***"
    }

    result$contrasts <- contrast_df
  } else {
    result$message <- paste(
      "No significant interaction detected (alpha =",
      alpha,
      "). Pairwise slope comparisons not performed."
    )
  }

  # Set class and attributes
  class(result) <- c("cp_posthoc", class(result))
  attr(result, "adjust") <- adjust
  attr(result, "type") <- "slopes"

  return(result)
}

#' Run pairwise intercept comparisons for cross-price demand model
#'
#' This function performs pairwise comparisons of intercepts between groups in a
#' cross-price demand model, but only when a significant interaction is present.
#' The emmeans table showing estimated marginal means for intercepts is always returned.
#'
#' @param object A cp_model_lmer object from fit_cp_linear
#' @param alpha Significance level for testing (default: 0.05)
#' @param adjust Method for p-value adjustment; see emmeans::contrast (default: "tukey")
#' @param ... Additional arguments passed to emmeans
#' @return List containing the emmeans table and optionally pairwise comparisons if interaction is significant
#' @importFrom emmeans emmeans emtrends contrast
#' @export
cp_posthoc_intercepts <- function(object, alpha = 0.05, adjust = "tukey", ...) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Package 'emmeans' is required for pairwise comparisons")
  }

  if (!inherits(object, "cp_model_lmer")) {
    stop("Object must be a cp_model_lmer object")
  }

  # Check if there's a significant interaction
  has_interaction <- has_significant_interaction(object, alpha)

  # Create emmeans specifications, handling log-transformed models
  if (isTRUE(object$log10x)) {
    # For models with log10(x), set x=1 so that log10(x)=0
    emm <- emmeans::emmeans(object$model, specs = ~group, at = list(x = 1), ...)
  } else {
    # For linear models without transformation, set x=0 for intercept
    emm <- emmeans::emmeans(object$model, specs = ~group, at = list(x = 0), ...)
  }

  # Always include the emmeans table in the result
  result <- list(
    emmeans = as.data.frame(summary(emm)),
    significant_interaction = has_interaction
  )

  # Only compute contrasts if there's a significant interaction
  if (has_interaction) {
    # Compute pairwise differences
    contrasts <- emmeans::contrast(
      emm,
      method = "pairwise",
      adjust = adjust,
      ...
    )

    # Convert to a clean data frame with standardized column names
    contrast_df <- as.data.frame(summary(contrasts))

    # Add significance indicators
    if ("p.value" %in% names(contrast_df)) {
      contrast_df$significance <- ""
      contrast_df$significance[contrast_df$p.value < alpha] <- "*"
      contrast_df$significance[contrast_df$p.value < alpha / 5] <- "**"
      contrast_df$significance[contrast_df$p.value < alpha / 20] <- "***"
    }

    result$contrasts <- contrast_df
  } else {
    result$message <- paste(
      "No significant interaction detected (alpha =",
      alpha,
      "). Pairwise intercept comparisons not performed."
    )
  }

  # Set class and attributes
  class(result) <- c("cp_posthoc", class(result))
  attr(result, "adjust") <- adjust
  attr(result, "type") <- "intercepts"

  return(result)
}

#' Print method for cp_posthoc objects
#'
#' @param x A cp_posthoc object
#' @param ... Additional arguments passed to print
#' @export
print.cp_posthoc <- function(x, ...) {
  # Get type attribute or default
  type <- attr(x, "type")
  if (is.null(type)) type <- "Post-hoc"

  # Create title based on type
  title <- switch(
    type,
    "slopes" = "Slope Estimates and Comparisons",
    "intercepts" = "Intercept Estimates and Comparisons",
    "Estimates and Post-hoc Comparisons"
  )

  cat(title, "\n")
  cat(paste(rep("=", nchar(title)), collapse = ""), "\n\n")

  # Print the emmeans table
  cat("Estimated Marginal Means:\n")
  print(x$emmeans, row.names = FALSE)
  cat("\n")

  # Print interaction status
  cat(
    "Significant interaction:",
    ifelse(x$significant_interaction, "Yes", "No"),
    "\n\n"
  )

  # If contrasts are available, print them
  if (!is.null(x$contrasts)) {
    cat("Pairwise Comparisons:\n")
    # Convert to a simple data frame and remove S3 class to avoid recursive calls
    df <- as.data.frame(unclass(x$contrasts))
    # Print the data frame in a simple way without row names
    print(df, row.names = FALSE)

    # Print significance legend if needed
    if ("significance" %in% names(df)) {
      cat("\nSignificance codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '' 1\n")
    }
  } else if (!is.null(x$message)) {
    cat(x$message, "\n")
  }

  # Print adjustment method if available
  adjust <- attr(x, "adjust")
  if (!is.null(adjust)) {
    cat("P-value adjustment method:", adjust, "\n")
  }

  # Return invisibly
  invisible(x)
}
