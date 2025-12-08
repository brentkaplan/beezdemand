#' Fit cross-price demand with NLS (+ robust fallbacks)
#'
#' @description
#' Fits a **cross-price demand** curve of the form
#'
#' - **Exponentiated** (default): \eqn{y = q_{alone} \; 10^{\, I \exp(-\beta x)}}
#'
#' - **Exponential** (additive on \eqn{\log_{10} y}): \eqn{\log_{10}(y) = \log_{10}(q_{alone}) + I \exp(-\beta x)}
#'
#' - **Additive** (level on \eqn{y}): \eqn{y = q_{alone} + I \exp(-\beta x)}
#'
#' where \eqn{x} is the alternative product price (or “cross” price) and \eqn{y}
#' is consumption of the target good. Parameters:
#' \itemize{
#'   \item \eqn{q_{alone}}: consumption when the alternative is effectively absent
#'         (interpretable as a baseline or “alone” level).
#'   \item \eqn{I}: cross-price **interaction intensity**; sign and magnitude reflect
#'         substitution/complementarity.
#'   \item \eqn{\beta}: rate at which cross-price influence decays as \eqn{x} increases.
#' }
#'
#' The function first attempts a multi-start nonlinear least squares fit
#' (`nls.multstart`). If that fails—or if explicit `start_vals` are provided—it
#' falls back to `minpack.lm::nlsLM`. Optionally, it will make a final attempt
#' with `nlsr::wrapnlsr`. Returns either the fitted model or a structured object
#' with metadata for downstream methods.
#'
#' @param data A data frame with columns `x` (alternative price) and `y` (consumption).
#'             Additional columns are ignored. Input is validated internally.
#' @param equation Character string; model family, one of
#'   `c("exponentiated", "exponential", "additive")`. Default is `"exponentiated"`.
#' @param start_vals Optional **named list** of initial values for parameters
#'   `qalone`, `I`, and `beta`. If `NULL`, the function derives plausible ranges
#'   from the data and uses multi-start search.
#' @param iter Integer; number of random starts for `nls.multstart` (default `100`).
#' @param bounds A list with numeric vectors `lower` and `upper` giving parameter
#'   bounds in the order `c(qalone, I, beta)`. `NA` entries mean “no bound”.
#'   Defaults set wide, conservative ranges (see source).
#' @param fallback_to_nlsr Logical; if `TRUE` (default), try `nlsr::wrapnlsr` when
#'   both multi-start NLS and `nlsLM` fail.
#' @param return_all Logical; if `TRUE` (default), return a list containing the
#'   model and useful metadata. If `FALSE`, return the bare fitted model object.
#'
#' @details
#' **Start values & bounds.** When `start_vals` is missing, the function:
#' (1) estimates a reasonable range for `qalone` from the observed `y`,
#' (2) respects user-provided bounds, and (3) launches a multi-start grid in
#' `nls.multstart`. For the `"exponential"` form, `qalone` is interpreted on the
#' \eqn{\log_{10}} scale; for `"exponentiated"`/`"additive"`, it is on the `y` scale.
#'
#' **Fitting pipeline (short-circuiting):**
#' \enumerate{
#'   \item `nls.multstart::nls_multstart()` with random starts within `bounds`.
#'   \item If that fails (or if `start_vals` provided): `minpack.lm::nlsLM()` using
#'         `start_vals` (user or internally estimated).
#'   \item If that fails and `fallback_to_nlsr = TRUE`: `nlsr::wrapnlsr()`.
#' }
#'
#' The returned object has class `"cp_model_nls"` (when `return_all = TRUE`) with
#' components: `model`, `method` (the algorithm used), `equation`, `start_vals`,
#' `nlsLM_fit`, `nlsr_fit`, and the `data` used. This is convenient for custom
#' print/summary/plot methods.
#'
#' @return
#' If `return_all = TRUE` (default): a list of class `"cp_model_nls"`:
#' \itemize{
#'   \item `model`: the fitted object from the successful backend.
#'   \item `method`: one of `"nls_multstart"`, `"nlsLM"`, or `"wrapnlsr"`.
#'   \item `equation`: the model family used.
#'   \item `start_vals`: named list of starting values (final used).
#'   \item `nlsLM_fit`, `nlsr_fit`: fits from later stages (if attempted).
#'   \item `data`: the 2-column data frame actually fit.
#' }
#' If `return_all = FALSE`: the fitted model object from the successful backend.
#'
#' @section Convergence & warnings:
#' - Check convergence codes and residual diagnostics from the underlying fit.
#' - Poor scaling or extreme `y` dispersion can make `qalone`/`I`/`beta`
#'   weakly identified. Consider transforming units or tightening `bounds`.
#' - For `"exponential"`, remember `qalone` is on the \eqn{\log_{10} y} scale.
#'
#' @seealso
#' \code{\link{check_unsystematic_cp}} for pre-fit data screening,
#' \code{\link{validate_cp_data}} for input validation.
#' @importFrom nls.multstart nls_multstart
#' @importFrom nlsr wrapnlsr
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @examples
#' ## --- Example: Real data (E-Cigarettes, id = 1) ---
#' dat <- structure(list(
#'   id = c(1, 1, 1, 1, 1, 1),
#'   x = c(2, 4, 8, 16, 32, 64),
#'   y = c(3, 5, 5, 16, 17, 13),
#'   target = c("alt", "alt", "alt", "alt", "alt", "alt"),
#'   group = c("E-Cigarettes", "E-Cigarettes", "E-Cigarettes",
#'             "E-Cigarettes", "E-Cigarettes", "E-Cigarettes")
#' ), row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"))
#'
#' ## Fit the default (exponentiated) cross-price form
#' fit_ecig <- fit_cp_nls(dat, equation = "exponentiated", return_all = TRUE)
#' summary(fit_ecig)         # model summary
#' fit_ecig$method           # backend actually used (e.g., "nls_multstart")
#' coef(fit_ecig$model)      # parameter estimates: qalone, I, beta
#' @export
fit_cp_nls <- function(
  data,
  equation = c("exponentiated", "exponential", "additive"),
  start_vals = NULL,
  iter = 100,
  bounds = list(
    lower = c(qalone = 0.1, I = -2, beta = 0.00001),
    upper = c(qalone = NA, I = 2, beta = 5)
  ),
  fallback_to_nlsr = TRUE,
  return_all = TRUE
) {
  equation <- match.arg(equation)

  # Validate: Only x and y are required here.
  data <- validate_cp_data(
    data,
    required_cols = c("x", "y"),
    filter_target = FALSE
  )
  data <- data[, c("x", "y")]

  # Compute data ranges for start value estimation
  y_range <- range(data$y, na.rm = TRUE)
  x_range <- range(data$x, na.rm = TRUE)
  x_width <- diff(x_range)

  used_method <- NULL

  # Define the model formula
  formula_nls <- switch(
    equation,
    exponentiated = y ~ qalone * 10^(I * exp(-beta * x)),
    exponential = y ~ log10(qalone) + I * exp(-beta * x),
    additive = y ~ qalone + I * exp(-beta * x)
  )

  # If no explicit start values, define parameter ranges.
  if (is.null(start_vals)) {
    if (equation == "exponential") {
      qalone_lower <- max(0.1, 10^(y_range[1] - 1))
      qalone_upper <- max(10^(y_range[2] + 1), 100)
    } else {
      qalone_lower <- max(0.1, y_range[1] * 0.5)
      qalone_upper <- max(y_range[2] * 2, 100)
    }

    start_lower <- c(
      qalone = ifelse(
        is.na(bounds$lower["qalone"]),
        qalone_lower,
        bounds$lower["qalone"]
      ),
      I = bounds$lower["I"],
      beta = bounds$lower["beta"]
    )

    start_upper <- c(
      qalone = ifelse(
        is.na(bounds$upper["qalone"]),
        qalone_upper,
        bounds$upper["qalone"]
      ),
      I = bounds$upper["I"],
      beta = bounds$upper["beta"]
    )

    # Try nls.multstart first.
    nls_multi_fit <- tryCatch(
      {
        nls.multstart::nls_multstart(
          formula = formula_nls,
          data = data,
          iter = iter,
          start_lower = start_lower,
          start_upper = start_upper,
          supp_errors = "Y",
          lower = bounds$lower,
          upper = bounds$upper
        )
      },
      error = function(e) e
    )

    if (!inherits(nls_multi_fit, "error")) {
      used_method <- "nls_multstart"
      if (return_all) {
        result <- list(
          model = nls_multi_fit,
          method = used_method,
          equation = equation,
          start_vals = as.list(coef(nls_multi_fit)),
          nlsLM_fit = NULL,
          nlsr_fit = NULL,
          data = data
        )
        class(result) <- "cp_model_nls"
        return(result)
      } else {
        return(nls_multi_fit)
      }
    } else {
      message("nls.multstart failed, falling back to nlsLM...")
    }
  }

  # If explicit start_vals provided or nls.multstart failed.
  if (
    !is.null(start_vals) ||
      (exists("nls_multi_fit") && inherits(nls_multi_fit, "error"))
  ) {
    if (is.null(start_vals)) {
      warning("nls.multstart failed; using estimated start values with nlsLM.")
      start_vals <- list(
        qalone = if (equation == "exponential") 10^mean(data$y) else
          mean(data$y),
        I = 0,
        beta = 1 / x_width
      )
    }

    nlsLM_fit <- tryCatch(
      minpack.lm::nlsLM(
        formula = formula_nls,
        data = data,
        start = start_vals,
        lower = bounds$lower,
        upper = bounds$upper,
        control = minpack.lm::nls.lm.control(maxiter = 200)
      ),
      error = function(e) e
    )

    if (!inherits(nlsLM_fit, "error")) {
      used_method <- "nlsLM"
      if (return_all) {
        result <- list(
          model = nlsLM_fit,
          method = used_method,
          equation = equation,
          start_vals = start_vals,
          nlsLM_fit = nlsLM_fit,
          nlsr_fit = NULL,
          data = data
        )
        class(result) <- "cp_model_nls"
        return(result)
      } else {
        return(nlsLM_fit)
      }
    }

    # Final fallback: try using nlsr (if enabled)
    if (fallback_to_nlsr) {
      message("nlsLM failed; attempting to use wrapnlsr as a final fallback...")
      formula_nlsr <- switch(
        equation,
        exponentiated = as.formula("y ~ qalone * 10^(I * exp(-beta * x))"),
        exponential = as.formula("y ~ log10(qalone) + I * exp(-beta * x)"),
        additive = as.formula("y ~ qalone + I * exp(-beta * x)")
      )

      nlsr_fit <- tryCatch(
        nlsr::wrapnlsr(
          formula = formula_nlsr,
          data = data,
          start = start_vals,
          lower = bounds$lower,
          upper = bounds$upper
        ),
        error = function(e) e
      )

      if (!inherits(nlsr_fit, "error")) {
        used_method <- "wrapnlsr"
        result <- list(
          model = nlsr_fit,
          method = used_method,
          equation = equation,
          start_vals = start_vals,
          nlsLM_fit = nlsLM_fit,
          nlsr_fit = nlsr_fit,
          data = data
        )
        class(result) <- "cp_model_nls"
        return(result)
      }
    }
  }

  stop(
    "Model fitting failed with all methods: nls.multstart, nlsLM, and wrapnlsr."
  )
}

#-------------------------------------------------------------------------------
# Linear Cross-Price Demand Model Fitting
#' Fit a Linear Cross-Price Demand Model
#'
#' @param data A data frame containing columns: x and y, and optionally target, id, and group.
#' @param type The type of model: "fixed" for standard linear or "mixed" for mixed effects.
#' @param formula Optional formula override. If NULL, a formula will be constructed based on other parameters.
#' @param log10x Logical; if TRUE and formula is NULL, uses log10(x) instead of x in the formula. Default is FALSE.
#' @param group_effects Logical or character; if TRUE, includes group as a factor with interactions.
#'        Can also be "intercept" for group intercepts only or "interaction" for full interactions. Default is FALSE.
#' @param random_slope Logical; for mixed models, if TRUE, includes random slopes for x. Default is FALSE.
#' @param return_all Logical; if TRUE, returns additional model metadata.
#' @param ... Additional arguments passed to underlying modeling functions.
#' @return Fitted linear model.
#' @importFrom lme4 lmer
#' @export
fit_cp_linear <- function(
  data,
  type = c("fixed", "mixed"),
  formula = NULL,
  log10x = FALSE,
  group_effects = FALSE,
  random_slope = FALSE,
  return_all = TRUE,
  ...
) {
  type <- match.arg(type)

  # Determine required columns based on parameters
  required_cols <- c("x", "y")
  if (
    isTRUE(group_effects) || group_effects %in% c("intercept", "interaction")
  ) {
    required_cols <- c(required_cols, "group")
  }

  # Handle predictor variable transformation
  x_term <- if (log10x) "log10(x)" else "x"

  if (type == "fixed") {
    # Validate and filter data for fixed effects model
    data <- validate_cp_data(
      data,
      required_cols = required_cols,
      filter_target = TRUE
    )

    if (log10x & any(data$x <= 0)) {
      data <- data[data$x > 0, ]
      warning("Filtered out non-positive x values for log10 transformation")
    }

    # Create formula based on parameters if not provided
    if (is.null(formula)) {
      if (isTRUE(group_effects) || group_effects == "interaction") {
        # Full interaction model with different intercepts and slopes per group
        formula <- as.formula(paste("y ~", x_term, "* group"))
      } else if (group_effects == "intercept") {
        # Only different intercepts per group
        formula <- as.formula(paste("y ~", x_term, "+ group"))
      } else {
        # Default simple model
        formula <- as.formula(paste("y ~", x_term))
      }
    }

    # Ensure group is a factor
    if (("group" %in% names(data)) && !is.factor(data$group)) {
      data$group <- as.factor(data$group)
    }

    # Fit model
    model <- lm(formula, data = data, ...)

    if (!return_all) {
      return(model)
    } else {
      result <- list(
        model = model,
        method = "lm",
        equation = if (log10x) "linear_log10x" else "linear",
        formula = formula,
        data = data,
        log10x = log10x,
        group_effects = group_effects
      )
      class(result) <- "cp_model_lm"
      return(result)
    }
  } else if (type == "mixed") {
    # Add id to required columns for mixed models
    required_cols <- c(required_cols, "id")

    # Validate data for mixed effects models
    data <- validate_cp_data(
      data,
      required_cols = required_cols,
      filter_target = TRUE,
      require_id = TRUE
    )

    if (log10x & any(data$x <= 0)) {
      data <- data[data$x > 0, ]
      warning("Filtered out non-positive x values for log10 transformation")
    }

    # Create formula based on parameters if not provided
    if (is.null(formula)) {
      # Fixed effects part
      fixed_part <- if (
        isTRUE(group_effects) || group_effects == "interaction"
      ) {
        paste(x_term, "* group")
      } else if (group_effects == "intercept") {
        paste(x_term, "+ group")
      } else {
        x_term
      }

      # Random effects part
      random_part <- if (random_slope) {
        "(1 + x | id)"
      } else {
        "(1 | id)"
      }

      # Combine into full formula
      formula <- as.formula(paste("y ~", fixed_part, "+", random_part))
    }

    # Ensure group is a factor
    if (("group" %in% names(data)) && !is.factor(data$group)) {
      data$group <- as.factor(data$group)
    }

    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' is required for mixed-effects models")
    }

    # Fit model
    model <- lme4::lmer(formula, data = data, ...)

    if (!return_all) {
      return(model)
    } else {
      result <- list(
        model = model,
        method = "lmer",
        equation = if (log10x) "linear_mixed_log10x" else "linear_mixed",
        formula = formula,
        data = data,
        log10x = log10x,
        group_effects = group_effects,
        random_slope = random_slope
      )
      class(result) <- "cp_model_lmer"
      return(result)
    }
  }
}


# S3 default method (alias for fixed-effects model)
#' @rdname fit_cp_linear
#' @export
fit_cp_linear.default <- function(
  data,
  formula = NULL,
  log10x = FALSE,
  return_all = FALSE,
  ...
) {
  fit_cp_linear(
    data,
    type = "fixed",
    formula = formula,
    log10x = log10x,
    return_all = return_all,
    ...
  )
}


# S3 method for mixed-effects models
#' @rdname fit_cp_linear
#' @export
fit_cp_linear.mixed <- function(
  data,
  formula = NULL,
  log10x = FALSE,
  return_all = FALSE,
  ...
) {
  fit_cp_linear(
    data,
    type = "mixed",
    formula = formula,
    log10x = log10x,
    return_all = return_all,
    ...
  )
}
