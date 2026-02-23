#' Fit cross-price demand with NLS (+ robust fallbacks)
#'
#' @description
#' Fits a **cross-price demand** curve using log10-parameterization for numerical
#' stability. The optimizer estimates parameters on the log10 scale where applicable,
#' ensuring positive constraints are naturally satisfied.
#'
#' **Equation forms:**
#'
#' - **Exponentiated** (default):
#'   \deqn{y = Q_{alone} \cdot 10^{I \cdot \exp(-\beta \cdot x)}}
#'
#' - **Exponential** (fits on log10 response scale):
#'   \deqn{\log_{10}(y) = \log_{10}(Q_{alone}) + I \cdot \exp(-\beta \cdot x)}
#'
#' - **Additive** (level on \eqn{y}):
#'   \deqn{y = Q_{alone} + I \cdot \exp(-\beta \cdot x)}
#'
#' where \eqn{x} is the alternative product price (or "cross" price) and \eqn{y}
#' is consumption of the target good.
#'
#' **Optimizer parameters (log10 parameterization):**
#' \itemize{
#'   \item \code{log10_qalone}: \eqn{\log_{10}(Q_{alone})} - baseline consumption
#'         when the alternative is effectively absent.
#'   \item \code{I}: cross-price **interaction intensity**; sign and magnitude reflect
#'         substitution/complementarity. Unconstrained (can be negative for substitutes).
#'   \item \code{log10_beta}: \eqn{\log_{10}(\beta)} - rate at which cross-price
#'         influence decays as \eqn{x} increases.
#' }
#'
#' Natural-scale values are recovered as \eqn{Q_{alone} = 10^{log10\_qalone}} and
#' \eqn{\beta = 10^{log10\_beta}}.
#'
#' The function first attempts a multi-start nonlinear least squares fit
#' (`nls.multstart`). If that fails—or if explicit `start_values` are provided—it
#' falls back to `minpack.lm::nlsLM`. Optionally, it will make a final attempt
#' with `nlsr::wrapnlsr`. Returns either the fitted model or a structured object
#' with metadata for downstream methods.
#'
#' @param data A data frame with columns for price and consumption.
#'             Additional columns are ignored. Input is validated internally.
#' @param equation Character string; model family, one of
#'   `c("exponentiated", "exponential", "additive")`. Default is `"exponentiated"`.
#' @param x_var Character string; name of the price column in `data`. Default is
#'   `"x"`. The column is renamed to `"x"` internally; see `validate_cp_data()`.
#' @param y_var Character string; name of the consumption column in `data`. Default
#'   is `"y"`. The column is renamed to `"y"` internally.
#' @param start_values Optional **named list** of initial values for parameters
#'   `log10_qalone`, `I`, and `log10_beta`. If `NULL`, the function derives
#'   plausible ranges from the data and uses multi-start search.
#' @param start_vals `r lifecycle::badge("deprecated")` Use `start_values` instead.
#' @param iter Integer; number of random starts for `nls.multstart` (default `100`).
#' @param bounds Deprecated. Log10-parameterized parameters are naturally unbounded.
#'   This argument is ignored but retained for backwards compatibility.
#' @param fallback_to_nlsr Logical; if `TRUE` (default), try `nlsr::wrapnlsr` when
#'   both multi-start NLS and `nlsLM` fail.
#' @param return_all Logical; if `TRUE` (default), return a list containing the
#'   model and useful metadata. If `FALSE`, return the bare fitted model object.
#'
#' @details
#' **Start values.** When `start_values` is missing, the function:
#' (1) estimates a reasonable range for `log10_qalone` from the observed `y`,
#' (2) estimates `log10_beta` from the price range, and (3) launches a multi-start
#' grid in `nls.multstart`.
#'
#' **Zero handling for exponential equation.** Since the exponential equation fits
#' on the \eqn{\log_{10}(y)} scale, observations with \eqn{y \le 0} are automatically
#' removed with a warning. Use the exponentiated or additive forms if you need to
#' retain zero consumption values.
#'
#' **Fitting pipeline (short-circuiting):**
#' \enumerate{
#'   \item `nls.multstart::nls_multstart()` with random starts.
#'   \item If that fails (or if `start_values` provided): `minpack.lm::nlsLM()` using
#'         `start_values` (user or internally estimated).
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
#'   \item `start_vals`: named list of starting values (final used; kept for backward compatibility).
#'   \item `nlsLM_fit`, `nlsr_fit`: fits from later stages (if attempted).
#'   \item `data`: the 2-column data frame actually fit.
#' }
#' If `return_all = FALSE`: the fitted model object from the successful backend.
#'
#' @section Convergence & warnings:
#' - Check convergence codes and residual diagnostics from the underlying fit.
#' - Poor scaling or extreme `y` dispersion can make parameters weakly identified.
#' - For `"exponential"`, the model fits on the \eqn{\log_{10}(y)} scale internally.
#'
#' @seealso
#' \code{\link{check_unsystematic_cp}} for pre-fit data screening,
#' \code{\link{validate_cp_data}} for input validation.
#' @importFrom lifecycle deprecated is_present deprecate_warn
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
#' coef(fit_ecig$model)      # parameter estimates: log10_qalone, I, log10_beta
#' @export
fit_cp_nls <- function(
  data,
  equation = c("exponentiated", "exponential", "additive"),
  x_var = "x",
  y_var = "y",
  start_values = NULL,
  start_vals = lifecycle::deprecated(),
  iter = 100,
  bounds = NULL,
  fallback_to_nlsr = TRUE,
  return_all = TRUE
) {
  equation <- match.arg(equation)

  # Handle deprecated start_vals argument
  if (lifecycle::is_present(start_vals)) {
    lifecycle::deprecate_warn(
      "0.3.0",
      "fit_cp_nls(start_vals=)",
      "fit_cp_nls(start_values=)"
    )
    start_values <- start_vals
  }

  # Validate: Only x and y are required here.
  data <- validate_cp_data(
    data,
    x_var = x_var,
    y_var = y_var,
    required_cols = c("x", "y"),
    filter_target = FALSE
  )
  data <- data[, c("x", "y")]

  # Zero handling for exponential equation (log10(y) transform requires y > 0)
  if (equation == "exponential") {
    n_zero_or_neg <- sum(data$y <= 0, na.rm = TRUE)
    if (n_zero_or_neg > 0) {
      warning(
        sprintf(
          "Removing %d observation(s) with y <= 0 for 'exponential' equation (log10 transform required).",
          n_zero_or_neg
        )
      )
      data <- data[data$y > 0, , drop = FALSE]
    }
    if (nrow(data) < 3) {
      stop("Insufficient data remaining after removing y <= 0 observations.")
    }
  }

  # Compute data ranges for start value estimation
  y_positive <- data$y[data$y > 0]
  if (length(y_positive) == 0) {
    y_positive <- 1
  }
  y_range <- range(y_positive, na.rm = TRUE)
  x_range <- range(data$x, na.rm = TRUE)
  x_width <- diff(x_range)
  if (x_width <= 0) x_width <- 1

  used_method <- NULL

  # Define the model formula with log10 parameterization
  # Per EQUATIONS_CONTRACT.md:
  # - Exponential: (log(y)/log(10)) ~ log10_qalone + I * exp(-(10^log10_beta) * x)
  # - Exponentiated: y ~ (10^log10_qalone) * 10^(I * exp(-(10^log10_beta) * x))
  # - Additive: y ~ (10^log10_qalone) + I * exp(-(10^log10_beta) * x)
  formula_nls <- switch(
    equation,
    exponentiated = y ~ (10^log10_qalone) * 10^(I * exp(-(10^log10_beta) * x)),
    exponential = (log(y) / log(10)) ~ log10_qalone + I * exp(-(10^log10_beta) * x),
    additive = y ~ (10^log10_qalone) + I * exp(-(10^log10_beta) * x)
  )

  # Define start value ranges for log10-parameterized parameters
  # log10_qalone: based on log10 of median positive y
  log10_qalone_mid <- log10(median(y_positive, na.rm = TRUE))
  log10_qalone_lower <- log10_qalone_mid - 2
  log10_qalone_upper <- log10_qalone_mid + 2

  # log10_beta: based on inverse of price range (typical decay scale)
  log10_beta_mid <- log10(1 / x_width)
  log10_beta_lower <- log10_beta_mid - 2
  log10_beta_upper <- log10_beta_mid + 2

  # I: unconstrained, centered around 0
  I_lower <- -3
  I_upper <- 3

  start_lower <- c(
    log10_qalone = log10_qalone_lower,
    I = I_lower,
    log10_beta = log10_beta_lower
  )

  start_upper <- c(
    log10_qalone = log10_qalone_upper,
    I = I_upper,
    log10_beta = log10_beta_upper
  )

  # If no explicit start values, use multi-start search
  if (is.null(start_values)) {
    # Try nls.multstart first (no bounds for log-transformed parameters)
    nls_multi_fit <- tryCatch(
      {
        nls.multstart::nls_multstart(
          formula = formula_nls,
          data = data,
          iter = iter,
          start_lower = start_lower,
          start_upper = start_upper,
          supp_errors = "Y"
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

  # If explicit start_values provided or nls.multstart failed.
  if (
    !is.null(start_values) ||
      (exists("nls_multi_fit") && inherits(nls_multi_fit, "error"))
  ) {
    if (is.null(start_values)) {
      warning("nls.multstart failed; using estimated start values with nlsLM.")
      start_values <- list(
        log10_qalone = log10_qalone_mid,
        I = 0,
        log10_beta = log10_beta_mid
      )
    }

    nlsLM_fit <- tryCatch(
      minpack.lm::nlsLM(
        formula = formula_nls,
        data = data,
        start = start_values,
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
          start_vals = start_values,
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
        exponentiated = as.formula(
          "y ~ (10^log10_qalone) * 10^(I * exp(-(10^log10_beta) * x))"
        ),
        exponential = as.formula(
          "(log(y) / log(10)) ~ log10_qalone + I * exp(-(10^log10_beta) * x)"
        ),
        additive = as.formula(
          "y ~ (10^log10_qalone) + I * exp(-(10^log10_beta) * x)"
        )
      )

      nlsr_fit <- tryCatch(
        nlsr::wrapnlsr(
          formula = formula_nlsr,
          data = data,
          start = start_values
        ),
        error = function(e) e
      )

      if (!inherits(nlsr_fit, "error")) {
        used_method <- "wrapnlsr"
        result <- list(
          model = nlsr_fit,
          method = used_method,
          equation = equation,
          start_vals = start_values,
          nlsLM_fit = nlsLM_fit,
          nlsr_fit = nlsr_fit,
          data = data
        )
        class(result) <- "cp_model_nls"
        return(result)
      }
    }
  }

  fitting_error(
    "Model fitting failed with all methods: nls.multstart, nlsLM, and wrapnlsr.",
    model_type = "nls"
  )
}

#-------------------------------------------------------------------------------
# Linear Cross-Price Demand Model Fitting
#' Fit a Linear Cross-Price Demand Model
#'
#' @param data A data frame containing columns for price, consumption, and
#'   optionally target indicator, subject identifier, and group.
#' @param type The type of model: `"fixed"` for standard linear or `"mixed"` for
#'   mixed effects.
#' @param x_var Character string; name of the price column. Default is `"x"`.
#'   Renamed to `"x"` internally.
#' @param y_var Character string; name of the consumption column. Default is
#'   `"y"`. Renamed to `"y"` internally.
#' @param id_var Character string; name of the subject identifier column.
#'   Default is `"id"`. Required for mixed models; renamed to `"id"` internally.
#' @param group_var Character string; name of the group column. Default is
#'   `"group"`. Renamed to `"group"` internally when present.
#' @param target_var Character string; name of the target indicator column.
#'   Default is `"target"`. Renamed to `"target"` internally when present.
#' @param filter_target Logical; if TRUE (default), filters to rows where the
#'   target column equals `target_level`.
#' @param target_level Character string; value of the target column to retain
#'   when `filter_target = TRUE`. Default is `"alt"`.
#' @param formula Optional formula override. If NULL, a formula will be
#'   constructed based on other parameters. If non-NULL and any `*_var` argument
#'   differs from its default, an error is thrown because the formula references
#'   canonical column names that no longer exist before renaming; rename columns
#'   before calling, or omit the `formula` argument.
#' @param log10x Logical; if TRUE and formula is NULL, uses `log10(x)` instead
#'   of `x` in the formula. Default is FALSE.
#' @param group_effects Logical or character; if TRUE, includes group as a factor
#'   with interactions. Can also be `"intercept"` for group intercepts only or
#'   `"interaction"` for full interactions. Default is FALSE.
#' @param random_slope Logical; for mixed models, if TRUE, includes random slopes
#'   for x. Default is FALSE.
#' @param return_all Logical; if TRUE, returns additional model metadata.
#' @param ... Additional arguments passed to underlying modeling functions.
#' @return Fitted linear model.
#' @importFrom lme4 lmer
#' @export
fit_cp_linear <- function(
  data,
  type = c("fixed", "mixed"),
  x_var = "x",
  y_var = "y",
  id_var = "id",
  group_var = "group",
  target_var = "target",
  filter_target = TRUE,
  target_level = "alt",
  formula = NULL,
  log10x = FALSE,
  group_effects = FALSE,
  random_slope = FALSE,
  return_all = TRUE,
  ...
) {
  type <- match.arg(type)

  # Formula conflict check: custom *_var with non-NULL formula is ambiguous
  non_default_vars <- !identical(x_var, "x") || !identical(y_var, "y") ||
    !identical(id_var, "id") || !identical(group_var, "group") ||
    !identical(target_var, "target")
  if (!is.null(formula) && non_default_vars) {
    stop(
      "Custom formulas must use canonical column names (x, y, id, group, target).\n",
      "When supplying a formula, rename your columns before calling fit_cp_linear() ",
      "or omit the formula argument and let fit_cp_linear() build it automatically."
    )
  }

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
      x_var = x_var,
      y_var = y_var,
      id_var = id_var,
      group_var = group_var,
      target_var = target_var,
      required_cols = required_cols,
      filter_target = filter_target,
      target_level = target_level
    )

    if (log10x && any(data$x <= 0)) {
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
      x_var = x_var,
      y_var = y_var,
      id_var = id_var,
      group_var = group_var,
      target_var = target_var,
      required_cols = required_cols,
      filter_target = filter_target,
      target_level = target_level,
      require_id = TRUE
    )

    if (log10x && any(data$x <= 0)) {
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
      missing_package_error("lme4", reason = "for mixed-effects models")
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
