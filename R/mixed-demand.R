#' Fit Nonlinear Mixed-Effects Demand Model
#'
#' Fits a nonlinear mixed-effects model for behavioral economic demand data.
#' The function allows Q0 and alpha parameters to vary by specified factors
#' and supports different demand equation forms.
#'
#' @param data A data frame.
#' @param y_var Character string, the name of the dependent variable column.
#'   For `equation_form = "zben"`, this should be log-transformed consumption (e.g., "y_ll4").
#'   For `equation_form = "simplified"` or `"exponentiated"`, this should be raw, untransformed consumption (e.g., "y").
#' @param x_var Character string, the name of the independent variable column (e.g., "x", price).
#' @param id_var Character string, the name of the subject/group identifier column for random effects.
#' @param factors A character vector of factor names (up to two) by which Q0 and alpha
#'   are expected to vary (e.g., `c("dose", "treatment")`).
#' @param factor_interaction Logical. If `TRUE` and two factors are provided,
#'   their interaction term is included in the fixed effects for Q0 and alpha.
#'   Defaults to `FALSE` (additive effects).
#' @param equation_form Character string specifying the demand equation form.
#'   Options are:
#'   \itemize{
#'     \item `"zben"` (default): Assumes `y_var` is log-transformed.
#'           Equation: `y_var ~ Q0 * exp(-(10^alpha / Q0) * (10^Q0) * x_var)`.
#'           Model parameter `Q0` represents `log10(True Max Consumption)`.
#'           Model parameter `alpha` represents `log10(True Alpha Sensitivity)`.
#'     \item `"simplified"`: Assumes `y_var` is raw (untransformed) consumption.
#'           Equation: `y_var ~ (10^Q0) * exp(-(10^alpha) * (10^Q0) * x_var)`.
#'           Model parameter `Q0` represents `log10(True Max Consumption)`.
#'           Model parameter `alpha` represents `log10(True Alpha Sensitivity)`.
#'     \item `"exponentiated"`: Koffarnus et al. (2015) exponentiated equation. Assumes `y_var`
#'           is raw (untransformed) consumption. Requires the `k` parameter.
#'           Equation (log10 param_space): `y_var ~ (10^Q0) * 10^(k * (exp(-(10^alpha) * (10^Q0) * x_var) - 1))`.
#'           Equation (natural param_space): `y_var ~ Q0 * 10^(k * (exp(-alpha * Q0 * x_var) - 1))`.
#'   }
#' @param param_space Character. Parameterization used for fitting core demand
#'   parameters. One of:
#'   - `"log10"`: treat `Q0` and `alpha` as `log10(Q0)` and `log10(alpha)` (default)
#'   - `"natural"`: treat `Q0` and `alpha` as natural-scale parameters
#'
#'   Notes:
#'   - For `equation_form = "zben"`, only `"log10"` is currently supported.
#'   - For `equation_form = "simplified"` or `"exponentiated"`, both `"log10"` and `"natural"` are supported.
#' @param k Numeric. Range parameter (in log10 units) used with `equation_form = "exponentiated"`.
#'   If `NULL` (default), k is calculated from the data range: `log10(max(y)) - log10(min(y)) + 0.5`.
#'   Ignored for other equation forms.
#' @param custom_model_formula An optional custom nonlinear model formula (nlme format).
#'   If provided, this overrides `equation_form`. The user is responsible for ensuring
#'   the `y_var` scale matches the formula and that starting values are appropriate.
#'   The formula should use parameters named `Q0` and `alpha`.
#' @param fixed_rhs Optional one-sided formula or character string specifying the
#'   right-hand side (RHS) for the fixed-effects linear models of `Q0` and `alpha`.
#'   When provided, this RHS is used for both parameters and overrides
#'   `factors`, `factor_interaction`, and `continuous_covariates` for building the
#'   fixed-effects design matrix. Example: `"~ 1 + drug * dose + session"`.
#' @param continuous_covariates Optional character vector of continuous (numeric)
#'   predictor names to be included additively in the fixed-effects RHS when
#'   `fixed_rhs` is `NULL`. These variables are not coerced to factors and are
#'   stored for downstream functions (e.g., plotting) to condition on.
#' @param start_value_method Character, method to generate starting values if `start_values` is NULL.
#'   Options: "heuristic" (default, uses data-driven heuristics) or
#'   "pooled_nls" (fits a simpler pooled NLS model first; falls back to heuristic if NLS fails).
#' @param random_effects A formula or a list of formulas for the random effects structure.
#'   Default `nlme::pdDiag(Q0 + alpha ~ 1)`.
#' @param covariance_structure Character, covariance structure for random effects.
#'   Options: `"pdDiag"` (default) or `"pdSymm"`
#' @param start_values Optional named list of starting values for fixed effects.
#'   If `NULL`, defaults are estimated based on `equation_form` and `y_var` scale.
#' @param collapse_levels Optional named list specifying factor level collapsing
#'   separately for Q0 and alpha parameters. Structure:
#'   ```
#'   list(
#'     Q0 = list(factor_name = list(new_level = c(old_levels), ...)),
#'     alpha = list(factor_name = list(new_level = c(old_levels), ...))
#'   )
#'   ```
#'   Either `Q0` or `alpha` (or both) can be omitted to use original factor levels
#'   for that parameter. This allows different collapsing schemes for each parameter.
#'   Ignored if `fixed_rhs` is provided.
#' @param nlme_control Control parameters for `nlme::nlme()`.
#' @param method Fitting method for `nlme::nlme()` ("ML" or "REML"). Default "ML".
#' @param ... Additional arguments passed to `nlme::nlme()`.
#'
#' @examples
#' \donttest{
#' # Basic mixed-effects demand fit with apt data
#' # Transform consumption using LL4 for the zben equation
#' apt_ll4 <- apt |> dplyr::mutate(y_ll4 = ll4(y))
#'
#' fit <- fit_demand_mixed(
#'   data = apt_ll4,
#'   y_var = "y_ll4",
#'   x_var = "x",
#'   id_var = "id",
#'   equation_form = "zben"
#' )
#' print(fit)
#' summary(fit)
#' }
#'
#' @return An object of class `beezdemand_nlme`.
#'
#' @importFrom nlme nlme pdDiag nlmeControl fixef
#' @importFrom stats as.formula median update model.matrix quantile coef
#' @importFrom rlang .data sym
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @export
fit_demand_mixed <- function(
  data,
  y_var,
  x_var,
  id_var,
  factors = NULL,
  factor_interaction = FALSE,
  equation_form = c("zben", "simplified", "exponentiated"),
  param_space = c("log10", "natural"),
  k = NULL,
  custom_model_formula = NULL,
  fixed_rhs = NULL,
  continuous_covariates = NULL,
  start_value_method = c("heuristic", "pooled_nls"),
  # random_effects = NULL,
  random_effects = Q0 + alpha ~ 1, # Changed default to a formula
  covariance_structure = c("pdDiag", "pdSymm"),
  start_values = NULL,
  collapse_levels = NULL,
  nlme_control = list(
    msMaxIter = 200,
    niterEM = 100,
    maxIter = 200,
    pnlsTol = 1e-3,
    tolerance = 1e-6,
    apVar = TRUE,
    minScale = 1e-9,
    opt = "nlminb",
    msVerbose = FALSE # Default msVerbose back to FALSE
  ),
  method = "ML",
  ...
) {
  call <- match.call()
  equation_form <- match.arg(equation_form)
  param_space <- match.arg(param_space)
  start_value_method <- match.arg(start_value_method)
  covariance_structure <- match.arg(covariance_structure)

  data <- validate_demand_data(
    data = data,
    y_var = y_var,
    x_var = x_var,
    id_var = id_var,
    factors = factors
  )

  # Helper to ensure all specified factor columns are factors in the data
  ensure_factors <- function(factor_names, dat) {
    if (!is.null(factor_names)) {
      for (f in factor_names) {
        if (f %in% names(dat) && !is.factor(dat[[f]])) {
          dat[[f]] <- factor(dat[[f]])
        }
      }
    }
    dat
  }

  # Enforce the two-factor limit and interaction rule only when using the legacy
  # path (no fixed_rhs). If fixed_rhs is provided, user has full control.
  if (is.null(fixed_rhs)) {
    if (!is.null(factors) && length(factors) > 2) {
      validation_error(
        "Up to two factors can be specified when 'fixed_rhs' is NULL.",
        arg = "factors"
      )
    }
    if (length(factors) < 2 && isTRUE(factor_interaction)) {
      warning(
        "factor_interaction is TRUE but less than two factors. Interaction ignored."
      )
      factor_interaction <- FALSE
    }
  }

  # Initialize factor tracking for separate Q0/alpha collapsing
  factors_Q0 <- factors
  factors_alpha <- factors
  collapse_info <- list(Q0 = list(), alpha = list())

  # Process collapse_levels if provided and fixed_rhs is NULL
  if (!is.null(collapse_levels) && is.null(fixed_rhs)) {
    validate_collapse_levels(collapse_levels)

    # Process Q0 collapsing (skip if empty list)
    if (!is.null(collapse_levels$Q0) && length(collapse_levels$Q0) > 0) {
      result_Q0 <- collapse_factor_levels(
        data = data,
        collapse_spec = collapse_levels$Q0,
        factors = factors,
        suffix = "Q0"
      )
      data <- result_Q0$data
      factors_Q0 <- result_Q0$new_factor_names
      collapse_info$Q0 <- result_Q0$info
    }

    # Process alpha collapsing (skip if empty list)
    if (!is.null(collapse_levels$alpha) && length(collapse_levels$alpha) > 0) {
      result_alpha <- collapse_factor_levels(
        data = data,
        collapse_spec = collapse_levels$alpha,
        factors = factors,
        suffix = "alpha"
      )
      data <- result_alpha$data
      factors_alpha <- result_alpha$new_factor_names
      collapse_info$alpha <- result_alpha$info
    }
  } else if (!is.null(collapse_levels) && !is.null(fixed_rhs)) {
    message(
      "Note: 'collapse_levels' is ignored when 'fixed_rhs' is provided."
    )
  }

  # Ensure all factor columns (including collapsed ones) are proper factors
  # This is needed before build_fixed_rhs to correctly filter single-level factors
  data <- ensure_factors(factors_Q0, data)
  data <- ensure_factors(factors_alpha, data)

  # Determine the effective fixed-effects RHS strings for Q0 and alpha
  # These may differ if collapse_levels specifies different collapsing
  if (!is.null(fixed_rhs)) {
    # Use user-provided RHS directly (can be character or formula)
    if (inherits(fixed_rhs, "formula")) {
      fixed_effects_formula_str_Q0 <- deparse(fixed_rhs)
      fixed_effects_formula_str_alpha <- deparse(fixed_rhs)
    } else if (is.character(fixed_rhs)) {
      fixed_effects_formula_str_Q0 <- fixed_rhs
      fixed_effects_formula_str_alpha <- fixed_rhs
    } else {
      stop(
        "'fixed_rhs' must be a one-sided formula or character string like '~ 1 + var'."
      )
    }
  } else {
    # Build separate RHS for Q0 and alpha using potentially different factors
    fixed_effects_formula_str_Q0 <- build_fixed_rhs(
      factors = factors_Q0,
      factor_interaction = factor_interaction,
      continuous_covariates = continuous_covariates,
      data = data
    )
    fixed_effects_formula_str_alpha <- build_fixed_rhs(
      factors = factors_alpha,
      factor_interaction = factor_interaction,
      continuous_covariates = continuous_covariates,
      data = data
    )
  }

  fixed_effects_list <- list(
    stats::as.formula(paste("Q0", fixed_effects_formula_str_Q0)),
    stats::as.formula(paste("alpha", fixed_effects_formula_str_alpha))
  )

  if (!is.null(custom_model_formula)) {
    nlme_model_formula_obj <- if (is.character(custom_model_formula)) {
      stats::as.formula(custom_model_formula)
    } else {
      custom_model_formula
    }
  } else {
    if (equation_form == "zben") {
      if (param_space != "log10") {
        stop(
          "'param_space = \"",
          param_space,
          "\"' is not supported for equation_form = \"zben\". Use 'param_space = \"log10\"'.",
          call. = FALSE
        )
      }
      nlme_model_formula_str <- paste0(
        y_var,
        " ~ Q0 * exp(-(10^alpha / Q0) * (10^Q0) * ",
        x_var,
        ")"
      )
    } else if (equation_form == "simplified") {
      nlme_model_formula_str <- if (param_space == "log10") {
        paste0(
          y_var,
          " ~ (10^Q0) * exp(-(10^alpha) * (10^Q0) * ",
          x_var,
          ")"
        )
      } else {
        paste0(
          y_var,
          " ~ Q0 * exp(-(alpha) * (Q0) * ",
          x_var,
          ")"
        )
      }
    } else if (equation_form == "exponentiated") {
      # Koffarnus et al. (2015) exponentiated equation
      # Uses 10^() wrapping the exponential decay term, requires k parameter
      if (is.null(k)) {
        # Calculate k from data range if not provided
        y_nonzero <- data[[y_var]][data[[y_var]] > 0]
        if (length(y_nonzero) > 0) {
          k <- log10(max(y_nonzero, na.rm = TRUE)) -
            log10(min(y_nonzero[y_nonzero > 0], na.rm = TRUE)) + 0.5
        } else {
          k <- 2  # Sensible default
        }
        message(sprintf("Using calculated k = %.3f for 'exponentiated' equation.", k))
      }
      nlme_model_formula_str <- if (param_space == "log10") {
        paste0(
          y_var,
          " ~ (10^Q0) * 10^(",
          k,
          " * (exp(-(10^alpha) * (10^Q0) * ",
          x_var,
          ") - 1))"
        )
      } else {
        paste0(
          y_var,
          " ~ Q0 * 10^(",
          k,
          " * (exp(-alpha * Q0 * ",
          x_var,
          ") - 1))"
        )
      }
    }
    nlme_model_formula_obj <- stats::as.formula(nlme_model_formula_str)
  }

  # if (is.null(random_effects)) {
  #   if (!requireNamespace("nlme", quietly = TRUE))
  #     stop("nlme package required.")
  #   random_effects_arg <- nlme::pdDiag(stats::as.formula("Q0 + alpha ~ 1"))
  # } else {
  #   random_effects_arg <- random_effects
  # }

  # Check the class of the 'random_effects' argument to decide how to proceed.
  if (inherits(random_effects, "formula")) {
    # CASE 1: User provided a simple formula (e.g., Q0 + alpha ~ 1).
    # We will build the pdMat object for them using the covariance_structure argument.

    # Select the appropriate pdMat constructor function based on user's choice.
    pd_class_constructor <- switch(
      covariance_structure,
      "pdDiag" = nlme::pdDiag,
      "pdSymm" = nlme::pdSymm,
      # Fallback in case of an invalid string.
      stop(
        "Invalid covariance_structure specified. Must be 'pdDiag' or 'pdSymm'."
      )
    )
    # Construct the final pdMat object to be passed to nlme().
    random_effects_arg <- pd_class_constructor(random_effects)
  } else if (
    inherits(random_effects, "pdMat") ||
      (is.list(random_effects) &&
        all(sapply(random_effects, inherits, "pdMat")))
  ) {
    # CASE 2: User provided an advanced, pre-constructed pdMat object
    # (or a list of them, e.g., for pdBlocked).
    # We pass it through directly to nlme(). The covariance_structure argument is ignored.

    if (covariance_structure != "pdDiag") {
      # "pdDiag" is the default
      message(
        "A custom 'random_effects' object was provided, so the 'covariance_structure' argument is being ignored."
      )
    }
    random_effects_arg <- random_effects
  } else {
    # CASE 3: The provided argument is invalid.
    stop(
      "'random_effects' must be a formula (for simple cases) or a pre-constructed pdMat object/list (for advanced cases)."
    )
  }

  # Calculate parameter counts separately for Q0 and alpha (may differ with collapse_levels)
  num_params_Q0 <- NA
  num_params_alpha <- NA

  # Helper to get xlev for a set of factor names and ensure they are factors
  # Only includes factors with 2+ levels (single-level factors are dropped from formulas)
  get_xlevs <- function(factor_names, dat) {
    xlevs <- list()
    if (!is.null(factor_names)) {
      for (f in factor_names) {
        if (f %in% names(dat)) {
          # Ensure the column is a factor before getting levels
          if (!is.factor(dat[[f]])) {
            dat[[f]] <- factor(dat[[f]])
          }
          # Only include in xlev if factor has 2+ levels
          # (single-level factors are filtered out of formulas by build_fixed_rhs)
          if (nlevels(dat[[f]]) >= 2) {
            xlevs[[f]] <- levels(dat[[f]])
          }
        }
      }
    }
    xlevs
  }

  if (is.null(start_values)) {
    message(
      "Generating starting values using method: '",
      start_value_method,
      "'"
    )
    q0_start_intercept <- NA
    alpha_start_intercept <- NA
    pooled_starts <- NULL

    if (start_value_method == "pooled_nls") {
      pooled_starts <- get_pooled_nls_starts(data, y_var, x_var, equation_form)
      if (!is.null(pooled_starts) && !any(is.na(unlist(pooled_starts)))) {
        q0_start_intercept <- pooled_starts$Q0
        alpha_start_intercept <- pooled_starts$alpha
      } else {
        message(
          "Pooled NLS method failed or returned NA, falling back to heuristic method for starting values."
        )
        # Fall through to heuristic if pooled_starts is NULL or contains NA
      }
    }

    # Heuristic method (either primary or fallback)
    if (is.na(q0_start_intercept) || is.na(alpha_start_intercept)) {
      # Check if not set by pooled_nls
      if (
        start_value_method == "heuristic" ||
          is.null(pooled_starts) ||
          any(is.na(unlist(pooled_starts)))
      ) {
        # Explicitly for heuristic or if pooled failed
        message("Using heuristic method for starting values.")
      }
      idx_low_x <- data[[x_var]] <=
        stats::quantile(data[[x_var]], 0.1, na.rm = TRUE)
      if (sum(idx_low_x, na.rm = TRUE) < 2) {
        idx_low_x <- TRUE
      }
      median_y_val_at_low_x <- stats::median(
        data[[y_var]][idx_low_x],
        na.rm = TRUE
      )
      if (is.na(median_y_val_at_low_x) || !is.finite(median_y_val_at_low_x)) {
        median_y_val_at_low_x <- stats::median(data[[y_var]], na.rm = TRUE)
      }

      if (equation_form == "zben") {
        if (is.na(median_y_val_at_low_x) || !is.finite(median_y_val_at_low_x)) {
          median_y_val_at_low_x <- 2
        }
        q0_start_intercept <- median_y_val_at_low_x
        if (abs(q0_start_intercept) < 0.1) {
          q0_start_intercept <- sign(q0_start_intercept) *
            max(0.1, abs(q0_start_intercept))
          if (q0_start_intercept == 0) q0_start_intercept <- 0.1
        }
      } else if (equation_form == "simplified" || equation_form == "exponentiated") {
        # Both simplified and exponentiated use raw consumption values
        if (
          is.na(median_y_val_at_low_x) ||
            !is.finite(median_y_val_at_low_x) ||
            median_y_val_at_low_x <= 0
        ) {
          median_y_val_at_low_x <- 100
        }
        q0_start_intercept <- if (param_space == "log10") {
          log10(median_y_val_at_low_x)
        } else {
          median_y_val_at_low_x
        }
      }
      alpha_start_intercept <- if (param_space == "log10") log10(0.001) else 0.001
    }

    if ((equation_form == "simplified" || equation_form == "exponentiated") &&
        param_space == "natural" &&
        !is.null(pooled_starts) &&
        !any(is.na(unlist(pooled_starts)))) {
      # pooled starts are on log10 scale (current helper); convert to natural if needed
      q0_start_intercept <- 10^q0_start_intercept
      alpha_start_intercept <- 10^alpha_start_intercept
    }

    # Calculate parameter count for Q0
    # Ensure factor columns are proper factors before model.matrix
    data <- ensure_factors(factors_Q0, data)
    xlevs_Q0 <- get_xlevs(factors_Q0, data)
    mm_Q0 <- stats::model.matrix(
      stats::as.formula(fixed_effects_formula_str_Q0),
      data = data,
      xlev = xlevs_Q0
    )
    num_params_Q0 <- ncol(mm_Q0)

    # Calculate parameter count for alpha
    # Ensure factor columns are proper factors before model.matrix
    data <- ensure_factors(factors_alpha, data)
    xlevs_alpha <- get_xlevs(factors_alpha, data)
    mm_alpha <- stats::model.matrix(
      stats::as.formula(fixed_effects_formula_str_alpha),
      data = data,
      xlev = xlevs_alpha
    )
    num_params_alpha <- ncol(mm_alpha)

    start_Q0_vec <- c(q0_start_intercept, rep(0, num_params_Q0 - 1))
    start_alpha_vec <- c(alpha_start_intercept, rep(0, num_params_alpha - 1))
    start_values <- list(fixed = c(start_Q0_vec, start_alpha_vec))
  } else {
    # User provided start_values
    # Calculate expected parameter counts for validation
    # Ensure factor columns are proper factors before model.matrix
    data <- ensure_factors(factors_Q0, data)
    xlevs_Q0 <- get_xlevs(factors_Q0, data)
    mm_Q0 <- stats::model.matrix(
      stats::as.formula(fixed_effects_formula_str_Q0),
      data = data,
      xlev = xlevs_Q0
    )
    num_params_Q0 <- ncol(mm_Q0)

    # Ensure factor columns are proper factors before model.matrix
    data <- ensure_factors(factors_alpha, data)
    xlevs_alpha <- get_xlevs(factors_alpha, data)
    mm_alpha <- stats::model.matrix(
      stats::as.formula(fixed_effects_formula_str_alpha),
      data = data,
      xlev = xlevs_alpha
    )
    num_params_alpha <- ncol(mm_alpha)

    expected_total <- num_params_Q0 + num_params_alpha
    if (length(start_values$fixed) != expected_total) {
      stop(paste0(
        "User-supplied 'start_values' have incorrect length. Expected ",
        expected_total,
        " values (",
        num_params_Q0,
        " for Q0 + ",
        num_params_alpha,
        " for alpha), got ",
        length(start_values$fixed),
        "."
      ))
    }
  }

  groups_formula <- stats::as.formula(paste("~", id_var))
  control_obj <- nlme_control

  message("--- Fitting NLME Model ---")
  message("Equation Form: ", equation_form)
  message("Param Space: ", param_space)
  message("NLME Formula: ", deparse(nlme_model_formula_obj))
  message(
    "Start values (first few): Q0_int=",
    signif(start_values$fixed[1], 3),
    ", alpha_int=",
    signif(start_values$fixed[num_params_Q0 + 1], 3)
  )
  message(
    "Number of fixed parameters: ",
    length(start_values$fixed),
    " (Q0: ",
    num_params_Q0,
    ", alpha: ",
    num_params_alpha,
    ")"
  )

  fitted_nlme_model <- tryCatch(
    {
      nlme::nlme(
        model = nlme_model_formula_obj,
        data = data,
        fixed = fixed_effects_list,
        random = random_effects_arg,
        groups = groups_formula,
        start = start_values,
        control = control_obj,
        method = method,
        ...
      )
    },
    error = function(e) {
      num_p_val <- if (!is.na(num_params_Q0) && !is.na(num_params_alpha)) {
        paste0(
          num_params_Q0 + num_params_alpha,
          " (Q0: ",
          num_params_Q0,
          ", alpha: ",
          num_params_alpha,
          ")"
        )
      } else {
        "unknown (check factors)"
      }
      warning(
        "NLME model fitting failed with error: ",
        e$message,
        "\n  Start values used (fixed): ",
        paste(
          names(start_values$fixed),
          "=",
          signif(start_values$fixed, 3),
          collapse = ", "
        ),
        "\n  Number of fixed parameters expected: ",
        num_p_val
      )
      return(NULL)
    }
  )

  if (!is.null(fitted_nlme_model)) {
    fitted_nlme_model$call$fixed <- fixed_effects_list
    fitted_nlme_model$call$random <- random_effects_arg
    fitted_nlme_model$call$data <- quote(data)
  }

  result <- list(
    model = fitted_nlme_model,
    call = call,
    data = data,
    formula_details = list(
      nlme_model_formula_obj = nlme_model_formula_obj,
      equation_form_selected = equation_form,
      fixed_effects_formula_str_Q0 = fixed_effects_formula_str_Q0,
      fixed_effects_formula_str_alpha = fixed_effects_formula_str_alpha,
      fixed_effects_list = fixed_effects_list,
      random_effects_formula = random_effects_arg
    ),
    param_info = list(
      y_var = y_var,
      x_var = x_var,
      id_var = id_var,
      factors = factors,
      factors_Q0 = factors_Q0,
      factors_alpha = factors_alpha,
      factor_interaction = factor_interaction,
      continuous_covariates = continuous_covariates,
      num_params_Q0 = num_params_Q0,
      num_params_alpha = num_params_alpha,
      param_space = param_space,
      k = if (equation_form == "exponentiated") k else NULL
    ),
    param_space = param_space,
    param_space_details = beezdemand_param_space_details_core(
      internal_names = list(Q0 = "Q0", alpha = "alpha", k = "k"),
      internal_spaces = list(
        Q0 = param_space,
        alpha = param_space,
        k = "natural"
      )
    ),
    start_values_used = start_values$fixed,
    collapse_info = if (
      length(collapse_info$Q0) > 0 || length(collapse_info$alpha) > 0
    ) {
      collapse_info
    } else {
      NULL
    },
    error_message = if (is.null(fitted_nlme_model)) {
      "NLME model fitting failed."
    } else {
      NULL
    }
  )
  class(result) <- "beezdemand_nlme"
  return(result)
}
