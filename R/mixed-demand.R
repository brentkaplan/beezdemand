#' Fit Nonlinear Mixed-Effects Demand Model
#'
#' Fits a nonlinear mixed-effects model for behavioral economic demand data.
#' The function allows Q0 and alpha parameters to vary by specified factors
#' and supports different demand equation forms.
#'
#' @param data A data frame.
#' @param y_var Character string, the name of the dependent variable column.
#'   For `equation_form = "zben"`, this should be log-transformed consumption (e.g., "y_ll4").
#'   For `equation_form = "simplified"`, this should be raw, untransformed consumption (e.g., "y").
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
#'   }
#' @param custom_model_formula An optional custom nonlinear model formula (nlme format).
#'   If provided, this overrides `equation_form`. The user is responsible for ensuring
#'   the `y_var` scale matches the formula and that starting values are appropriate.
#'   The formula should use parameters named `Q0` and `alpha`.
#' @param start_value_method Character, method to generate starting values if `start_values` is NULL.
#'   Options: `"heuristic"` (default, uses data-driven heuristics) or
#'   `"pooled_nls"` (fits a simpler pooled NLS model first; falls back to heuristic if NLS fails).
#' @param random_effects A formula or a list of formulas for the random effects structure.
#'   Default `nlme::pdDiag(Q0 + alpha ~ 1)`.
#' @param covariance_structure Character, covariance structure for random effects.
#'   Options: `"pdDiag"` (default) or `"pdSymm"`
#' @param start_values Optional named list of starting values for fixed effects.
#'   If `NULL`, defaults are estimated based on `equation_form` and `y_var` scale.
#' @param collapse_levels Optional named list to collapse factor levels.
#' @param nlme_control Control parameters for `nlme::nlme()`.
#' @param method Fitting method for `nlme::nlme()` ("ML" or "REML"). Default "ML".
#' @param ... Additional arguments passed to `nlme::nlme()`.
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
  equation_form = c("zben", "simplified"),
  custom_model_formula = NULL,
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
  start_value_method <- match.arg(start_value_method)
  covariance_structure <- match.arg(covariance_structure)

  data <- validate_demand_data(
    data = data,
    y_var = y_var,
    x_var = x_var,
    id_var = id_var,
    factors = factors
  )

  if (!is.null(factors) && length(factors) > 2) {
    stop("Up to two factors can be specified.")
  }
  if (length(factors) < 2 && factor_interaction) {
    warning(
      "factor_interaction is TRUE but less than two factors. Interaction ignored."
    )
    factor_interaction <- FALSE
  }

  original_factors_info <- list()
  if (!is.null(collapse_levels)) {
    # (Collapsing logic - assume this is correct from previous versions)
    if (!is.list(collapse_levels) || is.null(names(collapse_levels))) {
      stop("`collapse_levels` must be a named list.")
    }
    data_modified <- data
    for (factor_col in names(collapse_levels)) {
      if (!factor_col %in% factors) {
        next
      } # Skip if factor not in model
      if (!factor_col %in% names(data_modified)) {
        next
      } # Skip if factor not in data

      original_factors_info[[
        factor_col
      ]]$original_levels <- levels(data_modified[[factor_col]])
      level_map <- collapse_levels[[factor_col]]
      if (!is.list(level_map) || is.null(names(level_map))) {
        stop("Each element in `collapse_levels` must be a named list.")
      }
      new_factor_values <- as.character(data_modified[[factor_col]])
      for (new_level_name in names(level_map)) {
        old_levels_to_map <- level_map[[new_level_name]]
        new_factor_values[
          new_factor_values %in% old_levels_to_map
        ] <- new_level_name
      }
      data_modified[[factor_col]] <- droplevels(factor(new_factor_values))
      original_factors_info[[factor_col]]$new_levels <- levels(data_modified[[
        factor_col
      ]])
    }
    data <- data_modified
  }

  fixed_effects_formula_str <- "~ 1"
  if (!is.null(factors)) {
    fixed_effects_formula_str <- if (length(factors) == 1) {
      paste("~", factors[1])
    } else {
      if (factor_interaction) {
        paste("~", factors[1], "*", factors[2])
      } else {
        paste("~", factors[1], "+", factors[2])
      }
    }
  }

  fixed_effects_list <- list(
    stats::as.formula(paste("Q0", fixed_effects_formula_str)),
    stats::as.formula(paste("alpha", fixed_effects_formula_str))
  )

  if (!is.null(custom_model_formula)) {
    nlme_model_formula_obj <- if (is.character(custom_model_formula)) {
      stats::as.formula(custom_model_formula)
    } else {
      custom_model_formula
    }
  } else {
    if (equation_form == "zben") {
      nlme_model_formula_str <- paste0(
        y_var,
        " ~ Q0 * exp(-(10^alpha / Q0) * (10^Q0) * ",
        x_var,
        ")"
      )
    } else if (equation_form == "simplified") {
      nlme_model_formula_str <- paste0(
        y_var,
        " ~ (10^Q0) * exp(-(10^alpha) * (10^Q0) * ",
        x_var,
        ")"
      )
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

  num_params_per_var <- NA # Define for scope
  if (is.null(start_values)) {
    message(
      "Generating starting values using method: '",
      start_value_method,
      "'"
    )
    q0_start_intercept <- NA
    alpha_start_intercept <- NA

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
      } else if (equation_form == "simplified") {
        if (
          is.na(median_y_val_at_low_x) ||
            !is.finite(median_y_val_at_low_x) ||
            median_y_val_at_low_x <= 0
        ) {
          median_y_val_at_low_x <- 100
        }
        q0_start_intercept <- log10(median_y_val_at_low_x)
      }
      alpha_start_intercept <- log10(0.001)
    }

    current_xlevs <- list()
    if (!is.null(factors)) {
      for (f in factors) {
        current_xlevs[[f]] <- levels(data[[f]])
      }
    }
    mm_for_param_count <- stats::model.matrix(
      stats::as.formula(fixed_effects_formula_str),
      data = data,
      xlev = current_xlevs
    )
    num_params_per_var <- ncol(mm_for_param_count)

    start_Q0_vec <- c(q0_start_intercept, rep(0, num_params_per_var - 1))
    start_alpha_vec <- c(alpha_start_intercept, rep(0, num_params_per_var - 1))
    start_values <- list(fixed = c(start_Q0_vec, start_alpha_vec))
  } else {
    # User provided start_values
    # Need to determine num_params_per_var if factors are present, for length check
    if (!is.null(factors)) {
      current_xlevs <- list()
      for (f in factors) {
        current_xlevs[[f]] <- levels(data[[f]])
      }
      mm_for_param_count <- stats::model.matrix(
        stats::as.formula(fixed_effects_formula_str),
        data = data,
        xlev = current_xlevs
      )
      num_params_per_var <- ncol(mm_for_param_count)
      if (length(start_values$fixed) != (num_params_per_var * 2)) {
        stop(paste0(
          "User-supplied 'start_values' have incorrect length. Expected ",
          num_params_per_var * 2,
          " values, got ",
          length(start_values$fixed),
          "."
        ))
      }
    } else {
      # No factors, expect 2 start values (Q0_int, alpha_int)
      num_params_per_var <- 1 # For intercept only
      if (length(start_values$fixed) != 2) {
        stop(
          "User-supplied 'start_values' have incorrect length. Expected 2 values (Q0_int, alpha_int) when no factors, got ",
          length(start_values$fixed),
          "."
        )
      }
    }
  }

  groups_formula <- stats::as.formula(paste("~", id_var))
  control_obj <- nlme_control

  message("--- Fitting NLME Model ---")
  message("Equation Form: ", equation_form)
  message("NLME Formula: ", deparse(nlme_model_formula_obj))
  message(
    "Start values (first few): Q0_int=",
    signif(start_values$fixed[1], 3),
    ", alpha_int=",
    signif(start_values$fixed[num_params_per_var + 1], 3)
  )
  message("Number of fixed parameters: ", length(start_values$fixed))

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
      num_p_val <- if (!is.na(num_params_per_var)) {
        num_params_per_var * 2
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
      fixed_effects_formula_str = fixed_effects_formula_str,
      fixed_effects_list = fixed_effects_list,
      random_effects_formula = random_effects_arg
    ),
    param_info = list(
      y_var = y_var,
      x_var = x_var,
      id_var = id_var,
      factors = factors,
      factor_interaction = factor_interaction
    ),
    start_values_used = start_values$fixed,
    original_factors_info = if (length(original_factors_info) > 0) {
      original_factors_info
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
