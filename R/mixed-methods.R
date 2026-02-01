utils::globalVariables(c(
  "coefficient_name",
  "parameter",
  "condition",
  "coefficient_value"
))

#' Get Starting Values from a Pooled NLS Model (Internal Helper)
#'
#' Fits a simpler, pooled NLS model (ignoring random effects and fixed effect factors)
#' to derive initial estimates for global Q0 and alpha parameters.
#' These are then used as starting values for the main NLME model intercepts.
#'
#' @param data The input data frame.
#' @param y_var Name of the y-variable.
#' @param x_var Name of the x-variable.
#' @param equation_form The equation form ("zben" or "simplified").
#' @return A named list with `Q0` and `alpha` starting values if successful, else `NULL`.
#' @keywords internal
#' @importFrom stats median quantile
#' @importFrom minpack.lm nlsLM
get_pooled_nls_starts <- function(data, y_var, x_var, equation_form) {
  message("Attempting to derive starting values from a pooled NLS model...")

  # --- 1. Heuristic starting values FOR THE POOLED NLS MODEL ---
  idx_low_x_nls <- data[[x_var]] <=
    stats::quantile(data[[x_var]], 0.2, na.rm = TRUE) # Use a slightly wider quantile for pooled
  if (sum(idx_low_x_nls, na.rm = TRUE) < 5) {
    idx_low_x_nls <- TRUE
  } # Ensure enough data points

  median_y_val_nls <- stats::median(data[[y_var]][idx_low_x_nls], na.rm = TRUE)
  if (is.na(median_y_val_nls) || !is.finite(median_y_val_nls)) {
    median_y_val_nls <- stats::median(data[[y_var]], na.rm = TRUE)
  }

  # Q0_nls_start: This is the Q0 parameter for the nlsLM model.
  # alpha_nls_start: This is the alpha parameter for the nlsLM model.
  # Their interpretation aligns with how Q0 and alpha are defined in the main NLME fixed effects.

  if (equation_form == "zben") {
    # y_var is log-transformed. Q0_nls_start is log10(TrueQ0).
    if (is.na(median_y_val_nls) || !is.finite(median_y_val_nls)) {
      median_y_val_nls <- 2
    }
    q0_nls_start <- median_y_val_nls
    if (abs(q0_nls_start) < 0.1) {
      # Safeguard for Q0_param in denominator
      q0_nls_start <- sign(q0_nls_start) * max(0.1, abs(q0_nls_start))
      if (q0_nls_start == 0) q0_nls_start <- 0.1
    }
  } else {
    # "simplified"
    # y_var is raw. Q0_nls_start is log10(TrueQ0).
    if (
      is.na(median_y_val_nls) ||
        !is.finite(median_y_val_nls) ||
        median_y_val_nls <= 0
    ) {
      median_y_val_nls <- 100
    }
    q0_nls_start <- log10(median_y_val_nls)
  }
  alpha_nls_start <- log10(0.001) # log10(TrueAlpha)

  # --- 2. Define NLS model formula (no factors, no random effects) ---
  # Parameters are named Q0_p and alpha_p to avoid collision if called inside main function
  # But nlsLM scope is fine. For clarity, using Q0 and alpha directly.
  if (equation_form == "zben") {
    # y_var (log-scale) ~ Q0_param * exp(-(10^alpha_param / Q0_param) * (10^Q0_param) * x_var)
    nls_formula_str <- paste0(
      y_var,
      " ~ Q0_modelp * exp(-(10^alpha_modelp / Q0_modelp) * (10^Q0_modelp) * ",
      x_var,
      ")"
    )
  } else {
    # "simplified"
    # y_var (raw-scale) ~ (10^Q0_param) * exp(-(10^alpha_param) * (10^Q0_param) * x_var)
    nls_formula_str <- paste0(
      y_var,
      " ~ (10^Q0_modelp) * exp(-(10^alpha_modelp) * (10^Q0_modelp) * ",
      x_var,
      ")"
    )
  }
  nls_formula <- stats::as.formula(nls_formula_str)

  # --- 3. Fit pooled NLS model using nlsLM ---
  pooled_fit <- tryCatch(
    {
      minpack.lm::nlsLM(
        formula = nls_formula,
        data = data,
        start = list(Q0_modelp = q0_nls_start, alpha_modelp = alpha_nls_start),
        control = minpack.lm::nls.lm.control(maxiter = 100, maxfev = 500) # Reasonably quick control
      )
    },
    error = function(e) {
      warning(
        "Pooled NLS fitting for starting values failed: ",
        e$message,
        call. = FALSE
      )
      return(NULL)
    }
  )

  if (is.null(pooled_fit)) {
    return(NULL)
  } else {
    message(
      "Pooled NLS model converged. Using its estimates for Q0 and alpha intercept starts."
    )
    coefs <- stats::coef(pooled_fit)
    return(list(Q0 = coefs["Q0_modelp"], alpha = coefs["alpha_modelp"]))
  }
}

#' Get Estimated Marginal Means for Demand Parameters
#'
#' Calculates Estimated Marginal Means (EMMs) for Q0 and alpha parameters
#' from a `beezdemand_nlme` model for all combinations of specified factor levels.
#' Reports parameters on both their estimation scale (log10) and their
#' natural, back-transformed scale. Optionally includes Essential Value (EV).
#'
#' @param fit_obj A `beezdemand_nlme` object.
#' @param factors_in_emm Character vector of factor names to compute EMMs over.
#'   Defaults to all factors present in the `fit_obj`.
#' @param at Optional named list specifying levels of conditioning variables for `emmeans::ref_grid()`.
#' @param ci_level Confidence level for the EMMs (default 0.95).
#' @param include_ev Logical. If TRUE, calculates and includes Essential Value (EV)
#'   derived from alpha, along with its confidence interval (calculated by
#'   back-transforming the CI of alpha_param_log10). Default `FALSE`.
#' @param ... Additional arguments passed to `emmeans::emmeans()`.
#'
#' @return A tibble containing:
#'   \item{Factor levels}{Columns for each factor in `factors_in_emm`.}
#'   \item{Q0_param_log10, alpha_param_log10}{EMMs for the model parameters (log10 scale)
#'     with their respective confidence intervals (LCL_Q0_param, UCL_Q0_param, etc.).}
#'   \item{Q0_natural, alpha_natural}{EMMs back-transformed to the natural scale (10^param)
#'     with their respective confidence intervals (LCL_Q0_natural, UCL_Q0_natural, etc.).}
#'   \item{EV, LCL_EV, UCL_EV}{(If `include_ev=TRUE`) Essential Value and its CI.}
#'
#' @importFrom emmeans ref_grid emmeans
#' @importFrom dplyr full_join select rename mutate across all_of left_join
#' @importFrom tibble as_tibble
#' @importFrom tidyr crossing
#' @importFrom rlang .data `:=`
#' @export
get_demand_param_emms <- function(
  fit_obj,
  factors_in_emm = NULL,
  at = NULL,
  ci_level = 0.95,
  include_ev = FALSE, # New argument
  ...
) {
  if (!inherits(fit_obj, "beezdemand_nlme")) {
    stop("Input 'fit_obj' must be a 'beezdemand_nlme' object.")
  }
  if (is.null(fit_obj$model)) {
    stop("No model found in 'fit_obj'. Fitting may have failed.")
  }

  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Package 'emmeans' is required.")
  }

  nlme_model <- fit_obj$model
  model_data <- fit_obj$data
  all_model_factors <- fit_obj$param_info$factors

  # Check if collapse_levels was used - Q0 and alpha may have different factors
  collapse_was_used <- !is.null(fit_obj$collapse_info)
  factors_Q0 <- fit_obj$param_info$factors_Q0 %||% all_model_factors
  factors_alpha <- fit_obj$param_info$factors_alpha %||% all_model_factors

  if (is.null(factors_in_emm)) {
    factors_in_emm <- all_model_factors
    if (is.null(factors_in_emm) || length(factors_in_emm) == 0) {
      message(
        "No factors specified or found in model. Reporting global parameter estimates."
      )
      factors_in_emm <- character(0)
    }
  } else {
    if (!all(factors_in_emm %in% all_model_factors)) {
      stop(
        "Some 'factors_in_emm' not found in the model's original factors: ",
        paste(setdiff(factors_in_emm, all_model_factors), collapse = ", ")
      )
    }
  }

  # Build mapping from original factor names to collapsed names
  # This handles the case where collapse_levels created factor1_Q0 and factor1_alpha
  .get_actual_factors <- function(
    original_factors,
    param_factors,
    param_suffix
  ) {
    if (!collapse_was_used || is.null(param_factors)) {
      return(original_factors)
    }
    # For each original factor, find corresponding collapsed factor if it exists
    # and verify it has more than 1 level (otherwise it was removed from formula)
    actual_factors <- character(0)
    for (orig_fac in original_factors) {
      collapsed_name <- paste0(orig_fac, "_", param_suffix)
      if (collapsed_name %in% param_factors) {
        # Check if the collapsed factor has > 1 level in the data
        if (
          collapsed_name %in%
            names(model_data) &&
            is.factor(model_data[[collapsed_name]]) &&
            nlevels(model_data[[collapsed_name]]) >= 2
        ) {
          actual_factors <- c(actual_factors, collapsed_name)
        }
        # If only 1 level, skip this factor (it's intercept-only)
      } else if (orig_fac %in% param_factors) {
        actual_factors <- c(actual_factors, orig_fac)
      }
    }
    return(actual_factors)
  }

  # Get actual factor names for each parameter
  actual_factors_Q0 <- .get_actual_factors(factors_in_emm, factors_Q0, "Q0")
  actual_factors_alpha <- .get_actual_factors(
    factors_in_emm,
    factors_alpha,
    "alpha"
  )

  # Build specs formulas for each parameter
  .build_specs_formula <- function(factors) {
    if (length(factors) > 0) {
      stats::as.formula(paste("~", paste(factors, collapse = " * ")))
    } else {
      stats::as.formula("~ 1")
    }
  }

  specs_formula_Q0 <- .build_specs_formula(actual_factors_Q0)
  specs_formula_alpha <- .build_specs_formula(actual_factors_alpha)

  # --- Helper to get EMMs for a single parameter (Q0 or alpha) ---
  .get_single_param_emm_table <- function(
    param_name_model,
    param_name_natural_prefix,
    specs_formula,
    actual_factors,
    original_factors
  ) {
    emm_table_combined <- NULL
    rg <- tryCatch(
      emmeans::ref_grid(
        nlme_model,
        param = param_name_model,
        data = model_data,
        at = at
      ),
      error = function(e) {
        warning("ref_grid for ", param_name_model, " failed: ", e$message)
        NULL
      }
    )

    if (!is.null(rg)) {
      emms_log_scale <- tryCatch(
        emmeans::emmeans(rg, specs = specs_formula, level = ci_level, ...),
        error = function(e) {
          warning(
            "emmeans (log10 scale) for ",
            param_name_model,
            " failed: ",
            e$message
          )
          NULL
        }
      )

      if (!is.null(emms_log_scale)) {
        df_log_scale_summary <- summary(
          emms_log_scale,
          infer = TRUE,
          level = ci_level
        )
        summary_names_log <- names(df_log_scale_summary)

        # Handle intercept-only case (no factors)
        # emmeans creates a column "1" with value "overall" for ~ 1 specs
        if (length(actual_factors) == 0) {
          # For intercept-only, remove the "1" column if it exists
          if ("1" %in% summary_names_log) {
            df_log_scale_summary <- df_log_scale_summary[,
              names(df_log_scale_summary) != "1",
              drop = FALSE
            ]
            summary_names_log <- names(df_log_scale_summary)
          }
        }

        factor_cols_in_summary_log <- intersect(
          summary_names_log,
          actual_factors
        )
        potential_estimate_cols_log <- setdiff(
          summary_names_log,
          c(
            factor_cols_in_summary_log,
            "SE",
            "df",
            "lower.CL",
            "upper.CL",
            "t.ratio",
            "p.value"
          )
        )
        if (
          length(potential_estimate_cols_log) == 0 ||
            !(potential_estimate_cols_log[1] %in% summary_names_log)
        ) {
          estimate_col_name_log <- summary_names_log[
            length(factor_cols_in_summary_log) + 1
          ]
          if (!(estimate_col_name_log %in% summary_names_log)) {
            estimate_col_name_log <- "emmean"
          }
          warning(
            "Could not reliably identify estimate column for log10 scale of ",
            param_name_model,
            ". Using '",
            estimate_col_name_log,
            "'."
          )
        } else {
          estimate_col_name_log <- potential_estimate_cols_log[1]
        }

        # Build select columns - handle intercept-only case
        if (length(actual_factors) > 0) {
          emm_table_combined <- tibble::as_tibble(df_log_scale_summary) |>
            dplyr::select(
              dplyr::all_of(actual_factors),
              param_log10_estimate = dplyr::all_of(estimate_col_name_log),
              param_log10_LCL = "lower.CL",
              param_log10_UCL = "upper.CL"
            )
        } else {
          # Intercept-only: no factor columns to select
          emm_table_combined <- tibble::as_tibble(df_log_scale_summary) |>
            dplyr::select(
              param_log10_estimate = dplyr::all_of(estimate_col_name_log),
              param_log10_LCL = "lower.CL",
              param_log10_UCL = "upper.CL"
            )
        }

        emm_table_combined <- emm_table_combined |>
          dplyr::mutate(
            param_natural_estimate = 10^.data$param_log10_estimate,
            param_natural_LCL = 10^.data$param_log10_LCL,
            param_natural_UCL = 10^.data$param_log10_UCL
          ) |>
          dplyr::rename_with(
            ~ gsub(
              "param_log10_estimate",
              paste0(param_name_natural_prefix, "_param_log10"),
              .x,
              fixed = TRUE
            )
          ) |>
          dplyr::rename_with(
            ~ gsub(
              "param_log10_LCL",
              paste0("LCL_", param_name_natural_prefix, "_param_log10"),
              .x,
              fixed = TRUE
            )
          ) |>
          dplyr::rename_with(
            ~ gsub(
              "param_log10_UCL",
              paste0("UCL_", param_name_natural_prefix, "_param_log10"),
              .x,
              fixed = TRUE
            )
          ) |>
          dplyr::rename_with(
            ~ gsub(
              "param_natural_estimate",
              paste0(param_name_natural_prefix, "_natural"),
              .x,
              fixed = TRUE
            )
          ) |>
          dplyr::rename_with(
            ~ gsub(
              "param_natural_LCL",
              paste0("LCL_", param_name_natural_prefix, "_natural"),
              .x,
              fixed = TRUE
            )
          ) |>
          dplyr::rename_with(
            ~ gsub(
              "param_natural_UCL",
              paste0("UCL_", param_name_natural_prefix, "_natural"),
              .x,
              fixed = TRUE
            )
          )

        # Rename collapsed factor columns back to original names
        # We need to map actual_factors back to original_factors
        # Only do this if we have actual factors in the output
        if (length(actual_factors) > 0) {
          # Build a mapping from collapsed name to original name
          for (actual_fac in actual_factors) {
            if (actual_fac %in% names(emm_table_combined)) {
              # Find the corresponding original factor name
              # Collapsed names are like "factor1_Q0" or "factor1_alpha"
              orig_name <- sub("_(Q0|alpha)$", "", actual_fac)
              if (orig_name %in% original_factors && orig_name != actual_fac) {
                emm_table_combined <- dplyr::rename(
                  emm_table_combined,
                  !!orig_name := !!actual_fac
                )
              }
            }
          }
        }
      }
    }
    if (is.null(emm_table_combined)) {
      warning(
        "Could not retrieve and process EMMs for parameter: ",
        param_name_model
      )
    }
    return(emm_table_combined)
  }

  # --- Get EMMs for Q0 and alpha ---
  emm_q0 <- .get_single_param_emm_table(
    param_name_model = "Q0",
    param_name_natural_prefix = "Q0",
    specs_formula = specs_formula_Q0,
    actual_factors = actual_factors_Q0,
    original_factors = factors_in_emm
  )
  emm_alpha <- .get_single_param_emm_table(
    param_name_model = "alpha",
    param_name_natural_prefix = "alpha",
    specs_formula = specs_formula_alpha,
    actual_factors = actual_factors_alpha,
    original_factors = factors_in_emm
  )

  # --- Combine parameter estimates ---
  # Handle asymmetric factor structures (e.g., Q0 has factors, alpha is intercept-only,
  # or same factor name but different levels due to differential collapsing)
  if (!is.null(emm_q0) && !is.null(emm_alpha)) {
    # Find common factor columns in both results
    q0_factor_cols <- intersect(names(emm_q0), factors_in_emm)
    alpha_factor_cols <- intersect(names(emm_alpha), factors_in_emm)
    common_factors <- intersect(q0_factor_cols, alpha_factor_cols)

    # Check if common factors have matching values (they might not if differential
    # collapsing was used - e.g., Q0 has original levels, alpha has collapsed levels)
    joinable_factors <- character(0)
    disjoint_factors <- character(0)

    for (fac in common_factors) {
      q0_vals <- unique(as.character(emm_q0[[fac]]))
      alpha_vals <- unique(as.character(emm_alpha[[fac]]))
      if (length(intersect(q0_vals, alpha_vals)) > 0) {
        # At least some values match - can join on this factor
        joinable_factors <- c(joinable_factors, fac)
      } else {
        # Factor values are completely disjoint (due to collapsing)
        disjoint_factors <- c(disjoint_factors, fac)
      }
    }

    if (length(disjoint_factors) > 0) {
      # Differential collapsing: Q0 and alpha have different factor structures
      # Rename the disjoint factor columns to avoid confusion
      message(
        "Note: Differential collapsing detected for factor(s): ",
        paste(disjoint_factors, collapse = ", "),
        ". EMMs will show separate rows for Q0 (original levels) and alpha (collapsed levels)."
      )

      # Rename disjoint factors in alpha table to indicate they're for alpha
      for (fac in disjoint_factors) {
        emm_alpha <- dplyr::rename(emm_alpha, !!paste0(fac, "_alpha") := !!fac)
      }
      alpha_factor_cols_renamed <- setdiff(alpha_factor_cols, disjoint_factors)

      if (length(joinable_factors) > 0) {
        # Some factors can still be joined
        combined_estimates <- dplyr::full_join(
          emm_q0,
          emm_alpha,
          by = joinable_factors
        )
      } else {
        # No joinable factors - cross join (each Q0 row gets all alpha values)
        combined_estimates <- tidyr::crossing(emm_q0, emm_alpha)
      }
    } else if (length(common_factors) > 0) {
      # Both have common factors with matching values - standard join
      combined_estimates <- dplyr::full_join(
        emm_q0,
        emm_alpha,
        by = common_factors
      )
    } else if (length(q0_factor_cols) > 0 && length(alpha_factor_cols) == 0) {
      # Q0 has factors, alpha is intercept-only
      # Cross-join alpha values to each Q0 row
      alpha_cols <- setdiff(names(emm_alpha), factors_in_emm)
      for (col in alpha_cols) {
        emm_q0[[col]] <- emm_alpha[[col]][1]
      }
      combined_estimates <- emm_q0
    } else if (length(q0_factor_cols) == 0 && length(alpha_factor_cols) > 0) {
      # alpha has factors, Q0 is intercept-only
      q0_cols <- setdiff(names(emm_q0), factors_in_emm)
      for (col in q0_cols) {
        emm_alpha[[col]] <- emm_q0[[col]][1]
      }
      combined_estimates <- emm_alpha
    } else {
      # Both are intercept-only
      combined_estimates <- dplyr::bind_cols(emm_q0, emm_alpha)
    }
  } else if (!is.null(emm_q0)) {
    combined_estimates <- emm_q0
  } else if (!is.null(emm_alpha)) {
    combined_estimates <- emm_alpha
  } else {
    warning("No EMMs could be calculated for Q0 or alpha.")
    return(tibble::tibble()) # Return empty tibble
  }

  if (
    nrow(combined_estimates) == 0 &&
      (length(factors_in_emm) > 0 || !is.null(emm_q0) || !is.null(emm_alpha))
  ) {
    warning(
      "Combined estimates table is unexpectedly empty. Check factor levels and model structure."
    )
  }

  # --- Calculate Essential Value (EV) if requested ---
  if (include_ev) {
    if (
      !is.null(emm_alpha) &&
        paste0("alpha_param_log10") %in% names(combined_estimates)
    ) {
      message("Calculating Essential Value (EV)...")
      # We need LCL and UCL of alpha_param_log10 from combined_estimates
      # These were already calculated and stored in emm_alpha and then joined

      combined_estimates <- combined_estimates |>
        dplyr::mutate(
          # EV = 1 / (100 * alpha_natural)
          # alpha_natural was 10^alpha_param_log10
          # So, EV = 1 / (100 * (10^alpha_param_log10))
          EV = 1 / (100 * .data$alpha_natural), # Use the already back-transformed alpha_natural
          # For CIs of EV: transformation is f(x) = 1/(100*x) where x = alpha_natural. This is decreasing.
          # So EV_LCL uses alpha_natural_UCL, and EV_UCL uses alpha_natural_LCL.
          LCL_EV = 1 / (100 * .data[[paste0("UCL_alpha_natural")]]), # Use the UCL of alpha_natural
          UCL_EV = 1 / (100 * .data[[paste0("LCL_alpha_natural")]]) # Use the LCL of alpha_natural
        ) |>
        # Ensure LCL_EV is indeed less than UCL_EV after transformation
        dplyr::mutate(
          temp_LCL_EV = pmin(.data$LCL_EV, .data$UCL_EV, na.rm = TRUE),
          temp_UCL_EV = pmax(.data$LCL_EV, .data$UCL_EV, na.rm = TRUE),
          LCL_EV = .data$temp_LCL_EV,
          UCL_EV = .data$temp_UCL_EV
        ) |>
        dplyr::select(-"temp_LCL_EV", -"temp_UCL_EV")
    } else {
      warning(
        "Cannot calculate EV because alpha EMMs ('alpha_param_log10' or 'alpha_natural') are not available in the results."
      )
    }
  }

  return(tibble::as_tibble(combined_estimates))
}

#' Get Estimated Marginal Means for Observed Factor Combinations
#'
#' This function is a wrapper around `get_demand_param_emms`. It first calls
#' `get_demand_param_emms` to calculate Estimated Marginal Means (EMMs) for
#' Q0 and alpha parameters over all combinations of the specified factor levels.
#' It then filters these results to return EMMs only for the combinations of
#' factor levels that were actually present in the original dataset used to
#' fit the `beezdemand_nlme` model.
#'
#' @param fit_obj A `beezdemand_nlme` object returned by `fit_demand_mixed()`.
#' @param factors_in_emm Character vector of factor names to compute EMMs over.
#'   Defaults to all factors present in the `fit_obj`. These factors define the
#'   grid over which EMMs are initially calculated and then filtered.
#' @param at Optional named list specifying levels of conditioning variables for `emmeans::ref_grid()`.
#'   Passed to `get_demand_param_emms`.
#' @param ci_level Confidence level for the EMMs (default 0.95).
#'   Passed to `get_demand_param_emms`.
#' @param include_ev Logical. If TRUE, calculates and includes Essential Value (EV)
#'   derived from alpha. Passed to `get_demand_param_emms`. Default `FALSE`.
#' @param ... Additional arguments passed to `get_demand_param_emms` and subsequently
#'   to `emmeans::emmeans()`.
#'
#' @return A tibble similar to the output of `get_demand_param_emms`, but filtered
#'   to include only rows corresponding to factor level combinations that were
#'   observed in the original `fit_obj$data`. Contains:
#'   \item{Factor levels}{Columns for each factor in `factors_in_emm`.}
#'   \item{Q0_param_log10, alpha_param_log10}{EMMs for model parameters (log10 scale) and CIs.}
#'   \item{Q0_natural, alpha_natural}{EMMs back-transformed to natural scale and CIs.}
#'   \item{EV, LCL_EV, UCL_EV}{(If `include_ev=TRUE`) Essential Value and its CI.}
#'
#' @seealso \code{\link{get_demand_param_emms}}
#'
#' @importFrom dplyr distinct all_of semi_join select
#' @importFrom tibble as_tibble
#' @importFrom rlang !!! syms
#' @export
get_observed_demand_param_emms <- function(
  fit_obj,
  factors_in_emm = NULL,
  at = NULL,
  ci_level = 0.95,
  include_ev = FALSE,
  ...
) {
  if (!inherits(fit_obj, "beezdemand_nlme")) {
    stop("Input 'fit_obj' must be a 'beezdemand_nlme' object.")
  }
  if (is.null(fit_obj$model)) {
    stop("No model found in 'fit_obj'. Fitting may have failed.")
  }

  all_model_factors <- fit_obj$param_info$factors
  if (is.null(factors_in_emm)) {
    factors_in_emm <- all_model_factors
    if (is.null(factors_in_emm) || length(factors_in_emm) == 0) {
      message(
        "No factors specified in 'factors_in_emm' or found in model. ",
        "Returning global EMMs (which is always an 'observed' combination)."
      )
    }
  } else {
    if (!all(factors_in_emm %in% all_model_factors)) {
      stop(
        "Some 'factors_in_emm' not found in the model's original factors: ",
        paste(setdiff(factors_in_emm, all_model_factors), collapse = ", ")
      )
    }
  }

  full_emms <- get_demand_param_emms(
    fit_obj = fit_obj,
    factors_in_emm = factors_in_emm,
    at = at,
    ci_level = ci_level,
    include_ev = include_ev,
    ...
  )

  if (length(factors_in_emm) > 0 && nrow(full_emms) > 0) {
    # Check if collapse_levels was used - if so, EMMs have collapsed levels
    # but the original factor column has un-collapsed levels
    collapse_was_used <- !is.null(fit_obj$collapse_info)

    if (collapse_was_used) {
      # When collapse_levels is used, the EMMs may have:
      # 1. Collapsed levels only (same collapse for Q0 and alpha)
      # 2. Original + collapsed levels (differential collapse: Q0 uses original, alpha uses collapsed)

      # Check for differential collapsing by looking for "_alpha" suffixed columns in EMMs
      emm_factor_cols <- intersect(
        names(full_emms),
        c(factors_in_emm, paste0(factors_in_emm, "_alpha"))
      )

      # Check if differential collapsing occurred (both dose and dose_alpha columns exist)
      differential_collapse <- any(
        paste0(factors_in_emm, "_alpha") %in% names(full_emms)
      )

      if (differential_collapse) {
        # Differential collapsing: EMM table has both original (dose) and collapsed (dose_alpha) columns
        # For filtering, we need to ensure:
        # 1. The original factor values (e.g., dose = "3e-05") exist in original data
        # 2. The collapsed factor values (e.g., dose_alpha = "aa") are derived from those originals

        # Get observed Q0 factor levels from original columns
        # The original factor columns should be in both factors_in_emm AND in the data
        q0_cols_to_filter <- intersect(factors_in_emm, names(full_emms))
        q0_cols_to_filter <- intersect(q0_cols_to_filter, names(fit_obj$data))

        if (length(q0_cols_to_filter) > 0) {
          observed_q0_combinations <- fit_obj$data |>
            dplyr::distinct(!!!rlang::syms(q0_cols_to_filter))

          # Filter EMMs to include only observed Q0 factor combinations
          # The alpha (collapsed) factor levels are all valid since they're derived from observed originals
          filtered_emms <- full_emms |>
            dplyr::semi_join(observed_q0_combinations, by = q0_cols_to_filter)
        } else {
          # No Q0 columns to filter on - return all EMMs
          filtered_emms <- full_emms
        }
      } else {
        # Same collapse for Q0 and alpha, or only one param was collapsed
        # Use the original logic
        collapse_factor_cols <- character(0)
        for (orig_fac in factors_in_emm) {
          q0_col <- paste0(orig_fac, "_Q0")
          alpha_col <- paste0(orig_fac, "_alpha")
          if (q0_col %in% names(fit_obj$data)) {
            collapse_factor_cols <- c(collapse_factor_cols, q0_col)
          } else if (alpha_col %in% names(fit_obj$data)) {
            collapse_factor_cols <- c(collapse_factor_cols, alpha_col)
          } else if (orig_fac %in% names(fit_obj$data)) {
            collapse_factor_cols <- c(collapse_factor_cols, orig_fac)
          }
        }

        if (length(collapse_factor_cols) > 0) {
          # Get observed combinations from collapsed columns
          observed_combinations <- fit_obj$data |>
            dplyr::distinct(!!!rlang::syms(collapse_factor_cols))

          # Rename collapsed columns back to original names for the join
          for (i in seq_along(collapse_factor_cols)) {
            col_name <- collapse_factor_cols[i]
            orig_name <- sub("_(Q0|alpha)$", "", col_name)
            if (col_name != orig_name) {
              observed_combinations <- dplyr::rename(
                observed_combinations,
                !!orig_name := !!col_name
              )
            }
          }

          filtered_emms <- full_emms |>
            dplyr::semi_join(observed_combinations, by = factors_in_emm)
        } else {
          # Fallback: return all EMMs
          filtered_emms <- full_emms
        }
      }
    } else {
      # No collapse - use original factor columns
      missing_factors_in_data <- setdiff(factors_in_emm, names(fit_obj$data))
      if (length(missing_factors_in_data) > 0) {
        stop(
          "Specified 'factors_in_emm' not found in fit_obj$data: ",
          paste(missing_factors_in_data, collapse = ", ")
        )
      }

      observed_combinations <- fit_obj$data |>
        dplyr::distinct(!!!rlang::syms(factors_in_emm))

      filtered_emms <- full_emms |>
        dplyr::semi_join(observed_combinations, by = factors_in_emm)
    }

    if (nrow(filtered_emms) < nrow(full_emms)) {
      message(
        "Filtered EMMs to include only combinations of '",
        paste(factors_in_emm, collapse = "', '"),
        "' that were present in the original data."
      )
    }
    return(tibble::as_tibble(filtered_emms))
  } else {
    return(tibble::as_tibble(full_emms))
  }
}

#' Get Pairwise Comparisons for Demand Parameters
#'
#' Conducts pairwise comparisons for Q0 and/or alpha parameters from a
#' `beezdemand_nlme` model across levels of specified factors.
#' Comparisons are performed on the log10 scale of the parameters.
#' Results include estimates of differences (on log10 scale) and
#' optionally, ratios (on the natural scale by applying 10^difference).
#'
#' @param fit_obj A `beezdemand_nlme` object.
#' @param params_to_compare Character vector: "Q0", "alpha", or `c("Q0", "alpha")`. Default `c("Q0", "alpha")`.
#' @param compare_specs A formula specifying the factors whose levels are to be included in the EMM calculation
#'   prior to contrasting. This defines the "cells" of your design for EMMs.
#'   E.g., `~ factor1` (EMMs for levels of factor1, averaging over others),
#'   `~ factor1 * factor2` (EMMs for all cells of factor1 x factor2).
#'   If `NULL`, it defaults to an interaction of all factors in `fit_obj$param_info$factors`.
#' @param contrast_type Character string specifying the type of contrast (passed to `method` in `emmeans::contrast`).
#'   Commonly `"pairwise"`, `"revpairwise"`, `"eff"`, `"consec"`, `"poly"`. Default `"pairwise"`.
#' @param contrast_by Optional character vector of factor names to condition the contrasts by (passed to `by` in `emmeans::contrast`).
#'   If `NULL` (default), contrasts are performed over the primary terms implied by `compare_specs` and `contrast_type`.
#'   Example: If `compare_specs = ~ dose * drug`, `contrast_type = "pairwise"`, and `contrast_by = "dose"`,
#'   this will perform pairwise comparisons of `drug` levels within each level of `dose`.
#'   **Note:** If the original `fit_obj` model is additive for the factors involved (i.e., no interaction term
#'   was fitted), specifying `contrast_by` will result in identical contrast estimates across the levels
#'   of the `contrast_by` variable(s). In such cases, consider analyzing main effects directly
#'   (e.g., `compare_specs = ~drug`, `contrast_by = NULL`).
#' @param adjust P-value adjustment method. Default "tukey".
#' @param at Optional named list for `emmeans::ref_grid()`.
#' @param ci_level Confidence level. Default 0.95.
#' @param report_ratios Logical. If TRUE, reports contrasts as ratios. Default `TRUE`.
#' @param ... Additional arguments passed to `emmeans::emmeans()` or `emmeans::contrast()`.
#'
#' @return A list named by parameter. Each element contains:
#'   \item{emmeans}{Tibble of EMMs (log10 scale) with CIs.}
#'   \item{contrasts_log10}{Tibble of comparisons (log10 differences) with CIs and p-values.}
#'   \item{contrasts_ratio}{(If `report_ratios=TRUE` and successful) Tibble of comparisons
#'     as ratios (natural scale), with CIs for ratios.}
#'   S3 class `beezdemand_comparison` is assigned.
#'
#' @importFrom emmeans ref_grid emmeans contrast
#' @importFrom tibble as_tibble
#' @importFrom rlang `:=` .data
#' @importFrom dplyr select rename all_of any_of everything mutate
#' @export
get_demand_comparisons <- function(
  fit_obj,
  params_to_compare = c("Q0", "alpha"),
  compare_specs = NULL,
  contrast_type = "pairwise",
  contrast_by = NULL,
  adjust = "tukey",
  at = NULL,
  ci_level = 0.95,
  report_ratios = TRUE,
  ...
) {
  if (!inherits(fit_obj, "beezdemand_nlme")) {
    stop("Input 'fit_obj' must be a 'beezdemand_nlme' object.")
  }
  if (is.null(fit_obj$model)) {
    stop("No model found in 'fit_obj'. Fitting may have failed.")
  }
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Package 'emmeans' is required.")
  }

  nlme_model <- fit_obj$model
  model_data <- fit_obj$data
  all_model_factors <- fit_obj$param_info$factors
  model_had_interaction <- fit_obj$param_info$factor_interaction # From fit_demand_mixed

  # Check if collapse_levels was used - Q0 and alpha may have different factors
  collapse_was_used <- !is.null(fit_obj$collapse_info)
  factors_Q0 <- fit_obj$param_info$factors_Q0 %||% all_model_factors
  factors_alpha <- fit_obj$param_info$factors_alpha %||% all_model_factors

  # Helper to get actual factors for a parameter
  .get_actual_factors_for_param <- function(
    original_factors,
    param_factors,
    param_suffix
  ) {
    if (!collapse_was_used || is.null(param_factors)) {
      return(original_factors)
    }
    actual_factors <- character(0)
    for (orig_fac in original_factors) {
      collapsed_name <- paste0(orig_fac, "_", param_suffix)
      if (collapsed_name %in% param_factors) {
        if (
          collapsed_name %in%
            names(model_data) &&
            is.factor(model_data[[collapsed_name]]) &&
            nlevels(model_data[[collapsed_name]]) >= 2
        ) {
          actual_factors <- c(actual_factors, collapsed_name)
        }
      } else if (orig_fac %in% param_factors) {
        actual_factors <- c(actual_factors, orig_fac)
      }
    }
    return(actual_factors)
  }

  # Determine the base factors from compare_specs or default
  if (is.null(compare_specs)) {
    if (is.null(all_model_factors) || length(all_model_factors) == 0) {
      message(
        "No factors in model or 'compare_specs'. Getting overall intercept EMMs."
      )
      base_factors <- character(0)
    } else {
      base_factors <- all_model_factors
      message(
        "Using default 'compare_specs': ~ ",
        paste(all_model_factors, collapse = " * "),
        " for EMMs."
      )
    }
    user_provided_specs <- FALSE
  } else {
    if (is.character(compare_specs)) {
      compare_specs <- stats::as.formula(compare_specs)
    }
    if (!inherits(compare_specs, "formula")) {
      stop(
        "'compare_specs' must be a formula or a character string (e.g., '~ factor1 * factor2')."
      )
    }
    base_factors <- all.vars(compare_specs)
    user_provided_specs <- TRUE
  }

  results_list <- list()

  # Initialize effective_contrast_by so it exists even if EMMs fail and the loop
  # short-circuits before setting it. This avoids errors when setting attributes
  # after the loop.
  effective_contrast_by <- contrast_by

  for (param_name in params_to_compare) {
    if (!param_name %in% c("Q0", "alpha")) {
      warning(
        "Unknown parameter '",
        param_name,
        "' in `params_to_compare`. Skipping."
      )
      next
    }

    current_param_results <- list()
    message(paste0(
      "\n--- Processing comparisons for parameter: ",
      param_name,
      " ---"
    ))

    # Get actual factors for this parameter (handles collapse_levels)
    param_suffix <- param_name # "Q0" or "alpha"
    param_factors <- if (param_name == "Q0") factors_Q0 else factors_alpha
    actual_factors <- .get_actual_factors_for_param(
      base_factors,
      param_factors,
      param_suffix
    )

    # Build specs formula for this parameter
    if (length(actual_factors) > 0) {
      emm_specs_formula <- stats::as.formula(paste(
        "~",
        paste(actual_factors, collapse = " * ")
      ))
    } else {
      emm_specs_formula <- stats::as.formula("~ 1")
      if (length(base_factors) > 0) {
        message(
          "  Note: Parameter ",
          param_name,
          " has no factors (intercept-only) due to collapse_levels."
        )
      }
    }

    rg <- tryCatch(
      emmeans::ref_grid(
        nlme_model,
        param = param_name,
        data = model_data,
        at = at
      ),
      error = function(e) {
        message("  ref_grid for ", param_name, " failed: ", e$message)
        NULL
      }
    )

    if (is.null(rg)) {
      current_param_results$error <- paste(
        "Failed to create ref_grid for",
        param_name
      )
      results_list[[param_name]] <- current_param_results
      next
    }

    emms <- tryCatch(
      emmeans::emmeans(rg, specs = emm_specs_formula, level = ci_level, ...),
      error = function(e) {
        message(
          "  emmeans for ",
          param_name,
          " (log10 scale) failed: ",
          e$message
        )
        NULL
      }
    )

    if (!is.null(emms)) {
      current_param_results$emmeans <- tibble::as_tibble(summary(
        emms,
        infer = TRUE,
        level = ci_level,
        adjust = adjust
      ))

      # --- Warning for additive models with 'contrast_by' ---
      # This is a simplified check. A more robust check would parse fit_obj$formula_details$fixed_effects_formula_str
      # to see if an interaction term actually exists between the terms in emm_specs_formula and contrast_by.
      # For now, if the global factor_interaction flag from the fit was FALSE, and we have multiple factors and a 'by', warn.
      terms_in_emmspecs <- actual_factors
      if (
        !is.null(contrast_by) &&
          length(all_model_factors) > 1 &&
          !model_had_interaction
      ) {
        # Check if the 'by' variable is different from the primary contrasted variable(s)
        # A simple heuristic: if the main EMM spec does not inherently imply interaction being contrasted.
        # This warning is a general heads-up for additive models.
        message(
          "  Note: The original model fit for '",
          param_name,
          "' appears to be additive for factors: ",
          paste(all_model_factors, collapse = ", "),
          ". Contrasts using 'contrast_by = \"",
          paste(contrast_by, collapse = ", "),
          "\"' ",
          "will likely show identical estimates across levels of '",
          paste(contrast_by, collapse = ", "),
          "'."
        )
      }

      # Map contrast_by to collapsed factor name if needed
      effective_contrast_by <- contrast_by
      if (!is.null(contrast_by) && collapse_was_used) {
        mapped_contrast_by <- character(0)
        for (cb_fac in contrast_by) {
          collapsed_name <- paste0(cb_fac, "_", param_suffix)
          if (collapsed_name %in% actual_factors) {
            mapped_contrast_by <- c(mapped_contrast_by, collapsed_name)
          } else if (cb_fac %in% actual_factors) {
            mapped_contrast_by <- c(mapped_contrast_by, cb_fac)
          }
          # If factor not in actual_factors at all, skip it
        }
        if (length(mapped_contrast_by) > 0) {
          if (!identical(mapped_contrast_by, contrast_by)) {
            message(
              "  Mapped contrast_by from '",
              paste(contrast_by, collapse = ", "),
              "' to '",
              paste(mapped_contrast_by, collapse = ", "),
              "' for ",
              param_name,
              " due to collapse_levels."
            )
          }
          effective_contrast_by <- mapped_contrast_by
        } else {
          # contrast_by factors not available for this parameter
          effective_contrast_by <- NULL
        }
      }

      # Redundant 'by' check
      if (
        !is.null(effective_contrast_by) &&
          length(terms_in_emmspecs) == 1 &&
          identical(sort(terms_in_emmspecs), sort(effective_contrast_by))
      ) {
        message(
          "  `contrast_by` (",
          paste(effective_contrast_by, collapse = ", "),
          ") is redundant with `compare_specs` (",
          deparse(emm_specs_formula),
          ") for simple contrasts. Ignoring `contrast_by` for this parameter."
        )
        effective_contrast_by <- NULL
      }

      # Skip contrasts if intercept-only (no factors to contrast)
      if (length(actual_factors) == 0) {
        message(
          "  Skipping contrasts for ",
          param_name,
          " (intercept-only, no factors to compare)."
        )
        current_param_results$contrasts_log10 <- tibble::tibble()
        if (report_ratios) {
          current_param_results$contrasts_ratio <- tibble::tibble()
        }
        results_list[[param_name]] <- current_param_results
        next
      }

      contrasts_log10_obj <- tryCatch(
        emmeans::contrast(
          emms,
          method = contrast_type,
          by = effective_contrast_by,
          adjust = adjust,
          ...
        ),
        error = function(e) {
          message(
            "  contrast (log10 scale) for ",
            param_name,
            " failed: ",
            e$message
          )
          NULL
        }
      )

      if (!is.null(contrasts_log10_obj)) {
        if (
          inherits(contrasts_log10_obj, "emmGrid") &&
            nrow(contrasts_log10_obj@grid) == 0
        ) {
          message(
            "  Contrast object for ",
            param_name,
            " is empty (no comparisons were made)."
          )
          current_param_results$contrasts_log10 <- tibble::tibble()
          if (report_ratios) {
            current_param_results$contrasts_ratio <- tibble::tibble()
          }
        } else {
          df_contrasts_log10_summary <- summary(
            contrasts_log10_obj,
            infer = TRUE,
            level = ci_level,
            adjust = adjust
          )
          if (
            is.null(df_contrasts_log10_summary) ||
              nrow(as.data.frame(df_contrasts_log10_summary)) == 0
          ) {
            message("  Summary of contrasts for ", param_name, " is empty.")
            current_param_results$contrasts_log10 <- tibble::tibble()
            if (report_ratios) {
              current_param_results$contrasts_ratio <- tibble::tibble()
            }
          } else {
            df_contrasts_log10 <- tibble::as_tibble(df_contrasts_log10_summary)
            # (Rest of the column identification and renaming logic from previous correct version)
            # ...
            contrast_col_names_log10 <- names(df_contrasts_log10)
            by_vars_in_summary <- if (!is.null(effective_contrast_by)) {
              intersect(effective_contrast_by, contrast_col_names_log10)
            } else {
              character(0)
            }

            actual_contrast_def_col <- "contrast"
            if (!("contrast" %in% contrast_col_names_log10)) {
              potential_def_cols <- setdiff(
                contrast_col_names_log10,
                c(
                  by_vars_in_summary,
                  "estimate",
                  "SE",
                  "df",
                  "lower.CL",
                  "upper.CL",
                  "t.ratio",
                  "p.value"
                )
              )
              if (length(potential_def_cols) > 0) {
                actual_contrast_def_col <- potential_def_cols[1]
              } else {
                actual_contrast_def_col <- contrast_col_names_log10[
                  length(by_vars_in_summary) + 1
                ]
              }
            }
            if (!(actual_contrast_def_col %in% contrast_col_names_log10)) {
              actual_contrast_def_col <- contrast_col_names_log10[1]
            }

            current_param_results$contrasts_log10 <- df_contrasts_log10 |>
              dplyr::rename(
                contrast_definition = dplyr::all_of(actual_contrast_def_col)
              ) |>
              dplyr::select(
                dplyr::any_of(by_vars_in_summary),
                "contrast_definition",
                "estimate",
                "SE",
                "df",
                "lower.CL",
                "upper.CL",
                "t.ratio",
                "p.value"
              )

            if (report_ratios) {
              current_param_results$contrasts_ratio <- current_param_results$contrasts_log10 |>
                dplyr::mutate(
                  ratio_estimate = 10^.data$estimate,
                  LCL_ratio = 10^.data$lower.CL,
                  UCL_ratio = 10^.data$upper.CL
                ) |>
                dplyr::select(
                  dplyr::any_of(by_vars_in_summary),
                  "contrast_definition",
                  "ratio_estimate",
                  "LCL_ratio",
                  "UCL_ratio",
                  "p.value"
                )
            }
          }
        }
      } else {
        current_param_results$contrasts_log10_error <- paste(
          "Contrast calculation (log10 scale) failed for",
          param_name
        )
        current_param_results$contrasts_log10 <- tibble::tibble()
        if (report_ratios) {
          current_param_results$contrasts_ratio <- tibble::tibble()
        }
      }
    } else {
      current_param_results$emmeans_error <- paste(
        "EMM calculation (log10 scale) failed for",
        param_name
      )
    }
    results_list[[param_name]] <- current_param_results
  }

  class(results_list) <- "beezdemand_comparison"
  attr(results_list, "compare_specs_used") <- deparse(emm_specs_formula)
  attr(results_list, "contrast_type_used") <- contrast_type
  attr(results_list, "contrast_by_used") <- if (
    !is.null(effective_contrast_by)
  ) {
    paste(effective_contrast_by, collapse = ", ")
  } else {
    "NULL"
  }
  attr(results_list, "adjustment_method") <- adjust
  return(results_list)
}

#' Print method for beezdemand_comparison objects
#'
#' @param x A `beezdemand_comparison` object.
#' @param digits Number of significant digits to display for estimates.
#' @param ... Additional arguments (unused).
#' @export
print.beezdemand_comparison <- function(x, digits = 3, ...) {
  cat("Demand Parameter Comparisons (from beezdemand_nlme fit)\n")
  emm_specs_used <- attr(x, "compare_specs_used")
  contrast_type <- attr(x, "contrast_type_used")
  contrast_by <- attr(x, "contrast_by_used")
  adj_method <- attr(x, "adjustment_method")

  if (!is.null(emm_specs_used)) {
    cat("EMMs computed over:", emm_specs_used, "\n")
  }
  if (!is.null(contrast_type)) {
    cat("Contrast type:", contrast_type)
  }
  if (!is.null(contrast_by) && contrast_by != "NULL") {
    cat(", by levels of:", contrast_by, "\n")
  } else {
    cat("\n")
  }
  if (!is.null(adj_method)) {
    cat("P-value adjustment method:", adj_method, "\n")
  }
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
}

#' Get Trends (Slopes) of Demand Parameters with respect to Continuous Covariates
#'
#' Computes the trend (slope) of `Q0` and/or `alpha` with respect to one or more
#' continuous covariates using `emmeans::emtrends()` on a fitted `beezdemand_nlme`
#' model. Trends are computed on the parameter estimation scale (log10), consistent
#' with how parameters are modeled.
#'
#' @param fit_obj A `beezdemand_nlme` object from `fit_demand_mixed()`.
#' @param params Character vector of parameters to analyze: any of "Q0", "alpha".
#'   Default `c("Q0", "alpha")`.
#' @param covariates Character vector of continuous covariate names for which
#'   to compute trends.
#' @param specs A formula specifying the factors over which to produce trends
#'   (e.g., `~ drug` for trends by drug; `~ 1` for overall). Default `~ 1`.
#' @param at Optional named list to condition variables (factors or continuous)
#'   when computing trends (passed through to `emmeans::ref_grid`).
#' @param ci_level Confidence level for intervals. Default 0.95.
#' @param ... Additional args passed to `emmeans::emtrends()`.
#'
#' @return A tibble combining trends for each requested parameter and covariate,
#'   including columns for grouping factors (from `specs`), `parameter`,
#'   `covariate`, `trend` (slope on log10 scale), and its CI (`lower.CL`, `upper.CL`).
#'
#' @examples
#' \dontrun{
#' trends <- get_demand_param_trends(
#'   fit_obj = my_fit,
#'   params = c("Q0", "alpha"),
#'   covariates = c("age", "dose_num"),
#'   specs = ~ drug,
#'   at = list(age = 21, dose_num = 0.5)
#' )
#' }
#'
#' @importFrom emmeans ref_grid emtrends
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows select any_of rename
#' @export
get_demand_param_trends <- function(
  fit_obj,
  params = c("Q0", "alpha"),
  covariates,
  specs = ~1,
  at = NULL,
  ci_level = 0.95,
  ...
) {
  if (!inherits(fit_obj, "beezdemand_nlme")) {
    stop("Input 'fit_obj' must be a 'beezdemand_nlme' object.")
  }
  if (is.null(fit_obj$model)) {
    stop("No model found in 'fit_obj'. Fitting may have failed.")
  }
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Package 'emmeans' is required.")
  }
  if (missing(covariates) || length(covariates) == 0) {
    stop(
      "Please provide at least one continuous covariate name in 'covariates'."
    )
  }

  params <- match.arg(params, choices = c("Q0", "alpha"), several.ok = TRUE)
  nlme_model <- fit_obj$model
  model_data <- fit_obj$data

  # Normalize specs to a formula
  specs_formula <- if (is.character(specs)) stats::as.formula(specs) else specs
  if (!inherits(specs_formula, "formula")) {
    stop(
      "'specs' must be a formula or a character string, e.g., '~ drug' or '~ 1'."
    )
  }
  specs_vars <- tryCatch(all.vars(specs_formula[[2]]), error = function(e) {
    character(0)
  })

  out_list <- list()

  for (param_name in params) {
    for (cv in covariates) {
      tr_obj <- tryCatch(
        emmeans::emtrends(
          nlme_model,
          specs = specs_formula,
          var = cv,
          param = param_name,
          data = model_data,
          at = at,
          infer = c(TRUE, TRUE),
          level = ci_level,
          ...
        ),
        error = function(e) NULL
      )

      # Fallback: build ref_grid then emtrends, which can be more reliable for nlme params
      if (is.null(tr_obj)) {
        rg <- tryCatch(
          emmeans::ref_grid(
            nlme_model,
            param = param_name,
            data = model_data,
            at = at
          ),
          error = function(e) NULL
        )
        if (!is.null(rg)) {
          tr_obj <- tryCatch(
            emmeans::emtrends(
              rg,
              specs = specs_formula,
              var = cv,
              infer = c(TRUE, TRUE),
              level = ci_level,
              ...
            ),
            error = function(e) NULL
          )
        }
      }
      if (is.null(tr_obj)) {
        next
      }

      tr_sum <- tryCatch(
        summary(tr_obj, infer = TRUE, level = ci_level),
        error = function(e) NULL
      )
      if (is.null(tr_sum)) {
        next
      }

      df_tr <- tibble::as_tibble(tr_sum)
      coln <- names(df_tr)
      trend_col <- if ("trend" %in% coln) {
        "trend"
      } else {
        setdiff(
          coln,
          c(
            specs_vars,
            "SE",
            "df",
            "lower.CL",
            "upper.CL",
            "t.ratio",
            "p.value"
          )
        )[1]
      }
      keep_cols <- unique(c(
        specs_vars,
        trend_col,
        "SE",
        "df",
        "lower.CL",
        "upper.CL",
        "t.ratio",
        "p.value"
      ))
      keep_cols <- intersect(keep_cols, coln)

      df_tr <- df_tr |>
        dplyr::select(dplyr::any_of(keep_cols)) |>
        dplyr::rename(trend = dplyr::all_of(trend_col))

      df_tr$parameter <- param_name
      df_tr$covariate <- cv
      out_list[[paste(param_name, cv, sep = "::")]] <- df_tr
    }
  }

  if (length(out_list) == 0) {
    warning(
      "No trends could be calculated. Check 'covariates', 'specs', and 'at'."
    )
    return(tibble::as_tibble(data.frame()))
  }
  dplyr::bind_rows(out_list)
}

#' Print Method for beezdemand_nlme Objects
#'
#' Provides a concise summary of a `beezdemand_nlme` object, typically
#' displaying the call, model specifications, and key results from the
#' `nlme` fit if successful.
#'
#' @param x An object of class `beezdemand_nlme`.
#' @param digits Minimal number of significant digits, see `print.default`.
#' @param ... Additional arguments passed to `print.nlme` if the model exists.
#'
#' @return Invisibly returns the original object `x`.
#'
#' @method print beezdemand_nlme
#' @export
#' @examples
#' \dontrun{
#' # Assuming 'fit_two_factor_no_interaction' is a beezdemand_nlme object
#' print(fit_two_factor_no_interaction)
#'
#' # If fitting failed:
#' # fit_failed <- fit_demand_mixed(..., nlme_control=list(maxIter=1)) # To force a failure
#' # print(fit_failed)
#' }
print.beezdemand_nlme <- function(
  x,
  digits = max(3L, getOption("digits") - 3L),
  ...
) {
  cat("Demand NLME Model Fit ('beezdemand_nlme' object)\n")
  cat("---------------------------------------------------\n")

  cat("\nCall:\n")
  print(x$call)
  cat("\n")

  cat(
    "Equation Form Selected: ",
    x$formula_details$equation_form_selected,
    "\n"
  )
  cat("NLME Model Formula:\n")
  print(x$formula_details$nlme_model_formula_obj)

  # Print fixed effects formulas - may differ for Q0 and alpha with collapse_levels

  q0_formula <- x$formula_details$fixed_effects_formula_str_Q0
  alpha_formula <- x$formula_details$fixed_effects_formula_str_alpha

  if (
    !is.null(q0_formula) &&
      !is.null(alpha_formula) &&
      q0_formula == alpha_formula
  ) {
    cat("Fixed Effects Structure (Q0 & alpha): ", q0_formula, "\n")
  } else {
    if (!is.null(q0_formula)) {
      cat("Fixed Effects Structure (Q0):    ", q0_formula, "\n")
    }
    if (!is.null(alpha_formula)) {
      cat("Fixed Effects Structure (alpha): ", alpha_formula, "\n")
    }
  }
  if (!is.null(x$param_info$factors)) {
    cat("Factors: ", paste(x$param_info$factors, collapse = ", "), "\n")
    cat("Interaction Term Included: ", x$param_info$factor_interaction, "\n")
  } else {
    cat("Factors: None\n")
  }
  cat("ID Variable for Random Effects: ", x$param_info$id_var, "\n")

  cat("\nStart Values Used (Fixed Effects Intercepts):\n")
  if (!is.null(x$start_values_used)) {
    num_params_per_var_for_print <- length(x$start_values_used) / 2 # Assuming Q0 and alpha have same num params
    if (
      num_params_per_var_for_print >= 1 &&
        (length(x$start_values_used) %% 2 == 0)
    ) {
      cat(
        "  Q0 Intercept (log10 scale): ",
        format(x$start_values_used[1], digits = digits),
        "\n"
      )
      cat(
        "  alpha Intercept (log10 scale): ",
        format(
          x$start_values_used[num_params_per_var_for_print + 1],
          digits = digits
        ),
        "\n"
      )
    } else {
      cat(
        "  Could not determine Q0/alpha intercepts from start_values_used structure.\n"
      )
      cat("  Full start_values_used vector:\n")
      print(x$start_values_used, digits = digits)
    }
  } else {
    cat(
      "  Starting values not available in object (or were user-supplied directly to nlme).\n"
    )
  }
  cat("\n")

  if (is.null(x$model)) {
    cat("---------------------------------------------------\n")
    cat("MODEL FITTING FAILED.\n")
    if (!is.null(x$error_message)) {
      cat("Error Message: ", x$error_message, "\n")
    }
    cat("Refer to warnings during the fitting process for more details.\n")
  } else {
    cat("--- NLME Model Fit Summary (from nlme object) ---\n")
    # Leverage the print method for nlme objects
    # We can print specific parts or the whole thing.
    # For conciseness, let's print the standard nlme summary.
    # If you want just fixef, ranef, sigma:
    # print(fixef(x$model), digits = digits)
    # print(VarCorr(x$model), digits = digits) # VarCorr also has its own print method
    # cat("Residual standard error:", format(x$model$sigma, digits = digits), "\n")
    # But the default print.nlme is quite good:
    print(x$model, ...) # Pass extra arguments like digits to print.nlme

    cat("\n--- Additional Fit Statistics ---\n")
    logLik_val <- tryCatch(stats::logLik(x$model), error = function(e) NA)
    AIC_val <- tryCatch(stats::AIC(x$model), error = function(e) NA)
    BIC_val <- tryCatch(stats::BIC(x$model), error = function(e) NA)

    if (!is.na(logLik_val)) {
      cat("Log-likelihood: ", format(logLik_val[1], digits = digits), "\n")
    } else {
      cat("Log-likelihood: NA\n")
    }
    if (!is.na(AIC_val)) {
      cat("AIC: ", format(AIC_val, digits = digits), "\n")
    } else {
      cat("AIC: NA\n")
    }
    if (!is.null(BIC_val)) {
      cat("BIC: ", format(BIC_val, digits = digits), "\n")
    } else {
      cat("BIC: NA\n")
    }
  }
  cat("---------------------------------------------------\n")

  invisible(x)
}

#' Summary method for beezdemand_nlme
#'
#' Returns a structured summary object containing model coefficients,
#' fit statistics, and random effects information.
#'
#' @param object A beezdemand_nlme object
#' @param report_space Character. Reporting space for core parameters. One of
#'   `"natural"` or `"log10"` (default depends on `param_space` used for fitting).
#' @param ... Additional arguments (passed to summary.nlme)
#' @return A `summary.beezdemand_nlme` object (inherits from
#'   `beezdemand_summary`) with fields including:
#'   - `call`: The original function call
#'   - `model_class`: "beezdemand_nlme"
#'   - `backend`: "nlme"
#'   - `equation_form`: The equation form used ("zben" or "simplified")
#'   - `coefficients`: Tibble of fixed effects with std.error, statistic, p.value
#'   - `random_effects`: VarCorr output for random effects
#'   - `logLik`, `AIC`, `BIC`: Model fit statistics
#' @export
summary.beezdemand_nlme <- function(
  object,
  report_space = c("natural", "log10"),
  ...
) {
  report_space <- match.arg(report_space)
  # Handle failed models
  if (is.null(object$model)) {
    return(structure(
      list(
        call = object$call,
        model_class = "beezdemand_nlme",
        backend = "nlme",
        failed = TRUE,
        fail_reason = object$error_message %||% "Unknown error",
        equation_form = object$param_info$equation_form %||% NA_character_,
        formula = NULL,
        factors = object$param_info$factors,
        nobs = NA_integer_,
        n_subjects = NA_integer_,
        converged = FALSE,
        logLik = NA_real_,
        AIC = NA_real_,
        BIC = NA_real_,
        coefficients = beezdemand_empty_coefficients(),
        derived_metrics = beezdemand_empty_derived_metrics(),
        fixed_effects = NULL,
        random_effects = NULL,
        notes = c("Model fitting failed", object$error_message)
      ),
      class = c("summary.beezdemand_nlme", "beezdemand_summary")
    ))
  }

  nlme_summary <- summary(object$model, ...)

  # Extract fixed effects table
  ttable <- nlme_summary$tTable
  internal_space <- object$param_space %||% object$param_info$param_space %||% "log10"
  coefficients <- tibble::tibble(
    term = rownames(ttable),
    estimate = ttable[, "Value"],
    std.error = ttable[, "Std.Error"],
    statistic = ttable[, "t-value"],
    p.value = ttable[, "p-value"],
    component = "fixed",
    estimate_scale = internal_space,
    term_display = vapply(rownames(ttable), beezdemand_term_display_space, character(1), report_space = internal_space)
  )

  if (report_space != internal_space) {
    coefficients <- beezdemand_transform_coef_table(
      coef_tbl = coefficients,
      report_space = report_space,
      internal_space = internal_space
    )
    coefficients$statistic <- coefficients$estimate / coefficients$std.error
    coefficients$p.value <- 2 * stats::pnorm(-abs(coefficients$statistic))
  }

  # Random effects structure
  random_effects <- nlme::VarCorr(object$model)

  # Get n_obs and n_subjects
  n_obs <- tryCatch(
    NROW(object$data) %||% NA_integer_,
    error = function(e) NA_integer_
  )
  if (is.na(n_obs) || length(n_obs) == 0) {
    n_obs <- tryCatch(NROW(nlme::getData(object$model)), error = function(e) NA_integer_)
  }

  n_subjects <- tryCatch({
    id_var <- object$param_info$id_var
    if (!is.null(id_var) && !is.null(object$data) && id_var %in% names(object$data)) {
      length(unique(object$data[[id_var]]))
    } else {
      length(unique(object$model$groups[[1]]))
    }
  }, error = function(e) NA_integer_)

  structure(
    list(
      call = object$call,
      model_class = "beezdemand_nlme",
      backend = "nlme",
      failed = FALSE,
      equation_form = object$param_info$equation_form %||%
        object$formula_details$equation_form_selected,
      param_space = internal_space,
      report_space = report_space,
      formula = object$formula_details$nlme_model_formula_obj,
      factors = object$param_info$factors,
      factor_interaction = object$param_info$factor_interaction,
      id_var = object$param_info$id_var,
      nobs = n_obs,
      n_subjects = n_subjects,
      converged = TRUE,
      logLik = as.numeric(stats::logLik(object$model)),
      AIC = stats::AIC(object$model),
      BIC = stats::BIC(object$model),
      sigma = object$model$sigma,
      coefficients = coefficients,
      derived_metrics = beezdemand_empty_derived_metrics(),
      fixed_effects = ttable,
      random_effects = random_effects,
      notes = character(0)
    ),
    class = c("summary.beezdemand_nlme", "beezdemand_summary")
  )
}

#' Print method for summary.beezdemand_nlme
#'
#' @param x A summary.beezdemand_nlme object
#' @param digits Number of significant digits to print
#' @param n Number of rows to print for any tables (unused for this class).
#' @param ... Additional arguments (ignored)
#' @export
print.summary.beezdemand_nlme <- function(x, digits = 4, n = Inf, ...) {
  cat("\n")
  cat("Nonlinear Mixed-Effects Demand Model Summary\n")
  cat(strrep("=", 50), "\n\n")

  if (isTRUE(x$failed)) {
    cat("MODEL FITTING FAILED\n")
    cat("Reason:", x$fail_reason, "\n")
    return(invisible(x))
  }

  # Model specification
  cat("Model Specification:\n")
  cat("  Equation form:", x$equation_form, "\n")
  if (!is.null(x$factors) && length(x$factors) > 0) {
    cat("  Factors:", paste(x$factors, collapse = ", "), "\n")
    cat("  Interaction:", x$factor_interaction, "\n")
  }
  cat("  ID variable:", x$id_var, "\n")
  cat("\n")

  # Data summary
  cat("Data Summary:\n")
  cat("  Subjects:", x$n_subjects, "\n")
  cat("  Observations:", x$nobs, "\n\n")

  # Fixed effects - use transformed coefficients if available
  cat("Fixed Effects:\n")
  if (!is.null(x$coefficients) && nrow(x$coefficients) > 0) {
    # Build a coefficient matrix from the transformed coefficients tibble
    coef_df <- x$coefficients[x$coefficients$component == "fixed", , drop = FALSE]
    if (nrow(coef_df) > 0) {
      coef_mat <- matrix(
        c(coef_df$estimate, coef_df$std.error, coef_df$statistic, coef_df$p.value),
        ncol = 4,
        dimnames = list(
          coef_df$term_display,
          c("Value", "Std.Error", "t-value", "p-value")
        )
      )
      # Use DF from original fixed_effects if available
      if (!is.null(x$fixed_effects) && "DF" %in% colnames(x$fixed_effects)) {
        df_col <- x$fixed_effects[, "DF", drop = TRUE]
        coef_mat <- cbind(coef_mat[, 1:2, drop = FALSE], DF = df_col, coef_mat[, 3:4, drop = FALSE])
      }
      stats::printCoefmat(coef_mat, digits = digits, ...)
    } else {
      stats::printCoefmat(x$fixed_effects, digits = digits, ...)
    }
  } else {
    stats::printCoefmat(x$fixed_effects, digits = digits, ...)
  }
  cat("\n")

  # Random effects
  cat("Random Effects:\n")
  print(x$random_effects)
  cat("\n")

  cat("Residual standard error:", round(x$sigma, digits), "\n\n")

  # Model fit
  cat("Model Fit:\n")
  cat("  Log-Likelihood:", round(x$logLik, 2), "\n")
  cat("  AIC:", round(x$AIC, 2), "\n")
  cat("  BIC:", round(x$BIC, 2), "\n")

  if (length(x$notes) > 0) {
    cat("\nNotes:\n")
    for (note in x$notes) {
      cat("  -", note, "\n")
    }
  }

  invisible(x)
}

#' Tidy method for beezdemand_nlme
#'
#' @param x A beezdemand_nlme object
#' @param effects Which effects to include: "fixed" (default), "ran_pars", or both
#' @param report_space Character. Reporting space for core parameters. One of
#'   `"natural"` or `"log10"` (default depends on `param_space` used for fitting).
#' @param ... Additional arguments (ignored)
#' @return A tibble of model coefficients with columns:
#'   - `term`: Parameter name
#'   - `estimate`: Point estimate
#'   - `std.error`: Standard error
#'   - `statistic`: t-value
#'   - `p.value`: P-value
#'   - `component`: "fixed" or "variance"
#' @export
tidy.beezdemand_nlme <- function(
  x,
  effects = c("fixed", "ran_pars"),
  report_space = c("natural", "log10"),
  ...
) {
  report_space <- match.arg(report_space)
  if (is.null(x$model)) {
    return(beezdemand_empty_coefficients())
  }

  effects <- match.arg(effects, several.ok = TRUE)
  result <- tibble::tibble()
  internal_space <- x$param_space %||% x$param_info$param_space %||% "log10"

  if ("fixed" %in% effects) {
    nlme_summary <- summary(x$model)
    ttable <- nlme_summary$tTable
    fixed <- tibble::tibble(
      term = rownames(ttable),
      estimate = ttable[, "Value"],
      std.error = ttable[, "Std.Error"],
      statistic = ttable[, "t-value"],
      p.value = ttable[, "p-value"],
      component = "fixed",
      estimate_scale = internal_space,
      term_display = vapply(rownames(ttable), beezdemand_term_display_space, character(1), report_space = internal_space)
    )

    fixed <- beezdemand_transform_coef_table(
      coef_tbl = fixed,
      report_space = report_space,
      internal_space = internal_space
    )

    if (report_space != internal_space) {
      fixed <- fixed |>
        dplyr::mutate(
          statistic = .data$estimate / .data$std.error,
          p.value = 2 * stats::pnorm(-abs(.data$statistic))
        )
    }
    result <- dplyr::bind_rows(result, fixed)
  }

  if ("ran_pars" %in% effects) {
    # Extract variance components from VarCorr
    vc <- nlme::VarCorr(x$model)
    # VarCorr returns a matrix-like object; extract variances
    if (is.matrix(vc) || is.data.frame(vc)) {
      var_names <- rownames(vc)
      # The "Variance" column contains the variance estimates
      if ("Variance" %in% colnames(vc)) {
        variances <- as.numeric(vc[, "Variance"])
        var_tidy <- tibble::tibble(
          term = var_names,
          estimate = variances,
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          component = "variance",
          estimate_scale = "natural",
          term_display = var_names
        )
        result <- dplyr::bind_rows(result, var_tidy)
      }
    }
  }

  result
}

#' Glance method for beezdemand_nlme
#'
#' @param x A beezdemand_nlme object
#' @param ... Additional arguments (ignored)
#' @return A one-row tibble of model statistics with columns:
#'   - `model_class`: "beezdemand_nlme"
#'   - `backend`: "nlme"
#'   - `equation_form`: The equation form used
#'   - `nobs`: Number of observations
#'   - `n_subjects`: Number of subjects
#'   - `converged`: Convergence status
#'   - `logLik`, `AIC`, `BIC`: Model fit statistics
#'   - `sigma`: Residual standard error
#' @export
glance.beezdemand_nlme <- function(x, ...) {
  if (is.null(x$model)) {
    return(tibble::tibble(
      model_class = "beezdemand_nlme",
      backend = "nlme",
      equation_form = x$param_info$equation_form %||% NA_character_,
      nobs = NA_integer_,
      n_subjects = NA_integer_,
      converged = FALSE,
      logLik = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      sigma = NA_real_
    ))
  }

  n_obs <- tryCatch(
    NROW(x$data) %||% NA_integer_,
    error = function(e) NA_integer_
  )
  if (is.na(n_obs) || length(n_obs) == 0) {
    n_obs <- tryCatch(NROW(nlme::getData(x$model)), error = function(e) NA_integer_)
  }

  n_subjects <- tryCatch({
    id_var <- x$param_info$id_var
    if (!is.null(id_var) && !is.null(x$data) && id_var %in% names(x$data)) {
      length(unique(x$data[[id_var]]))
    } else {
      length(unique(x$model$groups[[1]]))
    }
  }, error = function(e) NA_integer_)

  tibble::tibble(
    model_class = "beezdemand_nlme",
    backend = "nlme",
    equation_form = x$param_info$equation_form %||%
      x$formula_details$equation_form_selected,
    nobs = n_obs,
    n_subjects = n_subjects,
    converged = TRUE,
    logLik = as.numeric(stats::logLik(x$model)),
    AIC = stats::AIC(x$model),
    BIC = stats::BIC(x$model),
    sigma = x$model$sigma
  )
}

#' Confidence Intervals for Mixed-Effects Demand Model Parameters
#'
#' Computes confidence intervals for fixed effect parameters from an NLME-based
#' mixed-effects demand model.
#'
#' @param object A `beezdemand_nlme` object from [fit_demand_mixed()].
#' @param parm Character vector of parameter names to compute CIs for.
#'   Default includes all fixed effect parameters.
#' @param level Confidence level (default 0.95).
#' @param method Character. Method for computing intervals:
#'   - `"wald"`: Wald-type intervals using asymptotic normality (default, fast)
#'   - `"profile"`: Profile likelihood intervals via `nlme::intervals()` (slower
#'     but more accurate for small samples)
#' @param ... Additional arguments passed to `nlme::intervals()` when
#'   `method = "profile"`.
#'
#' @return A tibble with columns: `term`, `estimate`, `conf.low`, `conf.high`,
#'   `level`, `component`.
#'
#' @details
#' For Wald intervals, confidence bounds are computed as estimate ± z * SE
#' using standard errors from the model summary.
#'
#' For profile intervals, `nlme::intervals()` is called on the underlying
#' nlme model object. This method provides more accurate intervals but can be
#' computationally intensive for complex models.
#'
#' @examples
#' \dontrun{
#' fit <- fit_demand_mixed(data, y_var = "y", x_var = "x", id_var = "id")
#' confint(fit)
#' confint(fit, level = 0.90)
#' confint(fit, method = "profile")  # More accurate but slower
#' }
#'
#' @importFrom stats qnorm
#' @export
confint.beezdemand_nlme <- function(
  object,
  parm = NULL,
  level = 0.95,
  method = c("wald", "profile"),
  ...
) {
  method <- match.arg(method)

  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("`level` must be a single number between 0 and 1.", call. = FALSE)
  }

  if (is.null(object$model)) {
    warning("No model found in object. Model fitting may have failed.", call. = FALSE)
    return(tibble::tibble(
      term = character(),
      estimate = numeric(),
      conf.low = numeric(),
      conf.high = numeric(),
      level = numeric(),
      component = character()
    ))
  }

  if (method == "profile") {
    # Use nlme::intervals() for profile-based intervals
    int_result <- tryCatch(
      nlme::intervals(object$model, level = level, which = "fixed", ...),
      error = function(e) {
        warning(
          "Profile intervals failed: ", conditionMessage(e),
          "\nFalling back to Wald intervals.", call. = FALSE
        )
        return(NULL)
      }
    )

    if (!is.null(int_result) && "fixed" %in% names(int_result)) {
      fixed_int <- int_result$fixed
      terms <- rownames(fixed_int)

      if (!is.null(parm)) {
        keep <- terms %in% parm
        fixed_int <- fixed_int[keep, , drop = FALSE]
        terms <- terms[keep]
      }

      return(tibble::tibble(
        term = terms,
        estimate = fixed_int[, "est."],
        conf.low = fixed_int[, "lower"],
        conf.high = fixed_int[, "upper"],
        level = level,
        component = "fixed"
      ))
    }
    # Fall through to Wald if profile failed
  }

  # Wald-type intervals
  summ <- summary(object$model)
  fixed_table <- summ$tTable

  if (is.null(fixed_table) || nrow(fixed_table) == 0) {
    return(tibble::tibble(
      term = character(),
      estimate = numeric(),
      conf.low = numeric(),
      conf.high = numeric(),
      level = numeric(),
      component = character()
    ))
  }

  terms <- rownames(fixed_table)
  estimates <- fixed_table[, "Value"]
  se <- fixed_table[, "Std.Error"]

  if (!is.null(parm)) {
    keep <- terms %in% parm
    terms <- terms[keep]
    estimates <- estimates[keep]
    se <- se[keep]
  }

  if (length(terms) == 0) {
    warning("No requested parameters found in model.", call. = FALSE)
    return(tibble::tibble(
      term = character(),
      estimate = numeric(),
      conf.low = numeric(),
      conf.high = numeric(),
      level = numeric(),
      component = character()
    ))
  }

  z <- stats::qnorm((1 + level) / 2)

  tibble::tibble(
    term = terms,
    estimate = unname(estimates),
    conf.low = unname(estimates - z * se),
    conf.high = unname(estimates + z * se),
    level = level,
    component = "fixed"
  )
}

#' Extract Coefficients from a beezdemand_nlme Model
#'
#' Provides methods to extract fixed effects, random effects, or subject-specific
#' (combined fixed + random) coefficients from a `beezdemand_nlme` object.
#' This is an S3 method for the generic `coef` function.
#'
#' @param object A `beezdemand_nlme` object.
#' @param type Character, type of coefficients to extract. One of:
#'   \itemize{
#'     \item `"fixed"`: Returns only fixed effects (equivalent to `fixef(object)`).
#'     \item `"random"`: Returns only random effects (equivalent to `ranef(object)`).
#'     \item `"combined"` (default): Returns subject-specific coefficients, where each
#'           subject's coefficient is the sum of the corresponding fixed effect
#'           and that subject's random effect deviation. This is equivalent to
#'           what `stats::coef()` on an `nlme` object returns.
#'   }
#' @param report_space Character. One of `"internal"` (default), `"natural"`, or `"log10"`.
#' @param ... Additional arguments passed to the underlying `nlme` coefficient extraction
#'   functions (`nlme::fixef()`, `nlme::ranef()`, or `stats::coef.nlme()`).
#'
#' @return Depending on `type`:
#'   \itemize{
#'     \item `type="fixed"`: A named numeric vector of fixed-effect coefficients.
#'     \item `type="random"`: A data frame (or list of data frames if multiple levels of grouping)
#'           of random effects, as returned by `ranef.nlme()`.
#'     \item `type="combined"`: A data frame where rows are subjects (from `id_var`)
#'           and columns are the Q0 and alpha parameters, representing subject-specific
#'           estimates (on the log10 scale).
#'   }
#' @export
#' @method coef beezdemand_nlme
#' @seealso \code{\link{fixef.beezdemand_nlme}}, \code{\link{ranef.beezdemand_nlme}}
#'
#' @examples
#' \dontrun{
#' # Assuming 'fit_one_factor' is a successfully fitted beezdemand_nlme object
#' if (!is.null(fit_one_factor$model)) {
#'   # Get fixed effects
#'   fixed_coeffs <- coef(fit_one_factor, type = "fixed")
#'   print(fixed_coeffs)
#'
#'   # Get random effects
#'   random_effects_summary <- coef(fit_one_factor, type = "random")
#'   print(random_effects_summary)
#'
#'   # Get subject-specific coefficients (default)
#'   subject_coeffs <- coef(fit_one_factor) # or type = "combined"
#'   print(subject_coeffs)
#' }
#' }
coef.beezdemand_nlme <- function(
  object,
  type = "combined",
  report_space = c("internal", "natural", "log10"),
  ...
) {
  report_space <- match.arg(report_space)
  if (!inherits(object, "beezdemand_nlme")) {
    stop("Input 'object' must be of class 'beezdemand_nlme'.")
  }
  if (is.null(object$model)) {
    stop("No model found in the object. Fitting may have failed.")
  }

  model <- object$model
  internal_space <- object$param_space %||% object$param_info$param_space %||% "log10"
  requested_space <- if (report_space == "internal") internal_space else report_space

  res <- switch(
    type,
    fixed = {
      nlme::fixef(model, ...)
    },
    random = {
      nlme::ranef(model, ...)
    },
    combined = {
      # This gets subject-specific estimates (fixed + random)
      # These are on the same scale as the model parameters (log10 for Q0, alpha)
      stats::coef(model, ...)
    },
    stop("Invalid 'type'. Choose from 'fixed', 'random', or 'combined'.")
  )

  if (identical(type, "random") || identical(requested_space, internal_space)) {
    return(res)
  }

  if (is.numeric(res) && !is.null(names(res))) {
    out <- res
    idx <- grepl("^Q0", names(out)) | grepl("^alpha", names(out))
    if (any(idx)) {
      if (internal_space == "log10" && requested_space == "natural") {
        out[idx] <- 10^out[idx]
      } else if (internal_space == "natural" && requested_space == "log10") {
        out[idx] <- log10(out[idx])
      }
    }
    return(out)
  }

  if (is.data.frame(res)) {
    out <- res
    for (col in intersect(c("Q0", "alpha"), names(out))) {
      if (internal_space == "log10" && requested_space == "natural") {
        out[[col]] <- 10^out[[col]]
      } else if (internal_space == "natural" && requested_space == "log10") {
        out[[col]] <- log10(out[[col]])
      }
    }
    return(out)
  }

  res
}

#' Extract Fixed Effects from a beezdemand_nlme Model
#'
#' S3 method for `fixef` for objects of class `beezdemand_nlme`.
#' Extracts the fixed-effect coefficients from the fitted `nlme` model.
#'
#' @param object A `beezdemand_nlme` object.
#' @param ... Additional arguments passed to `nlme::fixef()`.
#' @return A named numeric vector of fixed-effect coefficients.
#' @export
#' @method fixef beezdemand_nlme
#' @seealso \code{\link{coef.beezdemand_nlme}}, \code{\link{ranef.beezdemand_nlme}}
fixef.beezdemand_nlme <- function(object, ...) {
  if (!inherits(object, "beezdemand_nlme")) {
    stop("Input 'object' must be of class 'beezdemand_nlme'.")
  }
  if (is.null(object$model)) {
    stop("No model found in the object. Fitting may have failed.")
  }
  nlme::fixef(object$model, ...)
}

#' Extract Random Effects from a beezdemand_nlme Model
#'
#' S3 method for `ranef` for objects of class `beezdemand_nlme`.
#' Extracts the random effects from the fitted `nlme` model.
#'
#' @param object A `beezdemand_nlme` object.
#' @param ... Additional arguments passed to `nlme::ranef()`.
#' @return A data frame (or list of data frames if multiple levels of grouping)
#'   of random effects, as returned by `ranef.nlme()`.
#' @export
#' @method ranef beezdemand_nlme
#' @seealso \code{\link{coef.beezdemand_nlme}}, \code{\link{fixef.beezdemand_nlme}}
ranef.beezdemand_nlme <- function(object, ...) {
  if (!inherits(object, "beezdemand_nlme")) {
    stop("Input 'object' must be of class 'beezdemand_nlme'.")
  }
  if (is.null(object$model)) {
    stop("No model found in the object. Fitting may have failed.")
  }
  nlme::ranef(object$model, ...)
}

#' Predict Method for beezdemand_nlme Objects
#'
#' Generates point predictions from a fitted `beezdemand_nlme` model.
#' Predictions can be made at the population level (fixed effects only) or
#' group/subject level (fixed + random effects). The output scale depends
#' on the `equation_form` used during model fitting and whether `inv_fun` is applied.
#'
#' @param object A `beezdemand_nlme` object.
#' @param newdata Optional data frame for which to make predictions.
#'   Must contain `x_var` and all `factors` specified in the original model.
#'   If group-level predictions are desired (`level=1`), the `id_var` column from
#'   the original fit must also be present in `newdata` and its levels should
#'   correspond to those in the original data for meaningful random effect application.
#'   If `NULL`, predictions are made for the data used in fitting the model.
#' @param type One of `"response"` (default), `"link"`, `"population"`, or `"individual"`.
#'   `"population"` and `"individual"` are aliases that set `level` to `0` or `1`,
#'   respectively.
#' @param level Integer, prediction level for `nlme::predict.nlme()`:
#'   \itemize{
#'     \item `0`: Population predictions (based on fixed effects only).
#'     \item `1` (or higher, up to number of grouping levels in model): Group-specific
#'           predictions (fixed effects + random effects for the specified `id_var` level).
#'   }
#'   Default is `0`.
#' @param inv_fun Optional function to inverse-transform the predictions.
#'   Example: If `y_var` was log10-transformed during fitting and `equation_form`
#'   like "zben" produces predictions on that log10 scale, `inv_fun = function(x) 10^x`
#'   would convert predictions back to the original consumption scale.
#'   If `equation_form` was "simplified" (which models raw Y), `inv_fun` might be `identity`
#'   or not needed if predictions are already on the desired scale.
#' @param se.fit Logical; if `TRUE`, includes a `.se.fit` column (currently `NA`
#'   because standard errors are not implemented for `beezdemand_nlme` predictions).
#' @param interval One of `"none"` (default) or `"confidence"`. When requested,
#'   `.lower`/`.upper` are returned as `NA`.
#' @param interval_level Confidence level when `interval = "confidence"`. Currently
#'   used only for validation.
#' @param ... Additional arguments passed to `nlme::predict.nlme()`.
#'
#' @return A tibble containing the original `newdata` columns plus `.fitted`.
#'   When requested, `.se.fit` and `.lower`/`.upper` are included (currently `NA`).
#'
#' @method predict beezdemand_nlme
#' @export
#' @seealso \code{\link[nlme]{predict.nlme}}
#'
#' @examples
#' \dontrun{
#' # Assuming 'fit_one_factor' is a successfully fitted beezdemand_nlme object
#' # (e.g., using equation_form = "zben", y_var = "y_ll4")
#'
#' if (!is.null(fit_one_factor$model)) {
#'   # Population-level predictions for the original data
#'   preds_pop_log_scale <- predict(fit_one_factor, level = 0)
#'
#'   # Back-transform to natural scale
#'   preds_pop_natural_scale <- predict(fit_one_factor, level = 0, inv_fun = function(x) 10^x)
#'
#'   # Create some new data for prediction
#'   # Ensure all necessary columns (x, factors, id if level=1) are present
#'   # and factors have levels consistent with the model fit.
#'
#'   # Example: Predict for the first few rows of original data but at group level
#'   # Make sure the id and factor levels in new_data_subset exist in original data
#'   new_data_subset <- fit_one_factor$data[1:5, ]
#'
#'   preds_group_log_scale <- predict(fit_one_factor, newdata = new_data_subset, level = 1)
#'
#'   # If your model was, for example:
#'   # fit_simplified_raw_y <- fit_demand_mixed(data=ko, y_var="y", x_var="x",
#'   #                                          id_var="id", factors="dose",
#'   #                                          equation_form="simplified")
#'   # if (!is.null(fit_simplified_raw_y$model)) {
#'   #   preds_simplified_raw <- predict(fit_simplified_raw_y) # Already on raw y scale
#'   # }
#' }
#' }
predict.beezdemand_nlme <- function(
  object,
  newdata = NULL,
  type = c("response", "link", "population", "individual"),
  level = 0,
  inv_fun = identity,
  se.fit = FALSE,
  interval = c("none", "confidence"),
  interval_level = 0.95,
  ...
) {
  type <- match.arg(type)
  interval <- match.arg(interval)
  if (!is.null(interval_level) && (!is.numeric(interval_level) || length(interval_level) != 1 ||
    is.na(interval_level) || interval_level <= 0 || interval_level >= 1)) {
    stop("'interval_level' must be a single number between 0 and 1.", call. = FALSE)
  }
  if (!inherits(object, "beezdemand_nlme")) {
    stop("Input 'object' must be of class 'beezdemand_nlme'.")
  }
  if (is.null(object$model)) {
    stop("No model found in the object. Fitting may have failed.")
  }

  if (type == "population") level <- 0
  if (type == "individual") level <- 1

  # Validate newdata if provided
  if (!is.null(newdata)) {
    # Ensure newdata has the necessary columns (x_var and any factors)
    # and that factor levels are consistent with the model's training data.
    # This is crucial for nlme::predict.nlme to work correctly.

    # Check for x_var
    x_var_name <- object$param_info$x_var
    if (!(x_var_name %in% names(newdata))) {
      stop("Column '", x_var_name, "' (x_var) not found in newdata.")
    }

    # Check for factors and set levels
    model_factors <- object$param_info$factors
    if (!is.null(model_factors)) {
      for (fac in model_factors) {
        if (!(fac %in% names(newdata))) {
          stop("Factor column '", fac, "' not found in newdata.")
        }
        # Ensure newdata factor has levels from original data
        original_levels <- levels(object$data[[fac]])
        if (!is.factor(newdata[[fac]])) {
          newdata[[fac]] <- factor(newdata[[fac]], levels = original_levels)
        } else {
          # Check if all levels in newdata's factor are present in original_levels
          if (!all(levels(newdata[[fac]]) %in% original_levels)) {
            # Attempt to relevel; warn if new levels are introduced that weren't in original
            unknown_levels <- setdiff(levels(newdata[[fac]]), original_levels)
            if (length(unknown_levels) > 0) {
              warning(
                "Newdata factor '",
                fac,
                "' contains levels not present in original data: ",
                paste(unknown_levels, collapse = ", "),
                ". Predictions for these may be unreliable or cause errors."
              )
            }
            # Relevel to match original data, NAs for unknown levels if not already handled
            newdata[[fac]] <- factor(newdata[[fac]], levels = original_levels)
          }
        }
      }
    }

    # Check for id_var if level > 0
    if (level > 0) {
      id_var_name <- object$param_info$id_var
      if (!(id_var_name %in% names(newdata))) {
        stop(
          "Column '",
          id_var_name,
          "' (id_var) not found in newdata, but level > 0 specified."
        )
      }
      # Ensure id_var in newdata has levels from original data
      original_id_levels <- levels(object$data[[id_var_name]])
      if (!is.factor(newdata[[id_var_name]])) {
        newdata[[id_var_name]] <- factor(
          newdata[[id_var_name]],
          levels = original_id_levels
        )
      } else {
        if (!all(levels(newdata[[id_var_name]]) %in% original_id_levels)) {
          unknown_id_levels <- setdiff(
            levels(newdata[[id_var_name]]),
            original_id_levels
          )
          if (length(unknown_id_levels) > 0) {
            warning(
              "Newdata id_var '",
              id_var_name,
              "' contains levels not present in original data: ",
              paste(unknown_id_levels, collapse = ", "),
              ". Random effects for these will be treated as zero."
            )
          }
          newdata[[id_var_name]] <- factor(
            newdata[[id_var_name]],
            levels = original_id_levels
          )
        }
      }
    } # end newdata validation
  } else {
    newdata <- object$data
  }

  # Use the predict method for nlme objects
  raw_predictions <- stats::predict(
    object$model,
    newdata = newdata,
    level = level,
    ...
  )

  fitted <- if (type == "link") {
    as.numeric(raw_predictions)
  } else {
    as.numeric(inv_fun(raw_predictions))
  }

  out <- tibble::as_tibble(newdata)
  out$.fitted <- fitted

  if (isTRUE(se.fit) || interval != "none") {
    warning(
      "Standard errors/intervals are not implemented for `beezdemand_nlme` predictions; returning NA.",
      call. = FALSE
    )
    out$.se.fit <- NA_real_
    if (interval != "none") {
      out$.lower <- NA_real_
      out$.upper <- NA_real_
    }
  }

  out
}

#' Plot Method for beezdemand_nlme Objects
#'
#' Creates a ggplot2 visualization of a fitted `beezdemand_nlme` model,
#' showing observed data points and/or model prediction lines.
#'
#' @param x A `beezdemand_nlme` object.
#' @param type Plot type: "demand", "population", "individual", or "both".
#' @param ids Optional vector of subject IDs to plot.
#' @param show_observed Logical. If TRUE, plots the original data points. Default `TRUE`.
#' @param observed_point_alpha Alpha for observed points. Default `0.6`.
#' @param show_pred Which prediction layers to plot: "population", "individual",
#'   or "both".
#' @param n_points Integer. Number of points for prediction lines. Default `100`.
#' @param inv_fun Optional function to inverse-transform y-axis and predictions. Default `identity`.
#' @param facet Optional faceting formula (e.g., `~ dose`).
#' @param at Optional named list giving values for continuous covariates used in the
#'   fixed-effects RHS. When building prediction grids for population- or individual-
#'   level lines, these values will be used. If not provided, the function will
#'   default to the median of each continuous covariate found in the original
#'   model data. Factor variables are always handled as grids (population) or
#'   observed combinations (individual) as before.
#' @param color_by Optional character string: name of a factor to color lines and/or points by.
#'   Must be a column in `x$data`.
#' @param linetype_by Optional character string: name of a factor for linetypes of population prediction lines
#'   if individual lines are also shown (otherwise applies to the shown lines).
#'   Must be a model factor in `x$param_info$factors`.
#' @param shape_by Optional character string: name of a factor for shapes of observed points.
#'   Must be a column in `x$data`.
#' @param x_trans Character. Transformation for x-axis. Default "log".
#' @param y_trans Character. Transformation for y-axis. Default "log".
#' @param free_trans Value used to display free (x = 0) on log scales. Use NULL
#'   to drop x <= 0 values instead.
#' @param x_limits Optional numeric vector of length 2 for x-axis limits.
#' @param y_limits Optional numeric vector of length 2 for y-axis limits.
#' @param style Plot styling, passed to \code{theme_beezdemand()}.
#' @param title Optional plot title.
#' @param subtitle Optional subtitle for the plot.
#' @param x_lab Optional x-axis label.
#' @param y_lab Optional y-axis label.
#' @param xlab Deprecated alias for \code{x_lab}.
#' @param ylab Deprecated alias for \code{y_lab}.
#' @param observed_point_size Size for observed points. Default `2`.
#' @param pop_line_size Size for population prediction lines. Default `1`.
#' @param ind_line_size Size for individual prediction lines. Default `0.6`.
#' @param pop_line_alpha Alpha for population prediction lines. Default `0.9`.
#' @param ind_line_alpha Alpha for individual prediction lines. Default `0.3`.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2 object.
#'
#' @import ggplot2
#' @importFrom rlang .data !! sym syms !!! expr
#' @importFrom scales log10_trans pseudo_log_trans identity_trans
#' @importFrom dplyr group_by summarise n select distinct arrange across all_of
#' @importFrom tidyr expand_grid
#' @importFrom stats terms as.formula
#' @export
#' @method plot beezdemand_nlme
plot.beezdemand_nlme <- function(
  x,
  type = c("demand", "population", "individual", "both"),
  ids = NULL,
  show_observed = TRUE,
  observed_point_alpha = 0.6,
  show_pred = "population",
  n_points = 200,
  inv_fun = identity,
  facet = NULL,
  at = NULL,
  color_by = NULL,
  linetype_by = NULL,
  shape_by = NULL,
  x_trans = c("log10", "log", "linear", "pseudo_log"),
  y_trans = NULL,
  free_trans = 0.01,
  x_limits = NULL,
  y_limits = NULL,
  style = c("modern", "apa"),
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  xlab = NULL,
  ylab = NULL,
  observed_point_size = 2,
  pop_line_size = 1, # New: size for population lines
  ind_line_size = 0.6, # New: size for individual lines
  pop_line_alpha = 0.9, # New: alpha for population lines
  ind_line_alpha = 0.3, # New: alpha for individual lines
  ...
) {
  fit_obj <- x
  if (!inherits(fit_obj, "beezdemand_nlme") || is.null(fit_obj$model)) {
    stop("A valid 'beezdemand_nlme' object with a fitted model is required.")
  }
  type <- match.arg(type)
  style <- match.arg(style)
  x_trans <- match.arg(x_trans)
  y_trans_missing <- is.null(y_trans)

  labels <- beezdemand_normalize_plot_labels(x_lab, y_lab, xlab, ylab)
  xlab <- labels$x_lab
  ylab <- labels$y_lab

  if (type == "population") {
    show_pred <- "population"
  } else if (type == "individual") {
    show_pred <- "individual"
  } else if (type == "both") {
    show_pred <- "both"
  }

  show_pred_lines <- beezdemand_normalize_show_pred(show_pred)
  show_observed_data <- show_observed
  n_points_pred <- n_points
  facet_formula <- facet

  plot_data_orig <- fit_obj$data
  y_var_name <- fit_obj$param_info$y_var
  x_var_name <- fit_obj$param_info$x_var
  id_var_name <- fit_obj$param_info$id_var
  model_factors <- fit_obj$param_info$factors
  model_continuous <- fit_obj$param_info$continuous_covariates

  # Identify additional RHS variables from the stored fixed-effects formula strings
  # Check both Q0 and alpha formulas and union their variables (they may differ with collapse_levels)
  rhs_vars <- character(0)

  .extract_rhs_vars <- function(formula_str) {
    if (is.null(formula_str)) {
      return(character(0))
    }
    rhs_formula <- tryCatch(
      stats::as.formula(formula_str),
      error = function(e) NULL
    )
    if (is.null(rhs_formula)) {
      return(character(0))
    }
    vars <- tryCatch(
      all.vars(rhs_formula),
      error = function(e) character(0)
    )
    setdiff(vars, c("1"))
  }

  # Get vars from Q0 formula
  rhs_vars <- union(
    rhs_vars,
    .extract_rhs_vars(fit_obj$formula_details$fixed_effects_formula_str_Q0)
  )

  # Get vars from alpha formula
  rhs_vars <- union(
    rhs_vars,
    .extract_rhs_vars(fit_obj$formula_details$fixed_effects_formula_str_alpha)
  )

  # Identify collapsed factor columns from param_info (e.g., dose_alpha, dose_Q0)
  # These are factors but not in model_factors
  factors_Q0 <- fit_obj$param_info$factors_Q0 %||% model_factors
  factors_alpha <- fit_obj$param_info$factors_alpha %||% model_factors
  all_factor_cols <- unique(c(
    model_factors %||% character(0),
    factors_Q0 %||% character(0),
    factors_alpha %||% character(0)
  ))

  # Continuous candidates are RHS vars not declared as any kind of factor
  # AND not actually factor columns in the data
  cont_from_rhs <- setdiff(rhs_vars, all_factor_cols)
  # Further filter: only keep if it's truly numeric in the data
  if (length(cont_from_rhs) > 0) {
    is_numeric_mask <- vapply(
      cont_from_rhs,
      function(v) {
        v %in% names(plot_data_orig) && !is.factor(plot_data_orig[[v]])
      },
      logical(1)
    )
    cont_from_rhs <- cont_from_rhs[is_numeric_mask]
  }
  # Union with explicit metadata
  cont_covars_all <- unique(c(
    model_continuous %||% character(0),
    cont_from_rhs
  ))

  y_plot_col_name <- paste0(y_var_name, "_plotscale")
  plot_data_orig[[y_plot_col_name]] <- inv_fun(plot_data_orig[[y_var_name]])

  y_is_log <- identical(inv_fun, identity) &&
    grepl("^log", y_var_name, ignore.case = TRUE)
  if (y_trans_missing) {
    y_trans <- beezdemand_default_y_trans(type = type, y_is_log = y_is_log)
  }
  y_trans <- match.arg(y_trans, c("log10", "log", "linear", "pseudo_log"))
  y_trans_res <- beezdemand_resolve_y_trans(y_trans, y_is_log = y_is_log)
  y_trans <- y_trans_res$y_trans
  beezdemand_warn_log_override(y_trans_res$adjusted)

  if (!is.null(ids) && id_var_name %in% names(plot_data_orig)) {
    ids <- as.character(ids)
    plot_data_orig <- plot_data_orig[
      as.character(plot_data_orig[[id_var_name]]) %in% ids,
      ,
      drop = FALSE
    ]
  }

  free_trans_used <- FALSE
  subtitle_note <- FALSE
  free_obs <- beezdemand_apply_free_trans(plot_data_orig, x_var_name, x_trans, free_trans)
  plot_data_orig <- free_obs$data
  free_trans_used <- free_trans_used || free_obs$replaced

  obs_y <- beezdemand_drop_nonpositive_y(plot_data_orig, y_plot_col_name, y_trans)
  plot_data_orig <- obs_y$data
  subtitle_note <- subtitle_note || obs_y$dropped

  if (is.null(xlab)) {
    xlab <- x_var_name
  }
  if (is.null(ylab)) {
    ylab <- if (identical(inv_fun, identity)) y_var_name else y_plot_col_name
    if (!identical(inv_fun, identity)) {
      ylab <- gsub("_plotscale", " (natural scale)", ylab, fixed = TRUE)
    }
  }
  if (is.null(title)) {
    title_base <- paste(
      "Demand Model Fit:",
      fit_obj$formula_details$equation_form_selected
    )
    title <- if (!is.null(model_factors)) {
      paste(title_base, "by", paste(model_factors, collapse = " & "))
    } else {
      title_base
    }
  }

  p <- ggplot2::ggplot()

  if (show_observed_data) {
    aes_observed_list <- list(
      x = rlang::sym(x_var_name),
      y = rlang::sym(y_plot_col_name)
    )
    if (!is.null(color_by) && color_by %in% names(plot_data_orig)) {
      aes_observed_list$color <- rlang::sym(color_by)
    }
    if (!is.null(shape_by) && shape_by %in% names(plot_data_orig)) {
      aes_observed_list$shape <- rlang::sym(shape_by)
    }
    p <- p +
      ggplot2::geom_point(
        data = plot_data_orig,
        mapping = do.call(aes, aes_observed_list),
        size = observed_point_size,
        alpha = observed_point_alpha
      )
  }

  # Normalize show_pred_lines argument
  if (isTRUE(show_pred_lines)) {
    pred_levels_to_plot <- "population"
  } else if (isFALSE(show_pred_lines)) {
    pred_levels_to_plot <- character(0)
  } else {
    pred_levels_to_plot <- intersect(
      as.character(show_pred_lines),
      c("population", "individual")
    )
  }

  # --- Loop through prediction levels to plot (population and/or individual) ---
  for (pred_level_type in pred_levels_to_plot) {
    current_pred_level <- if (pred_level_type == "population") 0 else 1
    current_line_size <- if (pred_level_type == "population") {
      pop_line_size
    } else {
      ind_line_size
    }
    current_line_alpha <- if (pred_level_type == "population") {
      pop_line_alpha
    } else {
      ind_line_alpha
    }

    # Build prediction data using an x grid and either:
    # - full-factorial factor levels (population), or
    # - observed id × factor combinations (individual)
    x_grid <- seq(
      min(plot_data_orig[[x_var_name]], na.rm = TRUE),
      max(plot_data_orig[[x_var_name]], na.rm = TRUE),
      length.out = n_points_pred
    )
    if (current_pred_level == 0) {
      # Population-level grid: use OBSERVED factor combinations (not full factorial)
      # This is critical when collapse_levels creates derived columns (e.g., dose_alpha)
      # that are linked to original columns (e.g., dose). Full factorial would create
      # invalid combinations like dose="3e-05" with dose_alpha="bb".

      # Get all factor columns that exist in the data (original + collapsed)
      factor_cols_in_data <- all_factor_cols[
        all_factor_cols %in%
          names(plot_data_orig) &
          vapply(
            all_factor_cols,
            function(f) is.factor(plot_data_orig[[f]]),
            logical(1)
          )
      ]

      if (length(factor_cols_in_data) > 0) {
        # Get observed factor combinations from the data
        observed_factor_combos <- plot_data_orig |>
          dplyr::select(dplyr::all_of(factor_cols_in_data)) |>
          dplyr::distinct()

        # Ensure factor levels match the fitted data
        for (fac in factor_cols_in_data) {
          if (is.factor(fit_obj$data[[fac]])) {
            observed_factor_combos[[fac]] <- factor(
              as.character(observed_factor_combos[[fac]]),
              levels = levels(fit_obj$data[[fac]])
            )
          }
        }

        # Cross observed factor combinations with x grid
        pred_newdata <- tidyr::crossing(
          observed_factor_combos,
          !!rlang::sym(x_var_name) := x_grid
        )
      } else {
        # No factors - just use x grid
        pred_newdata <- tibble::tibble(!!rlang::sym(x_var_name) := x_grid)
      }

      # Add continuous covariates (single conditioning values)
      if (length(cont_covars_all) > 0) {
        for (cv in cont_covars_all) {
          if (
            cv %in% names(plot_data_orig) && !is.factor(plot_data_orig[[cv]])
          ) {
            val <- if (!is.null(at) && !is.null(at[[cv]])) {
              at[[cv]]
            } else {
              stats::median(plot_data_orig[[cv]], na.rm = TRUE)
            }
            pred_newdata[[cv]] <- val
          }
        }
      }
    } else {
      # Individual-level grid: only observed id × factor combinations
      if (!(id_var_name %in% names(plot_data_orig))) {
        warning(
          "id_var '",
          id_var_name,
          "' not found for group-level predictions. Skipping individual lines."
        )
        next # Skip this iteration for individual lines
      }

      # Include all factor columns (original + collapsed) that exist in data
      observed_factors <- if (length(all_factor_cols) > 0) {
        intersect(all_factor_cols, names(plot_data_orig))
      } else {
        character(0)
      }
      id_fac_cols <- c(id_var_name, observed_factors)

      id_fac_df <- plot_data_orig |>
        dplyr::select(dplyr::all_of(id_fac_cols)) |>
        dplyr::distinct()

      # Ensure id and factor columns carry the same levels as in fit_obj$data
      if (
        id_var_name %in%
          names(fit_obj$data) &&
          is.factor(fit_obj$data[[id_var_name]])
      ) {
        id_fac_df[[id_var_name]] <- factor(
          as.character(id_fac_df[[id_var_name]]),
          levels = levels(fit_obj$data[[id_var_name]])
        )
      }
      if (length(observed_factors) > 0) {
        for (fac in observed_factors) {
          if (fac %in% names(fit_obj$data) && is.factor(fit_obj$data[[fac]])) {
            id_fac_df[[fac]] <- factor(
              as.character(id_fac_df[[fac]]),
              levels = levels(fit_obj$data[[fac]])
            )
          }
        }
      }

      pred_newdata <- tidyr::crossing(
        id_fac_df,
        !!rlang::sym(x_var_name) := x_grid
      )
      # Add continuous covariates as columns with conditioning values
      if (length(cont_covars_all) > 0) {
        for (cv in cont_covars_all) {
          if (
            cv %in% names(plot_data_orig) && !is.factor(plot_data_orig[[cv]])
          ) {
            val <- if (!is.null(at) && !is.null(at[[cv]])) {
              at[[cv]]
            } else {
              stats::median(plot_data_orig[[cv]], na.rm = TRUE)
            }
            pred_newdata[[cv]] <- val
          }
        }
      }
    }

    # Ensure factors in pred_newdata have correct levels based on fit_obj$data
    factor_cols_to_fix <- setdiff(
      intersect(names(pred_newdata), names(fit_obj$data)),
      x_var_name
    )
    for (col_name in factor_cols_to_fix) {
      if (is.factor(fit_obj$data[[col_name]])) {
        pred_newdata[[col_name]] <- factor(
          as.character(pred_newdata[[col_name]]),
          levels = levels(fit_obj$data[[col_name]])
        )
      }
    }

    predicted_values_model_scale <- predict(
      fit_obj,
      newdata = pred_newdata,
      type = "link",
      level = current_pred_level
    )$.fitted
    pred_newdata$predicted_y_plotscale <- inv_fun(predicted_values_model_scale)

    free_pred <- beezdemand_apply_free_trans(
      pred_newdata,
      x_var_name,
      x_trans,
      free_trans
    )
    pred_newdata <- free_pred$data
    free_trans_used <- free_trans_used || free_pred$replaced

    pred_y <- beezdemand_drop_nonpositive_y(
      pred_newdata,
      "predicted_y_plotscale",
      y_trans
    )
    pred_newdata <- pred_y$data
    subtitle_note <- subtitle_note || pred_y$dropped

    # Sort pred_newdata
    grouping_vars_for_sort <- character(0)
    if (current_pred_level > 0 && id_var_name %in% names(pred_newdata)) {
      grouping_vars_for_sort <- c(grouping_vars_for_sort, id_var_name)
    }
    if (!is.null(model_factors)) {
      grouping_vars_for_sort <- c(
        grouping_vars_for_sort,
        intersect(model_factors, names(pred_newdata))
      )
    }
    grouping_vars_for_sort <- unique(grouping_vars_for_sort)

    if (length(grouping_vars_for_sort) > 0) {
      pred_newdata <- pred_newdata |>
        dplyr::arrange(
          dplyr::across(dplyr::all_of(grouping_vars_for_sort)),
          .data[[x_var_name]]
        )
    } else {
      pred_newdata <- pred_newdata |> dplyr::arrange(.data[[x_var_name]])
    }

    aes_pred_list <- list(
      x = rlang::sym(x_var_name),
      y = rlang::sym("predicted_y_plotscale")
    )
    line_group_vars <- character(0)

    # Grouping for individual lines: by id_var and any aesthetic factors
    if (current_pred_level > 0 && id_var_name %in% names(pred_newdata)) {
      line_group_vars <- c(line_group_vars, id_var_name)
    }

    # Aesthetics apply to both population and individual lines if specified
    # but population lines might simplify grouping if color/linetype not by id
    if (!is.null(color_by) && color_by %in% names(pred_newdata)) {
      aes_pred_list$color <- rlang::sym(color_by)
      if (!(color_by %in% line_group_vars)) {
        line_group_vars <- c(line_group_vars, color_by)
      }
    } else if (!is.null(color_by) && pred_level_type == "population") {
      # If color_by is specified but not a model factor (e.g., user wants to color all pop lines same)
      # This case is tricky; usually color_by is a factor in the data for lines.
      # For simplicity, if color_by not in pred_newdata, line color is default.
    }

    # Linetype: More common for population lines if individual lines also shown
    if (!is.null(linetype_by) && linetype_by %in% names(pred_newdata)) {
      aes_pred_list$linetype <- rlang::sym(linetype_by)
      if (!(linetype_by %in% line_group_vars)) {
        line_group_vars <- c(line_group_vars, linetype_by)
      }
    }

    # For population lines, ensure ALL model factors define distinct lines
    if (current_pred_level == 0 && !is.null(model_factors)) {
      for (fac in model_factors) {
        if (fac %in% names(pred_newdata) && !(fac %in% line_group_vars)) {
          line_group_vars <- c(line_group_vars, fac)
        }
      }
    }

    if (!is.null(facet_formula)) {
      parsed_facet_formula <- tryCatch(
        stats::as.formula(facet_formula),
        error = function(e) NULL
      )
      if (!is.null(parsed_facet_formula)) {
        tf <- stats::terms(parsed_facet_formula)
        facet_vars_actual <- c(
          if (attr(tf, "response") > 0) all.vars(lhs(parsed_facet_formula)),
          all.vars(rhs(parsed_facet_formula))
        )
        facet_vars_actual <- unique(facet_vars_actual[facet_vars_actual != "."])
        for (fv in facet_vars_actual) {
          clean_fv <- gsub("`", "", fv)
          if (
            clean_fv %in%
              names(pred_newdata) &&
              !(clean_fv %in% line_group_vars)
          ) {
            line_group_vars <- c(line_group_vars, clean_fv)
          }
        }
      }
    }
    line_group_vars <- unique(line_group_vars)

    if (length(line_group_vars) > 0) {
      aes_pred_list$group <- rlang::expr(interaction(
        !!!rlang::syms(line_group_vars),
        drop = TRUE
      ))
    } else {
      aes_pred_list$group <- 1
    }

    final_aes_pred <- do.call(ggplot2::aes, aes_pred_list)

    p <- p +
      ggplot2::geom_line(
        data = pred_newdata,
        mapping = final_aes_pred,
        linewidth = current_line_size,
        alpha = current_line_alpha
      )
  } # End loop over pred_level_type

  x_limits <- beezdemand_resolve_limits(x_limits, x_trans, axis = "x")
  y_limits <- beezdemand_resolve_limits(y_limits, y_trans, axis = "y")
  p <- p + ggplot2::scale_x_continuous(
    trans = beezdemand_get_trans(x_trans),
    limits = x_limits,
    labels = beezdemand_axis_labels()
  )
  p <- p + ggplot2::scale_y_continuous(
    trans = beezdemand_get_trans(y_trans),
    limits = y_limits,
    labels = beezdemand_axis_labels()
  )

  if (x_trans == "log10") {
    p <- p + ggplot2::annotation_logticks(sides = "b")
  }
  if (y_trans == "log10") {
    p <- p + ggplot2::annotation_logticks(sides = "l")
  }

  if (!is.null(facet_formula)) {
    if (is.character(facet_formula)) {
      facet_formula <- stats::as.formula(facet_formula)
    }
    p <- p + ggplot2::facet_wrap(facet_formula)
  }

  if (isTRUE(subtitle_note)) {
    if (is.null(subtitle)) {
      subtitle <- "Zeros omitted on log scale."
    } else {
      subtitle <- paste(subtitle, "Zeros omitted on log scale.")
    }
  }
  beezdemand_warn_free_trans(free_trans_used, free_trans)

  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    theme_beezdemand(style = style)

  legend_labs_list <- list()
  if (!is.null(color_by)) {
    legend_labs_list$color <- color_by
  }
  if (!is.null(shape_by)) {
    legend_labs_list$shape <- shape_by
  }
  if (!is.null(linetype_by)) {
    legend_labs_list$linetype <- linetype_by
  }

  if (length(legend_labs_list) > 0) {
    p <- p + do.call(ggplot2::labs, legend_labs_list)
  }

  if (!is.null(color_by)) {
    p <- beezdemand_apply_color_scale(p, style, plot_data_orig, color_by)
  }

  return(p)
}

#' Calculate Individual-Level Predicted Coefficients from beezdemand_nlme Model
#'
#' This function extracts and combines fixed and random effects to calculate
#' individual-level predicted coefficients for all parameter-factor combinations
#' from a beezdemand_nlme model object. It automatically detects the factor
#' structure and calculates coefficients for each individual and factor level.
#'
#' Individual-level coefficients represent the predicted parameter values for each
#' subject in the study. For models with factors, these coefficients combine:
#' 1. The baseline intercept effect (fixed + random)
#' 2. The factor-specific effect (fixed + random) for each factor level
#'
#' This is equivalent to manually calculating:
#' `coefficient = intercept_fixed + intercept_random + factor_fixed + factor_random`
#'
#' @param fit_obj A `beezdemand_nlme` object returned by `fit_demand_mixed()`.
#' @param params Character vector specifying which parameters to calculate.
#'   Options are "Q0", "alpha", or c("Q0", "alpha"). Default is c("Q0", "alpha").
#' @param format Character, output format. "wide" returns one row per individual
#'   with separate columns for each parameter-factor combination. "long" returns
#'   one row per individual-parameter-factor combination. Default is "wide".
#'
#' @return A data frame with individual-level predicted coefficients.
#'   - In "wide" format: rows are individuals, columns are parameter-factor combinations
#'   - In "long" format: columns are id, parameter, condition, coefficient_value
#'
#'   Column naming convention for wide format:
#'   - `estimated_\{param\}_intercept`: Baseline/reference level coefficient
#'   - `estimated_\{param\}_\{factor\}\{level\}`: Factor level-specific coefficient
#'
#'   All coefficients are on the log10 scale (same as model estimation scale).
#'   To convert to natural scale, use `10^coefficient`.
#'
#' @details
#' The function automatically handles:
#' - Models with or without factors
#' - Any number of factor levels
#' - Missing random effects (defaults to 0)
#' - Complex factor structures with multiple factors
#'
#' For models without factors, only intercept coefficients are calculated.
#' For models with factors, both intercept and factor-level coefficients are provided.
#'
#' @examples
#' \dontrun{
#' # Fit a mixed-effects demand model
#' fit <- fit_demand_mixed(data = mydata,
#'                        y_var = "consumption",
#'                        x_var = "price",
#'                        id_var = "subject",
#'                        factors = "treatment")
#'
#' # Get individual coefficients (wide format, both parameters)
#' individual_coefs <- get_individual_coefficients(fit)
#'
#' # Get only Q0 coefficients in long format
#' q0_coefs <- get_individual_coefficients(fit, params = "Q0", format = "long")
#'
#' # Convert to natural scale
#' individual_coefs$Q0_natural <- 10^individual_coefs$estimated_Q0_intercept
#' }
#'
#' @seealso
#' \code{\link{fit_demand_mixed}} for fitting the original model
#' \code{\link{coef.beezdemand_nlme}} for extracting model coefficients
#' \code{\link{get_demand_param_emms}} for estimated marginal means
#'
#' @importFrom dplyr bind_rows mutate select
#' @importFrom tidyr pivot_longer
#' @export
get_individual_coefficients <- function(
  fit_obj,
  params = c("Q0", "alpha"),
  format = c("wide", "long")
) {
  # Input validation
  if (!inherits(fit_obj, "beezdemand_nlme")) {
    stop("Input 'fit_obj' must be a 'beezdemand_nlme' object.")
  }
  if (is.null(fit_obj$model)) {
    stop("No model found in 'fit_obj'. Fitting may have failed.")
  }

  format <- match.arg(format)
  params <- match.arg(params, choices = c("Q0", "alpha"), several.ok = TRUE)

  # Extract fixed and random effects
  fixed_effects <- coef(fit_obj, type = "fixed")
  random_effects <- ranef(fit_obj)

  # Get individual IDs
  individual_ids <- rownames(random_effects)

  # Initialize results data frame
  results <- data.frame(id = individual_ids)

  # Process each parameter
  for (param in params) {
    # Process intercept (reference level) - this represents the baseline condition
    intercept_name <- paste0(param, ".(Intercept)")
    if (intercept_name %in% names(fixed_effects)) {
      fixed_intercept <- fixed_effects[intercept_name]
      random_intercept <- random_effects[[intercept_name]]

      results[[paste0("estimated_", param, "_intercept")]] <- random_intercept +
        fixed_intercept
    }

    # Get all coefficient names for this parameter EXCEPT the intercept
    all_param_coefs <- names(fixed_effects)[grepl(
      paste0("^", param, "\\."),
      names(fixed_effects)
    )]
    factor_coef_names <- all_param_coefs[all_param_coefs != intercept_name]

    # Process each factor level coefficient
    for (coef_name in factor_coef_names) {
      # Extract the suffix (everything after "param.")
      coef_suffix <- gsub(paste0("^", param, "\\."), "", coef_name)

      # Fixed effect for this factor level
      fixed_effect <- fixed_effects[coef_name]

      # Random effect for this factor level
      random_effect <- if (coef_name %in% names(random_effects)) {
        random_effects[[coef_name]]
      } else {
        rep(0, length(individual_ids))
      }

      # Combined coefficient = intercept effects + factor level effects
      combined_coef <- (random_effects[[intercept_name]] +
        fixed_effects[intercept_name]) +
        (random_effect + fixed_effect)

      # Create readable column name
      col_name <- paste0("estimated_", param, "_", coef_suffix)
      results[[col_name]] <- combined_coef
    }
  }

  # Convert to requested format
  if (format == "long") {
    id_col <- "id"
    coef_cols <- setdiff(names(results), id_col)

    results_long <- results |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(coef_cols),
        names_to = "coefficient_name",
        values_to = "coefficient_value"
      ) |>
      dplyr::mutate(
        parameter = ifelse(
          grepl("^estimated_Q0_", coefficient_name),
          "Q0",
          "alpha"
        ),
        condition = gsub("^estimated_(Q0_|alpha_)", "", coefficient_name)
      ) |>
      dplyr::select(id, parameter, condition, coefficient_value)

    return(results_long)
  } else {
    return(results)
  }
}

# Helper functions for parsing formula sides (if not available, e.g. older R or not loaded by default)
lhs <- function(form) if (length(form) == 3) form[[2]] else NULL
rhs <- function(form) {
  if (length(form) == 3) {
    form[[3]]
  } else if (length(form) == 2) {
    form[[2]]
  } else {
    NULL
  }
}


#' Augment a beezdemand_nlme Model with Fitted Values and Residuals
#'
#' @description
#' Returns the original data with fitted values and residuals from a nonlinear
#' mixed-effects demand model. This enables easy model diagnostics and
#' visualization with the tidyverse.
#'
#' @param x An object of class \code{beezdemand_nlme}.
#' @param newdata Optional data frame of new data for prediction. If NULL,
#'   uses the original data from the model.
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble containing the original data plus:
#'   \describe{
#'     \item{.fitted}{Fitted values on the model scale (may be transformed, e.g., LL4)}
#'     \item{.resid}{Residuals on the model scale}
#'     \item{.fixed}{Fitted values from fixed effects only (population-level)}
#'   }
#'
#' @details
#' The fitted values and residuals are on the same scale as the response variable
#' used in the model. For `equation_form = "zben"`, this is the LL4-transformed
#' scale. For `equation_form = "simplified"` or `"koff"`, this is the natural
#' consumption scale.
#'
#' To back-transform predictions to the natural scale for "zben" models, use:
#' `ll4_inv(augmented$.fitted)`
#'
#' @examples
#' \dontrun{
#' data(apt)
#' apt_ll4 <- apt |> dplyr::mutate(y_ll4 = ll4(y))
#' fit <- fit_demand_mixed(apt_ll4, y_var = "y_ll4", x_var = "x",
#'                         id_var = "id", equation_form = "zben")
#' augmented <- augment(fit)
#'
#' # Plot residuals
#' library(ggplot2)
#' ggplot(augmented, aes(x = .fitted, y = .resid)) +
#'   geom_point(alpha = 0.5) +
#'   geom_hline(yintercept = 0, linetype = "dashed")
#' }
#'
#' @importFrom tibble as_tibble
#' @export
augment.beezdemand_nlme <- function(x, newdata = NULL, ...) {
  if (is.null(x$model)) {
    stop("No model found in object. Model fitting may have failed.", call. = FALSE)
  }

  if (is.null(newdata)) {
    data <- x$data
  } else {
    data <- newdata
  }

  if (is.null(data)) {
    stop("No data available. Provide 'newdata' or ensure model contains data.",
         call. = FALSE)
  }

  # Get variable names
  y_var <- x$param_info$y_var

  # Get fitted values from the underlying nlme model
  fitted_vals <- stats::fitted(x$model)
  resid_vals <- stats::residuals(x$model)

  # For fixed effects only predictions
  fixed_vals <- stats::predict(x$model, level = 0)

  # Build output tibble
  out <- tibble::as_tibble(data)

  # Handle case where data might have been subset or reordered
  if (length(fitted_vals) == nrow(out)) {
    out$.fitted <- as.numeric(fitted_vals)
    out$.resid <- as.numeric(resid_vals)
    out$.fixed <- as.numeric(fixed_vals)
  } else {
    # Try to match by rownames if available
    warning("Fitted values length doesn't match data. Attempting to align.",
            call. = FALSE)
    out$.fitted <- NA_real_
    out$.resid <- NA_real_
    out$.fixed <- NA_real_
  }

  out
}
