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
#' @param param Character, one of `"both"` (default), `"Q0"`, or `"alpha"`.
#'   Controls which demand parameter's EMM columns are returned. `"both"`
#'   preserves the historical four-column-block structure (Q0 and alpha
#'   together). `"Q0"` returns only Q0 columns (and drops EV, since EV is a
#'   function of alpha); a warning is emitted if `include_ev = TRUE` is
#'   requested alongside `param = "Q0"`. `"alpha"` returns only alpha columns
#'   plus the EV block when `include_ev = TRUE`. Mirrors the `param` argument
#'   on the `beezdemand_tmb` method.
#' @param ... Additional arguments passed to `emmeans::emmeans()`.
#'
#' @return A tibble containing:
#'   \item{Factor levels}{Columns for each factor in `factors_in_emm`.}
#'   \item{Q0_param_log10, alpha_param_log10}{EMMs for the model parameters (log10 scale)
#'     with their respective confidence intervals (LCL_Q0_param, UCL_Q0_param, etc.).}
#'   \item{Q0_natural, alpha_natural}{EMMs back-transformed to the natural scale (10^param)
#'     with their respective confidence intervals (LCL_Q0_natural, UCL_Q0_natural, etc.).}
#'   \item{EV, LCL_EV, UCL_EV}{(If `include_ev=TRUE`) Essential Value and its CI.}
#'   When `param = "Q0"` or `param = "alpha"`, only the columns associated with
#'   the requested parameter (plus factor columns and, for `"alpha"`, the EV
#'   block) are returned.
#'
#' @examples
#' \donttest{
#' data(ko, package = "beezdemand")
#' ko$y_ll4 <- ll4(ko$y, lambda = 4)
#' fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'   id_var = "monkey", factors = "dose", equation_form = "zben")
#' get_demand_param_emms(fit)
#'
#' # Request only Q0 columns — convenient for pivoting and plotting
#' get_demand_param_emms(fit, param = "Q0")
#' }
#' @importFrom emmeans ref_grid emmeans
#' @importFrom dplyr full_join select rename mutate across all_of left_join
#' @importFrom tibble as_tibble
#' @importFrom tidyr crossing
#' @importFrom rlang .data `:=`
#' @export
get_demand_param_emms <- function(fit_obj, ...) {
  UseMethod("get_demand_param_emms")
}

#' @rdname get_demand_param_emms
#' @export
get_demand_param_emms.default <- function(fit_obj, ...) {
  stop("Input 'fit_obj' must be a 'beezdemand_nlme' or 'beezdemand_tmb' object.",
       call. = FALSE)
}

#' @rdname get_demand_param_emms
#' @export
get_demand_param_emms.beezdemand_nlme <- function(
  fit_obj,
  factors_in_emm = NULL,
  at = NULL,
  ci_level = 0.95,
  include_ev = FALSE,
  param = c("both", "Q0", "alpha"),
  ...
) {
  param <- match.arg(param)

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
  # EV requires alpha; if the caller asked for Q0-only but also include_ev,
  # warn and drop the include_ev request before computation.
  if (include_ev && param == "Q0") {
    cli::cli_warn(
      "EV is a function of alpha; ignored when {.code param = \"Q0\"}."
    )
    include_ev <- FALSE
  }

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

  # --- Filter columns based on `param` argument ---
  # `param = "both"` preserves the full combined_estimates tibble (default).
  # `param = "Q0"` drops all alpha_* and EV columns.
  # `param = "alpha"` drops all Q0_* columns; EV (if present) stays because
  #   EV is derived from alpha.
  if (param != "both") {
    drop_pattern <- switch(
      param,
      "Q0" = "^(alpha_|LCL_alpha_|UCL_alpha_|EV$|LCL_EV$|UCL_EV$)",
      "alpha" = "^(Q0_|LCL_Q0_|UCL_Q0_)"
    )
    keep_cols <- !grepl(drop_pattern, names(combined_estimates))
    combined_estimates <- combined_estimates[, keep_cols, drop = FALSE]
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
#' @examples
#' \donttest{
#' data(ko, package = "beezdemand")
#' ko$y_ll4 <- ll4(ko$y, lambda = 4)
#' fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'   id_var = "monkey", factors = "dose", equation_form = "zben")
#' get_observed_demand_param_emms(fit)
#' }
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

#' Population-level demand metrics for a mixed-effects NLME fit
#'
#' Computes parameter-first-marginalized Pmax, Omax, Qmax, and
#' elasticity-at-Pmax for a [fit_demand_mixed()] model, mirroring the return
#' contract of [calc_group_metrics()] for `beezdemand_tmb` fits: a flat scalar
#' list, NOT a tibble.
#'
#' Fixed-effect log-Q0 and log-alpha estimated marginal means are averaged
#' across the reference grid (continuous covariates at their training mean by
#' default, factor levels equally weighted) on the natural scale (a geometric
#' mean), then the scalar metrics are derived from the marginalized parameters
#' via [beezdemand_calc_pmax_omax()]. `model_type` follows the equation form:
#' `"exponentiated"` (which carries a range parameter `k`) uses the Hursh &
#' Silberberg solution; `"zben"`/`"simplified"` use the simplified (SND)
#' solution.
#'
#' @param object A `beezdemand_nlme` object from [fit_demand_mixed()].
#' @param at Optional named list conditioning continuous covariates / factor
#'   levels (same shape as the `beezdemand_tmb` method). Covariates default to
#'   their training mean; factors are marginalized with equal weights unless a
#'   level is supplied.
#' @param ... Unused.
#'
#' @return A flat list with scalar `Pmax`, `Omax`, `Qmax`,
#'   `elasticity_at_pmax`, character `method`, and `conditioned_on` (a list of
#'   `$covariates` and/or `$factors`, or `NULL` when the fit has neither).
#'
#' @examples
#' \donttest{
#' data(apt_full, package = "beezdemand")
#' apt_full$y_ll4 <- ll4(apt_full$y, lambda = 4)
#' fit <- fit_demand_mixed(
#'   apt_full, equation_form = "zben", factors = "gender",
#'   y_var = "y_ll4", x_var = "x", id_var = "id")
#' calc_group_metrics(fit)
#' calc_group_metrics(fit, at = list(gender = "Male"))
#' }
#'
#' @seealso [calc_group_metrics()], [get_demand_param_emms()]
#' @export
#' @keywords internal
calc_group_metrics.beezdemand_nlme <- function(object, at = NULL, ...) {
  pinfo <- object$param_info
  all_factors <- unique(c(pinfo$factors, pinfo$factors_Q0, pinfo$factors_alpha))
  all_factors <- all_factors[nzchar(all_factors) & !is.na(all_factors)]
  cov_names <- pinfo$continuous_covariates %||% character(0)

  # Validate `at` inline (mirrors .tmb_validate_at logic with NLME field
  # names: factors_Q0 / factors_alpha are capitalized on the NLME side).
  if (!is.null(at)) {
    if (is.null(names(at)) || any(!nzchar(names(at)))) {
      cli::cli_abort(
        "All elements of {.arg at} must be named (use {.code list(factor = level, cov = value)}).")
    }
    valid_names <- c(all_factors, cov_names)
    bad <- setdiff(names(at), valid_names)
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "Unknown name{?s} in {.arg at}: {.field {bad}}.",
        "i" = "Valid names are the fit's factors and continuous covariates: {.field {valid_names}}.",
        "x" = "Did you mistype a factor or covariate name?"
      ))
    }
    for (nm in names(at)) {
      v <- at[[nm]]
      if (length(v) < 1L) {
        cli::cli_abort("{.field {nm}} in {.arg at} must be a non-empty vector.")
      }
      if (nm %in% all_factors) {
        observed <- sort(unique(as.character(object$data[[nm]])))
        bad_vals <- setdiff(as.character(v), observed)
        if (length(bad_vals) > 0L) {
          cli::cli_abort(c(
            "{.field {nm}} = {.val {bad_vals}} not an observed level.",
            "i" = "Observed levels: {.val {observed}}."
          ))
        }
      } else {
        v_num <- suppressWarnings(as.numeric(v))
        if (any(is.na(v_num)) || any(!is.finite(v_num))) {
          cli::cli_abort(
            "{.field {nm}} value{?s} {.val {as.character(v)}} must be finite numeric.")
        }
      }
    }
  }

  # Model type / k from the equation form (mirror the TMB has_k mapping:
  # exponential|exponentiated -> k; NLME's only k-form is exponentiated).
  eq <- object$formula_details$equation_form_selected
  has_k <- identical(eq, "exponentiated")
  k_val <- pinfo$k

  # Parameter-first marginalization: geometric mean of the per-cell natural
  # EMMs. get_demand_param_emms() joins Q0+alpha internally regardless of
  # `param`, so the per-param table can carry NA join rows under
  # overlapping-label collapse_levels -- filter to finite-positive before the
  # geometric mean, and abort if a parameter has no usable cells. emmeans
  # SE-related warnings are irrelevant (only point estimates are used), so they
  # are suppressed for silence-parity with the TMB method.
  emm_q0 <- suppressWarnings(suppressMessages(get_demand_param_emms(
    object, param = "Q0", at = at, factors_in_emm = NULL, include_ev = FALSE)))
  emm_alpha <- suppressWarnings(suppressMessages(get_demand_param_emms(
    object, param = "alpha", at = at, factors_in_emm = NULL, include_ev = FALSE)))

  .marginal_geom_mean <- function(vals, lbl) {
    vals <- vals[is.finite(vals) & vals > 0]
    if (length(vals) == 0L) {
      cli::cli_abort(c(
        "No usable {lbl} EMM rows to marginalize.",
        "i" = "All emmeans values were non-finite/non-positive (possible with overlapping {.arg collapse_levels} labels)."
      ))
    }
    exp(mean(log(vals)))
  }
  Q0 <- .marginal_geom_mean(emm_q0$Q0_natural, "Q0")
  alpha_val <- .marginal_geom_mean(emm_alpha$alpha_natural, "alpha")

  # conditioned_on description (mirror the TMB method).
  conditioned_on <- list()
  if (length(cov_names) > 0L) {
    cov_values <- vapply(cov_names, function(cv) {
      if (!is.null(at) && cv %in% names(at)) {
        as.numeric(at[[cv]][1])
      } else {
        mean(object$data[[cv]], na.rm = TRUE)
      }
    }, numeric(1))
    names(cov_values) <- cov_names
    conditioned_on$covariates <- cov_values
  }
  if (length(all_factors) > 0L) {
    factor_treatment <- vector("list", length(all_factors))
    names(factor_treatment) <- all_factors
    for (f in all_factors) {
      factor_treatment[[f]] <- if (!is.null(at) && f %in% names(at)) {
        as.character(at[[f]])
      } else {
        "marginal"
      }
    }
    conditioned_on$factors <- factor_treatment
  }
  if (length(conditioned_on) == 0L) conditioned_on <- NULL

  result <- if (has_k) {
    beezdemand_calc_pmax_omax(
      model_type = "hs",
      params = list(alpha = alpha_val, q0 = Q0, k = k_val),
      param_scales = list(alpha = "natural", q0 = "natural", k = "natural"))
  } else {
    beezdemand_calc_pmax_omax(
      model_type = "snd",
      params = list(alpha = alpha_val, q0 = Q0),
      param_scales = list(alpha = "natural", q0 = "natural"))
  }

  list(
    Pmax = result$pmax_model,
    Omax = result$omax_model,
    Qmax = result$q_at_pmax_model,
    elasticity_at_pmax = result$elasticity_at_pmax_model,
    method = result$method_model,
    conditioned_on = conditioned_on
  )
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
#' @param param Character vector: "Q0", "alpha", or `c("Q0", "alpha")`. Default
#'   `c("Q0", "alpha")` (both). This is the canonical argument name, shared with
#'   the TMB backend ([get_demand_comparisons.beezdemand_tmb()]).
#' @param params_to_compare `r lifecycle::badge("deprecated")` Use `param`
#'   instead (deprecated in 0.3.0 to harmonize with the TMB backend).
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
#' @param adjust P-value adjustment method. Default `"holm"` (changed from
#'   `"tukey"` in 0.3.0 for cross-backend reproducibility; pass
#'   `adjust = "tukey"` to retain the previous default).
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
#'   S3 class `beezdemand_comparison` is assigned. When `contrast_by` is active,
#'   the nested contrast tables carry leading by-column(s) named with the
#'   user-requested *original* factor name (e.g. `dose`, not the
#'   collapse-mapped `dose_alpha`), harmonized with the TMB backend and the flat
#'   [tidy()][tidy.beezdemand_comparison] output (TICKET-033).
#'
#' @examples
#' \donttest{
#' data(ko, package = "beezdemand")
#' ko$y_ll4 <- ll4(ko$y, lambda = 4)
#' fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'   id_var = "monkey", factors = "dose", equation_form = "zben")
#' get_demand_comparisons(fit)
#' }
#' @importFrom emmeans ref_grid emmeans contrast
#' @importFrom tibble as_tibble
#' @importFrom rlang `:=` .data
#' @importFrom dplyr select rename all_of any_of everything mutate
#' @export
get_demand_comparisons <- function(fit_obj, ...) {
  UseMethod("get_demand_comparisons")
}

#' @rdname get_demand_comparisons
#' @export
get_demand_comparisons.default <- function(fit_obj, ...) {
  stop("Input 'fit_obj' must be a 'beezdemand_nlme' or 'beezdemand_tmb' object.",
       call. = FALSE)
}

#' @rdname get_demand_comparisons
#' @export
get_demand_comparisons.beezdemand_nlme <- function(
  fit_obj,
  param = c("Q0", "alpha"),
  compare_specs = NULL,
  contrast_type = "pairwise",
  contrast_by = NULL,
  adjust = "holm",
  at = NULL,
  ci_level = 0.95,
  report_ratios = TRUE,
  params_to_compare = lifecycle::deprecated(),
  ...
) {
  # TICKET-016: `params_to_compare` deprecated in favor of the canonical `param`
  # (harmonizes the argument name with the TMB backend).
  if (lifecycle::is_present(params_to_compare)) {
    if (!missing(param)) {
      cli::cli_abort(
        "Supply only one of {.arg param} and the deprecated {.arg params_to_compare}."
      )
    }
    lifecycle::deprecate_warn(
      "0.3.0",
      "get_demand_comparisons(params_to_compare = )",
      "get_demand_comparisons(param = )"
    )
    param <- params_to_compare
  }
  param <- match.arg(param, c("Q0", "alpha"), several.ok = TRUE)

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
    # F3 + follow-up (Codex re-review): validate the requested factors at the
    # boundary, mirroring the TMB backend, instead of letting a bogus name fall
    # through emmeans() into a silent partial result. Under asymmetric
    # `collapse_levels`, `compare_specs` may name either the original factor
    # (which `.get_actual_factors_for_param()` maps per parameter) OR the
    # parameter's collapsed column directly -- so validate against the union of
    # original names and per-parameter columns. Cross-parameter aliases that
    # don't resolve for a given parameter are caught later in the per-param
    # loop with a clearer parameter-scoped message.
    valid_factors <- unique(c(
      all_model_factors,
      fit_obj$param_info$factors_Q0,
      fit_obj$param_info$factors_alpha
    ))
    valid_factors <- valid_factors[!is.na(valid_factors) & nzchar(valid_factors)]
    bad_factors <- setdiff(base_factors, valid_factors)
    if (length(bad_factors) > 0L) {
      cli::cli_abort(c(
        "{.arg compare_specs} names factor{?s} not in the fit: {.val {bad_factors}}.",
        "i" = "Fitted factors: {.val {valid_factors}}."
      ))
    }
    user_provided_specs <- TRUE
  }

  # Boundary validation for contrast_by (TICKET-032), mirroring compare_specs
  # and the TMB backend: a name not in the fit (typo) aborts here, once, before
  # the per-parameter loop. Without this, a typo on a collapse fit was silently
  # dropped to NULL during per-param mapping (Codex review Blocking 1).
  if (!is.null(contrast_by)) {
    if (length(contrast_by) == 0L) {
      contrast_by <- NULL
    } else if (!is.character(contrast_by)) {
      cli::cli_abort("{.arg contrast_by} must be {.code NULL} or a character vector of factor name(s).")
    } else {
      valid_by <- unique(c(
        all_model_factors,
        fit_obj$param_info$factors_Q0,
        fit_obj$param_info$factors_alpha
      ))
      valid_by <- valid_by[!is.na(valid_by) & nzchar(valid_by)]
      bad_by <- setdiff(contrast_by, valid_by)
      if (length(bad_by) > 0L) {
        cli::cli_abort(c(
          "{.arg contrast_by} names factor{?s} not in the fit: {.val {bad_by}}.",
          "i" = "Fitted factors: {.val {valid_by}}."
        ))
      }
    }
  }

  results_list <- list()

  # Per-parameter original -> effective contrast_by map (TICKET-032). Mirrors
  # the TMB backend's attribute; populated inside the loop.
  contrast_by_map_list <- list()

  # Initialize effective_contrast_by so it exists even if EMMs fail and the loop
  # short-circuits before setting it. This avoids errors when setting attributes
  # after the loop.
  effective_contrast_by <- contrast_by

  for (param_name in param) {
    if (!param_name %in% c("Q0", "alpha")) {
      warning(
        "Unknown parameter '",
        param_name,
        "' in `param`. Skipping."
      )
      next
    }

    current_param_results <- list()
    contrast_by_map_list[[param_name]] <- stats::setNames(character(0), character(0))
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

    # F3 follow-up: a non-empty `base_factors` that resolves to zero
    # `actual_factors` has two distinct causes, which must be handled
    # differently (test_emms_comparisons.R:375 pins this):
    #   (a) Cross-parameter alias under asymmetric `collapse_levels` -- the
    #       requested factor is not in this parameter's design at all (e.g.
    #       ~ age_group_alpha for param = "Q0"). Abort with a parameter-scoped
    #       message; matches the TMB resolver guard.
    #   (b) Factor collapsed to a single level for this parameter -- the
    #       factor IS in this parameter's design (its collapsed column is in
    #       `param_factors`) but has < 2 levels, so no contrast is possible.
    #       Preserve the prior silent intercept-only behavior; the empty
    #       contrasts table is the expected output for that case.
    if (user_provided_specs &&
        length(base_factors) > 0L &&
        length(actual_factors) == 0L) {
      resolvable_in_param <- any(
        base_factors %in% param_factors |
          paste0(base_factors, "_", param_suffix) %in% param_factors
      )
      if (!resolvable_in_param) {
        cli::cli_abort(c(
          "{cli::qty(base_factors)}{.arg compare_specs} factor{?s} {.val {base_factors}} {?does/do} not resolve for {param_name}.",
          "i" = "{param_name} factors: {.val {param_factors}}.",
          "x" = "Under asymmetric {.arg collapse_levels} a factor can be retained for one parameter but not the other."
        ))
      }
    }

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
      # --- Filter EMMs to observed factor combinations ---
      if (length(actual_factors) > 0) {
        emm_grid <- as.data.frame(emms@grid)
        emm_grid$.row_idx <- seq_len(nrow(emm_grid))

        observed_combos <- model_data |>
          dplyr::distinct(dplyr::across(dplyr::any_of(actual_factors)))

        matched <- dplyr::semi_join(
          emm_grid, observed_combos,
          by = actual_factors
        )

        if (nrow(matched) < nrow(emm_grid)) {
          emms <- emms[sort(matched$.row_idx)]
          message(
            "  Filtered EMMs to ", nrow(matched), " observed combinations ",
            "(from ", nrow(emm_grid), " full factorial)."
          )
        }
      }

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

      # Map contrast_by to collapsed factor name if needed. `cb_map_param`
      # records the original -> effective resolution and drives three things:
      # the construction-time rename of the nested by-column back to the
      # user-requested original (TICKET-033), the `contrast_by_map` metadata
      # attribute, and the flat tidy() fallback lookup (TICKET-032).
      effective_contrast_by <- contrast_by
      cb_map_param <- if (is.null(contrast_by)) {
        stats::setNames(character(0), character(0))
      } else {
        stats::setNames(contrast_by, contrast_by)
      }
      if (!is.null(contrast_by) && collapse_was_used) {
        mapped_contrast_by <- character(0)
        cb_map_param <- stats::setNames(character(0), character(0))
        for (cb_fac in contrast_by) {
          collapsed_name <- paste0(cb_fac, "_", param_suffix)
          # Resolve against the FULL parameter factor set (factors_Q0 /
          # factors_alpha), NOT the compare_specs subset (`actual_factors`).
          # A by-var that resolves here but is absent from compare_specs is
          # then caught by the pre-validation guard below (loud abort) rather
          # than silently dropped (Codex review Blocking 1). A genuine typo was
          # already rejected by the boundary check above.
          if (collapsed_name %in% param_factors) {
            mapped_contrast_by <- c(mapped_contrast_by, collapsed_name)
            cb_map_param[cb_fac] <- collapsed_name
          } else if (cb_fac %in% param_factors) {
            mapped_contrast_by <- c(mapped_contrast_by, cb_fac)
            cb_map_param[cb_fac] <- cb_fac
          }
          # If factor not in this parameter's design at all, skip it
          # (collapse-induced asymmetry; defensive).
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

      # TICKET-033 (Codex R2): mirror the TMB within-param collision guard
      # (R/tmb-methods.R duplicate-effective check). Two requested by-vars that
      # resolve to the SAME effective column -- e.g.
      # contrast_by = c("age_group", "age_group_alpha") under asymmetric
      # collapse (both map to age_group_alpha for alpha), or a literal duplicate
      # c("gender", "gender") with no collapse -- would otherwise pass a
      # malformed `by` to emmeans::contrast() and mislabel the renamed nested
      # by-column. Abort loudly per parameter, before the contrast call.
      if (!is.null(effective_contrast_by) &&
          any(duplicated(effective_contrast_by))) {
        cli::cli_abort(c(
          "Two {.arg contrast_by} variables resolve to the same column for {param_name}.",
          "i" = "Resolved columns: {.val {effective_contrast_by}}."
        ))
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

      # Pre-validation (TICKET-032): a `contrast_by` that resolves to a factor
      # NOT in this parameter's `compare_specs` aborts loudly. This replaces the
      # old silent-empty path (emmeans::contrast() would error inside
      # .find.by.rows() and we returned an empty table + $contrasts_log10_error).
      # Backend-consistent message with the TMB backend.
      if (!is.null(effective_contrast_by) &&
          length(actual_factors) > 0L &&
          !all(effective_contrast_by %in% actual_factors)) {
        not_in <- setdiff(effective_contrast_by, actual_factors)
        cli::cli_abort(c(
          "{cli::qty(not_in)}{.arg contrast_by} factor{?s} {.val {not_in}} {?is/are} not in {.arg compare_specs} for {param_name}.",
          "i" = "{cli::qty(actual_factors)}{.arg compare_specs} factor{?s} for {param_name}: {.val {actual_factors}}.",
          "x" = "Name the by-variable(s) in {.arg compare_specs} to condition contrasts on them."
        ))
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

      # Record the per-parameter map actually used for by-grouping (drop any
      # originals whose effective resolution was dropped by the redundant-by
      # check). Set here, on the non-intercept path, before the contrast call.
      contrast_by_map_list[[param_name]] <- cb_map_param[
        unname(cb_map_param) %in% (effective_contrast_by %||% character(0))
      ]

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

            # TICKET-033: rename the nested by-column(s) from the EFFECTIVE
            # (collapse-mapped) name (e.g. "age_group_alpha") back to the
            # user-requested ORIGINAL ("age_group"), so the NLME nested
            # $contrasts_log10 / $contrasts_ratio by-columns match the TMB
            # backend and the flat tidy() output. No-op when no collapse-mapping
            # occurred (effective == original) or when by-grouping fell through
            # (map filtered to empty). MUST run AFTER the $contrasts_ratio block
            # above, which selects the effective by-cols from the log10 table.
            cb_rename <- contrast_by_map_list[[param_name]] # original -> effective
            cb_rename <- cb_rename[names(cb_rename) != unname(cb_rename)]
            if (length(cb_rename) > 0L) {
              present <- unname(cb_rename) %in%
                names(current_param_results$contrasts_log10)
              cb_rename <- cb_rename[present]
            }
            if (length(cb_rename) > 0L) {
              # Collision guard (Codex B1): the user-original target name must
              # not already exist among the NON-source columns of either nested
              # table (e.g. a factor literally named `estimate`/`df`/`p.value`).
              # Abort loudly rather than let dplyr::rename() error cryptically.
              existing_cols <- unique(c(
                names(current_param_results$contrasts_log10),
                if (report_ratios &&
                    !is.null(current_param_results$contrasts_ratio)) {
                  names(current_param_results$contrasts_ratio)
                } else {
                  character(0)
                }
              ))
              clash <- intersect(
                names(cb_rename),
                setdiff(existing_cols, unname(cb_rename))
              )
              if (length(clash) > 0L) {
                cli::cli_abort(c(
                  "Cannot harmonize {.arg contrast_by} column name{?s} for {param_name}.",
                  "x" = "{cli::qty(clash)}Factor name{?s} {.val {clash}} collide{?s/} with a reserved contrast column.",
                  "i" = "Rename the offending factor before fitting the model."
                ))
              }
              # dplyr::rename(new = "old"); cb_rename is c(original = "effective").
              current_param_results$contrasts_log10 <- dplyr::rename(
                current_param_results$contrasts_log10, !!!cb_rename
              )
              if (report_ratios &&
                  !is.null(current_param_results$contrasts_ratio) &&
                  ncol(current_param_results$contrasts_ratio) > 0L) {
                keep <- unname(cb_rename) %in%
                  names(current_param_results$contrasts_ratio)
                if (any(keep)) {
                  current_param_results$contrasts_ratio <- dplyr::rename(
                    current_param_results$contrasts_ratio, !!!cb_rename[keep]
                  )
                }
              }
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
  attr(results_list, "backend") <- "nlme"
  # Record the user's requested comparison spec (or the all-factors default),
  # NOT the last loop iteration's per-parameter `emm_specs_formula` -- under
  # asymmetric `collapse_levels` that formula differs per parameter (and may
  # carry an internal collapsed column name), so the previous value was both
  # misleading and order-dependent. Mirrors the TMB backend's label
  # (release-audit C4).
  attr(results_list, "compare_specs_used") <- if (user_provided_specs) {
    deparse(compare_specs)
  } else {
    "all fitted factors"
  }
  attr(results_list, "contrast_type_used") <- contrast_type
  # `contrast_by_used` reports the user-requested ORIGINAL name(s) (TICKET-032),
  # so it survives collapse-mapping and is consistent across backends -- but
  # only when by-grouping was actually applied for at least one parameter,
  # otherwise "NULL" (so the flattener/print do not synthesize an all-NA
  # by-column for a fully-redundant/ignored request; Codex review Recommended 1).
  any_by_applied <- any(vapply(contrast_by_map_list, length, integer(1)) > 0L)
  attr(results_list, "contrast_by_used") <- if (!is.null(contrast_by) && any_by_applied) {
    paste(contrast_by, collapse = ", ")
  } else {
    "NULL"
  }
  attr(results_list, "contrast_by_map") <- contrast_by_map_list
  attr(results_list, "adjustment_method") <- adjust
  return(results_list)
}

# Is the recorded `contrast_by_used` attribute inactive (no by-grouping)?
# Handles NULL, empty, and the literal "NULL" / "" string sentinels that both
# backends historically write (TICKET-032 Decision: Codex v2 Finding 4).
.contrast_by_inactive <- function(x) {
  is.null(x) || length(x) == 0L ||
    identical(x, "") || identical(x, "NULL")
}

# Backend-aware flattener shared by tidy.beezdemand_comparison() and
# print.beezdemand_comparison(). Maps each backend's NATIVE nested dialect to
# the neutral cross-backend schema (Decision 4). TMB contrast labels come from
# the STRUCTURED `std_labels` attribute (built from ref-grid level values, not
# regex), so they match emmeans' native level-value labels on the NLME side.
#
# TICKET-032/033: when `contrast_by` is active, by-columns (user-requested
# ORIGINAL names) are inserted BEFORE `param`. As of TICKET-033 BOTH backends
# carry the user-original by-col name in the nested tables (TMB built them that
# way; NLME now renames effective -> original at construction). The flattener
# therefore prefers the original name and falls back to the effective name only
# defensively (e.g. an externally-constructed or pre-033 cached object).
.beezdemand_comparison_flat <- function(x, exponentiate = FALSE) {
  backend <- attr(x, "backend") %||% "nlme"
  by_used <- attr(x, "contrast_by_used")
  by_active <- !.contrast_by_inactive(by_used)
  by_names <- if (by_active) trimws(strsplit(by_used, ",")[[1]]) else character(0)
  cb_map <- attr(x, "contrast_by_map")

  base_cols <- list(
    param = character(), contrast = character(), estimate = numeric(),
    std.error = numeric(), statistic = numeric(), df = numeric(),
    conf.low = numeric(), conf.high = numeric(), p.value = numeric()
  )

  # Resolve the source column in `cl` for each user-requested by-name. Both
  # backends now carry the user-original name (TICKET-033); prefer it, and fall
  # back to the effective (collapse-mapped) name for defensiveness against
  # externally-constructed or pre-033 cached objects on the NLME side.
  by_source_col <- function(nm, cl, p) {
    if (nm %in% names(cl)) return(nm)
    if (!identical(backend, "tmb")) {
      pm <- if (!is.null(cb_map)) cb_map[[p]] else NULL
      eff <- if (!is.null(pm) && nm %in% names(pm)) pm[[nm]] else nm
      if (eff %in% names(cl)) return(eff)
    }
    NA_character_
  }

  rows <- lapply(names(x), function(p) {
    cl <- x[[p]]$contrasts_log10
    if (is.null(cl) || nrow(cl) == 0L || !("estimate" %in% names(cl))) {
      return(NULL)
    }
    if (identical(backend, "tmb")) {
      lab <- attr(cl, "std_labels")
      if (is.null(lab) || length(lab) != nrow(cl)) lab <- cl$contrast
      base <- tibble::tibble(
        param = p, contrast = lab,
        estimate = cl$estimate, std.error = cl$std.error,
        statistic = cl$statistic, df = cl$df,
        conf.low = cl$conf.low, conf.high = cl$conf.high,
        p.value = cl$p.value
      )
    } else {
      base <- tibble::tibble(
        param = p, contrast = cl$contrast_definition,
        estimate = cl$estimate, std.error = cl$SE,
        statistic = cl$t.ratio, df = cl$df,
        conf.low = cl$lower.CL, conf.high = cl$upper.CL,
        p.value = cl$p.value
      )
    }
    if (by_active) {
      by_cols <- lapply(by_names, function(nm) {
        src <- by_source_col(nm, cl, p)
        if (is.na(src)) rep(NA_character_, nrow(cl)) else as.character(cl[[src]])
      })
      base <- dplyr::bind_cols(
        tibble::as_tibble(stats::setNames(by_cols, by_names)), base
      )
    }
    base
  })

  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0L) {
    out <- tibble::as_tibble(base_cols)
    if (by_active) {
      by_empty <- stats::setNames(
        rep(list(character()), length(by_names)), by_names
      )
      out <- dplyr::bind_cols(tibble::as_tibble(by_empty), out)
    }
  }

  if (isTRUE(exponentiate)) {
    # Base-invariant ratios; std.error is NA per broom's exponentiated-fit
    # convention (the delta-method SE does not transform multiplicatively).
    out$estimate <- 10^out$estimate
    out$conf.low <- 10^out$conf.low
    out$conf.high <- 10^out$conf.high
    out$std.error <- NA_real_
  }
  out
}

#' Tidy a demand-parameter comparison into a flat contrasts frame
#'
#' @description
#' Backend-agnostic [broom::tidy()] method for `beezdemand_comparison` objects
#' (returned by [get_demand_comparisons()] on both the NLME and TMB backends).
#' This flat long tibble is the cross-backend contract: identical column names
#' and order regardless of backend. The nested object itself keeps each
#' backend's native dialect (see [get_demand_comparisons()]).
#'
#' @param x A `beezdemand_comparison` object.
#' @param exponentiate Logical. If `TRUE`, return base-invariant ratios
#'   (`estimate = 10^estimate`, CIs back-transformed); `std.error` becomes `NA`
#'   following broom's convention for exponentiated fits. Default `FALSE`.
#' @param ... Unused.
#'
#' @return A tibble with columns `param`, `contrast`, `estimate`, `std.error`,
#'   `statistic`, `df`, `conf.low`, `conf.high`, `p.value`. Estimates and CIs
#'   are on the log10 scale (or ratios when `exponentiate = TRUE`). `statistic`
#'   is a *t* ratio with finite `df` on the NLME backend and an asymptotic *z*
#'   (`df = Inf`) on the TMB backend (the value differs by backend, by design).
#'
#' @examples
#' \donttest{
#' data(apt_full)
#' dat <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
#' fit <- fit_demand_tmb(dat, equation = "exponential",
#'                       factors = "gender", verbose = 0)
#' res <- get_demand_comparisons(fit, param = c("Q0", "alpha"))
#' tidy(res)
#' tidy(res, exponentiate = TRUE)
#' }
#'
#' @export
tidy.beezdemand_comparison <- function(x, exponentiate = FALSE, ...) {
  .beezdemand_comparison_flat(x, exponentiate = exponentiate)
}

#' Print method for beezdemand_comparison objects
#'
#' @param x A `beezdemand_comparison` object.
#' @param digits Number of significant digits to display for estimates.
#' @param ... Additional arguments (unused).
#' @return Invisibly returns the input object \code{x}.
#' @export
print.beezdemand_comparison <- function(x, digits = 3, ...) {
  backend <- attr(x, "backend") %||% "nlme"
  emm_specs_used <- attr(x, "compare_specs_used")
  contrast_type <- attr(x, "contrast_type_used")
  contrast_by <- attr(x, "contrast_by_used")
  adj_method <- attr(x, "adjustment_method")

  cat(sprintf("Demand Parameter Comparisons (%s backend)\n", backend))
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
  cat(strrep("=", 50), "\n")

  flat <- .beezdemand_comparison_flat(x)
  by_active <- !.contrast_by_inactive(contrast_by)
  by_cols <- if (by_active) trimws(strsplit(contrast_by, ",")[[1]]) else character(0)
  by_cols <- by_cols[by_cols %in% names(flat)]
  for (p in names(x)) {
    cat(sprintf("\n%s (log10-scale contrasts):\n", p))
    sub <- flat[flat$param == p,
                c(by_cols, "contrast", "estimate", "std.error",
                  "conf.low", "conf.high", "p.value")]
    if (nrow(sub) == 0L) {
      cat("  <no contrasts>\n")
    } else {
      num <- vapply(sub, is.numeric, logical(1))
      sub[num] <- lapply(sub[num], round, digits = digits)
      print(as.data.frame(sub), row.names = FALSE)
    }
  }

  invisible(x)
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
#' \donttest{
#' data(ko)
#' ko$dose_num <- as.numeric(as.character(ko$dose))
#' fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'                         id_var = "monkey", factors = "drug",
#'                         equation_form = "zben")
#' trends <- get_demand_param_trends(fit, covariates = "dose_num",
#'                                   specs = ~ drug)
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
#' \donttest{
#' data(ko)
#' fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'                         id_var = "monkey", equation_form = "zben")
#' print(fit)
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

# Recompute fixed-effect test statistics and p-values after a delta-method
# parameter transformation (when report_space != internal_space). The transform
# rescales estimate/SE but does NOT change the underlying t-distribution, so
# nlme's containment-based degrees of freedom are reused; the z-test is only a
# fallback when DF are unavailable (TICKET-006). Shared by
# summary.beezdemand_nlme() and tidy.beezdemand_nlme() so the two methods report
# identical inference on the same fit and cannot drift (release-audit C1: tidy
# had silently used pnorm() while summary used pt()).
.nlme_recompute_transformed_stats <- function(estimate, std.error, df_residual) {
  statistic <- estimate / std.error
  p.value <- if (all(is.na(df_residual))) {
    2 * stats::pnorm(-abs(statistic))
  } else {
    2 * stats::pt(-abs(statistic), df = df_residual)
  }
  list(statistic = statistic, p.value = p.value)
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
  # Preserve nlme's containment-based degrees of freedom for use after parameter
  # transformation. The delta method changes estimate/SE but not the underlying
  # t-distribution, so reusing these df keeps inference correctly t-based for
  # small N (TICKET-006).
  df_residual <- if ("DF" %in% colnames(ttable)) ttable[, "DF"] else NA_real_
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
    rec <- .nlme_recompute_transformed_stats(
      coefficients$estimate, coefficients$std.error, df_residual
    )
    coefficients$statistic <- rec$statistic
    coefficients$p.value <- rec$p.value
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

  # Operational convergence gate, shared with glance.beezdemand_nlme() via the
  # same helper (TICKET-020). summary() previously hard-coded converged = TRUE,
  # which contradicted glance() on an unusable (non-PD apVar) fit (release-audit
  # C2). The diagnostic message (if any) is surfaced in `notes`.
  conv <- .check_nlme_convergence(object)
  conv_notes <- if (!isTRUE(conv$converged) && !is.null(conv$message)) {
    conv$message
  } else {
    character(0)
  }

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
      converged = conv$converged,
      logLik = as.numeric(stats::logLik(object$model)),
      AIC = stats::AIC(object$model),
      BIC = stats::BIC(object$model),
      sigma = object$model$sigma,
      coefficients = coefficients,
      derived_metrics = beezdemand_empty_derived_metrics(),
      fixed_effects = ttable,
      random_effects = random_effects,
      notes = conv_notes
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
#' @return Invisibly returns the input object \code{x}.
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
#' @param effects Character. Which effects to include: `"fixed"`,
#'   `"ran_pars"`, or both (the default).
#' @param report_space Character. Reporting space for core parameters. One of
#'   `"natural"` or `"log10"` (default depends on `param_space` used for fitting).
#' @param ... Additional arguments (ignored)
#' @return A tibble of model terms with columns:
#'   - `term`: Parameter name
#'   - `estimate`: Point estimate. For `component == "variance"` rows this is
#'     a *standard deviation* (pulled from `nlme::VarCorr(model)[, "StdDev"]`),
#'     matching [tidy.beezdemand_tmb()] and the `broom.mixed::tidy.lme`
#'     convention.
#'   - `std.error`: Standard error (`NA` for variance components)
#'   - `statistic`: t-value (`NA` for variance components)
#'   - `p.value`: P-value (`NA` for variance components)
#'   - `component`: `"fixed"` or `"variance"`
#'   - `estimate_scale`: Scale that `estimate` is reported on
#'   - `term_display`: Display label for `term`
#'   - `estimate_internal`: Pre-transform estimate; present whenever
#'     `effects` includes `"fixed"`
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
    # Preserve nlme's containment-based DF for the post-transform recompute
    # (see .nlme_recompute_transformed_stats / TICKET-006).
    df_residual <- if ("DF" %in% colnames(ttable)) ttable[, "DF"] else NA_real_
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
      # Reuse the DF-aware recompute shared with summary.beezdemand_nlme() so
      # the two methods never disagree (release-audit C1).
      rec <- .nlme_recompute_transformed_stats(
        fixed$estimate, fixed$std.error, df_residual
      )
      fixed$statistic <- rec$statistic
      fixed$p.value <- rec$p.value
    }
    result <- dplyr::bind_rows(result, fixed)
  }

  if ("ran_pars" %in% effects) {
    # Extract variance components from VarCorr
    vc <- nlme::VarCorr(x$model)
    # VarCorr returns a matrix-like object with both "Variance" and "StdDev"
    # columns (StdDev = sqrt(Variance) exactly). TICKET-030 switched the
    # reporting from variance to SD to align with broom.mixed::tidy.lme and
    # with tidy.beezdemand_tmb. Callers needing the variance can square the
    # estimate or read nlme::VarCorr(fit$model)[, "Variance"] directly.
    if (is.matrix(vc) || is.data.frame(vc)) {
      var_names <- rownames(vc)
      if ("StdDev" %in% colnames(vc)) {
        variances <- as.numeric(vc[, "StdDev"])
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
#'   - `n_random_effects`: Number of random-effect terms (e.g. 2 for
#'     `Q0 + alpha ~ 1`)
#'   - `converged`: Operational convergence status. `TRUE` when the final fit is
#'     usable for inference — i.e. `apVar` (nlme's approximate covariance of the
#'     variance-covariance parameters) is positive-definite AND there is no
#'     terminal error. Alias for `final_fit_ok`. As of TICKET-020 this is no
#'     longer flipped to `FALSE` by iteration-level optimizer warnings (see
#'     `fit_warned`).
#'   - `final_fit_ok`: The canonical usable-for-inference gate (`apVar` PD and no
#'     terminal error); identical to `converged`. NLME-only.
#'   - `fit_warned`: Diagnostic flag — `TRUE` when nlme emitted iteration-level
#'     convergence warnings (false convergence, singular, step-halving, iteration
#'     limit, ...) during PNLS-LME alternation. Informational only; does not gate
#'     `converged`. NLME-only.
#'   - `logLik`, `AIC`, `BIC`: Model fit statistics
#'   - `sigma`: Residual standard error (NLME-only)
#'
#'   The shared canonical columns (through `converged`, `logLik`, `AIC`, `BIC`)
#'   match [glance.beezdemand_tmb()], so backend-agnostic code needs no dispatch
#'   glue; `final_fit_ok` and `fit_warned` are additive NLME-only diagnostics.
#' @export
glance.beezdemand_nlme <- function(x, ...) {
  if (is.null(x$model)) {
    return(tibble::tibble(
      model_class = "beezdemand_nlme",
      backend = "nlme",
      equation_form = x$param_info$equation_form %||% NA_character_,
      nobs = NA_integer_,
      n_subjects = NA_integer_,
      n_random_effects = NA_integer_,
      converged = FALSE,
      final_fit_ok = FALSE,
      fit_warned = FALSE,
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

  n_random_effects <- tryCatch(
    ncol(nlme::ranef(x$model)),
    error = function(e) NA_integer_
  )

  conv <- .check_nlme_convergence(x)

  tibble::tibble(
    model_class = "beezdemand_nlme",
    backend = "nlme",
    equation_form = x$param_info$equation_form %||%
      x$formula_details$equation_form_selected,
    nobs = n_obs,
    n_subjects = n_subjects,
    n_random_effects = n_random_effects,
    converged = conv$converged,
    final_fit_ok = conv$final_fit_ok,
    fit_warned = conv$fit_warned,
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
#' \donttest{
#' data(ko)
#' fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'                         id_var = "monkey", equation_form = "zben")
#' confint(fit)
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
#' \donttest{
#' data(ko)
#' fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'                         id_var = "monkey", equation_form = "zben")
#' coef(fit, type = "fixed")
#' coef(fit, type = "random")
#' coef(fit, type = "combined")
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

# ===========================================================================
# get_subject_pars.beezdemand_nlme() (TICKET-034)
#
# Subject-level demand parameters for an NLME fit, reconstructed as
# `param_{i,cell} = back(X_row %*% fixef + Z_row %*% ranef[subject])`. Mirrors
# the verified TMB path (.tmb_compute_subject_pars) but back-transforms with
# `10^` (NLME's internal scale is log10) instead of `exp`. Z is built from the
# random-effects formula via model.matrix (NOT by parsing ranef() labels);
# ranef() column names are used only to align each design column to its BLUP.
# ===========================================================================

# Build the xlev list for a set of factor names (factors with >= 2 levels),
# using training-data levels so contrasts match the fitted model.
.nlme_get_xlevs <- function(factor_names, dat) {
  xlevs <- list()
  for (f in factor_names) {
    if (f %in% names(dat)) {
      col <- dat[[f]]
      if (!is.factor(col)) col <- factor(col)
      if (nlevels(col) >= 2L) xlevs[[f]] <- levels(col)
    }
  }
  xlevs
}

# Factor/character variables referenced by a one-sided fixed-effects formula
# string that are present in the training data.
.nlme_fixed_factor_vars <- function(form_str, train_data) {
  vars <- all.vars(stats::as.formula(form_str))
  vars <- intersect(vars, names(train_data))
  vars[vapply(vars, function(v) {
    is.factor(train_data[[v]]) || is.character(train_data[[v]])
  }, logical(1))]
}

# Coerce every newdata column that is a factor/character in the training data
# to a factor carrying the FULL training levels, so model.matrix() (for both X
# and Z) produces the same contrast / indicator columns the fit used.
.nlme_coerce_training_factors <- function(newdata, train_data) {
  for (v in names(newdata)) {
    if (v %in% names(train_data) &&
        (is.factor(train_data[[v]]) || is.character(train_data[[v]]))) {
      newdata[[v]] <- factor(newdata[[v]], levels = levels(factor(train_data[[v]])))
    }
  }
  newdata
}

# Fixed-effect design matrix for one parameter, built with training levels.
# Aborts if any row is dropped (would misalign with newdata).
.nlme_fixed_design <- function(form_str, newdata, train_data) {
  form <- stats::as.formula(form_str)
  fac_vars <- .nlme_fixed_factor_vars(form_str, train_data)
  xlev <- .nlme_get_xlevs(fac_vars, train_data)
  X <- stats::model.matrix(form, data = newdata, xlev = xlev)
  if (nrow(X) != nrow(newdata)) {
    cli::cli_abort(c(
      "Internal error building the fixed-effect design in {.fn get_subject_pars}.",
      "i" = "{nrow(newdata) - nrow(X)} row(s) were dropped (NA or unmatched factor level)."
    ))
  }
  X
}

# Match one (parameter, RE term) to its ranef() column index. nlme names a
# parameter's RE column `<p>.<term>` when it has multiple terms, but a bare
# `<p>` for an intercept-only parameter -- accept both. Aborts when the term
# cannot be matched, or when the column name is duplicated (the only way two
# matches arise for a single term lookup).
.nlme_match_re_column <- function(re_cols, p, t) {
  cands <- paste0(p, ".", t)
  if (t == "(Intercept)") cands <- c(cands, p)
  matches <- which(re_cols %in% cands)
  if (length(matches) == 0L) {
    cli::cli_abort(c(
      "Cannot align random effect {.val {paste0(p, '.', t)}} to {.fn ranef} output.",
      "i" = "Available columns: {.val {re_cols}}"
    ))
  }
  if (length(matches) > 1L) {
    cli::cli_abort(c(
      "Random-effect term {.val {paste0(p, '.', t)}} is ambiguous (duplicate across blocks).",
      "i" = "Duplicate RE terms across {.cls pdBlocked} blocks are not supported; use distinct terms per block."
    ))
  }
  matches
}

# Reconstruct per-row natural-scale Q0 and alpha for an NLME fit.
# `newdata` may be one row per subject (wide path) or one row per
# (subject, factor-cell) (expanded path).
.nlme_build_predicted_pars <- function(object, newdata) {
  pinfo <- object$param_info
  fd <- object$formula_details
  internal_space <- object$param_space %||% pinfo$param_space %||% "log10"
  id_var <- pinfo$id_var

  newdata <- .nlme_coerce_training_factors(newdata, object$data)

  fe <- nlme::fixef(object$model)
  re <- nlme::ranef(object$model)
  re_parsed <- .normalize_re_input(
    fd$random_effects_formula,
    covariance_structure = "pdDiag",
    data = object$data
  )

  subj_idx <- match(as.character(newdata[[id_var]]), rownames(re))
  if (anyNA(subj_idx)) {
    bad <- unique(as.character(newdata[[id_var]])[is.na(subj_idx)])
    cli::cli_abort(c(
      "{.fn get_subject_pars} received subject id(s) not present in the fitted model:",
      "x" = "{.val {bad}}"
    ))
  }

  compute_param <- function(p) {
    form_str <- if (p == "Q0") {
      fd$fixed_effects_formula_str_Q0
    } else {
      fd$fixed_effects_formula_str_alpha
    }
    X <- .nlme_fixed_design(form_str, newdata, object$data)
    # nlme names a parameter's coefficients `<p>.<term>` when it has >1 term,
    # but a bare `<p>` when it has only an intercept. Normalize both to the
    # model-matrix term name ("(Intercept)").
    beta <- fe[names(fe) == p | startsWith(names(fe), paste0(p, "."))]
    bn <- names(beta)
    names(beta) <- ifelse(bn == p, "(Intercept)", sub(paste0("^", p, "\\."), "", bn))
    if (anyDuplicated(colnames(X)) || anyDuplicated(names(beta))) {
      cli::cli_abort(c(
        "Duplicate column/coefficient names in the fixed-effect design for {.field {p}}.",
        "i" = "design: {.val {colnames(X)}}",
        "i" = "coefs: {.val {names(beta)}}"
      ))
    }
    if (!setequal(colnames(X), names(beta))) {
      cli::cli_abort(c(
        "Fixed-effect design columns for {.field {p}} do not match the fitted coefficients.",
        "i" = "design: {.val {colnames(X)}}",
        "i" = "coefs: {.val {names(beta)}}"
      ))
    }
    eta <- as.numeric(X %*% beta[colnames(X)])

    p_low <- tolower(p)
    consumed <- integer(0)
    for (b in re_parsed$blocks) {
      terms_p <- if (p == "Q0") b$terms_q0 else b$terms_alpha
      if (length(terms_p) == 0L) next
      Zb <- .tmb_block_design_columns(b, newdata, parameter = p_low)
      if (nrow(Zb) != nrow(newdata)) {
        cli::cli_abort("Internal error: random-effect design row mismatch in {.fn get_subject_pars}.")
      }
      for (t in terms_p) {
        idx <- .nlme_match_re_column(colnames(re), p, t)
        if (idx %in% consumed) {
          # Same term supplied by two different blocks -> points at one ranef
          # column; double-counting would be silently wrong.
          cli::cli_abort(c(
            "Random-effect term {.val {paste0(p, '.', t)}} is ambiguous (duplicate across blocks).",
            "i" = "Duplicate RE terms across {.cls pdBlocked} blocks are not supported; use distinct terms per block."
          ))
        }
        consumed <- c(consumed, idx)
        eta <- eta + Zb[, t] * re[subj_idx, idx]
      }
    }
    if (internal_space == "log10") 10^eta else eta
  }

  list(Q0 = compute_param("Q0"), alpha = compute_param("alpha"))
}

# Build X_q0/X_alpha/Z_q0/Z_alpha over the TRAINING rows plus the per-row
# subject id, for the within-id design-variation check.
.nlme_subject_design <- function(object) {
  fd <- object$formula_details
  data <- object$data
  id_var <- object$param_info$id_var
  data2 <- .nlme_coerce_training_factors(data, data)

  X_q0 <- .nlme_fixed_design(fd$fixed_effects_formula_str_Q0, data2, data)
  X_alpha <- .nlme_fixed_design(fd$fixed_effects_formula_str_alpha, data2, data)
  re_parsed <- .normalize_re_input(
    fd$random_effects_formula,
    covariance_structure = "pdDiag",
    data = data
  )
  zb <- .tmb_build_z_matrices(re_parsed, data2, id_var)

  list(
    X_q0 = X_q0, X_alpha = X_alpha,
    Z_q0 = zb$Z_q0, Z_alpha = zb$Z_alpha,
    subject_id = as.character(data[[id_var]])
  )
}

# Per-subject within-id design-column variation (port of the TMB
# .check_within_id at R/tmb-demand.R:872-905). Returns a per-subject logical
# flag (ordered by first appearance of the id) plus the named version and the
# offending column names.
.nlme_check_within_id <- function(design, subject_id) {
  subj_levels <- unique(subject_id)
  affected <- stats::setNames(logical(length(subj_levels)),
                              as.character(subj_levels))
  offending <- character(0)
  check_mat <- function(mat, nm) {
    if (is.null(mat) || ncol(mat) == 0L) return(invisible(NULL))
    cn <- colnames(mat)
    if (is.null(cn)) cn <- paste0(nm, "[,", seq_len(ncol(mat)), "]")
    for (j in seq_len(ncol(mat))) {
      sp <- split(mat[, j], subject_id)
      varies <- vapply(sp, function(v) length(unique(v)) > 1L, logical(1))
      if (any(varies)) {
        offending <<- c(offending, cn[j])
        affected[names(varies)[varies]] <<- TRUE
      }
    }
  }
  check_mat(design$X_q0, "X_q0")
  check_mat(design$X_alpha, "X_alpha")
  check_mat(design$Z_q0, "Z_q0")
  check_mat(design$Z_alpha, "Z_alpha")
  list(
    affected = unname(affected),
    affected_named = affected,
    subjects = as.character(subj_levels),
    offending_cols = unique(offending)
  )
}

# Resolve the `expanded` argument for the NLME method. NLME has no fit-time
# subject_pars cache, so the within-id signal comes from the design check.
# Abort + warning strings copied verbatim from .resolve_subject_pars_expanded
# (R/tmb-methods.R) so behavior is identical across backends.
.resolve_subject_pars_expanded_nlme <- function(object, expanded, any_within_id) {
  if (is.null(expanded)) {
    return(isTRUE(any_within_id))
  }
  if (!is.logical(expanded) || length(expanded) != 1L || is.na(expanded)) {
    cli::cli_abort(c(
      "{.arg expanded} must be {.code TRUE}, {.code FALSE}, or {.code NULL}.",
      "i" = "Got {.cls {class(expanded)[1]}} of length {length(expanded)}."
    ))
  }
  if (!expanded && isTRUE(any_within_id)) {
    cli::cli_warn(c(
      "{.field subject_pars} returned with {.field Q0}/{.field alpha} as {.val NA} for affected subjects.",
      "i" = "Call {.code get_subject_pars(fit)} (auto-detect) or {.code get_subject_pars(fit, expanded = TRUE)} for per-(subject, factor-level) values."
    ))
  }
  expanded
}

# Discover within-id candidate variables and build the long
# one-row-per-(subject, factor-cell) newdata. Duplicated from the
# backend-agnostic TMB scaffold (R/tmb-methods.R:1574-1716); RE-RHS variables
# are taken from all.vars() of each parsed RE block formula (so RE-only
# factors are caught), and within-id numerics are conditioned at subject mean.
.nlme_subject_pars_long_newdata <- function(object) {
  pinfo <- object$param_info
  fd <- object$formula_details
  data <- object$data
  id_var <- pinfo$id_var

  re_parsed <- .normalize_re_input(
    fd$random_effects_formula,
    covariance_structure = "pdDiag",
    data = data
  )
  re_rhs_vars <- unique(unlist(lapply(re_parsed$blocks, function(b) {
    setdiff(all.vars(b$formula), c("Q0", "alpha"))
  })))
  # Variables that enter only via fixed_rhs are NOT in pinfo$factors* /
  # continuous_covariates, so pull them straight off the fixed-effect formula
  # strings; otherwise a within-id fixed_rhs term would be silently collapsed
  # to the subject's first row instead of expanded / mean-conditioned.
  fixed_rhs_vars <- unique(c(
    all.vars(stats::as.formula(fd$fixed_effects_formula_str_Q0)),
    all.vars(stats::as.formula(fd$fixed_effects_formula_str_alpha))
  ))

  candidate_vars <- unique(c(
    pinfo$factors, pinfo$factors_Q0, pinfo$factors_alpha,
    pinfo$continuous_covariates, re_rhs_vars, fixed_rhs_vars
  ))
  candidate_vars <- candidate_vars[
    !is.na(candidate_vars) & nzchar(candidate_vars) &
      candidate_vars %in% names(data)
  ]

  classify <- function(var) {
    vals <- data[[var]]
    if (is.factor(vals) || is.character(vals)) {
      type <- "factor"
      lvls <- if (is.factor(vals)) levels(vals) else sort(unique(vals))
    } else if (is.numeric(vals)) {
      type <- "numeric"
      lvls <- NULL
    } else {
      cli::cli_abort(c(
        "Cannot expand {.field subject_pars} over term {.field {var}} of type {.cls {class(vals)[1]}}.",
        "i" = "Pass {.code expanded = FALSE}, or pre-process the variable into a factor or numeric before fitting."
      ))
    }
    by_id <- split(vals, data[[id_var]])
    varies <- any(vapply(by_id, function(v) length(unique(v)) > 1L, logical(1)))
    list(var = var, type = type, varies = varies, levels = lvls)
  }
  classification <- lapply(candidate_vars, classify)
  names(classification) <- candidate_vars

  expand_factors <- candidate_vars[
    vapply(classification,
           function(cls) cls$type == "factor" && cls$varies, logical(1))
  ]

  if (length(expand_factors) > 0L) {
    expand_grid <- expand.grid(
      lapply(expand_factors, function(var) classification[[var]]$levels),
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    )
    names(expand_grid) <- expand_factors
  } else {
    expand_grid <- data.frame(.row = 1L)[, FALSE, drop = FALSE]
  }

  subj_ids <- as.character(unique(data[[id_var]]))
  newdata_rows <- vector("list", length(subj_ids))
  for (i in seq_along(subj_ids)) {
    sid <- subj_ids[i]
    subj_rows <- data[as.character(data[[id_var]]) == sid, , drop = FALSE]
    if (nrow(subj_rows) == 0L) next

    if (ncol(expand_grid) > 0L) {
      cell_rows <- expand_grid
    } else {
      cell_rows <- data.frame(.placeholder = NA)[, character(0), drop = FALSE]
      cell_rows[1L, ".tmp"] <- NA
      cell_rows <- cell_rows[, character(0), drop = FALSE]
    }
    cell_rows[[id_var]] <- sid

    if (!is.null(pinfo$x_var) && nzchar(pinfo$x_var)) {
      cell_rows[[pinfo$x_var]] <- subj_rows[[pinfo$x_var]][1]
    }

    other_vars <- setdiff(
      names(data),
      c(id_var, pinfo$x_var, pinfo$y_var, expand_factors)
    )
    for (v in other_vars) {
      cls <- classification[[v]]
      if (is.null(cls)) {
        cell_rows[[v]] <- subj_rows[[v]][1]
      } else if (cls$type == "factor" && !cls$varies) {
        cell_rows[[v]] <- subj_rows[[v]][1]
      } else if (cls$type == "numeric" && cls$varies) {
        cell_rows[[v]] <- mean(subj_rows[[v]], na.rm = TRUE)
      } else if (cls$type == "numeric" && !cls$varies) {
        cell_rows[[v]] <- subj_rows[[v]][1]
      }
    }

    for (v in expand_factors) {
      cell_rows[[v]] <- factor(cell_rows[[v]], levels = classification[[v]]$levels)
    }

    newdata_rows[[i]] <- cell_rows
  }
  newdata_long <- do.call(rbind, newdata_rows)
  rownames(newdata_long) <- NULL

  list(newdata_long = newdata_long, expand_factors = expand_factors)
}

# Compute Pmax/Omax for a vector of natural-scale (Q0, alpha) with a
# row-aligned price_list (one element per output row, keyed by that row's
# subject) -- matching beezdemand_calc_pmax_omax_vec()'s by-row indexing.
.nlme_pmax_omax <- function(object, Q0, alpha, row_subject_ids) {
  pinfo <- object$param_info
  data <- object$data
  k_val <- pinfo$k
  has_k <- !is.null(k_val)

  price_per_subject <- split(data[[pinfo$x_var]], as.character(data[[pinfo$id_var]]))
  price_list <- lapply(as.character(row_subject_ids), function(sid) {
    ps <- price_per_subject[[sid]]
    if (is.null(ps)) numeric(0) else ps
  })

  if (has_k) {
    params_df <- data.frame(alpha = alpha, q0 = Q0, k = rep(k_val, length(Q0)))
    param_scales <- list(alpha = "natural", q0 = "natural", k = "natural")
    model_type <- "hs"
  } else {
    params_df <- data.frame(alpha = alpha, q0 = Q0)
    param_scales <- list(alpha = "natural", q0 = "natural")
    model_type <- "snd"
  }

  res <- beezdemand_calc_pmax_omax_vec(
    params_df = params_df,
    model_type = model_type,
    param_scales = param_scales,
    price_list = price_list,
    compute_observed = FALSE
  )
  list(Pmax = res$pmax_model, Omax = res$omax_model)
}

#' Get Subject-Specific Parameters from an NLME Demand Model
#'
#' Subject-level demand parameters for a \code{beezdemand_nlme} fit,
#' matching the column / scale / \code{expanded} contract of
#' \code{\link{get_subject_pars.beezdemand_tmb}}. Combines the population
#' fixed effects with each subject's random-effect deviations and
#' back-transforms to the natural scale.
#'
#' @param object A \code{beezdemand_nlme} object.
#' @param expanded Controls the return shape for fits with within-id-varying
#'   design columns (within-subject factors, within-id covariates, or
#'   multi-block \code{pdBlocked} specs).
#'   \itemize{
#'     \item \code{NULL} (default): auto-detect. Expands to one row per
#'       (subject, factor-level) cell when within-id variation is present;
#'       otherwise returns the wide one-row-per-subject shape.
#'     \item \code{TRUE}: always attempt expansion (no-op when there is no
#'       within-id variation).
#'     \item \code{FALSE}: always return the wide shape; emits a one-line
#'       warning when within-id variation is present (the affected subjects'
#'       \code{Q0}, \code{alpha}, \code{Pmax}, \code{Omax} are \code{NA}).
#'   }
#' @param ... Currently unused.
#'
#' @return A data frame. Wide form: \code{id}, \code{b_i}, \code{c_i} (if
#'   alpha has random effects), \code{Q0}, \code{alpha}, \code{Pmax},
#'   \code{Omax}. Expanded form additionally includes the within-subject
#'   factor column(s) with one row per (subject, factor-level) cell.
#'   \code{Q0}, \code{alpha}, \code{Pmax}, \code{Omax} are on the natural
#'   scale.
#'
#' @section Random-effect aliases (\code{b_i} / \code{c_i}):
#'   \code{b_i} / \code{c_i} are the subject's first-block random-effect
#'   deviation for Q0 / alpha. For parity with the TMB method these are
#'   reported on the natural-log linear-predictor scale: for the default
#'   \code{param_space = "log10"} the stored log10 deviation is multiplied by
#'   \code{log(10)}; for \code{param_space = "natural"} the deviation is
#'   returned on the natural parameter scale. The full per-coefficient random
#'   effects remain available via \code{ranef()}.
#'
#' @seealso \code{\link{get_subject_pars.beezdemand_tmb}}
#' @method get_subject_pars beezdemand_nlme
#' @export
get_subject_pars.beezdemand_nlme <- function(object, expanded = NULL, ...) {
  if (!inherits(object, "beezdemand_nlme")) {
    stop("Input 'object' must be of class 'beezdemand_nlme'.")
  }
  if (is.null(object$model)) {
    cli::cli_abort("No fitted model found in the object; fitting may have failed.")
  }

  pinfo <- object$param_info
  internal_space <- object$param_space %||% pinfo$param_space %||% "log10"
  id_var <- pinfo$id_var

  design <- .nlme_subject_design(object)
  check <- .nlme_check_within_id(design, design$subject_id)
  any_within_id <- any(check$affected)
  expanded <- .resolve_subject_pars_expanded_nlme(object, expanded, any_within_id)

  # Per-subject random-effect aliases (natural-log scale to match TMB).
  re <- nlme::ranef(object$model)
  ids <- rownames(re)
  scale_re <- if (internal_space == "log10") log(10) else 1
  # Match `Q0`/`alpha` (bare, intercept-only) or `Q0.`/`alpha.` (multi-term).
  q0_re_cols <- grep("^Q0(\\.|$)", colnames(re), value = TRUE)
  alpha_re_cols <- grep("^alpha(\\.|$)", colnames(re), value = TRUE)
  b_i <- if (length(q0_re_cols) > 0L) re[[q0_re_cols[1]]] * scale_re else NULL
  c_i <- if (length(alpha_re_cols) > 0L) re[[alpha_re_cols[1]]] * scale_re else NULL

  # Wide path: when expanded is FALSE, OR when there is no within-id variation
  # (so expansion is a no-op -- matches the TMB method's early return and keeps
  # row order consistent with ranef()).
  if (!expanded || !any_within_id) {
    # Wide: one row per subject, parameters from the subject's first obs row.
    data <- object$data
    first_idx <- match(ids, as.character(data[[id_var]]))
    newdata_wide <- data[first_idx, , drop = FALSE]
    pp <- .nlme_build_predicted_pars(object, newdata_wide)
    Q0 <- pp$Q0
    alpha <- pp$alpha
    po <- .nlme_pmax_omax(object, Q0, alpha, ids)
    Pmax <- po$Pmax
    Omax <- po$Omax

    # NA only the affected subjects (per-subject parity with TMB).
    aff <- check$affected_named[ids]
    aff[is.na(aff)] <- FALSE
    if (any(aff)) {
      Q0[aff] <- NA_real_
      alpha[aff] <- NA_real_
      Pmax[aff] <- NA_real_
      Omax[aff] <- NA_real_
    }

    out <- data.frame(id = ids, stringsAsFactors = FALSE)
    if (!is.null(b_i)) out$b_i <- b_i
    if (!is.null(c_i)) out$c_i <- c_i
    out$Q0 <- Q0
    out$alpha <- alpha
    out$Pmax <- Pmax
    out$Omax <- Omax
    cols <- c("id", intersect(c("b_i", "c_i"), names(out)),
              "Q0", "alpha", "Pmax", "Omax")
    return(out[, cols])
  }

  # Expanded: one row per (subject, factor-cell).
  nd <- .nlme_subject_pars_long_newdata(object)
  newdata_long <- nd$newdata_long
  expand_factors <- nd$expand_factors

  pp <- .nlme_build_predicted_pars(object, newdata_long)
  Q0 <- pp$Q0
  alpha <- pp$alpha
  row_ids <- as.character(newdata_long[[id_var]])
  po <- .nlme_pmax_omax(object, Q0, alpha, row_ids)

  out <- data.frame(id = newdata_long[[id_var]], stringsAsFactors = FALSE)
  for (v in expand_factors) out[[v]] <- newdata_long[[v]]
  spars_match <- match(row_ids, ids)
  if (!is.null(b_i)) out$b_i <- b_i[spars_match]
  if (!is.null(c_i)) out$c_i <- c_i[spars_match]
  out$Q0 <- Q0
  out$alpha <- alpha
  out$Pmax <- po$Pmax
  out$Omax <- po$Omax
  out
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
#' \donttest{
#' data(ko)
#' fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'                         id_var = "monkey", equation_form = "zben")
#' # Population-level predictions
#' preds <- predict(fit, level = 0)
#'
#' # Subject-level predictions
#' preds_subj <- predict(fit, level = 1)
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

  # --- Collapse-aware display factor mapping ---
  # When collapse_levels was used, model_factors (e.g., "dose") map to collapsed
  # columns (e.g., "dose_alpha") for visual grouping in population-level plots.
  collapse_was_used <- !is.null(fit_obj$collapse_info)
  display_factor_map <- character(0) # named vec: original_name -> display_col
  display_factors <- model_factors # default: same as model_factors

  if (collapse_was_used && !is.null(model_factors)) {
    for (fac in model_factors) {
      alpha_col <- paste0(fac, "_alpha")
      q0_col <- paste0(fac, "_Q0")
      alpha_ok <- alpha_col %in% factors_alpha &&
        alpha_col %in% names(plot_data_orig) &&
        is.factor(plot_data_orig[[alpha_col]])
      q0_ok <- q0_col %in% factors_Q0 &&
        q0_col %in% names(plot_data_orig) &&
        is.factor(plot_data_orig[[q0_col]])

      if (alpha_ok && q0_ok) {
        # When Q0 and alpha have different collapse structures, pick the column
        # with fewer levels for display grouping. This is a simplification:
        # the plot shows one set of groups, not both structures simultaneously.
        n_alpha <- nlevels(plot_data_orig[[alpha_col]])
        n_q0 <- nlevels(plot_data_orig[[q0_col]])
        display_factor_map[[fac]] <- if (n_alpha <= n_q0) {
          alpha_col
        } else {
          q0_col
        }
      } else if (alpha_ok) {
        display_factor_map[[fac]] <- alpha_col
      } else if (q0_ok) {
        display_factor_map[[fac]] <- q0_col
      }
    }

    if (length(display_factor_map) > 0) {
      display_factors <- vapply(
        model_factors,
        function(f) {
          if (f %in% names(display_factor_map)) {
            display_factor_map[[f]]
          } else {
            f
          }
        },
        character(1),
        USE.NAMES = FALSE
      )
    }
  }

  .remap_aesthetic <- function(var_name) {
    if (is.null(var_name)) {
      return(NULL)
    }
    if (var_name %in% names(display_factor_map)) {
      display_factor_map[[var_name]]
    } else {
      var_name
    }
  }

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

  # --- Remap aesthetic arguments to use display (collapsed) factor columns ---
  color_by_orig <- color_by
  shape_by_orig <- shape_by
  linetype_by_orig <- linetype_by

  color_by <- .remap_aesthetic(color_by)
  shape_by <- .remap_aesthetic(shape_by)
  linetype_by <- .remap_aesthetic(linetype_by)

  if (!is.null(facet_formula) && is.character(facet_formula) &&
    length(display_factor_map) > 0) {
    for (orig_name in names(display_factor_map)) {
      facet_formula <- gsub(
        paste0("\\b", orig_name, "\\b"),
        display_factor_map[[orig_name]],
        facet_formula
      )
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

    # --- Aggregate population predictions for collapsed groups ---
    # When collapse_levels is used, multiple original factor combos map to the
    # same collapsed group. Average their population predictions so the plot
    # shows one line per collapsed group (consistent with emmeans output).
    if (current_pred_level == 0 && collapse_was_used &&
      length(display_factor_map) > 0) {
      # After aggregation, only display_factors + x + continuous + predicted_y
      # survive. Original factor columns are dropped, which is fine since
      # downstream aesthetics (color_by, linetype_by) have been remapped to
      # display_factors and line grouping uses display_factors.
      agg_group_cols <- c(display_factors, x_var_name, cont_covars_all)
      agg_group_cols <- intersect(agg_group_cols, names(pred_newdata))
      if (length(agg_group_cols) > 0) {
        pred_newdata <- pred_newdata |>
          dplyr::group_by(dplyr::across(dplyr::all_of(agg_group_cols))) |>
          dplyr::summarise(
            predicted_y_plotscale = mean(
              .data$predicted_y_plotscale,
              na.rm = TRUE
            ),
            .groups = "drop"
          )
      }
    }

    # Sort pred_newdata
    grouping_vars_for_sort <- character(0)
    if (current_pred_level > 0 && id_var_name %in% names(pred_newdata)) {
      grouping_vars_for_sort <- c(grouping_vars_for_sort, id_var_name)
    }
    sort_factors <- if (current_pred_level == 0 && collapse_was_used) {
      display_factors
    } else {
      model_factors
    }
    if (!is.null(sort_factors)) {
      grouping_vars_for_sort <- c(
        grouping_vars_for_sort,
        intersect(sort_factors, names(pred_newdata))
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

    # For population lines, ensure ALL relevant factors define distinct lines
    # Use display_factors (collapsed columns) when collapse_levels was used
    if (current_pred_level == 0) {
      pop_grouping_factors <- if (collapse_was_used &&
        length(display_factor_map) > 0) {
        display_factors
      } else {
        model_factors
      }
      if (!is.null(pop_grouping_factors)) {
        for (fac in pop_grouping_factors) {
          if (fac %in% names(pred_newdata) && !(fac %in% line_group_vars)) {
            line_group_vars <- c(line_group_vars, fac)
          }
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

  # Use original (user-facing) names for legend titles, not collapsed column names
  legend_labs_list <- list()
  if (!is.null(color_by)) {
    legend_labs_list$color <- color_by_orig
  }
  if (!is.null(shape_by)) {
    legend_labs_list$shape <- shape_by_orig
  }
  if (!is.null(linetype_by)) {
    legend_labs_list$linetype <- linetype_by_orig
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
#' \donttest{
#' data(ko)
#' fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'                         id_var = "monkey", factors = "drug",
#'                         equation_form = "zben")
#' individual_coefs <- get_individual_coefficients(fit)
#' head(individual_coefs)
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
#' scale. For `equation_form = "simplified"` or `"exponentiated"`, this is the natural
#' consumption scale.
#'
#' To back-transform predictions to the natural scale for "zben" models, use:
#' `ll4_inv(augmented$.fitted)`
#'
#' @examples
#' \donttest{
#' data(ko)
#' fit <- fit_demand_mixed(ko, y_var = "y_ll4", x_var = "x",
#'                         id_var = "monkey", factors = "dose", equation_form = "zben")
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
    if (is.null(data)) {
      stop("No data available. Provide 'newdata' or ensure model contains data.",
           call. = FALSE)
    }

    fitted_vals <- stats::fitted(x$model)
    resid_vals <- stats::residuals(x$model)
    fixed_vals <- stats::predict(x$model, level = 0)

    out <- tibble::as_tibble(data)

    if (length(fitted_vals) == nrow(out)) {
      out$.fitted <- as.numeric(fitted_vals)
      out$.resid <- as.numeric(resid_vals)
      out$.fixed <- as.numeric(fixed_vals)
      return(out)
    }

    warning(
      "Fitted values length doesn't match data; returning NA diagnostics columns.",
      call. = FALSE
    )
    out$.fitted <- NA_real_
    out$.resid <- NA_real_
    out$.fixed <- NA_real_
    return(out)
  }

  data <- if (is.data.frame(newdata)) newdata else as.data.frame(newdata)
  out <- tibble::as_tibble(data)

  id_var <- x$param_info$id_var
  y_var <- x$param_info$y_var
  has_id <- !is.null(id_var) && id_var %in% names(data)

  fitted_tbl <- predict(x,
    newdata = data,
    type = if (has_id) "individual" else "population"
  )
  fixed_tbl <- predict(x, newdata = data, type = "population")

  out$.fitted <- fitted_tbl$.fitted
  out$.fixed <- fixed_tbl$.fitted

  if (!is.null(y_var) && y_var %in% names(data)) {
    out$.resid <- as.numeric(data[[y_var]]) - out$.fitted
  } else {
    out$.resid <- NA_real_
  }

  out
}
