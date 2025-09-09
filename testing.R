#' Calculate Individual-Level Predicted Coefficients from beezdemand_nlme Model
#'
#' This function extracts and combines fixed and random effects to calculate
#' individual-level predicted coefficients for all parameter-factor combinations
#' from a beezdemand_nlme model object. It automatically detects the factor
#' structure and calculates coefficients for each individual and factor level.
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

#' Calculate Individual-Level Predicted Coefficients from beezdemand_nlme Model
#' (FIXED VERSION)
get_individual_coefficients_v2 <- function(
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

    # Test the v2 function
    if (exists("tmp")) {
      cat("=== TESTING VERSION 2 ===\n")

      individual_coefs_v2 <- get_individual_coefficients_v2(
        tmp,
        format = "wide"
      )
      print("Version 2 results:")
      print(individual_coefs_v2)

      cat("\nColumn names in v2:\n")
      print(names(individual_coefs_v2))

      # Compare with manual calculation
      cat("\n=== COMPARISON WITH MANUAL CALCULATION ===\n")
      cat("Manual estimated_Q0_intercept:\n")
      print(estimated_Q0_intercept)
      cat("V2 estimated_Q0_intercept:\n")
      print(individual_coefs_v2$estimated_Q0_intercept)
      cat(
        "Match:",
        all.equal(
          as.numeric(estimated_Q0_intercept),
          individual_coefs_v2$estimated_Q0_intercept
        ),
        "\n"
      )
    }
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

# Test the function with your current model
if (exists("tmp")) {
  cat("Testing the FIXED function with your current model:\n")

  # Test wide format
  individual_coefs_wide_fixed <- get_individual_coefficients(
    tmp,
    format = "wide"
  )
  print("Fixed Wide format:")
  print(individual_coefs_wide_fixed)

  cat("\nColumn names in fixed version:\n")
  print(names(individual_coefs_wide_fixed))
}

# Debug the filtering issue
if (exists("tmp")) {
  # Extract fixed and random effects
  fixed_effects <- coef(tmp, type = "fixed")
  random_effects <- ranef(tmp)

  cat("=== DEBUGGING COEFFICIENT PROCESSING ===\n")

  for (param in c("Q0", "alpha")) {
    cat("\n--- Processing parameter:", param, "---\n")

    # Get coefficient names for this parameter from fixed effects
    param_coef_names <- names(fixed_effects)[grepl(
      paste0("^", param, "\\."),
      names(fixed_effects)
    )]
    cat("All param_coef_names for", param, ":\n")
    print(param_coef_names)

    # Skip the intercept since we already processed it above
    factor_coef_names <- param_coef_names[
      !grepl("\\.(Intercept)$", param_coef_names)
    ]
    cat("After filtering out intercept:\n")
    print(factor_coef_names)

    # Check if intercept is being filtered correctly
    intercept_pattern_match <- grepl("\\.(Intercept)$", param_coef_names)
    cat("Intercept pattern matches:\n")
    print(data.frame(
      coef_name = param_coef_names,
      is_intercept = intercept_pattern_match
    ))
  }
}
