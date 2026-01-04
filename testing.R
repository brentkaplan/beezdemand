devtools::document()
devtools::load_all()
devtools::test()


library(tidyverse)

load("data/apt_full.RData")



# ISSUE: does this actually fit in log10 space?
fit_fixed <- fit_demand_fixed(
  apt_full,
  equation = "koff",
  k = 2,
  y_var = "y",
  x_var = "x",
  id_var = "id",
  param_space = "log10"
)

fit_fixed
summary(fit_fixed)
summary(fit_fixed, report_space = "log10")

tidy(fit_fixed)
tidy(fit_fixed, report_space = "log10")
glance(fit_fixed)
plot(fit_fixed, type = "individual", ids = 1:10, style = "modern")
plot(fit_fixed, type = "individual", ids = 1:50, style = "apa")

# how is population being plotted when it's by individual?
plot(fit_fixed, type = "population")
# this shows a facet with a plot on the left and right. They have different x axes and there is one point showing on the left?
plot(fit_fixed, type = "both", ids = 3)

plot(fit_fixed, style = "modern")
plot(fit_fixed, style = "apa")

# FIX: make sure it always uses empirical metrics except alpha
calculate_amplitude_persistence(fit_fixed)

fit_mixed <- fit_demand_mixed(
  apt_full,
  equation = "simplified",
  y_var = "y",
  x_var = "x",
  id_var = "id",
  start_value_method = "pooled",
  param_space = "natural"
)

fit_mixed
# report_space doesn't do anything here?
summary(fit_mixed, report_space = "log10")
tidy(fit_mixed)
glance(fit_mixed)
coef(fit_mixed, type = "fixed")
coef(fit_mixed, type = "random")
plot(fit_mixed, style = "modern")
plot(fit_mixed, y_trans = "linear", style = "apa")
# make consistent with other plotting?
# i like the options of this plotting function.
plot(fit_mixed, show_pred_lines = "population")
plot(fit_mixed, type = "population")
# PROBLEM WITH Y AXIS
plot(fit_mixed, type = "both",  y_trans = "linear", style = "modern")
plot(fit_mixed, type = "individual",  y_trans = "log10", style = "modern")
plot(fit_mixed, type = c("both"), ids = 1:5, style = "apa", facet = "~id", free_trans = 0.01)
calculate_amplitude_persistence(fit_mixed)

# FIX
# > calculate_amplitude_persistence(fit_mixed)
# Warning message:
# In calculate_amplitude_persistence.default(df_final, amplitude = amplitude,  :
#   Non-positive values found in 'alpha'. These are treated as NA before computing 1/alpha.
#      id         z_Q0       z_BP0      z_Pmax      z_Omax z_inv_alpha    Amplitude Persistence
# 1     1  1.000527918          NA  0.55342006  1.39812407          NA  1.000527918  0.97577206
# 2     2 -0.099980872 -0.15545449 -0.43354827 -0.29260940          NA -0.099980872 -0.29387072
# 3     3  1.065404471  2.09230893 -0.23615460  0.59059475          NA  1.065404471  0.81558303
# 4     4  0.047359803  2.09230893 -0.43354827 -0.16039125          NA  0.047359803  0.49945647
# 5     5 -1.209065140          NA  0.35602639 -0.74330529          NA -1.209065140 -0.19363945
# 6     6  0.591599421  0.21917275 -0.23615460  0.28851207          NA  0.591599421  0.09051007
# 7     7 -0.214735081  1.15574084 -0.43354827 -0.32611564          NA -0.214735081  0.13202564
# 8     8  0.083854522  1.15574084 -0.23615460 -0.01088961          NA  0.083854522  0.30289887


fit1 <- fit_demand_hurdle(
  apt_full,
  y_var = "y",
  x_var = "x",
  id_var = "id",
  random_effects = c("zeros", "q0", "alpha"),
  verbose = 1
)

fit1
summary(fit1)
summary(fit1, report_space = "natural")
tidy(fit1)
glance(fit1)
coef(fit1)
plot(fit1)
plot(fit1, type = "parameters", parameters = c("Q0", "alpha"))
plot(fit1, type = "probability")
plot(fit1, type = "both", ids = 1:10, style = "modern")
# should this ONLY show individuals?
plot(fit1, type = c("individual"), ids = 1:10, style = "apa")
plot(fit1, type = c("population"), ids = 1:10, style = "apa")
calculate_amplitude_persistence(fit1)

# different:
plot(fit1, type = c("both"), ids = 1:10, style = "apa")
plot(fit_mixed, type = c("both"), ids = 1:10, style = "apa", facet = "~id")


# Single subject with observed data
plot_subject(fit1, subject_id = "5", show_data = TRUE, show_population = TRUE)

## systematic between demand and cross-price
sys_demand_legacy <- CheckUnsystematic(apt_full)



sys_demand <- check_systematic_demand(apt_full)
# does this give same results whether arranged by contiguous ids
sys_cp <- check_systematic_cp(lnic |> dplyr::filter(target == "fixed"))


head(sys_demand_legacy, 10)
summary(sys_demand)
tidy(sys_demand)
summary(sys_cp)
## fix logic?
tidy(sys_cp)

vignette("cross-price-models", package = "beezdemand")

data("lowNicClean", package = "beezdemand")
lnic <- lowNicClean |>
    mutate(
        target = if_else(commodity == "cigarettesAdj", "adjusting", "fixed"),
        group = "cigarettes"
    ) |>
    select(
        -commodity
    ) |>
    dplyr::relocate(condition, .after = group)


fit_cp_fixed <- fit_cp_nls(
    dat = lnic |> filter(target == "fixed"),
    equation = "exponentiated",
    return_all = TRUE
)

summary(fit_cp_fixed)
tidy(fit_cp_fixed)
glance(fit_cp_fixed)
plot(fit_cp_fixed)

fit_hurdle_3re <- fit_cp_hurdle(
    data = lnic |> dplyr::filter(target == "fixed"),
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "qalone", "I"),
    verbose = 1
)

fit_hurdle_3re
summary(fit_hurdle_3re)
## FIX:
# Interpretation:
#   I > 0: Products are COMPLEMENTS (alternative increases consumption)
tidy(fit_hurdle_3re)
glance(fit_hurdle_3re)
coef(fit_hurdle_3re)
plot(fit_hurdle_3re)
plot(fit_hurdle_3re, type = "parameters")[[2]]
plot(fit_hurdle_3re, type = "probability")
plot(fit_hurdle_3re, type = "individual", ids = c("R_6iq6ODQxRMuSiTr", "R_3vHnLN36lvWj0tT", "R_0JaPVcb1BFHlnDL",
"R_8oxGzmd90OztYd7", "R_50h99dyJJwnNu29", "R_9uJ0rRVEU0bSZ2R",
"R_6MaMsTIFtS3Vy2V", "R_3mYZlAmf0ZW3H6t", "R_0etAcPeTv9Wjkd7",
"R_783sSMqrXMsoViB"), style = "apa")



## brms:

# Example brms setup (Outside of Source Material)
library(brms)

# Define the nonlinear formula
# hu is the hurdle (probability of being zero)
# Q is the quantity part using the exponential equation
demand_formula <- bf(
  consumption ~ Q0 * 10^(k * (exp(-alpha * Q0 * price) - 1)),
  alpha ~ 1 + (1|ID),
  Q0 ~ 1 + (1|ID),
  hu ~ log(price + 0.001) + (1|ID), # The logistic/hurdle part
  nl = TRUE
)

# Set priors (BHMs require prior distributions)
demand_priors <- c(
  prior(normal(10, 5), nlpar = "Q0"),
  prior(normal(0.001, 0.01), nlpar = "alpha"),
  prior(normal(0, 5), class = "b", dpar = "hu")
)

# Fit the model
fit_hurdle <- brm(
  formula = demand_formula,
  data = your_data,
  family = hurdle_lognormal(), # Or hurdle_gamma() depending on residuals
  prior = demand_priors,
  iter = 4000,
  chains = 4
)


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
