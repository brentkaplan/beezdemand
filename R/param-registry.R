#' Parameter Naming Registry for beezdemand
#'
#' This file defines canonical parameter naming conventions across all model
#' families. It serves as the single source of truth for parameter names,
#' scales, and mappings.
#'
#' @section Naming Convention:
#' Parameters follow snake_case with scale prefixes:
#' \itemize{
#'   \item \code{log10_<param>}: parameter in log10 space (e.g., log10_alpha)
#'   \item \code{log_<param>}: parameter in natural log space (e.g., log_alpha)
#'   \item \code{natural_<param>}: parameter in natural/linear space (e.g., natural_alpha)
#'   \item \code{<param>}: canonical name without prefix (context determines scale)
#' }
#'
#' @section Equation Registry:
#' Equations are grouped by model family:
#' \itemize{
#'   \item \strong{NLS (FitCurves)}: \code{hs}, \code{koff}, \code{linear}. The \code{k}
#'     parameter defaults to fixed but supports multiple modes: \code{"fit"}, \code{"ind"},
#'     \code{"share"}, \code{"range"}.
#'   \item \strong{NLME (fit_demand_mixed)}: \code{zben}, \code{nlme_simplified},
#'     \code{nlme_exponentiated}. Only \code{nlme_exponentiated} uses \code{k} (always fixed).
#'   \item \strong{Hurdle Part II (fit_demand_hurdle)}: \code{hurdle_zhao},
#'     \code{hurdle_hs}, \code{hurdle_snd}. Parameters are in log space. \code{hurdle_snd}
#'     has no \code{k} parameter. Each variant has 2RE and 3RE sub-models with different
#'     variance/correlation parameters.
#'   \item \strong{Cross-price (fit_cp_nls)}: \code{cp_exponential}, \code{cp_exponentiated},
#'     \code{cp_additive}. All use \code{log10_qalone}, \code{I}, \code{log10_beta}.
#' }
#'
#' @section Derived Metrics Naming:
#' \itemize{
#'   \item \code{pmax_model}: Pmax derived from fitted model parameters
#'   \item \code{pmax_obs}: Pmax from observed/empirical data (max expenditure price)
#'   \item \code{omax_model}: Omax derived from fitted model parameters
#'   \item \code{omax_obs}: Omax from observed/empirical data
#'   \item \code{alpha_star}: Normalized alpha (Strategy B; Rzeszutek et al., 2025).
#'     Makes alpha comparable across different k values by adjusting for the
#'     scaling constant. Computed as \eqn{\alpha^* = -\alpha / \ln(1 - 1/(k \cdot
#'     \ln(b)))}{alpha* = -alpha / ln(1 - 1/(k*ln(b)))} where \eqn{b} is the
#'     logarithmic base (10 for HS/Koff, \eqn{e} for hurdle models). Returned by
#'     \code{\link{FitCurves}}, \code{\link[=tidy.beezdemand_fixed]{tidy()}} on
#'     fixed fits, and \code{\link[=tidy.beezdemand_hurdle]{tidy()}} on hurdle
#'     fits. Standard error (\code{alpha_star_se}) is obtained via the delta
#'     method. Requires \eqn{k \cdot \ln(b) > 1}{k*ln(b) > 1}; otherwise
#'     \code{NA} is returned.
#' }
#'
#' @section Legacy Column Mappings:
#' Legacy FitCurves output uses different naming. The mappings are:
#' \itemize{
#'   \item \code{Pmaxd}: derived Pmax from model (approximate formula)
#'   \item \code{Pmaxa}: analytic Pmax from Lambert W
#'   \item \code{Pmaxe}: empirical/observed Pmax from data
#'   \item \code{Omaxd}: derived Omax from model
#'   \item \code{Omaxa}: analytic Omax from Lambert W
#'   \item \code{Omaxe}: empirical/observed Omax from data
#'   \item \code{Q0d}: fitted Q0 (intensity)
#'   \item \code{Alpha}: fitted alpha (elasticity parameter)
#'   \item \code{K}: k parameter (range in log units)
#' }
#'
#' @name param-registry
#' @keywords internal
NULL


# =============================================================================
# Parameter Registry Data Structures
# =============================================================================

#' Canonical Parameter Definitions
#'
#' A list defining all canonical parameters, their constraints, and scale info.
#'
#' @keywords internal
.beezdemand_param_registry <- list(
  # Core demand parameters
  q0 = list(
    canonical = "q0",
    description = "Intensity (consumption at price 0)",
    constraint = "q0 > 0",
    valid_scales = c("natural", "log", "log10"),
    default_scale = "natural",
    synonyms = c("Q0", "Q0d", "Intensity", "intensity")
  ),

  alpha = list(
    canonical = "alpha",
    description = "Elasticity/decay parameter",
    constraint = "alpha > 0",
    valid_scales = c("natural", "log", "log10"),
    default_scale = "natural",
    synonyms = c("Alpha", "Elasticity", "elasticity")
  ),

  k = list(
    canonical = "k",
    description = "Range parameter (log units)",
    constraint = "k > 0",
    valid_scales = c("natural", "log", "log10"),
    default_scale = "natural",
    synonyms = c("K")
  ),

  # Cross-price parameters
  qalone = list(
    canonical = "qalone",
    description = "Consumption of target alone (cross-price)",
    constraint = "qalone > 0",
    valid_scales = c("natural", "log", "log10"),
    default_scale = "log10",
    synonyms = c("Qalone", "Q_alone")
  ),

  # Note: Cross-price formulas use uppercase "I" as the parameter name
  i = list(
    canonical = "i",
    description = "Cross-price interaction parameter",
    constraint = "real",
    valid_scales = c("natural"),
    default_scale = "natural",
    synonyms = c("I", "Interaction")
  ),

  beta = list(
    canonical = "beta",
    description = "Cross-price decay parameter",
    constraint = "beta > 0",
    valid_scales = c("natural", "log", "log10"),
    default_scale = "log10",
    synonyms = c("Beta")
  ),

  # Hurdle model probability parameters
  beta0 = list(
    canonical = "beta0",
    description = "Intercept for zero probability (logit scale)",
    constraint = "real",
    valid_scales = c("natural"),
    default_scale = "natural",
    synonyms = c("Beta0")
  ),

  beta1 = list(
    canonical = "beta1",
    description = "Slope for zero probability (logit scale)",
    constraint = "real",
    valid_scales = c("natural"),
    default_scale = "natural",
    synonyms = c("Beta1")
  ),

  # Note: gamma0/gamma1 exist in joint hurdle models only (fit_joint_hurdle),
  # which are not yet part of the public API. They will be added here if/when
  # joint-hurdle models become public.

  # Hurdle model variance/correlation parameters (TMB-estimated)
  logsigma_a = list(
    canonical = "logsigma_a",
    description = "Log SD of random effect a_i (Part I zero probability)",
    constraint = "real",
    valid_scales = c("natural"),
    default_scale = "natural",
    synonyms = character(0)
  ),

  logsigma_b = list(
    canonical = "logsigma_b",
    description = "Log SD of random effect b_i (Part II Q0)",
    constraint = "real",
    valid_scales = c("natural"),
    default_scale = "natural",
    synonyms = character(0)
  ),

  logsigma_c = list(
    canonical = "logsigma_c",
    description = "Log SD of random effect c_i (Part II alpha; 3RE only)",
    constraint = "real",
    valid_scales = c("natural"),
    default_scale = "natural",
    synonyms = character(0)
  ),

  logsigma_e = list(
    canonical = "logsigma_e",
    description = "Log SD of residual error (Part II)",
    constraint = "real",
    valid_scales = c("natural"),
    default_scale = "natural",
    synonyms = character(0)
  ),

  rho_ab_raw = list(
    canonical = "rho_ab_raw",
    description = "Unbounded correlation between a_i and b_i random effects",
    constraint = "real",
    valid_scales = c("natural"),
    default_scale = "natural",
    synonyms = character(0)
  ),

  rho_ac_raw = list(
    canonical = "rho_ac_raw",
    description = "Unbounded correlation between a_i and c_i random effects (3RE only)",
    constraint = "real",
    valid_scales = c("natural"),
    default_scale = "natural",
    synonyms = character(0)
  ),

  rho_bc_raw = list(
    canonical = "rho_bc_raw",
    description = "Unbounded partial correlation between b_i and c_i random effects (3RE only)",
    constraint = "real",
    valid_scales = c("natural"),
    default_scale = "natural",
    synonyms = character(0)
  )
)


#' Derived Metrics Registry
#'
#' Defines canonical names for derived metrics and their legacy mappings.
#'
#' @keywords internal
.beezdemand_derived_registry <- list(
  pmax_model = list(
    canonical = "pmax_model",
    description = "Price at maximum expenditure (model-derived)",
    legacy_synonyms = c("Pmaxd", "Pmaxa", "Pmax")
  ),

  pmax_obs = list(
    canonical = "pmax_obs",
    description = "Price at maximum expenditure (observed/empirical)",
    legacy_synonyms = c("Pmaxe")
  ),

  omax_model = list(
    canonical = "omax_model",
    description = "Maximum expenditure (model-derived)",
    legacy_synonyms = c("Omaxd", "Omaxa", "Omax")
  ),

  omax_obs = list(
    canonical = "omax_obs",
    description = "Maximum expenditure (observed/empirical)",
    legacy_synonyms = c("Omaxe")
  ),

  q_at_pmax_model = list(
    canonical = "q_at_pmax_model",
    description = "Consumption at Pmax (model-derived)",
    legacy_synonyms = c("Qmax")
  ),

  elasticity_at_pmax = list(
    canonical = "elasticity_at_pmax_model",
    description = "Elasticity at Pmax (should be ~-1)",
    legacy_synonyms = character(0)
  ),

  ev = list(
    canonical = "ev",
    description = "Essential value",
    legacy_synonyms = c("EV")
  ),

  alpha_star = list(
    canonical = "alpha_star",
    description = "Normalized alpha (Strategy B; comparable across k values)",
    legacy_synonyms = character(0)
  ),

  alpha_star_se = list(
    canonical = "alpha_star_se",
    description = "Standard error of alpha_star (delta method)",
    legacy_synonyms = character(0)
  ),

  breakpoint = list(
    canonical = "breakpoint",
    description = "Price at which consumption reaches zero",
    legacy_synonyms = c("BP0", "BP", "Breakpoint")
  )
)


#' Equation Registry
#'
#' Maps equation IDs to their parameter requirements and scales.
#'
#' @keywords internal
.beezdemand_equation_registry <- list(
  # ---------------------------------------------------------------------------
  # NLS equations (FitCurves)
  # ---------------------------------------------------------------------------
  # Note: In FitCurves(), k defaults to fixed but supports multiple modes:
  # numeric (fixed value), "fit" (free parameter), "ind" (per-individual),
  # "share" (group-shared), "range" (empirical sample range).

  hs = list(
    id = "hs",
    name = "Hursh & Silberberg (2008) Exponential",
    future_id = "exponential_demand",
    params_estimated = c("q0", "alpha"),
    params_default_fixed = c("k"),
    k_modes = c("fixed", "fit", "ind", "share", "range"),
    default_param_space = "natural",
    response_scale = "log10",
    zero_handling = "drop"
  ),

  koff = list(
    id = "koff",
    name = "Koffarnus et al. (2015) Exponentiated",
    future_id = "exponentiated_demand",
    params_estimated = c("q0", "alpha"),
    params_default_fixed = c("k"),
    k_modes = c("fixed", "fit", "ind", "share", "range"),
    default_param_space = "natural",
    response_scale = "natural",
    zero_handling = "keep"
  ),

  simplified = list(
    id = "simplified",
    name = "Simplified Exponential (Rzeszutek et al., 2025)",
    future_id = "simplified_exponential_demand",
    params_estimated = c("q0", "alpha"),
    params_fixed = character(0),
    default_param_space = "natural",
    response_scale = "natural",
    zero_handling = "keep"
  ),

  # Note: internal formula uses lowercase "l" but output columns use uppercase "L"
  linear = list(
    id = "linear",
    name = "Linear Demand",
    future_id = "linear_demand",
    params_estimated = c("L", "b", "a"),
    params_fixed = character(0),
    default_param_space = "natural",
    response_scale = "natural",
    zero_handling = "keep"
  ),

  # ---------------------------------------------------------------------------
  # NLME equations (fit_demand_mixed)
  # ---------------------------------------------------------------------------

  zben = list(
    id = "zben",
    name = "Zero-Bounded Exponential (LL4)",
    future_id = "zben_ll4_demand",
    params_estimated = c("q0", "alpha"),
    # k is not used in this equation form
    params_fixed = character(0),
    default_param_space = "log10",
    response_scale = "ll4",
    zero_handling = "transform"
  ),

  nlme_simplified = list(
    id = "nlme_simplified",
    name = "Simplified Normalized Demand (NLME)",
    future_id = "simple_demand",
    params_estimated = c("q0", "alpha"),
    params_fixed = character(0),
    default_param_space = "natural",
    response_scale = "natural",
    zero_handling = "keep"
  ),

  nlme_exponentiated = list(
    id = "nlme_exponentiated",
    name = "Koffarnus Exponentiated (NLME)",
    future_id = "nlme_exponentiated_demand",
    params_estimated = c("q0", "alpha"),
    # k is always fixed in NLME context (calculated from data if NULL)
    params_fixed = c("k"),
    default_param_space = "log10",
    response_scale = "natural",
    zero_handling = "keep"
  ),

  # ---------------------------------------------------------------------------
  # Hurdle Part II equations (fit_demand_hurdle)
  # ---------------------------------------------------------------------------
  # All hurdle equations include Part I (logistic: beta0, beta1) and Part II
  # (nonlinear demand on log consumption). TMB parameters are in log space.
  # Variance/correlation parameters differ between 2RE and 3RE sub-models.

  hurdle_zhao = list(
    id = "hurdle_zhao",
    name = "Hurdle Zhao Exponential (Part II)",
    future_id = "hurdle_zhao_demand",
    params_estimated = c("beta0", "beta1", "log_q0", "log_k", "log_alpha"),
    params_fixed = character(0),
    default_param_space = "log",
    response_scale = "log",
    zero_handling = "model",
    variance_params_2re = c(
      "logsigma_a",
      "logsigma_b",
      "logsigma_e",
      "rho_ab_raw"
    ),
    variance_params_3re = c(
      "logsigma_a",
      "logsigma_b",
      "logsigma_c",
      "logsigma_e",
      "rho_ab_raw",
      "rho_ac_raw",
      "rho_bc_raw"
    )
  ),

  hurdle_hs = list(
    id = "hurdle_hs",
    name = "Hurdle HS/StdQ0 Exponential (Part II)",
    future_id = "hurdle_hs_demand",
    params_estimated = c("beta0", "beta1", "log_q0", "log_k", "log_alpha"),
    params_fixed = character(0),
    default_param_space = "log",
    response_scale = "log",
    zero_handling = "model",
    variance_params_2re = c(
      "logsigma_a",
      "logsigma_b",
      "logsigma_e",
      "rho_ab_raw"
    ),
    variance_params_3re = c(
      "logsigma_a",
      "logsigma_b",
      "logsigma_c",
      "logsigma_e",
      "rho_ab_raw",
      "rho_ac_raw",
      "rho_bc_raw"
    )
  ),

  hurdle_snd = list(
    id = "hurdle_snd",
    name = "Hurdle Simplified/SND (Part II)",
    future_id = "hurdle_snd_demand",
    # No log_k parameter in SND variant
    params_estimated = c("beta0", "beta1", "log_q0", "log_alpha"),
    params_fixed = character(0),
    default_param_space = "log",
    response_scale = "log",
    zero_handling = "model",
    variance_params_2re = c(
      "logsigma_a",
      "logsigma_b",
      "logsigma_e",
      "rho_ab_raw"
    ),
    variance_params_3re = c(
      "logsigma_a",
      "logsigma_b",
      "logsigma_c",
      "logsigma_e",
      "rho_ab_raw",
      "rho_ac_raw",
      "rho_bc_raw"
    )
  ),

  # ---------------------------------------------------------------------------
  # TMB mixed-effects equations (fit_demand_tmb)
  # ---------------------------------------------------------------------------

  tmb_exponential = list(
    id = "tmb_exponential",
    name = "TMB Exponential (Hursh & Silberberg, 2008)",
    future_id = "tmb_exponential_demand",
    params_estimated = c("beta_q0", "beta_alpha", "log_k"),
    params_fixed = character(0),
    default_param_space = "log",
    response_scale = "log",
    zero_handling = "drop",
    variance_params_1re = c("logsigma_b", "logsigma_e"),
    variance_params_2re = c("logsigma_b", "logsigma_c", "logsigma_e", "rho_bc_raw")
  ),

  tmb_exponentiated = list(
    id = "tmb_exponentiated",
    name = "TMB Exponentiated (Koffarnus et al., 2015)",
    future_id = "tmb_exponentiated_demand",
    params_estimated = c("beta_q0", "beta_alpha", "log_k"),
    params_fixed = character(0),
    default_param_space = "log",
    response_scale = "natural",
    zero_handling = "keep",
    variance_params_1re = c("logsigma_b", "logsigma_e"),
    variance_params_2re = c("logsigma_b", "logsigma_c", "logsigma_e", "rho_bc_raw")
  ),

  tmb_simplified = list(
    id = "tmb_simplified",
    name = "TMB Simplified Exponential",
    future_id = "tmb_simplified_demand",
    params_estimated = c("beta_q0", "beta_alpha"),
    params_fixed = character(0),
    default_param_space = "log",
    response_scale = "natural",
    zero_handling = "keep",
    variance_params_1re = c("logsigma_b", "logsigma_e"),
    variance_params_2re = c("logsigma_b", "logsigma_c", "logsigma_e", "rho_bc_raw")
  ),

  tmb_zben = list(
    id = "tmb_zben",
    name = "TMB Zero-Bounded Exponential (LL4)",
    future_id = "tmb_zben_demand",
    params_estimated = c("beta_q0", "beta_alpha"),
    params_fixed = character(0),
    default_param_space = "log",
    response_scale = "ll4",
    zero_handling = "transform",
    variance_params_1re = c("logsigma_b", "logsigma_e"),
    variance_params_2re = c("logsigma_b", "logsigma_c", "logsigma_e", "rho_bc_raw")
  ),

  # ---------------------------------------------------------------------------
  # Cross-price equations (fit_cp_nls)
  # ---------------------------------------------------------------------------

  cp_exponential = list(
    id = "exponential",
    name = "Cross-Price Exponential",
    future_id = "cp_exponential",
    params_estimated = c("log10_qalone", "I", "log10_beta"),
    params_fixed = character(0),
    default_param_space = "log10",
    response_scale = "log10",
    zero_handling = "drop"
  ),

  cp_exponentiated = list(
    id = "exponentiated",
    name = "Cross-Price Exponentiated",
    future_id = "cp_exponentiated",
    params_estimated = c("log10_qalone", "I", "log10_beta"),
    params_fixed = character(0),
    default_param_space = "log10",
    response_scale = "natural",
    zero_handling = "keep"
  ),

  cp_additive = list(
    id = "additive",
    name = "Cross-Price Additive",
    future_id = "cp_additive",
    params_estimated = c("log10_qalone", "I", "log10_beta"),
    params_fixed = character(0),
    default_param_space = "log10",
    response_scale = "natural",
    zero_handling = "keep"
  )
)


# =============================================================================
# Helper Functions
# =============================================================================

#' Format Parameter Name with Scale Prefix
#'
#' Formats a canonical parameter name with the appropriate scale prefix.
#'
#' @param param Character. Canonical parameter name (e.g., "alpha").
#' @param scale Character. One of "natural", "log", "log10".
#' @return Character. Formatted parameter name (e.g., "log10_alpha").
#'
#' @keywords internal
format_param_name <- function(param, scale = c("natural", "log", "log10")) {
  scale <- match.arg(scale)
  paste0(scale, "_", param)
}


#' Get Canonical Parameter Name
#'
#' Maps legacy or variant parameter names to canonical names.
#'
#' @param name Character. Input parameter name.
#' @return Character. Canonical parameter name or original if no mapping found.
#'
#' @keywords internal
get_canonical_param <- function(name) {
  for (param in names(.beezdemand_param_registry)) {
    reg <- .beezdemand_param_registry[[param]]
    if (name == reg$canonical || name %in% reg$synonyms) {
      return(reg$canonical)
    }
  }
  # Return original if no mapping found
  tolower(name)
}


#' Get Canonical Derived Metric Name
#'
#' Maps legacy derived metric names to canonical names.
#'
#' @param name Character. Input metric name.
#' @return Character. Canonical metric name or original if no mapping found.
#'
#' @keywords internal
get_canonical_metric <- function(name) {
  for (metric in names(.beezdemand_derived_registry)) {
    reg <- .beezdemand_derived_registry[[metric]]
    if (name == reg$canonical || name %in% reg$legacy_synonyms) {
      return(reg$canonical)
    }
  }
  # Return original if no mapping found
  tolower(name)
}


#' Get Equation Specification
#'
#' Returns the full specification for an equation ID.
#'
#' @param equation_id Character. Equation identifier (e.g., "hs", "koff").
#' @return List. Equation specification or NULL if not found.
#'
#' @keywords internal
get_equation_spec <- function(equation_id) {
  equation_id <- tolower(equation_id)
  .beezdemand_equation_registry[[equation_id]]
}


#' Validate Parameter Scale
#'
#' Checks if a scale is valid for a given parameter.
#'
#' @param param Character. Canonical parameter name.
#' @param scale Character. Scale to validate.
#' @return Logical. TRUE if valid, FALSE otherwise.
#'
#' @keywords internal
validate_param_scale <- function(param, scale) {
  reg <- .beezdemand_param_registry[[param]]
  if (is.null(reg)) {
    return(TRUE)
  } # Unknown param, assume valid
  scale %in% reg$valid_scales
}


#' Legacy to Canonical Mapping Table
#'
#' Returns a data frame mapping legacy column names to canonical names.
#'
#' @return Data frame with columns: legacy, canonical, type.
#'
#' @keywords internal
get_legacy_mapping <- function() {
  param_rows <- lapply(names(.beezdemand_param_registry), function(param) {
    reg <- .beezdemand_param_registry[[param]]
    if (length(reg$synonyms) > 0) {
      data.frame(
        legacy = reg$synonyms,
        canonical = reg$canonical,
        type = "parameter",
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  metric_rows <- lapply(names(.beezdemand_derived_registry), function(metric) {
    reg <- .beezdemand_derived_registry[[metric]]
    if (length(reg$legacy_synonyms) > 0) {
      data.frame(
        legacy = reg$legacy_synonyms,
        canonical = reg$canonical,
        type = "derived_metric",
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  do.call(rbind, c(param_rows, metric_rows))
}
