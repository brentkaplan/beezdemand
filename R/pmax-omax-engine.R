#' @title Pmax/Omax Engine
#' @description Internal engine for calculating pmax, omax, and observed metrics
#'   with parameter-space safety and method reporting.
#' @name pmax-omax-engine
#' @keywords internal
NULL

# ==============================================================================
# PARAMETER SPACE CONVERSION UTILITIES
# ==============================================================================

#' Convert Parameter from Specified Scale to Natural Scale
#'
#' @param value Numeric value to convert
#' @param scale Character: "natural", "log", or "log10"
#' @return Numeric value on natural scale
#' @keywords internal
.to_natural_scale <- function(value, scale) {

  if (is.na(value) || is.null(scale)) return(NA_real_)
  scale <- tolower(as.character(scale))
  

  switch(scale,
    "natural" = value,
    "log" = exp(value),
    "ln" = exp(value),
    "log10" = 10^value,
    stop("Unknown scale: ", scale, ". Must be 'natural', 'log', or 'log10'.")
  )
}

#' Validate and Convert Parameters to Natural Scale
#'
#' @param params Named list or vector of parameters
#' @param param_scales Named list mapping parameter names to their scales
#' @return List with natural-scale parameters and conversion notes
#' @keywords internal
.standardize_params_to_natural <- function(params, param_scales = NULL) {
  if (is.null(param_scales)) {
    # Default: assume all natural
    param_scales <- setNames(
      rep("natural", length(params)),
      names(params)
    )

  }
  
  result <- list(
    params_natural = list(),
    conversions = list(),
    notes = character(0)
  )
  
  for (nm in names(params)) {
    scale_in <- param_scales[[nm]] %||% "natural"
    val_in <- params[[nm]]
    
    if (is.na(val_in)) {
      result$params_natural[[nm]] <- NA_real_
      result$conversions[[nm]] <- list(
        original = val_in,
        scale_in = scale_in,
        converted = NA_real_
      )
      next
    }
    
    val_nat <- .to_natural_scale(val_in, scale_in)
    result$params_natural[[nm]] <- val_nat
    
    result$conversions[[nm]] <- list(
      original = val_in,
      scale_in = scale_in,
      converted = val_nat
    )
    
    if (scale_in != "natural") {
      result$notes <- c(
        result$notes,
        sprintf("%s: converted from %s (%.6g) to natural (%.6g)",
                nm, scale_in, val_in, val_nat)
      )
    }
  }
  
  result
}

# ==============================================================================
# ANALYTIC PMAX SOLUTIONS
# ==============================================================================

#' Analytic Pmax for HS/Exponential Model (Lambert W)
#'
#' For the exponential model: Q(p) = Q0 * 10^(k * (exp(-alpha * Q0 * p) - 1))
#' Pmax = -W_0(-1 / (k * ln(10))) / (alpha * Q0)
#'
#' @param alpha_nat Natural-scale alpha
#' @param q0_nat Natural-scale Q0
#' @param k_nat Natural-scale k
#' @return List with pmax, method, and notes
#' @keywords internal
.pmax_analytic_hs <- function(alpha_nat, q0_nat, k_nat) {
  # Check existence conditions
  # Real Lambert W solution requires k > e/ln(10) ≈ 1.18
  threshold <- exp(1) / log(10)
  
  if (is.na(alpha_nat) || is.na(q0_nat) || is.na(k_nat)) {
    return(list(
      pmax = NA_real_,
      method = "analytic_lambert_w",
      success = FALSE,
      note = "Missing parameter values"
    ))
  }
  
  if (alpha_nat <= 0 || q0_nat <= 0 || k_nat <= 0) {
    return(list(
      pmax = NA_real_,
      method = "analytic_lambert_w",
      success = FALSE,
      note = "Parameters must be positive"
    ))
  }
  
  if (k_nat <= threshold) {
    return(list(
      pmax = NA_real_,
      method = "analytic_lambert_w",
      success = FALSE,
      note = sprintf(
        "k (%.4f) <= e/ln(10) (~%.4f): Lambert W has no real solution",
        k_nat, threshold
      )
    ))
  }
  
  # Compute Lambert W
  # W_0(-1 / (k * ln(10)))
  w_arg <- -1 / (k_nat * log(10))
  w_val <- tryCatch(
    lambertW(z = w_arg),
    error = function(e) NA_real_
  )
  
  if (is.na(w_val) || !is.finite(w_val) || is.complex(w_val)) {
    return(list(
      pmax = NA_real_,
      method = "analytic_lambert_w",
      success = FALSE,
      note = "Lambert W computation failed or returned complex value"
    ))
  }
  
  pmax <- -as.numeric(w_val) / (alpha_nat * q0_nat)
  
  if (!is.finite(pmax) || pmax <= 0) {
    return(list(
      pmax = NA_real_,
      method = "analytic_lambert_w",
      success = FALSE,
      note = sprintf("Computed pmax (%.4g) is not positive finite", pmax)
    ))
  }
  
  list(
    pmax = pmax,
    method = "analytic_lambert_w",
    success = TRUE,
    note = NULL
  )
}

#' Analytic Pmax for Hurdle Model (Natural-parameter exponential form)
#'
#' For hurdle: Q(p) = Q0 * exp(k * (exp(-alpha * p) - 1))
#' Note: No Q0 normalization in exponent (unlike HS)
#' Pmax = -W_0(-1/k) / alpha
#'
#' @param alpha_nat Natural-scale alpha
#' @param k_nat Natural-scale k
#' @return List with pmax, method, and notes
#' @keywords internal
.pmax_analytic_hurdle <- function(alpha_nat, k_nat) {
  # Real Lambert W solution requires k >= e
  threshold <- exp(1)
  
  if (is.na(alpha_nat) || is.na(k_nat)) {
    return(list(
      pmax = NA_real_,
      method = "analytic_lambert_w_hurdle",
      success = FALSE,
      note = "Missing parameter values"
    ))
  }
  
  if (alpha_nat <= 0 || k_nat <= 0) {
    return(list(
      pmax = NA_real_,
      method = "analytic_lambert_w_hurdle",
      success = FALSE,
      note = "Parameters must be positive"
    ))
  }
  
  if (k_nat < threshold) {
    return(list(
      pmax = NA_real_,
      method = "analytic_lambert_w_hurdle",
      success = FALSE,
      note = sprintf(
        "k (%.4f) < e (~%.4f): no interior maximum exists",
        k_nat, threshold
      )
    ))
  }
  
  # Compute Lambert W: W_0(-1/k)
  w_arg <- -1 / k_nat
  w_val <- tryCatch(
    lambertW(z = w_arg),
    error = function(e) NA_real_
  )
  
  if (is.na(w_val) || !is.finite(w_val) || is.complex(w_val)) {
    return(list(
      pmax = NA_real_,
      method = "analytic_lambert_w_hurdle",
      success = FALSE,
      note = "Lambert W computation failed"
    ))
  }
  
  pmax <- -as.numeric(w_val) / alpha_nat
  
  if (!is.finite(pmax) || pmax <= 0) {
    return(list(
      pmax = NA_real_,
      method = "analytic_lambert_w_hurdle",
      success = FALSE,
      note = sprintf("Computed pmax (%.4g) is not positive finite", pmax)
    ))
  }
  
  list(
    pmax = pmax,
    method = "analytic_lambert_w_hurdle",
    success = TRUE,
    note = NULL
  )
}

#' Analytic Pmax for Simplified/SND Model
#'
#' For SND: Q(p) = Q0 * exp(-alpha * Q0 * p)
#' Pmax = 1 / (alpha * Q0)
#'
#' @param alpha_nat Natural-scale alpha
#' @param q0_nat Natural-scale Q0
#' @return List with pmax, method, and notes
#' @keywords internal
.pmax_analytic_snd <- function(alpha_nat, q0_nat) {
  if (is.na(alpha_nat) || is.na(q0_nat)) {
    return(list(
      pmax = NA_real_,
      method = "analytic_snd",
      success = FALSE,
      note = "Missing parameter values"
    ))
  }
  
  if (alpha_nat <= 0 || q0_nat <= 0) {
    return(list(
      pmax = NA_real_,
      method = "analytic_snd",
      success = FALSE,
      note = "Parameters must be positive"
    ))
  }
  
  pmax <- 1 / (alpha_nat * q0_nat)
  
  if (!is.finite(pmax) || pmax <= 0) {
    return(list(
      pmax = NA_real_,
      method = "analytic_snd",
      success = FALSE,
      note = sprintf("Computed pmax (%.4g) is not positive finite", pmax)
    ))
  }
  
  list(
    pmax = pmax,
    method = "analytic_snd",
    success = TRUE,
    note = NULL
  )
}

# ==============================================================================
# NUMERICAL FALLBACK
# ==============================================================================

#' Numerical Pmax via Optimization
#'
#' @param expenditure_fn Function E(p) returning expenditure at price p
#' @param price_range Numeric vector c(min, max) for search interval
#' @return List with pmax, method, boundary info, and notes
#' @keywords internal
.pmax_numerical <- function(expenditure_fn, price_range) {
  if (is.null(expenditure_fn) || !is.function(expenditure_fn)) {
    return(list(
      pmax = NA_real_,
      omax = NA_real_,
      method = "numerical_optimize",
      is_boundary = NA,
      success = FALSE,
      note = "No valid expenditure function provided"
    ))
  }
  
  if (is.null(price_range) || length(price_range) < 2) {
    return(list(
      pmax = NA_real_,
      omax = NA_real_,
      method = "numerical_optimize",
      is_boundary = NA,
      success = FALSE,
      note = "Invalid price range"
    ))
  }
  
  p_min <- max(price_range[1], 1e-6)  # Avoid exact zero

  p_max <- price_range[2]
  
  if (p_min >= p_max) {
    return(list(
      pmax = NA_real_,
      omax = NA_real_,
      method = "numerical_optimize",
      is_boundary = NA,
      success = FALSE,
      note = "Price range is degenerate"
    ))
  }
  
  opt_result <- tryCatch(
    {
      stats::optimize(
        f = function(p) {
          val <- expenditure_fn(p)
          if (!is.finite(val)) return(-Inf)
          val
        },
        interval = c(p_min, p_max),
        maximum = TRUE,
        tol = .Machine$double.eps^0.5
      )
    },
    error = function(e) NULL
  )
  
  if (is.null(opt_result) || !is.finite(opt_result$maximum)) {
    return(list(
      pmax = NA_real_,
      omax = NA_real_,
      method = "numerical_optimize",
      is_boundary = NA,
      success = FALSE,
      note = "Numerical optimization failed"
    ))
  }
  
  pmax <- opt_result$maximum
  omax <- opt_result$objective
  
  # Check if result is at boundary
  tol_boundary <- (p_max - p_min) * 0.001
  is_boundary <- (abs(pmax - p_min) < tol_boundary) ||
    (abs(pmax - p_max) < tol_boundary)
  
  list(
    pmax = pmax,
    omax = omax,
    method = "numerical_optimize_observed_domain",
    is_boundary = is_boundary,
    success = TRUE,
    note = if (is_boundary) {
      sprintf("Maximum at domain boundary [%.4g, %.4g]", p_min, p_max)
    } else {
      NULL
    }
  )
}

# ==============================================================================
# ELASTICITY CALCULATION
# ==============================================================================
 
#' Calculate Elasticity at a Given Price
#'
#' Computes η(p) = d log(Q(p)) / d log(p) via central finite differences
#'
#' @param demand_fn Function Q(p) returning demand at price p
#' @param price Price point at which to evaluate elasticity
#' @param delta Relative step size for finite difference (default 1e-5)
#' @return Numeric elasticity value
#' @keywords internal
.elasticity_at_price <- function(demand_fn, price, delta = 1e-5) {
  if (is.na(price) || price <= 0 || !is.function(demand_fn)) {
    return(NA_real_)
  }
  
  # Use relative step size
  h <- price * delta
  if (h < 1e-10) h <- 1e-10  # Floor for very small prices
  
  p_lo <- price - h
  p_hi <- price + h
  
  # Ensure p_lo > 0

  if (p_lo <= 0) {
    p_lo <- price * 0.5
    p_hi <- price * 1.5
    h <- (p_hi - p_lo) / 2
  }
  
  q_lo <- tryCatch(demand_fn(p_lo), error = function(e) NA_real_)
  q_hi <- tryCatch(demand_fn(p_hi), error = function(e) NA_real_)
  
  if (is.na(q_lo) || is.na(q_hi) || q_lo <= 0 || q_hi <= 0) {
    return(NA_real_)
  }
  
  # η = d log(Q) / d log(p) = (p/Q) * (dQ/dp)
  # Using central difference: dQ/dp ≈ (Q(p+h) - Q(p-h)) / (2h)
  dq_dp <- (q_hi - q_lo) / (2 * h)
  q_at_p <- tryCatch(demand_fn(price), error = function(e) NA_real_)
  
  if (is.na(q_at_p) || q_at_p <= 0) {
    return(NA_real_)
  }
  
  eta <- (price / q_at_p) * dq_dp
  
  if (!is.finite(eta)) return(NA_real_)
  eta
}

#' Check Unit Elasticity Condition
#'
#' @param elasticity Numeric elasticity value
#' @param tol Tolerance for comparing to -1 (default 0.1)
#' @return Logical TRUE if |eta + 1| < tol
#' @keywords internal
.check_unit_elasticity <- function(elasticity, tol = 0.1) {
  if (is.na(elasticity)) return(NA)
  abs(elasticity + 1) < tol
}

# ==============================================================================
# OBSERVED PMAX/OMAX CALCULATION
# ==============================================================================

#' Calculate Observed Pmax and Omax for a Single Subject
#'
#' @param price Numeric vector of prices
#' @param consumption Numeric vector of consumption values
#' @return List with observed metrics
#' @keywords internal
.calc_observed_pmax_omax <- function(price, consumption) {
  if (length(price) != length(consumption)) {
    stop("price and consumption must have same length")
  }
  
  n_obs <- length(price)
  
  # Check for duplicate prices
  n_unique_prices <- length(unique(price))
  has_duplicate_prices <- n_unique_prices < n_obs
  
  # Calculate expenditure per row
  expenditure <- price * consumption
  
  # Handle all-NA case
  if (all(is.na(expenditure))) {
    return(list(
      pmax_obs = NA_real_,
      omax_obs = NA_real_,
      method_obs = "row_wise_max",
      tie_break_obs = "min_price",
      n_obs_rows = n_obs,
      n_unique_prices = n_unique_prices,
      has_duplicate_prices = has_duplicate_prices,
      n_max_ties = NA_integer_,
      note_obs = "All expenditure values are NA"
    ))
  }
  
  # Find maximum expenditure
  omax_obs <- max(expenditure, na.rm = TRUE)
  
  # Find rows achieving maximum
  is_max <- !is.na(expenditure) & (expenditure == omax_obs)
  n_max_ties <- sum(is_max)
  
  # Tie-break: take minimum price among max rows (stable "first max" rule)
  pmax_obs <- min(price[is_max], na.rm = TRUE)
  
  list(
    pmax_obs = pmax_obs,
    omax_obs = omax_obs,
    method_obs = "row_wise_max",
    tie_break_obs = "min_price",
    n_obs_rows = n_obs,
    n_unique_prices = n_unique_prices,
    has_duplicate_prices = has_duplicate_prices,
    n_max_ties = n_max_ties,
    note_obs = if (has_duplicate_prices) {
      sprintf(
        "Duplicate prices detected: %d rows but only %d unique prices",
        n_obs, n_unique_prices
      )
    } else {
      NULL
    }
  )
}

# ==============================================================================
# MAIN ENGINE FUNCTION
# ==============================================================================

#' Calculate Pmax and Omax with Method Reporting and Parameter-Space Safety
#'
#' @description
#' Unified internal engine for pmax/omax computation. Supports analytic solutions
#' (Lambert W for HS/hurdle, closed-form for SND), numerical fallback, and
#' observed (row-wise) metrics. Handles parameter-space conversions transparently.
#'
#' @param model_type Character: "hs", "koff", "hurdle", "snd", "simplified", or NULL
#' @param params Named list of parameters. Names depend on model_type:
#'   - hs/koff: alpha, q0, k
#'   - hurdle: alpha, q0, k (note: hurdle uses different formula)
#'   - snd/simplified: alpha, q0
#' @param param_scales Named list mapping parameter names to their input scales:
#'   "natural", "log", or "log10". Default assumes all natural.
#' @param expenditure_fn Optional function E(p) for numerical fallback. If NULL
#'   and model_type is provided, will be constructed from params.
#' @param demand_fn Optional function Q(p) for elasticity calculation.
#' @param price_obs Numeric vector of observed prices (required for observed
#'   metrics and domain constraints).
#' @param consumption_obs Numeric vector of observed consumption (for observed metrics).
#' @param tol Tolerance for unit elasticity check (default 0.1).
#' @param compute_observed Logical; compute observed metrics? Default TRUE if
#'   price_obs and consumption_obs are provided.
#'
#' @return A list with snake_case fields:
#' \describe{
#'   \item{pmax_model}{Model-based pmax}
#'   \item{omax_model}{Model-based omax}
#'   \item{q_at_pmax_model}{Quantity at pmax}
#'   \item{method_model}{Method used: "analytic_lambert_w", "analytic_snd", "numerical_optimize_observed_domain"}
#'   \item{domain_model}{Price domain used for computation}
#'   \item{is_boundary_model}{Logical; is pmax at domain boundary?}
#'   \item{elasticity_at_pmax_model}{Elasticity evaluated at pmax}
#'   \item{unit_elasticity_pass_model}{Logical; is elasticity near -1?}
#'   \item{note_model}{Any notes about model computation}
#'   \item{pmax_obs}{Observed pmax}
#'   \item{omax_obs}{Observed omax}
#'   \item{method_obs}{Method for observed: "row_wise_max"}
#'   \item{tie_break_obs}{Tie-break rule: "min_price"}
#'   \item{n_obs_rows}{Number of observation rows}
#'   \item{n_unique_prices}{Number of unique prices}
#'   \item{has_duplicate_prices}{Logical; duplicate prices detected?}
#'   \item{n_max_ties}{Number of rows achieving omax}
#'   \item{note_obs}{Notes about observed computation}
#'   \item{alpha_scale_in}{Input scale for alpha}
#'   \item{q0_scale_in}{Input scale for Q0}
#'   \item{k_scale_in}{Input scale for k (if applicable)}
#'   \item{note_param_space}{Notes about parameter conversions}
#' }
#'
#' @keywords internal
#' @export
beezdemand_calc_pmax_omax <- function(
  model_type = NULL,
  params = NULL,
  param_scales = NULL,
  expenditure_fn = NULL,
  demand_fn = NULL,
  price_obs = NULL,
  consumption_obs = NULL,
  tol = 0.1,
  compute_observed = NULL
) {
  # Initialize result structure
  result <- list(
    # Model-based metrics
    pmax_model = NA_real_,
    omax_model = NA_real_,
    q_at_pmax_model = NA_real_,
    method_model = NA_character_,
    domain_model = NA_character_,
    is_boundary_model = NA,
    elasticity_at_pmax_model = NA_real_,
    unit_elasticity_pass_model = NA,
    note_model = NULL,
    
    # Observed metrics
    pmax_obs = NA_real_,
    omax_obs = NA_real_,
    method_obs = NA_character_,
    tie_break_obs = NA_character_,
    n_obs_rows = NA_integer_,
    n_unique_prices = NA_integer_,
    has_duplicate_prices = NA,
    n_max_ties = NA_integer_,
    note_obs = NULL,
    
    # Parameter space diagnostics
    alpha_scale_in = NA_character_,
    q0_scale_in = NA_character_,
    k_scale_in = NA_character_,
    note_param_space = NULL
  )
  
  # Determine if we should compute observed metrics
  if (is.null(compute_observed)) {
    compute_observed <- !is.null(price_obs) && !is.null(consumption_obs) &&
      length(price_obs) > 0 && length(consumption_obs) > 0
  }
  
  # Compute observed metrics if requested
  if (compute_observed && !is.null(price_obs) && !is.null(consumption_obs)) {
    obs_result <- .calc_observed_pmax_omax(price_obs, consumption_obs)
    result$pmax_obs <- obs_result$pmax_obs
    result$omax_obs <- obs_result$omax_obs
    result$method_obs <- obs_result$method_obs
    result$tie_break_obs <- obs_result$tie_break_obs
    result$n_obs_rows <- obs_result$n_obs_rows
    result$n_unique_prices <- obs_result$n_unique_prices
    result$has_duplicate_prices <- obs_result$has_duplicate_prices
    result$n_max_ties <- obs_result$n_max_ties
    result$note_obs <- obs_result$note_obs
    
    if (obs_result$has_duplicate_prices) {
      warning(obs_result$note_obs, call. = FALSE)
    }
  }
  
  # If no model_type and no expenditure_fn, return with observed only
  if (is.null(model_type) && is.null(expenditure_fn)) {
    result$note_model <- "No model type or expenditure function provided"
    return(result)
  }
  
  # Standardize parameters to natural scale
  if (!is.null(params)) {
    std_params <- .standardize_params_to_natural(params, param_scales)
    params_nat <- std_params$params_natural
    
    result$alpha_scale_in <- param_scales[["alpha"]] %||% 
      param_scales[["log_alpha"]] %||% "natural"
    result$q0_scale_in <- param_scales[["q0"]] %||% 
      param_scales[["log_q0"]] %||% "natural"
    result$k_scale_in <- param_scales[["k"]] %||% 
      param_scales[["log_k"]] %||% "natural"
    
    if (length(std_params$notes) > 0) {
      result$note_param_space <- paste(std_params$notes, collapse = "; ")
    }
  } else {
    params_nat <- NULL
  }
  
  # Get price range for domain constraints
  if (!is.null(price_obs) && length(price_obs) > 0) {
    price_range <- range(price_obs, na.rm = TRUE)
    result$domain_model <- sprintf("[%.4g, %.4g]", price_range[1], price_range[2])
  } else {
    price_range <- NULL
  }
  
  # Attempt model-based calculation with priority hierarchy
  model_result <- NULL
  model_type_lower <- tolower(model_type %||% "")
  
  # Priority 1: Analytic solutions
  if (model_type_lower %in% c("hs", "koff", "exponential", "exponentiated")) {
    # HS/Koff model: Pmax = -W_0(-1/(k*ln(10))) / (alpha * Q0)
    alpha_nat <- params_nat[["alpha"]] %||% params_nat[["log_alpha"]]
    q0_nat <- params_nat[["q0"]] %||% params_nat[["log_q0"]]
    k_nat <- params_nat[["k"]] %||% params_nat[["log_k"]]
    
    model_result <- .pmax_analytic_hs(alpha_nat, q0_nat, k_nat)
    
    if (model_result$success) {
      result$pmax_model <- model_result$pmax
      result$method_model <- model_result$method
      result$is_boundary_model <- FALSE
      
      # Construct demand function for Omax and elasticity
      if (is.null(demand_fn)) {
        demand_fn <- function(p) {
          q0_nat * 10^(k_nat * (exp(-alpha_nat * q0_nat * p) - 1))
        }
      }
    }
    
  } else if (model_type_lower == "hurdle") {
    # Hurdle model: Pmax = -W_0(-1/k) / alpha (no Q0 normalization)
    alpha_nat <- params_nat[["alpha"]] %||% params_nat[["log_alpha"]]
    q0_nat <- params_nat[["q0"]] %||% params_nat[["log_q0"]]
    k_nat <- params_nat[["k"]] %||% params_nat[["log_k"]]
    
    model_result <- .pmax_analytic_hurdle(alpha_nat, k_nat)
    
    if (model_result$success) {
      result$pmax_model <- model_result$pmax
      result$method_model <- model_result$method
      result$is_boundary_model <- FALSE
      
      # Construct demand function for Omax and elasticity
      if (is.null(demand_fn)) {
        demand_fn <- function(p) {
          q0_nat * exp(k_nat * (exp(-alpha_nat * p) - 1))
        }
      }
    }
    
  } else if (model_type_lower %in% c("snd", "simplified")) {
    # SND model: Pmax = 1 / (alpha * Q0)
    alpha_nat <- params_nat[["alpha"]] %||% params_nat[["log_alpha"]]
    q0_nat <- params_nat[["q0"]] %||% params_nat[["log_q0"]]
    
    model_result <- .pmax_analytic_snd(alpha_nat, q0_nat)
    
    if (model_result$success) {
      result$pmax_model <- model_result$pmax
      result$method_model <- model_result$method
      result$is_boundary_model <- FALSE
      
      # Construct demand function for Omax and elasticity
      if (is.null(demand_fn)) {
        demand_fn <- function(p) {
          q0_nat * exp(-alpha_nat * q0_nat * p)
        }
      }
    }
  }
  
  # Priority 2: Numerical fallback if analytic failed
  if (is.null(model_result) || !model_result$success) {
    result$note_model <- model_result$note %||% "Analytic solution not available"
    
    # Build expenditure function if not provided
    if (is.null(expenditure_fn) && !is.null(demand_fn)) {
      expenditure_fn <- function(p) p * demand_fn(p)
    } else if (is.null(expenditure_fn) && !is.null(params_nat)) {
      # Try to construct from params
      alpha_nat <- params_nat[["alpha"]] %||% params_nat[["log_alpha"]]
      q0_nat <- params_nat[["q0"]] %||% params_nat[["log_q0"]]
      k_nat <- params_nat[["k"]] %||% params_nat[["log_k"]]
      
      if (model_type_lower %in% c("hs", "koff", "exponential", "exponentiated")) {
        demand_fn <- function(p) {
          q0_nat * 10^(k_nat * (exp(-alpha_nat * q0_nat * p) - 1))
        }
      } else if (model_type_lower == "hurdle") {
        demand_fn <- function(p) {
          q0_nat * exp(k_nat * (exp(-alpha_nat * p) - 1))
        }
      } else if (model_type_lower %in% c("snd", "simplified")) {
        demand_fn <- function(p) {
          q0_nat * exp(-alpha_nat * q0_nat * p)
        }
      }
      
      if (!is.null(demand_fn)) {
        expenditure_fn <- function(p) p * demand_fn(p)
      }
    }
    
    # Use numerical optimization
    if (!is.null(expenditure_fn) && !is.null(price_range)) {
      num_result <- .pmax_numerical(expenditure_fn, price_range)
      
      if (num_result$success) {
        result$pmax_model <- num_result$pmax
        result$omax_model <- num_result$omax
        result$method_model <- num_result$method
        result$is_boundary_model <- num_result$is_boundary
        
        if (!is.null(num_result$note)) {
          result$note_model <- paste(
            c(result$note_model, num_result$note),
            collapse = "; "
          )
        }
      }
    }
  }
  
  # Calculate Omax if we have pmax but not omax yet
  if (!is.na(result$pmax_model) && is.na(result$omax_model)) {
    if (!is.null(expenditure_fn)) {
      result$omax_model <- tryCatch(
        expenditure_fn(result$pmax_model),
        error = function(e) NA_real_
      )
    } else if (!is.null(demand_fn)) {
      q_at_pmax <- tryCatch(
        demand_fn(result$pmax_model),
        error = function(e) NA_real_
      )
      result$q_at_pmax_model <- q_at_pmax
      result$omax_model <- result$pmax_model * q_at_pmax
    }
  }
  
  # Calculate Q at Pmax if we have demand_fn
  if (!is.na(result$pmax_model) && is.na(result$q_at_pmax_model) && !is.null(demand_fn)) {
    result$q_at_pmax_model <- tryCatch(
      demand_fn(result$pmax_model),
      error = function(e) NA_real_
    )
  }
  
  # Calculate elasticity at Pmax
  if (!is.na(result$pmax_model) && !is.null(demand_fn)) {
    result$elasticity_at_pmax_model <- .elasticity_at_price(
      demand_fn, result$pmax_model
    )
    result$unit_elasticity_pass_model <- .check_unit_elasticity(
      result$elasticity_at_pmax_model, tol
    )
  }
  
  result
}

# ==============================================================================
# VECTORIZED VERSION FOR MULTIPLE SUBJECTS
# ==============================================================================

#' Calculate Pmax/Omax for Multiple Subjects
#'
#' @param params_df Data frame with one row per subject, containing parameter columns
#' @param model_type Character: model type (same for all subjects)
#' @param param_scales Named list of parameter scales
#' @param price_list Optional list of price vectors (one per subject)
#' @param consumption_list Optional list of consumption vectors (one per subject)
#' @param ... Additional arguments passed to beezdemand_calc_pmax_omax
#'
#' @return Data frame with pmax/omax results for each subject
#' @keywords internal
#' @export
beezdemand_calc_pmax_omax_vec <- function(
  params_df,
  model_type,
  param_scales = NULL,
  price_list = NULL,
  consumption_list = NULL,
  ...
) {
  n <- nrow(params_df)
  
  results <- lapply(seq_len(n), function(i) {
    params_i <- as.list(params_df[i, , drop = FALSE])
    
    price_i <- if (!is.null(price_list)) price_list[[i]] else NULL
    cons_i <- if (!is.null(consumption_list)) consumption_list[[i]] else NULL
    
    beezdemand_calc_pmax_omax(
      model_type = model_type,
      params = params_i,
      param_scales = param_scales,
      price_obs = price_i,
      consumption_obs = cons_i,
      ...
    )
  })
  
  # Convert list to data frame
  do.call(rbind, lapply(results, function(x) {
    data.frame(
      pmax_model = x$pmax_model,
      omax_model = x$omax_model,
      q_at_pmax_model = x$q_at_pmax_model,
      method_model = x$method_model,
      is_boundary_model = x$is_boundary_model,
      elasticity_at_pmax_model = x$elasticity_at_pmax_model,
      unit_elasticity_pass_model = x$unit_elasticity_pass_model,
      pmax_obs = x$pmax_obs,
      omax_obs = x$omax_obs,
      has_duplicate_prices = x$has_duplicate_prices,
      n_max_ties = x$n_max_ties,
      stringsAsFactors = FALSE
    )
  }))
}

#' Calculate Observed Pmax/Omax Grouped by ID
#'
#' @param data Data frame with id, price, and consumption columns
#' @param id_var Name of ID column
#' @param price_var Name of price column
#' @param consumption_var Name of consumption column
#'
#' @return Data frame with observed pmax/omax for each subject
#' @export
calc_observed_pmax_omax <- function(
  data,
  id_var = "id",
  price_var = "x",
  consumption_var = "y"
) {
  if (!all(c(id_var, price_var, consumption_var) %in% names(data))) {
    stop("Required columns not found in data: ",
         paste(c(id_var, price_var, consumption_var), collapse = ", "))
  }
  
  ids <- unique(data[[id_var]])
  
  results <- lapply(ids, function(id) {
    idx <- data[[id_var]] == id
    price <- data[[price_var]][idx]
    consumption <- data[[consumption_var]][idx]
    
    obs <- .calc_observed_pmax_omax(price, consumption)
    
    data.frame(
      id = id,
      pmax_obs = obs$pmax_obs,
      omax_obs = obs$omax_obs,
      method_obs = obs$method_obs,
      tie_break_obs = obs$tie_break_obs,
      n_obs_rows = obs$n_obs_rows,
      n_unique_prices = obs$n_unique_prices,
      has_duplicate_prices = obs$has_duplicate_prices,
      n_max_ties = obs$n_max_ties,
      note_obs = obs$note_obs %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, results)
}
