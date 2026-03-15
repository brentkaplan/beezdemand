#' @useDynLib beezdemand, .registration = TRUE
NULL

# ==============================================================================
# Internal Helper Functions for TMB Mixed-Effects Demand Models
# ==============================================================================

#' Prepare Data for TMB Mixed-Effects Demand Model
#'
#' @description
#' Equation-aware data preparation for TMB continuous-only demand models.
#' Handles zero filtering, log transformation, and subject ID mapping.
#'
#' @param data A validated data frame.
#' @param y_var Character string, name of consumption variable.
#' @param x_var Character string, name of price variable.
#' @param id_var Character string, name of subject ID variable.
#' @param equation Character string, one of "exponential", "exponentiated",
#'   "simplified", "zben".
#'
#' @return A list containing prepared data vectors and subject mapping.
#' @keywords internal
.tmb_prepare_data <- function(data, y_var, x_var, id_var, equation) {
  ids <- data[[id_var]]
  price <- as.numeric(data[[x_var]])
  consumption <- as.numeric(data[[y_var]])

  # Equation-specific response handling
  n_zeros <- sum(consumption == 0, na.rm = TRUE)
  n_dropped <- 0L

  if (equation == "exponential") {
    # Filter to Q > 0, compute log(Q)
    pos_idx <- consumption > 0
    n_dropped <- sum(!pos_idx)
    if (n_dropped > 0) {
      message(sprintf(
        "  equation='exponential': Dropped %d zero-consumption observations (%d remaining).",
        n_dropped, sum(pos_idx)
      ))
    }
    ids <- ids[pos_idx]
    price <- price[pos_idx]
    consumption <- consumption[pos_idx]
    y <- log(consumption)
  } else if (equation == "exponentiated" || equation == "simplified") {
    # Raw Q, zeros OK
    y <- consumption
  } else {
    # zben: user provides LL4-transformed y directly
    y <- consumption
  }

  # Create subject mapping (0-indexed for C++)
  subject_levels <- levels(droplevels(as.factor(ids)))
  n_subjects <- length(subject_levels)
  subject_map <- setNames(
    seq_along(subject_levels) - 1L,
    subject_levels
  )
  subject_id <- as.integer(subject_map[as.character(ids)])

  list(
    y = y,
    price = price,
    subject_id = subject_id,
    subject_levels = subject_levels,
    n_subjects = n_subjects,
    n_obs = length(y),
    n_zeros = n_zeros,
    n_dropped = n_dropped,
    data = data  # Keep original for downstream

  )
}


#' Build Design Matrices for TMB Model
#'
#' @param data Data frame with factors already applied.
#' @param factors_q0 Character vector of factor names for Q0.
#' @param factors_alpha Character vector of factor names for alpha.
#' @param factor_interaction Logical.
#' @param continuous_covariates Character vector.
#'
#' @return A list with X_q0, X_alpha design matrices and formula strings.
#' @keywords internal
.tmb_build_design_matrices <- function(
  data,
  factors_q0 = NULL,
  factors_alpha = NULL,
  factor_interaction = FALSE,
  continuous_covariates = NULL
) {
  rhs_q0 <- build_fixed_rhs(
    factors = factors_q0,
    factor_interaction = factor_interaction,
    continuous_covariates = continuous_covariates,
    data = data
  )
  rhs_alpha <- build_fixed_rhs(
    factors = factors_alpha,
    factor_interaction = factor_interaction,
    continuous_covariates = continuous_covariates,
    data = data
  )

  # Build model matrices
  X_q0 <- model.matrix(stats::as.formula(rhs_q0), data = data)
  X_alpha <- model.matrix(stats::as.formula(rhs_alpha), data = data)

  list(
    X_q0 = X_q0,
    X_alpha = X_alpha,
    rhs_q0 = rhs_q0,
    rhs_alpha = rhs_alpha
  )
}


#' Build TMB Data List
#'
#' @param prepared Output from .tmb_prepare_data().
#' @param design Output from .tmb_build_design_matrices().
#' @param equation Character string, equation type.
#' @param n_re Integer, number of random effects.
#'
#' @return A list suitable for TMB::MakeADFun data argument.
#' @keywords internal
.tmb_build_tmb_data <- function(prepared, design, equation, n_re) {
  eqn_type <- switch(equation,
    exponential = 0L,
    exponentiated = 1L,
    simplified = 2L,
    zben = 3L,
    stop("Unknown equation: ", equation)
  )

  list(
    model = "MixedDemand",
    y = prepared$y,
    price = prepared$price,
    subject_id = prepared$subject_id,
    n_subjects = prepared$n_subjects,
    X_q0 = design$X_q0,
    X_alpha = design$X_alpha,
    eqn_type = eqn_type,
    n_re = n_re
  )
}


#' Generate Default Starting Values
#'
#' @param prepared Output from .tmb_prepare_data().
#' @param design Output from .tmb_build_design_matrices().
#' @param equation Character string.
#' @param n_re Integer.
#' @param has_k Logical.
#' @param k_fixed Numeric or NULL.
#'
#' @return Named list of starting values.
#' @keywords internal
.tmb_default_starts <- function(prepared, design, equation, n_re, has_k,
                                 k_fixed = NULL) {
  y <- prepared$y
  price <- prepared$price
  n_subjects <- prepared$n_subjects
  p_q0 <- ncol(design$X_q0)
  p_alpha <- ncol(design$X_alpha)

  # Data-driven Q0 intercept
  if (equation == "exponential") {
    # y is already log(Q); use median of zero-price or lowest-price obs
    zero_price_idx <- price == 0 | price == min(price)
    q0_start <- median(y[zero_price_idx], na.rm = TRUE)
    if (is.na(q0_start) || !is.finite(q0_start)) q0_start <- log(10)
  } else if (equation == "zben") {
    # y is LL4-transformed; Q0 in log10 space for zben
    zero_price_idx <- price == 0 | price == min(price)
    q0_log10 <- median(y[zero_price_idx], na.rm = TRUE)
    if (is.na(q0_log10) || !is.finite(q0_log10)) q0_log10 <- 1
    q0_start <- log(q0_log10 * log(10))  # log(Q0) from log10(Q0)
  } else {
    # exponentiated/simplified: raw Q
    zero_price_idx <- price == 0 | price == min(price)
    q0_raw <- median(y[zero_price_idx], na.rm = TRUE)
    if (is.na(q0_raw) || q0_raw <= 0) q0_raw <- 10
    q0_start <- log(q0_raw)
  }

  # beta_q0: intercept = q0_start, rest = 0
  beta_q0 <- rep(0, p_q0)
  beta_q0[1] <- q0_start

  # beta_alpha: intercept = log(0.001), rest = 0
  beta_alpha <- rep(0, p_alpha)
  beta_alpha[1] <- log(0.001)

  starts <- list(
    beta_q0 = beta_q0,
    beta_alpha = beta_alpha,
    log_k = if (has_k && !is.null(k_fixed)) log(k_fixed) else log(2),
    logsigma_b = log(0.5),
    logsigma_c = log(0.5),
    logsigma_e = log(1),
    rho_bc_raw = 0,
    u = matrix(0, nrow = n_subjects, ncol = n_re)
  )

  starts
}


#' Build TMB Map List
#'
#' @param has_k Logical, whether k is estimated.
#' @param n_re Integer, number of random effects.
#'
#' @return Named list for TMB map argument.
#' @keywords internal
.tmb_build_map <- function(has_k, n_re) {
  map <- list()

  # Map out k for simplified/zben

  if (!has_k) {
    map$log_k <- factor(NA)
  }

  # Map out alpha RE variance/correlation if n_re == 1
  if (n_re == 1) {
    map$logsigma_c <- factor(NA)
    map$rho_bc_raw <- factor(NA)
  }

  map
}


#' Multi-Start TMB Optimization
#'
#' @param tmb_data TMB data list.
#' @param start_values Default starting values list.
#' @param map TMB map list.
#' @param n_re Integer.
#' @param tmb_control Control parameters.
#' @param verbose Integer verbosity level.
#'
#' @return List with obj, opt, start_used.
#' @keywords internal
.tmb_multi_start <- function(tmb_data, start_values, map, n_re,
                              tmb_control, verbose) {
  # Generate 3 starting value sets
  start_sets <- list()
  start_sets[[1]] <- start_values  # Data-driven

  # Heuristic starts
  s2 <- start_values
  s2$beta_q0[1] <- start_values$beta_q0[1] + 1
  s2$beta_alpha[1] <- log(0.0001)
  if (!is.null(map$log_k) && is.na(map$log_k)) {
    # k is fixed, don't change
  } else {
    s2$log_k <- log(3)
  }
  s2$logsigma_b <- log(1)
  s2$logsigma_e <- log(0.5)
  start_sets[[2]] <- s2

  # Conservative starts
  s3 <- start_values
  s3$beta_q0[1] <- start_values$beta_q0[1] - 0.5
  s3$beta_alpha[1] <- log(0.01)
  if (!is.null(map$log_k) && is.na(map$log_k)) {
    # k is fixed
  } else {
    s3$log_k <- log(1.5)
  }
  s3$logsigma_b <- log(0.3)
  s3$logsigma_e <- log(2)
  start_sets[[3]] <- s3

  best_nll <- Inf

best_result <- NULL

  for (s in seq_along(start_sets)) {
    starts_i <- start_sets[[s]]
    opt_warnings_i <- character(0)

    result <- tryCatch({
      obj_i <- TMB::MakeADFun(
        data = tmb_data,
        parameters = starts_i,
        map = map,
        random = "u",
        DLL = "beezdemand",
        silent = verbose < 2
      )

      opt_i <- withCallingHandlers(
        nlminb(
          start = obj_i$par,
          objective = obj_i$fn,
          gradient = obj_i$gr,
          control = list(
            eval.max = tmb_control$eval_max,
            iter.max = tmb_control$iter_max,
            trace = if (verbose >= 2) 1 else 0
          )
        ),
        warning = function(w) {
          opt_warnings_i <<- c(opt_warnings_i, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )

      list(obj = obj_i, opt = opt_i, nll = opt_i$objective, start_idx = s,
           opt_warnings = opt_warnings_i)
    }, error = function(e) {
      if (verbose >= 2) {
        message(sprintf("  Start set %d failed: %s", s, e$message))
      }
      NULL
    })

    if (!is.null(result) && is.finite(result$nll) && result$nll < best_nll) {
      best_nll <- result$nll
      best_result <- result
    }
  }

  if (is.null(best_result)) {
    stop(
      "All starting value sets failed. ",
      "Check data quality or try different start values.",
      call. = FALSE
    )
  }

  if (verbose >= 1) {
    message(sprintf(
      "  Multi-start: best NLL = %.2f (start set %d of %d)",
      best_nll, best_result$start_idx, length(start_sets)
    ))
  }

  best_result
}


#' Extract Estimates from TMB Fit
#'
#' @param obj TMB objective function object.
#' @param opt nlminb optimization result.
#' @param n_re Integer.
#' @param n_subjects Integer.
#' @param has_k Logical.
#' @param verbose Integer.
#'
#' @return List with coefficients, se, sdr, variance_components, u_hat.
#' @keywords internal
.tmb_extract_estimates <- function(obj, opt, n_re, n_subjects, has_k, verbose) {
  # Compute sdreport
  sdr <- tryCatch(
    TMB::sdreport(obj),
    error = function(e1) {
      sdr2 <- tryCatch(
        TMB::sdreport(obj, getJointPrecision = FALSE),
        error = function(e2) NULL
      )
      if (is.null(sdr2) && verbose >= 1) {
        warning("Standard error computation failed: ", e1$message)
      }
      sdr2
    }
  )

  # Extract fixed effects
  par_full <- opt$par
  par_names <- names(par_full)

  # Build coefficient vector with proper names
  coefficients <- par_full
  se_vec <- rep(NA_real_, length(par_full))
  names(se_vec) <- par_names

  if (!is.null(sdr)) {
    fixed_summary <- summary(sdr, "fixed")
    # Match by name
    for (nm in par_names) {
      idx <- which(rownames(fixed_summary) == nm)
      if (length(idx) > 0) {
        se_vec[nm] <- fixed_summary[idx[1], "Std. Error"]
      }
    }

    # Handle vector parameters (beta_q0, beta_alpha)
    beta_q0_idx <- which(par_names == "beta_q0")
    if (length(beta_q0_idx) > 0) {
      fixed_beta_q0 <- fixed_summary[rownames(fixed_summary) == "beta_q0", , drop = FALSE]
      if (nrow(fixed_beta_q0) == length(beta_q0_idx)) {
        se_vec[beta_q0_idx] <- fixed_beta_q0[, "Std. Error"]
      }
    }
    beta_alpha_idx <- which(par_names == "beta_alpha")
    if (length(beta_alpha_idx) > 0) {
      fixed_beta_alpha <- fixed_summary[rownames(fixed_summary) == "beta_alpha", , drop = FALSE]
      if (nrow(fixed_beta_alpha) == length(beta_alpha_idx)) {
        se_vec[beta_alpha_idx] <- fixed_beta_alpha[, "Std. Error"]
      }
    }

    # Extract random effects
    re_summary <- summary(sdr, "random")
    u_hat <- matrix(re_summary[, "Estimate"], nrow = n_subjects, ncol = n_re)
  } else {
    u_hat <- matrix(0, nrow = n_subjects, ncol = n_re)
  }

  # Extract variance components from ADREPORT
  variance_components <- NULL
  if (!is.null(sdr)) {
    adr <- tryCatch(summary(sdr, "report"), error = function(e) NULL)
    if (!is.null(adr)) {
      variance_components <- adr
    }
  }

  list(
    coefficients = coefficients,
    se = se_vec,
    sdr = sdr,
    variance_components = variance_components,
    u_hat = u_hat
  )
}


#' Compute Subject-Specific Parameters
#'
#' @param coefficients Named coefficient vector.
#' @param u_hat Random effects matrix.
#' @param subject_levels Character vector of subject IDs.
#' @param n_re Integer.
#' @param has_k Logical.
#' @param equation Character.
#' @param price Numeric vector of prices.
#' @param subject_id Integer vector of 0-indexed subject IDs.
#'
#' @return Data frame of subject-specific parameters.
#' @keywords internal
.tmb_compute_subject_pars <- function(
  coefficients, u_hat, subject_levels, n_re, has_k,
  equation, price, subject_id, k_fixed = NULL,
  X_q0 = NULL, X_alpha = NULL
) {
  n_subjects <- length(subject_levels)

  # Get beta vectors
  beta_q0_idx <- which(names(coefficients) == "beta_q0")
  beta_alpha_idx <- which(names(coefficients) == "beta_alpha")
  beta_q0 <- unname(coefficients[beta_q0_idx])
  beta_alpha <- unname(coefficients[beta_alpha_idx])

  # Build covariance matrix and transform u_hat
  sigma_b <- exp(coefficients[["logsigma_b"]])

  if (n_re == 2) {
    sigma_c <- exp(coefficients[["logsigma_c"]])
    rho_bc <- tanh(coefficients[["rho_bc_raw"]])

    Sigma <- matrix(
      c(sigma_b^2, sigma_b * sigma_c * rho_bc,
        sigma_b * sigma_c * rho_bc, sigma_c^2),
      nrow = 2
    )
    L <- tryCatch(
      t(chol(Sigma)),
      error = function(e) {
        warning(
          "Covariance matrix not positive definite; dropping RE correlation. ",
          "Subject-specific parameters assume independent random effects.",
          call. = FALSE
        )
        diag(c(sigma_b, sigma_c))
      }
    )
    re_mat <- t(L %*% t(u_hat))
    colnames(re_mat) <- c("b_i", "c_i")
  } else {
    re_mat <- matrix(sigma_b * u_hat[, 1], ncol = 1)
    colnames(re_mat) <- "b_i"
  }

  # Subject-specific Q0 and alpha using full design matrix rows
  subj_log_q0 <- numeric(n_subjects)
  subj_log_alpha <- numeric(n_subjects)

  for (i in seq_len(n_subjects)) {
    # Find first observation for this subject to get their design matrix row
    first_obs <- which(subject_id == (i - 1L))[1]

    if (!is.null(X_q0) && ncol(X_q0) > 1) {
      subj_log_q0[i] <- sum(X_q0[first_obs, ] * beta_q0) + re_mat[i, "b_i"]
    } else {
      subj_log_q0[i] <- beta_q0[1] + re_mat[i, "b_i"]
    }

    if (!is.null(X_alpha) && ncol(X_alpha) > 1) {
      log_alpha_i <- sum(X_alpha[first_obs, ] * beta_alpha)
    } else {
      log_alpha_i <- beta_alpha[1]
    }

    if (n_re == 2) {
      subj_log_alpha[i] <- log_alpha_i + re_mat[i, "c_i"]
    } else {
      subj_log_alpha[i] <- log_alpha_i
    }
  }

  subj_Q0 <- exp(subj_log_q0)
  subj_alpha <- exp(subj_log_alpha)

  # Compute Pmax/Omax
  if (has_k) {
    if ("log_k" %in% names(coefficients)) {
      k_val <- exp(coefficients[["log_k"]])
    } else if (!is.null(k_fixed)) {
      k_val <- k_fixed
    } else {
      k_val <- 2  # fallback default
    }

    # Both exponential and exponentiated use the same Pmax formula
    model_type <- "hs"

    price_split <- split(price, subject_id)
    price_list <- lapply(seq_len(n_subjects), function(i) {
      ps <- price_split[[as.character(i - 1L)]]
      if (is.null(ps)) numeric(0) else ps
    })

    omax_pmax <- beezdemand_calc_pmax_omax_vec(
      params_df = data.frame(
        alpha = subj_alpha,
        q0 = subj_Q0,
        k = rep(k_val, n_subjects)
      ),
      model_type = model_type,
      param_scales = list(alpha = "natural", q0 = "natural", k = "natural"),
      price_list = price_list,
      compute_observed = FALSE
    )
  } else {
    # simplified/zben: no k
    price_split <- split(price, subject_id)
    price_list <- lapply(seq_len(n_subjects), function(i) {
      ps <- price_split[[as.character(i - 1L)]]
      if (is.null(ps)) numeric(0) else ps
    })

    omax_pmax <- beezdemand_calc_pmax_omax_vec(
      params_df = data.frame(
        alpha = subj_alpha,
        q0 = subj_Q0
      ),
      model_type = "snd",
      param_scales = list(alpha = "natural", q0 = "natural"),
      price_list = price_list,
      compute_observed = FALSE
    )
  }

  # Build output
  out <- data.frame(
    id = subject_levels,
    b_i = re_mat[, "b_i"],
    Q0 = subj_Q0,
    alpha = subj_alpha,
    Pmax = omax_pmax$pmax_model,
    Omax = omax_pmax$omax_model,
    stringsAsFactors = FALSE
  )

  if (n_re == 2) {
    out$c_i <- re_mat[, "c_i"]
    # Reorder columns
    out <- out[, c("id", "b_i", "c_i", "Q0", "alpha", "Pmax", "Omax")]
  }

  out
}


# ==============================================================================
# Main Fitting Function
# ==============================================================================

#' Fit Mixed-Effects Demand Models via TMB
#'
#' @description
#' Fits nonlinear mixed-effects demand models using Template Model Builder (TMB)
#' for exact automatic differentiation and Laplace approximation. This provides
#' reliable convergence where traditional NLME approaches fail.
#'
#' @param data A data frame in long format with columns for subject ID, price,
#'   and consumption.
#' @param y_var Character. Name of the consumption/response variable.
#' @param x_var Character. Name of the price variable.
#' @param id_var Character. Name of the subject identifier variable.
#' @param equation Character. The demand equation to fit. One of:
#'   \describe{
#'     \item{`"exponentiated"`}{Koffarnus et al. (2015). Gaussian on raw Q. Zeros
#'       allowed. Has k parameter.}
#'     \item{`"exponential"`}{Hursh & Silberberg (2008). Gaussian on log(Q).
#'       Observations with Q = 0 are automatically dropped. Has k parameter.}
#'     \item{`"simplified"`}{Simplified exponential (no k). Gaussian on raw Q.
#'       Zeros allowed.}
#'     \item{`"zben"`}{Zero-bounded exponential (no k). Gaussian on LL4-
#'       transformed Q. User must pass LL4-transformed y_var.}
#'   }
#' @param estimate_k Logical. If `TRUE` (default), estimate k as a free
#'   parameter. If `FALSE`, fix k at the value given in `k`. Only relevant for
#'   "exponentiated" and "exponential" equations.
#' @param k Numeric or `NULL`. Fixed value of k when `estimate_k = FALSE`.
#'   If `NULL` and `estimate_k = FALSE`, k defaults to 2.
#' @param random_effects Character vector specifying random effects. One of:
#'   \describe{
#'     \item{`c("q0", "alpha")`}{Random effects on both Q0 and alpha (default).}
#'     \item{`"q0"`}{Random effect on Q0 only.}
#'   }
#' @param factors Character vector of factor variable names for group comparisons.
#' @param factor_interaction Logical. If `TRUE` and two factors provided, include
#'   their interaction.
#' @param continuous_covariates Character vector of continuous covariate names.
#' @param collapse_levels Named list for asymmetric factor collapsing. Structure:
#'   `list(Q0 = list(factor = list(new = c(old))), alpha = list(...))`.
#' @param start_values Named list of starting values. If `NULL`, data-driven
#'   defaults are used.
#' @param tmb_control List of control parameters:
#'   \describe{
#'     \item{`iter_max`}{Maximum iterations (default 1000).}
#'     \item{`eval_max`}{Maximum function evaluations (default 2000).}
#'   }
#' @param multi_start Logical. If `TRUE` (default), try 3 starting value sets
#'   and select the best.
#' @param verbose Integer. Verbosity level: 0 = silent, 1 = progress, 2 = debug.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class `beezdemand_tmb` containing:
#'   \describe{
#'     \item{model}{List with coefficients, se, variance_components}
#'     \item{subject_pars}{Data frame of subject-specific Q0, alpha, Pmax, Omax}
#'     \item{tmb_obj}{TMB objective function object}
#'     \item{opt}{nlminb optimization result}
#'     \item{sdr}{TMB sdreport object}
#'     \item{converged}{Logical convergence indicator}
#'     \item{loglik}{Log-likelihood at convergence}
#'     \item{AIC}{Akaike Information Criterion}
#'     \item{BIC}{Bayesian Information Criterion}
#'     \item{data}{Original data (after any filtering)}
#'     \item{param_info}{List of model metadata}
#'     \item{formula_details}{Design matrix and formula information}
#'     \item{collapse_info}{Collapse levels information (if used)}
#'   }
#'
#' @details
#' Traditional NLME approaches using \code{nlme::nlme()} universally fail for
#' demand equations because the PNLS algorithm with numerical finite-difference
#' gradients cannot navigate the likelihood surface. TMB succeeds using exact
#' automatic differentiation, Laplace approximation, and joint marginal
#' likelihood optimization.
#'
#' When \code{estimate_k = TRUE}, k is estimated as a free parameter alongside
#' Q0 and alpha. This typically improves model fit substantially. The
#' conventional fixed-k approach (Hursh & Silberberg, 2008) often overestimates
#' k by 3-8x.
#'
#' @examples
#' \donttest{
#' data(apt)
#'
#' # Exponential (HS) on log(Q)
#' fit <- fit_demand_tmb(apt, y_var = "y", x_var = "x", id_var = "id",
#'                       equation = "exponential")
#' summary(fit)
#' plot(fit)
#'
#' # Exponentiated (Koffarnus) on raw Q
#' fit2 <- fit_demand_tmb(apt, y_var = "y", x_var = "x", id_var = "id",
#'                        equation = "exponentiated")
#'
#' # With covariates
#' data(apt_full)
#' fit3 <- fit_demand_tmb(apt_full, y_var = "y", x_var = "x", id_var = "id",
#'                        equation = "exponential", factors = "gender")
#' get_demand_param_emms(fit3, param = "alpha")
#' }
#'
#' @seealso [fit_demand_mixed()] for NLME-based fitting,
#'   [fit_demand_hurdle()] for two-part hurdle models.
#'
#' @export
fit_demand_tmb <- function(
  data,
  y_var = "y",
  x_var = "x",
  id_var = "id",
  equation = c("exponentiated", "exponential", "simplified", "zben"),
  estimate_k = TRUE,
  k = NULL,
  random_effects = c("q0", "alpha"),
  factors = NULL,
  factor_interaction = FALSE,
  continuous_covariates = NULL,
  collapse_levels = NULL,
  start_values = NULL,
  tmb_control = list(iter_max = 1000, eval_max = 2000),
  multi_start = TRUE,
  verbose = 1,
  ...
) {
  cl <- match.call()
  equation <- match.arg(equation)

  # Validate random_effects
  valid_re <- c("q0", "alpha")
  if (!all(random_effects %in% valid_re)) {
    stop("random_effects must be a subset of: ",
         paste(valid_re, collapse = ", "), call. = FALSE)
  }
  if (!("q0" %in% random_effects)) {
    stop("random_effects must include at least 'q0'.", call. = FALSE)
  }
  n_re <- length(random_effects)

  # Determine if k is used
  has_k <- equation %in% c("exponentiated", "exponential")
  if (has_k && !estimate_k) {
    if (is.null(k)) k <- 2
    if (!is.numeric(k) || length(k) != 1 || k <= 0) {
      stop("`k` must be a positive number.", call. = FALSE)
    }
  }

  # Validate and prepare data
  data <- validate_demand_data(
    data = data,
    y_var = y_var,
    x_var = x_var,
    id_var = id_var,
    factors = factors
  )

  # Handle collapse_levels
  collapse_info <- NULL
  factors_q0 <- factors
  factors_alpha <- factors

  if (!is.null(collapse_levels)) {
    validate_collapse_levels(collapse_levels)

    if ("Q0" %in% names(collapse_levels) && !is.null(collapse_levels$Q0)) {
      cl_q0 <- collapse_factor_levels(
        data, collapse_levels$Q0, factors, suffix = "Q0"
      )
      data <- cl_q0$data
      factors_q0 <- cl_q0$new_factor_names
      collapse_info$Q0 <- cl_q0$info
    }

    if ("alpha" %in% names(collapse_levels) && !is.null(collapse_levels$alpha)) {
      cl_alpha <- collapse_factor_levels(
        data, collapse_levels$alpha, factors, suffix = "alpha"
      )
      data <- cl_alpha$data
      factors_alpha <- cl_alpha$new_factor_names
      collapse_info$alpha <- cl_alpha$info
    }
  }

  # Validate continuous covariates
  if (!is.null(continuous_covariates)) {
    missing_cov <- setdiff(continuous_covariates, names(data))
    if (length(missing_cov) > 0) {
      stop("Continuous covariates not found in data: ",
           paste(missing_cov, collapse = ", "), call. = FALSE)
    }
  }

  # Prepare data
  if (verbose >= 1) {
    message(sprintf("Fitting TMB mixed-effects demand model..."))
    message(sprintf("  Equation: %s", equation))
  }

  prepared <- .tmb_prepare_data(data, y_var, x_var, id_var, equation)

  # Build design matrices (using the data after any filtering)
  # For exponential equation, we need the filtered data
  if (equation == "exponential") {
    # Rebuild data for positive observations only
    pos_idx <- data[[y_var]] > 0
    data_for_design <- data[pos_idx, , drop = FALSE]
  } else {
    data_for_design <- data
  }

  design <- .tmb_build_design_matrices(
    data = data_for_design,
    factors_q0 = factors_q0,
    factors_alpha = factors_alpha,
    factor_interaction = factor_interaction,
    continuous_covariates = continuous_covariates
  )

  if (verbose >= 1) {
    message(sprintf(
      "  Subjects: %d, Observations: %d",
      prepared$n_subjects, prepared$n_obs
    ))
    message(sprintf(
      "  Random effects: %d (%s)",
      n_re, paste(random_effects, collapse = ", ")
    ))
    message(sprintf(
      "  Design matrices: X_q0 [%d x %d], X_alpha [%d x %d]",
      nrow(design$X_q0), ncol(design$X_q0),
      nrow(design$X_alpha), ncol(design$X_alpha)
    ))
  }

  # Build TMB data
  tmb_data <- .tmb_build_tmb_data(prepared, design, equation, n_re)

  # Build map
  map <- .tmb_build_map(has_k = has_k && estimate_k, n_re = n_re)

  # Default starting values
  default_starts <- .tmb_default_starts(
    prepared, design, equation, n_re,
    has_k = has_k && estimate_k,
    k_fixed = if (has_k && !estimate_k) k else NULL
  )

  # Merge user start values
  if (!is.null(start_values)) {
    for (nm in names(start_values)) {
      if (nm %in% names(default_starts)) {
        default_starts[[nm]] <- start_values[[nm]]
      }
    }
  }

  # Fix k if not estimated
  if (has_k && !estimate_k) {
    default_starts$log_k <- log(k)
  }

  # Fill in default tmb_control values
  default_control <- list(iter_max = 1000, eval_max = 2000)
  tmb_control <- modifyList(default_control, tmb_control)

  # Fit model
  if (verbose >= 1) message("  Optimizing...")

  if (isTRUE(multi_start)) {
    result <- .tmb_multi_start(
      tmb_data, default_starts, map, n_re, tmb_control, verbose
    )
    obj <- result$obj
    opt <- result$opt
  } else {
    obj <- TMB::MakeADFun(
      data = tmb_data,
      parameters = default_starts,
      map = map,
      random = "u",
      DLL = "beezdemand",
      silent = verbose < 2
    )

    opt_warnings <- character(0)
    opt <- withCallingHandlers(
      nlminb(
        start = obj$par,
        objective = obj$fn,
        gradient = obj$gr,
        control = list(
          eval.max = tmb_control$eval_max,
          iter.max = tmb_control$iter_max,
          trace = if (verbose >= 2) 1 else 0
        )
      ),
      warning = function(w) {
        opt_warnings <<- c(opt_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  }

  # Collect optimization warnings
  if (isTRUE(multi_start)) {
    opt_warnings <- result$opt_warnings %||% character(0)
  }
  if (length(opt_warnings) > 0 && verbose >= 1) {
    unique_warnings <- unique(opt_warnings)
    for (w in unique_warnings) {
      message("  Optimizer warning: ", w)
    }
  }

  converged <- opt$convergence == 0
  try(obj$fn(opt$par), silent = TRUE)

  if (verbose >= 1) {
    if (converged) {
      message(sprintf("  Converged (NLL = %.2f)", opt$objective))
    } else {
      message(sprintf(
        "  WARNING: Did not converge (code %d: %s)",
        opt$convergence, opt$message
      ))
    }
  }

  # Extract estimates
  if (verbose >= 1) message("  Computing standard errors...")
  estimates <- .tmb_extract_estimates(
    obj, opt, n_re, prepared$n_subjects,
    has_k = has_k && estimate_k, verbose = verbose
  )

  # Compute subject-specific parameters
  subject_pars <- .tmb_compute_subject_pars(
    coefficients = estimates$coefficients,
    u_hat = estimates$u_hat,
    subject_levels = prepared$subject_levels,
    n_re = n_re,
    has_k = has_k,
    equation = equation,
    price = prepared$price,
    subject_id = prepared$subject_id,
    k_fixed = if (has_k && !estimate_k) k else NULL,
    X_q0 = design$X_q0,
    X_alpha = design$X_alpha
  )

  # Compute log-likelihood, AIC, BIC
  nll <- opt$objective
  loglik <- -nll
  n_fixed_params <- length(opt$par)
  n_obs <- prepared$n_obs
  aic <- 2 * nll + 2 * n_fixed_params
  bic <- 2 * nll + n_fixed_params * log(n_obs)

  # Build return object
  result <- structure(
    list(
      call = cl,
      model = list(
        coefficients = estimates$coefficients,
        se = estimates$se,
        variance_components = estimates$variance_components
      ),
      random_effects = random_effects,
      opt_warnings = opt_warnings,
      se_available = !is.null(estimates$sdr),
      subject_pars = subject_pars,
      tmb_obj = obj,
      opt = opt,
      sdr = estimates$sdr,
      converged = converged,
      loglik = loglik,
      AIC = aic,
      BIC = bic,
      data = if (equation == "exponential") data_for_design else data,
      data_all = data,
      param_info = list(
        equation = equation,
        n_subjects = prepared$n_subjects,
        n_obs = prepared$n_obs,
        n_zeros = prepared$n_zeros,
        n_dropped = prepared$n_dropped,
        n_random_effects = n_re,
        random_effects_spec = random_effects,
        has_k = has_k,
        estimate_k = estimate_k,
        k_fixed = if (has_k && !estimate_k) k else NULL,
        y_var = y_var,
        x_var = x_var,
        id_var = id_var,
        factors = factors,
        factors_q0 = factors_q0,
        factors_alpha = factors_alpha,
        factor_interaction = factor_interaction,
        continuous_covariates = continuous_covariates,
        subject_levels = prepared$subject_levels
      ),
      formula_details = list(
        X_q0 = design$X_q0,
        X_alpha = design$X_alpha,
        rhs_q0 = design$rhs_q0,
        rhs_alpha = design$rhs_alpha
      ),
      collapse_info = collapse_info
    ),
    class = "beezdemand_tmb"
  )

  if (verbose >= 1) message("Done.")

  result
}
