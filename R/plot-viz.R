# Advanced Visualization Functions for beezdemand
#
# Loss surface, demand overlay, model comparison, expenditure,
# elasticity, alpha distribution, and RE diagnostics.


# =============================================================================
# Internal Helpers
# =============================================================================

#' Map hurdle model Part II equation to SSR computation form
#' @noRd
.map_hurdle_equation <- function(part2) {
  mapping <- c(
    zhao_exponential = "zhao_exponential",
    exponential = "exponential",
    simplified_exponential = "simplified_exponential"
  )
  eq <- unname(mapping[part2])
  if (is.na(eq)) {
    stop(
      "Unrecognized hurdle Part II equation: '", part2,
      "'. Expected one of: ",
      paste(names(mapping), collapse = ", "),
      call. = FALSE
    )
  }
  eq
}


#' Extract population MLE from hurdle model
#' @noRd
.extract_hurdle_mle <- function(object) {
  coefs <- object$model$coefficients

  # Normalize legacy naming
  if ("logQ0" %in% names(coefs) && !"log_q0" %in% names(coefs)) {
    names(coefs)[names(coefs) == "logQ0"] <- "log_q0"
  }

  has_k <- "log_k" %in% names(coefs)

  list(
    Q0       = exp(coefs[["log_q0"]]),
    alpha    = exp(coefs[["log_alpha"]]),
    k        = if (has_k) exp(coefs[["log_k"]]) else NULL,
    ln_q0    = unname(coefs[["log_q0"]]),
    ln_alpha = unname(coefs[["log_alpha"]]),
    ln_k     = if (has_k) unname(coefs[["log_k"]]) else NULL,
    beta0    = unname(coefs[["beta0"]]),
    beta1    = unname(coefs[["beta1"]])
  )
}


#' Map TMB equation name to SSR computation form
#' @noRd
.map_tmb_equation <- function(equation) {
  mapping <- c(
    exponential = "tmb_exponential",
    exponentiated = "tmb_exponentiated",
    simplified = "tmb_simplified",
    zben = "tmb_zben"
  )
  eq <- unname(mapping[equation])
  if (is.na(eq)) {
    stop(
      "Unrecognized TMB equation: '", equation,
      "'. Expected one of: ",
      paste(names(mapping), collapse = ", "),
      call. = FALSE
    )
  }
  eq
}


#' Extract population MLE from TMB model
#' @noRd
.extract_tmb_mle <- function(object) {
  coefs <- object$model$coefficients
  beta_q0_idx <- which(names(coefs) == "beta_q0")
  beta_alpha_idx <- which(names(coefs) == "beta_alpha")

  log_q0 <- unname(coefs[beta_q0_idx[1]])
  log_alpha <- unname(coefs[beta_alpha_idx[1]])
  has_k <- object$param_info$has_k

  list(
    Q0       = exp(log_q0),
    alpha    = exp(log_alpha),
    k        = if (has_k) .tmb_get_k(object) else NULL,
    ln_q0    = log_q0,
    ln_alpha = log_alpha,
    ln_k     = if (has_k) log(.tmb_get_k(object)) else NULL
  )
}


#' Check that TMB model is intercept-only (no covariates)
#' @noRd
.check_tmb_intercept_only <- function(object) {
  coefs <- object$model$coefficients
  n_q0 <- length(which(names(coefs) == "beta_q0"))
  n_alpha <- length(which(names(coefs) == "beta_alpha"))
  if (n_q0 > 1 || n_alpha > 1) {
    stop(
      "Loss surface requires intercept-only models. ",
      "Use get_demand_param_emms() for covariate models.",
      call. = FALSE
    )
  }
  invisible(NULL)
}


#' Aggregate data on TMB model's native response scale
#' @noRd
.aggregate_tmb_means <- function(object) {
  data <- object$data
  x_var <- object$param_info$x_var %||% "x"
  y_var <- object$param_info$y_var %||% "y"
  equation <- object$param_info$equation

  if (equation == "exponential") {
    # Exponential equation works on log scale; zeros already dropped by fit
    work <- data[data[[y_var]] > 0, , drop = FALSE]
    if (nrow(work) == 0) {
      stop("No positive consumption observations found in model data.",
        call. = FALSE
      )
    }
    work$.response <- log(work[[y_var]])
  } else {
    # exponentiated, simplified, zben: use raw y
    work <- data
    work$.response <- work[[y_var]]
  }

  agg <- stats::aggregate(
    work[".response"],
    by = list(price = work[[x_var]]),
    FUN = mean,
    na.rm = TRUE
  )
  names(agg) <- c("price", "mean_response")

  n_agg <- stats::aggregate(
    work[".response"],
    by = list(price = work[[x_var]]),
    FUN = length
  )
  agg$n <- n_agg[[2]]
  agg[order(agg$price), ]
}


#' Map NLME equation form to SSR computation form
#' @noRd
.map_nlme_equation <- function(equation_form) {
  mapping <- c(
    zben          = "tmb_zben",
    simplified    = "tmb_simplified",
    exponentiated = "tmb_exponentiated"
  )
  eq <- unname(mapping[equation_form])
  if (is.na(eq)) {
    stop(
      "Unrecognized NLME equation form: '", equation_form,
      "'. Expected one of: ",
      paste(names(mapping), collapse = ", "),
      call. = FALSE
    )
  }
  eq
}


#' Extract population MLE from NLME model
#' @noRd
.extract_nlme_mle <- function(object) {
  fe <- nlme::fixef(object$model)
  param_space <- object$param_info$param_space %||% "log10"

  num_q0 <- object$param_info$num_params_Q0
  Q0_internal <- unname(fe[1])
  alpha_internal <- unname(fe[num_q0 + 1])

  if (param_space == "log10") {
    Q0_nat    <- 10^Q0_internal
    alpha_nat <- 10^alpha_internal
  } else {
    Q0_nat    <- Q0_internal
    alpha_nat <- alpha_internal
  }

  list(
    Q0       = Q0_nat,
    alpha    = alpha_nat,
    k        = object$param_info$k,
    ln_q0    = log(Q0_nat),
    ln_alpha = log(alpha_nat)
  )
}


#' Check that NLME model is intercept-only (no covariates)
#' @noRd
.check_nlme_intercept_only <- function(object) {
  n_q0 <- object$param_info$num_params_Q0
  n_alpha <- object$param_info$num_params_alpha
  if (n_q0 > 1 || n_alpha > 1) {
    stop(
      "Loss surface requires intercept-only models. ",
      "Use get_demand_param_emms() for covariate models.",
      call. = FALSE
    )
  }
  invisible(NULL)
}


#' Aggregate NLME response data by price
#' @noRd
.aggregate_nlme_means <- function(object) {
  data <- object$data
  x_var <- object$param_info$x_var
  y_var <- object$param_info$y_var

  work <- data
  work$.response <- work[[y_var]]

  agg <- stats::aggregate(
    work[".response"],
    by = list(price = work[[x_var]]),
    FUN = mean,
    na.rm = TRUE
  )
  names(agg) <- c("price", "mean_response")

  n_agg <- stats::aggregate(
    work[".response"],
    by = list(price = work[[x_var]]),
    FUN = length
  )
  agg$n <- n_agg[[2]]
  agg[order(agg$price), ]
}


#' Check NLME convergence for loss surface plotting
#' @noRd
.check_nlme_convergence_for_plot <- function(object) {
  if (is.null(object$model)) {
    stop("Model fitting failed; cannot compute loss surface.", call. = FALSE)
  }
  if (is.character(object$model$apVar)) {
    warning(
      "Model Hessian is not positive definite. The loss surface may help ",
      "diagnose convergence issues, but parameter estimates may be unreliable.",
      call. = FALSE
    )
  }
  invisible(NULL)
}


#' Validate loss surface input
#' @noRd
.check_loss_surface_input <- function(object) {
  if (!isTRUE(object$converged)) {
    warning(
      "Model did not converge. The loss surface may help diagnose ",
      "convergence issues, but parameter estimates may be unreliable.",
      call. = FALSE
    )
  }
  invisible(NULL)
}


#' Aggregate positive observations to mean log(y) by price
#' @noRd
.aggregate_positive_log_means <- function(object) {
  data <- object$data
  x_var <- object$param_info$x_var %||% "x"
  y_var <- object$param_info$y_var %||% "y"

  pos_data <- data[data[[y_var]] > 0, , drop = FALSE]
  if (nrow(pos_data) == 0) {
    stop("No positive consumption observations found in model data.", call. = FALSE)
  }

  pos_data$.log_y <- log(pos_data[[y_var]])

  agg <- stats::aggregate(
    pos_data[".log_y"],
    by = list(price = pos_data[[x_var]]),
    FUN = mean,
    na.rm = TRUE
  )
  names(agg) <- c("price", "mean_log_y")

  n_agg <- stats::aggregate(
    pos_data[".log_y"],
    by = list(price = pos_data[[x_var]]),
    FUN = length
  )
  agg$n <- n_agg[[2]]
  agg[order(agg$price), ]
}


#' Vectorized SSR computation over a parameter grid
#'
#' @param ln_q0_vec Vector of log(Q0) values (natural log).
#' @param alpha_vec Vector of alpha values (natural scale).
#' @param k Scalar k value (fixed at MLE). NULL for simplified equation.
#' @param prices Vector of price points.
#' @param observed Vector of mean log(y) values at each price.
#' @param equation Character: equation form name.
#' @return Vector of SSR values, one per grid row.
#' @noRd
.compute_ssr_grid <- function(ln_q0_vec, alpha_vec, k, prices, observed,
                              equation) {
  pred_matrix <- switch(equation,
    zhao_exponential = {
      # log(Q) = ln_q0 + k * (exp(-alpha * P) - 1)
      exp_term <- exp(outer(-alpha_vec, prices))
      sweep(k * (exp_term - 1), 1, ln_q0_vec, "+")
    },
    exponential = {
      # log(Q) = ln_q0 + k * (exp(-alpha * Q0 * P) - 1)
      Q0_vec <- exp(ln_q0_vec)
      aQP <- outer(alpha_vec * Q0_vec, prices)
      sweep(k * (exp(-aQP) - 1), 1, ln_q0_vec, "+")
    },
    simplified_exponential = {
      # log(Q) = ln_q0 - alpha * Q0 * P
      Q0_vec <- exp(ln_q0_vec)
      aQP <- outer(alpha_vec * Q0_vec, prices)
      sweep(-aQP, 1, ln_q0_vec, "+")
    },
    tmb_exponential = {
      # ln(Q) = ln_q0 + k*ln(10)*(exp(-alpha*Q0*P) - 1)
      Q0_vec <- exp(ln_q0_vec)
      aQP <- outer(alpha_vec * Q0_vec, prices)
      sweep(k * log(10) * (exp(-aQP) - 1), 1, ln_q0_vec, "+")
    },
    tmb_exponentiated = {
      # Q = exp(ln_q0 + k*ln(10)*(exp(-alpha*Q0*P) - 1))
      Q0_vec <- exp(ln_q0_vec)
      aQP <- outer(alpha_vec * Q0_vec, prices)
      exp(sweep(k * log(10) * (exp(-aQP) - 1), 1, ln_q0_vec, "+"))
    },
    tmb_simplified = {
      # Q = Q0 * exp(-alpha * Q0 * P)
      Q0_vec <- exp(ln_q0_vec)
      aQP <- outer(alpha_vec * Q0_vec, prices)
      sweep(exp(-aQP), 1, Q0_vec, "*")
    },
    tmb_zben = {
      # LL4(Q) = log10(Q0) * exp(-(alpha/log10(Q0)) * Q0 * P)
      Q0_log10 <- ln_q0_vec / log(10)
      Q0_log10 <- sign(Q0_log10) * pmax(abs(Q0_log10), 1e-6)
      Q0_vec <- exp(ln_q0_vec)
      # rate = (alpha / log10(Q0)) * Q0 for each (alpha, Q0) pair
      rate_vec <- (alpha_vec / Q0_log10) * Q0_vec
      sweep(exp(outer(-rate_vec, prices)), 1, Q0_log10, "*")
    },
    stop("Unknown equation: ", equation, call. = FALSE)
  )

  # Cap extreme predictions to prevent Inf SSR
  pred_matrix[!is.finite(pred_matrix)] <- NA_real_
  pred_matrix[pred_matrix > 50] <- 50
  pred_matrix[pred_matrix < -50] <- -50

  resid_matrix <- sweep(pred_matrix, 2, observed)
  rowSums(resid_matrix^2, na.rm = FALSE)
}


#' Compute demand prediction at specific parameters (single point)
#' @noRd
.demand_curve_at <- function(ln_q0, alpha, k, prices, equation) {
  switch(equation,
    zhao_exponential = {
      ln_q0 + k * (exp(-alpha * prices) - 1)
    },
    exponential = {
      Q0 <- exp(ln_q0)
      ln_q0 + k * (exp(-alpha * Q0 * prices) - 1)
    },
    simplified_exponential = {
      Q0 <- exp(ln_q0)
      ln_q0 - alpha * Q0 * prices
    },
    stop("Unknown equation: ", equation, call. = FALSE)
  )
}


# =============================================================================
# Linearized Marginal NLL for NLME Models
# =============================================================================

#' Evaluate NLME demand equation at given parameters
#'
#' Computes the demand curve prediction for the equation forms used by
#' \code{fit_demand_mixed()} with \code{param_space = "log10"}.
#'
#' @param x Numeric vector of prices.
#' @param Q0_log10 Numeric scalar, Q0 in log10 space.
#' @param alpha_log10 Numeric scalar, alpha in log10 space.
#' @param k Numeric scalar or NULL. Scaling constant (exponentiated only).
#' @param equation_form One of "exponentiated", "simplified", "zben".
#' @return Numeric vector of predicted consumption (same length as x).
#' @noRd
.nlme_eval_demand <- function(x, Q0_log10, alpha_log10, k, equation_form) {
  switch(equation_form,
    exponentiated = {
      # y = (10^Q0) * 10^(k * (exp(-(10^alpha) * (10^Q0) * x) - 1))
      Q0_nat <- 10^Q0_log10
      alpha_nat <- 10^alpha_log10
      Q0_nat * 10^(k * (exp(-alpha_nat * Q0_nat * x) - 1))
    },
    simplified = {
      # y = (10^Q0) * exp(-(10^alpha) * (10^Q0) * x)
      Q0_nat <- 10^Q0_log10
      alpha_nat <- 10^alpha_log10
      Q0_nat * exp(-alpha_nat * Q0_nat * x)
    },
    zben = {
      # y = Q0_log10 * exp(-((10^alpha) / Q0_log10) * (10^Q0) * x)
      Q0_nat <- 10^Q0_log10
      alpha_nat <- 10^alpha_log10
      Q0_log10 * exp(-(alpha_nat / Q0_log10) * Q0_nat * x)
    },
    stop("Unknown NLME equation form: ", equation_form, call. = FALSE)
  )
}


#' Compute Jacobian of demand equation via central finite differences
#'
#' Returns a matrix of partial derivatives df/dQ0_log10 and df/dalpha_log10
#' evaluated at the given parameters.
#'
#' @param x Numeric vector of prices.
#' @param Q0_log10 Numeric scalar.
#' @param alpha_log10 Numeric scalar.
#' @param k Numeric or NULL.
#' @param equation_form Character string.
#' @param h Numeric scalar, step size for finite differences.
#' @return Matrix (length(x) x 2), columns = (dQ0, dalpha).
#' @noRd
.nlme_compute_jacobian <- function(x, Q0_log10, alpha_log10, k,
                                   equation_form, h = 1e-6) {
  f_q0_plus <- .nlme_eval_demand(x, Q0_log10 + h, alpha_log10, k,
                                 equation_form)
  f_q0_minus <- .nlme_eval_demand(x, Q0_log10 - h, alpha_log10, k,
                                  equation_form)
  f_alpha_plus <- .nlme_eval_demand(x, Q0_log10, alpha_log10 + h, k,
                                    equation_form)
  f_alpha_minus <- .nlme_eval_demand(x, Q0_log10, alpha_log10 - h, k,
                                     equation_form)

  cbind(
    dQ0 = (f_q0_plus - f_q0_minus) / (2 * h),
    dalpha = (f_alpha_plus - f_alpha_minus) / (2 * h)
  )
}


#' Precompute linearized marginal NLL components from fitted NLME model
#'
#' Extracts fixed effects, random effects, and variance components from a
#' \code{beezdemand_nlme} object, then computes per-subject Jacobians,
#' pseudo-responses, and marginal covariance inverses. All components that
#' do not depend on the grid of fixed-effect values are computed once.
#'
#' @param object A fitted \code{beezdemand_nlme} object.
#' @return List with components:
#' \describe{
#'   \item{subjects}{List of per-subject structs (F_i, y_star_i, V_inv_i,
#'     log_det_V_i)}
#'   \item{beta_hat}{Named numeric vector of fixed effects in log10 space}
#'   \item{N}{Total number of observations}
#'   \item{log_2pi_term}{(N/2)*log(2*pi) constant}
#' }
#' @noRd
.nlme_precompute_marginal <- function(object) {
  model <- object$model
  data <- object$data
  equation_form <- object$formula_details$equation_form_selected
  x_var <- object$param_info$x_var
  y_var <- object$param_info$y_var
  id_var <- object$param_info$id_var
  k <- object$param_info$k  # NULL for simplified/zben

  # Fixed effects (log10 space)
  fe <- nlme::fixef(model)
  num_q0 <- object$param_info$num_params_Q0
  beta_hat <- c(Q0 = unname(fe[1]), alpha = unname(fe[num_q0 + 1]))

  # Random effects per subject
  re <- nlme::ranef(model)
  subject_ids <- rownames(re)

  # Variance components
  vc <- nlme::VarCorr(model)
  sigma <- as.numeric(vc[nrow(vc), "StdDev"])  # residual SD

  # RE variance-covariance matrix D
  re_names <- colnames(re)
  n_re <- ncol(re)
  if (n_re == 1) {
    D <- matrix(as.numeric(vc[1, "Variance"]), 1, 1)
  } else {
    # Extract D from VarCorr — diagonal variances + off-diagonal correlations
    D <- matrix(0, n_re, n_re)
    for (j in seq_len(n_re)) {
      D[j, j] <- as.numeric(vc[j, "Variance"])
    }
    # Off-diagonal: VarCorr stores correlation in the Corr column
    if (n_re == 2 && "Corr" %in% colnames(vc)) {
      corr_val <- suppressWarnings(as.numeric(vc[2, "Corr"]))
      if (!is.na(corr_val)) {
        sd1 <- sqrt(D[1, 1])
        sd2 <- sqrt(D[2, 2])
        D[1, 2] <- D[2, 1] <- corr_val * sd1 * sd2
      }
    }
  }

  # Determine which fixed-effect indices have random effects
  # For beezdemand NLME: Q0 is always RE; alpha is RE if n_re == 2
  re_col_map <- seq_len(n_re)  # maps RE columns to beta columns
  # Q0 RE → beta column 1; alpha RE → beta column 2
  # This works because for intercept-only models:
  #   fe[1] = Q0, fe[2] = alpha
  #   re columns = Q0 (and optionally alpha)

  # Process each subject
  N <- 0L
  subjects <- vector("list", length(subject_ids))
  names(subjects) <- subject_ids

  for (s in seq_along(subject_ids)) {
    sid <- subject_ids[s]
    idx <- which(data[[id_var]] == sid)
    x_i <- data[[x_var]][idx]
    y_i <- data[[y_var]][idx]
    n_i <- length(x_i)
    N <- N + n_i

    # Subject-specific parameters: beta_hat + b_hat_i (in log10 space)
    b_hat_i <- as.numeric(re[sid, ])
    theta_Q0 <- beta_hat["Q0"] + b_hat_i[1]
    theta_alpha <- if (n_re >= 2) {
      beta_hat["alpha"] + b_hat_i[2]
    } else {
      beta_hat["alpha"]
    }

    # Evaluate f at subject-specific parameters
    f_i <- .nlme_eval_demand(x_i, theta_Q0, theta_alpha, k, equation_form)

    # Compute Jacobian F_i (n_i x 2) w.r.t. Q0_log10, alpha_log10
    F_i <- .nlme_compute_jacobian(x_i, theta_Q0, theta_alpha, k,
                                  equation_form)

    # Z_i: columns of F_i corresponding to random effects
    Z_i <- F_i[, re_col_map, drop = FALSE]

    # Pseudo-response: y* = y - f(theta) + F * beta + Z * b
    y_star_i <- y_i - f_i + F_i %*% beta_hat + Z_i %*% b_hat_i

    # Marginal covariance: V_i = Z_i D Z_i' + sigma^2 I
    V_i <- Z_i %*% D %*% t(Z_i) + sigma^2 * diag(n_i)

    # Invert V_i and compute log-determinant
    V_chol <- chol(V_i)
    V_inv_i <- chol2inv(V_chol)
    log_det_V_i <- 2 * sum(log(diag(V_chol)))

    subjects[[s]] <- list(
      F_i = F_i,
      y_star_i = as.numeric(y_star_i),
      V_inv_i = V_inv_i,
      log_det_V_i = log_det_V_i
    )
  }

  list(
    subjects = subjects,
    beta_hat = beta_hat,
    N = N,
    log_2pi_term = (N / 2) * log(2 * pi)
  )
}


#' Evaluate linearized marginal NLL on a grid of (Q0, alpha) values
#'
#' @param precomputed List from \code{.nlme_precompute_marginal()}.
#' @param beta_grid Data frame with columns \code{log10_q0} and
#'   \code{log10_alpha}.
#' @return Numeric vector of NLL values (same length as \code{nrow(beta_grid)}).
#' @noRd
.nlme_marginal_nll_grid <- function(precomputed, beta_grid) {
  subjects <- precomputed$subjects
  log_2pi <- precomputed$log_2pi_term
  n_grid <- nrow(beta_grid)

  # Precompute constant part: log_2pi + (1/2) * sum log|V_i|
  sum_log_det <- sum(vapply(subjects, function(s) s$log_det_V_i, numeric(1)))
  const <- log_2pi + 0.5 * sum_log_det

  nll <- vapply(seq_len(n_grid), function(g) {
    beta <- c(beta_grid$log10_q0[g], beta_grid$log10_alpha[g])

    # Sum quadratic forms across subjects
    quad_sum <- 0
    for (s in subjects) {
      r_i <- s$y_star_i - s$F_i %*% beta
      quad_sum <- quad_sum + as.numeric(t(r_i) %*% s$V_inv_i %*% r_i)
    }

    const + 0.5 * quad_sum
  }, numeric(1))

  nll
}


#' Get population-level demand predictions from a fitted model
#'
#' @param object A fitted demand model.
#' @param prices Numeric vector of prices.
#' @return data.frame with columns: price, consumption.
#' @noRd
.get_demand_predictions <- function(object, prices) {
  if (inherits(object, "beezdemand_tmb")) {
    pred <- predict(object, type = "demand", prices = prices, scale = "natural")
    return(data.frame(price = pred$price, consumption = as.numeric(pred$.fitted)))
  }

  if (inherits(object, "beezdemand_hurdle")) {
    x_var <- object$param_info$x_var %||% "x"
    nd <- stats::setNames(data.frame(x = prices), x_var)
    pred <- predict(object, newdata = nd, type = "demand")
    y <- if (".fitted" %in% names(pred)) pred$.fitted else pred[[1]]
    return(data.frame(price = prices, consumption = as.numeric(y)))
  }

  if (inherits(object, "beezdemand_nlme")) {
    x_var <- object$param_info$x_var %||% "x"
    id_var <- object$param_info$id_var %||% "id"
    first_id <- levels(object$data[[id_var]])[1] %||%
      unique(object$data[[id_var]])[1]
    nd <- data.frame(x = prices, id = rep(first_id, length(prices)))
    names(nd) <- c(x_var, id_var)
    # Add factor columns at reference level
    if (length(object$param_info$factors) > 0) {
      for (fac in object$param_info$factors) {
        if (!fac %in% names(nd)) {
          fac_levels <- levels(object$data[[fac]])
          if (is.null(fac_levels)) fac_levels <- unique(object$data[[fac]])
          nd[[fac]] <- factor(fac_levels[1], levels = fac_levels)
        }
      }
    }
    pred <- predict(object, newdata = nd, level = 0)
    return(data.frame(price = prices, consumption = as.numeric(pred)))
  }

  if (inherits(object, "beezdemand_fixed")) {
    if (!is.null(object$agg)) {
      fit <- object$fits[[1]]
      nd <- data.frame(x = prices)
      pred <- stats::predict(fit, newdata = nd)
      return(data.frame(price = prices, consumption = as.numeric(pred)))
    }
    stop(
      "Individual fixed-effect models do not have a single population curve. ",
      "Use fit_demand_fixed() with agg = 'mean' or 'pooled' for overlay.",
      call. = FALSE
    )
  }

  stop(
    "Unsupported model class for demand predictions: ",
    paste(class(object), collapse = ", "),
    call. = FALSE
  )
}


#' Resolve model list from dots and/or explicit list
#' @noRd
.resolve_model_list <- function(..., model_list = NULL, labels = NULL) {
  dots <- list(...)
  models <- c(dots, model_list)
  if (length(models) == 0) {
    stop("At least one model must be provided.", call. = FALSE)
  }
  if (is.null(labels)) {
    labels <- if (!is.null(names(models)) && all(nzchar(names(models)))) {
      names(models)
    } else {
      vapply(seq_along(models), function(i) {
        cls <- class(models[[i]])[1]
        if (length(models) == 1) cls else paste0(cls, " (", i, ")")
      }, character(1))
    }
  }
  if (length(labels) != length(models)) {
    stop("Length of 'labels' must match number of models.", call. = FALSE)
  }
  list(models = models, labels = labels)
}


# =============================================================================
# plot_loss_surface() — 2D Loss Surface Visualization
# =============================================================================

#' Plot Loss Surface for Demand Model Parameters
#'
#' @description
#' Visualizes the sum-of-squared-residuals (SSR) surface over a grid of Q0 and
#' alpha values, holding other parameters (k, variance components) fixed at
#' their MLE. The SSR is computed on aggregated mean log-consumption by price
#' for the Part II (continuous) component of hurdle models.
#'
#' @param object A fitted model object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A ggplot2 object.
#'
#' @details
#' **Important:** This function computes SSR on price-aggregated means, not a
#' true profile likelihood. The resulting surface shows how well different
#' (Q0, alpha) pairs explain the average demand pattern, but does not account
#' for individual variation. For models with large random effects, the surface
#' may appear sharper than the full-data objective.
#'
#' Supported model classes: `beezdemand_hurdle`, `beezdemand_tmb`, and
#' `beezdemand_nlme`. Models with factor covariates on Q0 or alpha are not
#' supported; use [get_demand_param_emms()] instead.
#'
#' @seealso [plot_loss_profile()] for 1D profile slices
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' plot_loss_surface(fit)
#' plot_loss_surface(fit, resolution = 50, show_contours = TRUE)
#' }
#'
#' @export
plot_loss_surface <- function(object, ...) {
  UseMethod("plot_loss_surface")
}


#' @rdname plot_loss_surface
#' @param resolution Integer; grid resolution per axis (default 80).
#' @param q0_range Numeric vector of length 2; Q0 range (natural scale).
#'   Default: MLE +/- 3 orders of magnitude.
#' @param alpha_range Numeric vector of length 2; alpha range (natural scale).
#'   Default: MLE +/- 3 orders of magnitude.
#' @param fill_palette Character; viridis palette option (default `"D"`).
#' @param show_mle Logical; overlay MLE point (default `TRUE`).
#' @param show_contours Logical; add contour lines (default `FALSE`).
#' @param style Character; plot style, `"modern"` or `"apa"`.
#' @export
plot_loss_surface.beezdemand_hurdle <- function(
    object,
    resolution = 80,
    q0_range = NULL,
    alpha_range = NULL,
    fill_palette = "D",
    show_mle = TRUE,
    show_contours = FALSE,
    style = c("modern", "apa"),
    ...) {
  style <- match.arg(style)

  .check_loss_surface_input(object)

  mle <- .extract_hurdle_mle(object)
  part2 <- object$param_info$part2 %||% "zhao_exponential"
  equation <- .map_hurdle_equation(part2)
  agg <- .aggregate_positive_log_means(object)

  # Default ranges: +/- 3 orders of magnitude from MLE (log10 scale)
  if (is.null(q0_range)) {
    q0_range <- 10^(log10(mle$Q0) + c(-3, 3))
  }
  if (is.null(alpha_range)) {
    alpha_range <- 10^(log10(mle$alpha) + c(-3, 3))
  }

  # Build grid in log10 space for even visual spacing
  log10_q0_seq <- seq(
    log10(q0_range[1]), log10(q0_range[2]),
    length.out = resolution
  )
  log10_alpha_seq <- seq(
    log10(alpha_range[1]), log10(alpha_range[2]),
    length.out = resolution
  )
  grid <- expand.grid(
    log10_q0 = log10_q0_seq,
    log10_alpha = log10_alpha_seq
  )

  # Convert to natural scale for equation evaluation
  ln_q0_vec <- grid$log10_q0 * log(10)
  alpha_vec <- 10^grid$log10_alpha
  k_val <- mle$k  # NULL for simplified

  grid$ssr <- .compute_ssr_grid(
    ln_q0_vec = ln_q0_vec,
    alpha_vec = alpha_vec,
    k = k_val,
    prices = agg$price,
    observed = agg$mean_log_y,
    equation = equation
  )

  # Cap SSR for visualization (drop extreme outliers)
  ssr_finite <- grid$ssr[is.finite(grid$ssr)]
  if (length(ssr_finite) > 0) {
    ssr_cap <- stats::quantile(ssr_finite, 0.99)
    grid$ssr[!is.finite(grid$ssr) | grid$ssr > ssr_cap] <- ssr_cap
  }

  # Warn if MLE is near grid boundary
  mle_log10_q0 <- log10(mle$Q0)
  mle_log10_alpha <- log10(mle$alpha)
  margin <- 0.05 * diff(range(log10_q0_seq))
  if (mle_log10_q0 < min(log10_q0_seq) + margin ||
    mle_log10_q0 > max(log10_q0_seq) - margin ||
    mle_log10_alpha < min(log10_alpha_seq) + margin ||
    mle_log10_alpha > max(log10_alpha_seq) - margin) {
    warning(
      "MLE is near the grid boundary. Consider increasing the range ",
      "for a more complete view of the surface.",
      call. = FALSE
    )
  }

  p <- ggplot2::ggplot(
    grid,
    ggplot2::aes(
      x = .data$log10_q0,
      y = .data$log10_alpha,
      fill = .data$ssr
    )
  ) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::scale_fill_viridis_c(
      option = fill_palette,
      direction = -1,
      name = "SSR"
    ) +
    ggplot2::labs(
      x = expression(log[10](Q[0])),
      y = expression(log[10](alpha)),
      title = "Loss Surface (SSR)",
      subtitle = paste0("Equation: ", part2, " | k fixed at MLE")
    ) +
    theme_beezdemand(style = style)

  if (show_contours) {
    p <- p + ggplot2::geom_contour(
      ggplot2::aes(z = .data$ssr),
      color = "white",
      alpha = 0.6,
      linewidth = 0.4
    )
  }

  if (show_mle) {
    mle_df <- data.frame(
      log10_q0 = mle_log10_q0,
      log10_alpha = mle_log10_alpha
    )
    p <- p + ggplot2::geom_point(
      data = mle_df,
      ggplot2::aes(x = .data$log10_q0, y = .data$log10_alpha),
      inherit.aes = FALSE,
      shape = 4, size = 4, color = "white", stroke = 1.5
    )
  }

  p
}


#' @rdname plot_loss_surface
#' @export
plot_loss_surface.beezdemand_tmb <- function(
    object,
    resolution = 80,
    q0_range = NULL,
    alpha_range = NULL,
    fill_palette = "D",
    show_mle = TRUE,
    show_contours = FALSE,
    style = c("modern", "apa"),
    ...) {
  style <- match.arg(style)

  .check_tmb_intercept_only(object)
  .check_loss_surface_input(object)

  mle <- .extract_tmb_mle(object)
  equation <- .map_tmb_equation(object$param_info$equation)
  agg <- .aggregate_tmb_means(object)

  if (is.null(q0_range)) {
    q0_range <- 10^(log10(mle$Q0) + c(-3, 3))
  }
  if (is.null(alpha_range)) {
    alpha_range <- 10^(log10(mle$alpha) + c(-3, 3))
  }

  log10_q0_seq <- seq(
    log10(q0_range[1]), log10(q0_range[2]),
    length.out = resolution
  )
  log10_alpha_seq <- seq(
    log10(alpha_range[1]), log10(alpha_range[2]),
    length.out = resolution
  )
  grid <- expand.grid(
    log10_q0 = log10_q0_seq,
    log10_alpha = log10_alpha_seq
  )

  ln_q0_vec <- grid$log10_q0 * log(10)
  alpha_vec <- 10^grid$log10_alpha
  k_val <- mle$k

  grid$ssr <- .compute_ssr_grid(
    ln_q0_vec = ln_q0_vec,
    alpha_vec = alpha_vec,
    k = k_val,
    prices = agg$price,
    observed = agg$mean_response,
    equation = equation
  )

  ssr_finite <- grid$ssr[is.finite(grid$ssr)]
  if (length(ssr_finite) > 0) {
    ssr_cap <- stats::quantile(ssr_finite, 0.99)
    grid$ssr[!is.finite(grid$ssr) | grid$ssr > ssr_cap] <- ssr_cap
  }

  mle_log10_q0 <- log10(mle$Q0)
  mle_log10_alpha <- log10(mle$alpha)
  margin <- 0.05 * diff(range(log10_q0_seq))
  if (mle_log10_q0 < min(log10_q0_seq) + margin ||
    mle_log10_q0 > max(log10_q0_seq) - margin ||
    mle_log10_alpha < min(log10_alpha_seq) + margin ||
    mle_log10_alpha > max(log10_alpha_seq) - margin) {
    warning(
      "MLE is near the grid boundary. Consider increasing the range ",
      "for a more complete view of the surface.",
      call. = FALSE
    )
  }

  eq_display <- object$param_info$equation
  k_label <- if (!is.null(mle$k)) " | k fixed at MLE" else ""

  p <- ggplot2::ggplot(
    grid,
    ggplot2::aes(
      x = .data$log10_q0,
      y = .data$log10_alpha,
      fill = .data$ssr
    )
  ) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::scale_fill_viridis_c(
      option = fill_palette,
      direction = -1,
      name = "SSR"
    ) +
    ggplot2::labs(
      x = expression(log[10](Q[0])),
      y = expression(log[10](alpha)),
      title = "Loss Surface (SSR)",
      subtitle = paste0("Equation: ", eq_display, k_label)
    ) +
    theme_beezdemand(style = style)

  if (show_contours) {
    p <- p + ggplot2::geom_contour(
      ggplot2::aes(z = .data$ssr),
      color = "white",
      alpha = 0.6,
      linewidth = 0.4
    )
  }

  if (show_mle) {
    mle_df <- data.frame(
      log10_q0 = mle_log10_q0,
      log10_alpha = mle_log10_alpha
    )
    p <- p + ggplot2::geom_point(
      data = mle_df,
      ggplot2::aes(x = .data$log10_q0, y = .data$log10_alpha),
      inherit.aes = FALSE,
      shape = 4, size = 4, color = "white", stroke = 1.5
    )
  }

  p
}


#' @rdname plot_loss_surface
#' @export
plot_loss_surface.beezdemand_nlme <- function(
    object,
    resolution = 80,
    q0_range = NULL,
    alpha_range = NULL,
    fill_palette = "D",
    show_mle = TRUE,
    show_contours = FALSE,
    style = c("modern", "apa"),
    type = c("ssr", "marginal"),
    ...) {
  style <- match.arg(style)
  type <- match.arg(type)

  .check_nlme_intercept_only(object)
  .check_nlme_convergence_for_plot(object)

  mle <- .extract_nlme_mle(object)
  equation_form <- object$formula_details$equation_form_selected

  if (is.null(q0_range)) {
    q0_range <- 10^(log10(mle$Q0) + c(-3, 3))
  }
  if (is.null(alpha_range)) {
    alpha_range <- 10^(log10(mle$alpha) + c(-3, 3))
  }

  log10_q0_seq <- seq(
    log10(q0_range[1]), log10(q0_range[2]),
    length.out = resolution
  )
  log10_alpha_seq <- seq(
    log10(alpha_range[1]), log10(alpha_range[2]),
    length.out = resolution
  )
  grid <- expand.grid(
    log10_q0 = log10_q0_seq,
    log10_alpha = log10_alpha_seq
  )

  if (type == "marginal") {
    # Linearized marginal NLL: integrate random effects via pseudo-LME
    pre <- .nlme_precompute_marginal(object)
    grid$value <- .nlme_marginal_nll_grid(pre, grid)
    fill_name <- "NLL"
    title_text <- "Loss Surface (Marginal NLL)"
    k_label <- if (!is.null(mle$k)) {
      paste0(" | k = ", round(mle$k, 3))
    } else {
      ""
    }
    subtitle_text <- paste0(
      "Equation: ", equation_form,
      " (NLME) | Linearized marginal NLL (accurate near MLE)", k_label
    )
  } else {
    # SSR on aggregated means (original behavior)
    equation <- .map_nlme_equation(equation_form)
    agg <- .aggregate_nlme_means(object)
    ln_q0_vec <- grid$log10_q0 * log(10)
    alpha_vec <- 10^grid$log10_alpha
    grid$value <- .compute_ssr_grid(
      ln_q0_vec = ln_q0_vec,
      alpha_vec = alpha_vec,
      k = mle$k,
      prices = agg$price,
      observed = agg$mean_response,
      equation = equation
    )
    fill_name <- "SSR"
    title_text <- "Loss Surface (SSR)"
    k_label <- if (!is.null(mle$k)) {
      paste0(" | k = ", round(mle$k, 3))
    } else {
      ""
    }
    subtitle_text <- paste0("Equation: ", equation_form, " (NLME)", k_label)
  }

  # Cap outliers for visualization
  val_finite <- grid$value[is.finite(grid$value)]
  if (length(val_finite) > 0) {
    val_cap <- stats::quantile(val_finite, 0.99)
    grid$value[!is.finite(grid$value) | grid$value > val_cap] <- val_cap
  }

  mle_log10_q0 <- log10(mle$Q0)
  mle_log10_alpha <- log10(mle$alpha)
  margin <- 0.05 * diff(range(log10_q0_seq))
  if (mle_log10_q0 < min(log10_q0_seq) + margin ||
    mle_log10_q0 > max(log10_q0_seq) - margin ||
    mle_log10_alpha < min(log10_alpha_seq) + margin ||
    mle_log10_alpha > max(log10_alpha_seq) - margin) {
    warning(
      "MLE is near the grid boundary. Consider increasing the range ",
      "for a more complete view of the surface.",
      call. = FALSE
    )
  }

  p <- ggplot2::ggplot(
    grid,
    ggplot2::aes(
      x = .data$log10_q0,
      y = .data$log10_alpha,
      fill = .data$value
    )
  ) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::scale_fill_viridis_c(
      option = fill_palette,
      direction = -1,
      name = fill_name
    ) +
    ggplot2::labs(
      x = expression(log[10](Q[0])),
      y = expression(log[10](alpha)),
      title = title_text,
      subtitle = subtitle_text
    ) +
    theme_beezdemand(style = style)

  if (show_contours) {
    p <- p + ggplot2::geom_contour(
      ggplot2::aes(z = .data$value),
      color = "white",
      alpha = 0.6,
      linewidth = 0.4
    )
  }

  if (show_mle) {
    mle_df <- data.frame(
      log10_q0 = mle_log10_q0,
      log10_alpha = mle_log10_alpha
    )
    p <- p + ggplot2::geom_point(
      data = mle_df,
      ggplot2::aes(x = .data$log10_q0, y = .data$log10_alpha),
      inherit.aes = FALSE,
      shape = 4, size = 4, color = "white", stroke = 1.5
    )
  }

  p
}


# =============================================================================
# plot_loss_profile() — 1D Profile Slices
# =============================================================================

#' Plot Loss Profile for a Single Parameter
#'
#' @description
#' Plots 1D slices of the SSR surface, fixing one parameter at the MLE and
#' varying the other.
#'
#' @inheritParams plot_loss_surface
#' @param parameter Character; which parameter to profile: `"q0"`, `"alpha"`,
#'   or `"both"` (default).
#' @param resolution Integer; number of grid points (default 200).
#' @param range Numeric vector of length 2; range in log10 units relative to
#'   MLE (default `c(-3, 3)`).
#'
#' @return A ggplot2 object. If `parameter = "both"` and patchwork is
#'   available, returns a combined patchwork object.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' plot_loss_profile(fit, parameter = "q0")
#' plot_loss_profile(fit, parameter = "both")
#' }
#'
#' @export
plot_loss_profile <- function(object, ...) {
  UseMethod("plot_loss_profile")
}


#' @rdname plot_loss_profile
#' @export
plot_loss_profile.beezdemand_hurdle <- function(
    object,
    parameter = c("both", "q0", "alpha"),
    resolution = 200,
    range = c(-3, 3),
    style = c("modern", "apa"),
    ...) {
  parameter <- match.arg(parameter)
  style <- match.arg(style)

  .check_loss_surface_input(object)

  mle <- .extract_hurdle_mle(object)
  part2 <- object$param_info$part2 %||% "zhao_exponential"
  equation <- .map_hurdle_equation(part2)
  agg <- .aggregate_positive_log_means(object)

  .make_profile <- function(param_name) {
    log10_seq <- seq(range[1], range[2], length.out = resolution)

    if (param_name == "q0") {
      log10_vals <- log10(mle$Q0) + log10_seq
      ln_q0_vec <- log10_vals * log(10)
      alpha_vec <- rep(mle$alpha, resolution)
    } else {
      log10_vals <- log10(mle$alpha) + log10_seq
      ln_q0_vec <- rep(mle$ln_q0, resolution)
      alpha_vec <- 10^log10_vals
    }

    ssr <- .compute_ssr_grid(
      ln_q0_vec = ln_q0_vec,
      alpha_vec = alpha_vec,
      k = mle$k,
      prices = agg$price,
      observed = agg$mean_log_y,
      equation = equation
    )

    mle_val <- if (param_name == "q0") log10(mle$Q0) else log10(mle$alpha)

    df <- data.frame(log10_val = log10_vals, ssr = ssr)
    df <- df[is.finite(df$ssr), ]

    # Smooth out numerical instability spikes by replacing values that deviate
    # more than 5x MAD from the median with interpolated values
    if (nrow(df) > 5) {
      med_ssr <- stats::median(df$ssr, na.rm = TRUE)
      mad_ssr <- stats::mad(df$ssr, na.rm = TRUE)
      if (mad_ssr > 0) {
        spike <- abs(df$ssr - med_ssr) > 5 * mad_ssr
        if (any(spike) && sum(!spike) >= 2) {
          df$ssr[spike] <- stats::approx(
            x = df$log10_val[!spike],
            y = df$ssr[!spike],
            xout = df$log10_val[spike]
          )$y
        }
        df <- df[is.finite(df$ssr), ]
      }
    }

    x_lab <- if (param_name == "q0") {
      expression(log[10](Q[0]))
    } else {
      expression(log[10](alpha))
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$log10_val, y = .data$ssr)) +
      ggplot2::geom_line(
        color = beezdemand_style_color(style, "primary"),
        linewidth = 0.8
      ) +
      ggplot2::geom_vline(
        xintercept = mle_val,
        linetype = "dashed",
        color = beezdemand_style_color(style, "secondary"),
        linewidth = 0.6
      ) +
      ggplot2::labs(x = x_lab, y = "SSR") +
      theme_beezdemand(style = style)

    p
  }

  if (parameter == "both") {
    p_q0 <- .make_profile("q0") +
      ggplot2::ggtitle("Q0 Profile (alpha fixed at MLE)")
    p_alpha <- .make_profile("alpha") +
      ggplot2::ggtitle("Alpha Profile (Q0 fixed at MLE)")

    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(p_q0 + p_alpha + patchwork::plot_layout(ncol = 2))
    }
    message(
      "Install 'patchwork' to combine profile plots. ",
      "Returning list of individual plots."
    )
    plots <- list(q0 = p_q0, alpha = p_alpha)
    class(plots) <- c("beezdemand_diagnostic_plots", "list")
    return(plots)
  }

  .make_profile(parameter)
}


#' @rdname plot_loss_profile
#' @export
plot_loss_profile.beezdemand_tmb <- function(
    object,
    parameter = c("both", "q0", "alpha"),
    resolution = 200,
    range = c(-3, 3),
    style = c("modern", "apa"),
    ...) {
  parameter <- match.arg(parameter)
  style <- match.arg(style)

  .check_tmb_intercept_only(object)
  .check_loss_surface_input(object)

  mle <- .extract_tmb_mle(object)
  equation <- .map_tmb_equation(object$param_info$equation)
  agg <- .aggregate_tmb_means(object)

  .make_profile <- function(param_name) {
    log10_seq <- seq(range[1], range[2], length.out = resolution)

    if (param_name == "q0") {
      log10_vals <- log10(mle$Q0) + log10_seq
      ln_q0_vec <- log10_vals * log(10)
      alpha_vec <- rep(mle$alpha, resolution)
    } else {
      log10_vals <- log10(mle$alpha) + log10_seq
      ln_q0_vec <- rep(mle$ln_q0, resolution)
      alpha_vec <- 10^log10_vals
    }

    ssr <- .compute_ssr_grid(
      ln_q0_vec = ln_q0_vec,
      alpha_vec = alpha_vec,
      k = mle$k,
      prices = agg$price,
      observed = agg$mean_response,
      equation = equation
    )

    mle_val <- if (param_name == "q0") log10(mle$Q0) else log10(mle$alpha)

    df <- data.frame(log10_val = log10_vals, ssr = ssr)
    df <- df[is.finite(df$ssr), ]

    # Smooth out numerical instability spikes by replacing values that deviate
    # more than 5x MAD from the median with interpolated values
    if (nrow(df) > 5) {
      med_ssr <- stats::median(df$ssr, na.rm = TRUE)
      mad_ssr <- stats::mad(df$ssr, na.rm = TRUE)
      if (mad_ssr > 0) {
        spike <- abs(df$ssr - med_ssr) > 5 * mad_ssr
        if (any(spike) && sum(!spike) >= 2) {
          df$ssr[spike] <- stats::approx(
            x = df$log10_val[!spike],
            y = df$ssr[!spike],
            xout = df$log10_val[spike]
          )$y
        }
        df <- df[is.finite(df$ssr), ]
      }
    }

    x_lab <- if (param_name == "q0") {
      expression(log[10](Q[0]))
    } else {
      expression(log[10](alpha))
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$log10_val, y = .data$ssr)) +
      ggplot2::geom_line(
        color = beezdemand_style_color(style, "primary"),
        linewidth = 0.8
      ) +
      ggplot2::geom_vline(
        xintercept = mle_val,
        linetype = "dashed",
        color = beezdemand_style_color(style, "secondary"),
        linewidth = 0.6
      ) +
      ggplot2::labs(x = x_lab, y = "SSR") +
      theme_beezdemand(style = style)

    p
  }

  if (parameter == "both") {
    p_q0 <- .make_profile("q0") +
      ggplot2::ggtitle("Q0 Profile (alpha fixed at MLE)")
    p_alpha <- .make_profile("alpha") +
      ggplot2::ggtitle("Alpha Profile (Q0 fixed at MLE)")

    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(p_q0 + p_alpha + patchwork::plot_layout(ncol = 2))
    }
    message(
      "Install 'patchwork' to combine profile plots. ",
      "Returning list of individual plots."
    )
    plots <- list(q0 = p_q0, alpha = p_alpha)
    class(plots) <- c("beezdemand_diagnostic_plots", "list")
    return(plots)
  }

  .make_profile(parameter)
}


#' @rdname plot_loss_profile
#' @export
plot_loss_profile.beezdemand_nlme <- function(
    object,
    parameter = c("both", "q0", "alpha"),
    resolution = 200,
    range = c(-3, 3),
    style = c("modern", "apa"),
    type = c("ssr", "marginal"),
    ...) {
  parameter <- match.arg(parameter)
  style <- match.arg(style)
  type <- match.arg(type)

  .check_nlme_intercept_only(object)
  .check_nlme_convergence_for_plot(object)

  mle <- .extract_nlme_mle(object)
  equation_form <- object$formula_details$equation_form_selected

  # Precompute marginal NLL components if needed (once, shared across profiles)
  pre <- if (type == "marginal") .nlme_precompute_marginal(object) else NULL

  # SSR components (only needed for type == "ssr")
  equation <- if (type == "ssr") .map_nlme_equation(equation_form) else NULL
  agg <- if (type == "ssr") .aggregate_nlme_means(object) else NULL

  y_label <- if (type == "marginal") "Marginal NLL" else "SSR"

  .make_profile <- function(param_name) {
    log10_seq <- seq(range[1], range[2], length.out = resolution)

    if (param_name == "q0") {
      log10_vals <- log10(mle$Q0) + log10_seq
    } else {
      log10_vals <- log10(mle$alpha) + log10_seq
    }

    if (type == "marginal") {
      # Marginal NLL: vary one parameter, fix the other at MLE
      if (param_name == "q0") {
        beta_grid <- data.frame(
          log10_q0 = log10_vals,
          log10_alpha = rep(pre$beta_hat["alpha"], resolution)
        )
      } else {
        beta_grid <- data.frame(
          log10_q0 = rep(pre$beta_hat["Q0"], resolution),
          log10_alpha = log10_vals
        )
      }
      values <- .nlme_marginal_nll_grid(pre, beta_grid)
    } else {
      # SSR on aggregated means (original behavior)
      if (param_name == "q0") {
        ln_q0_vec <- log10_vals * log(10)
        alpha_vec <- rep(mle$alpha, resolution)
      } else {
        ln_q0_vec <- rep(mle$ln_q0, resolution)
        alpha_vec <- 10^log10_vals
      }
      values <- .compute_ssr_grid(
        ln_q0_vec = ln_q0_vec,
        alpha_vec = alpha_vec,
        k = mle$k,
        prices = agg$price,
        observed = agg$mean_response,
        equation = equation
      )
    }

    mle_val <- if (param_name == "q0") log10(mle$Q0) else log10(mle$alpha)

    df <- data.frame(log10_val = log10_vals, value = values)
    df <- df[is.finite(df$value), ]

    # Smooth out numerical instability spikes
    if (nrow(df) > 5) {
      med_val <- stats::median(df$value, na.rm = TRUE)
      mad_val <- stats::mad(df$value, na.rm = TRUE)
      if (mad_val > 0) {
        spike <- abs(df$value - med_val) > 5 * mad_val
        if (any(spike) && sum(!spike) >= 2) {
          df$value[spike] <- stats::approx(
            x = df$log10_val[!spike],
            y = df$value[!spike],
            xout = df$log10_val[spike]
          )$y
        }
        df <- df[is.finite(df$value), ]
      }
    }

    x_lab <- if (param_name == "q0") {
      expression(log[10](Q[0]))
    } else {
      expression(log[10](alpha))
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(
      x = .data$log10_val,
      y = .data$value
    )) +
      ggplot2::geom_line(
        color = beezdemand_style_color(style, "primary"),
        linewidth = 0.8
      ) +
      ggplot2::geom_vline(
        xintercept = mle_val,
        linetype = "dashed",
        color = beezdemand_style_color(style, "secondary"),
        linewidth = 0.6
      ) +
      ggplot2::labs(x = x_lab, y = y_label) +
      theme_beezdemand(style = style)

    p
  }

  type_label <- if (type == "marginal") " (Marginal NLL)" else ""

  if (parameter == "both") {
    p_q0 <- .make_profile("q0") +
      ggplot2::ggtitle(paste0("Q0 Profile", type_label,
                              " (alpha fixed at MLE)"))
    p_alpha <- .make_profile("alpha") +
      ggplot2::ggtitle(paste0("Alpha Profile", type_label,
                              " (Q0 fixed at MLE)"))

    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(p_q0 + p_alpha + patchwork::plot_layout(ncol = 2))
    }
    message(
      "Install 'patchwork' to combine profile plots. ",
      "Returning list of individual plots."
    )
    plots <- list(q0 = p_q0, alpha = p_alpha)
    class(plots) <- c("beezdemand_diagnostic_plots", "list")
    return(plots)
  }

  .make_profile(parameter)
}


# =============================================================================
# plot_demand_overlay() — Overlay Multiple Model Fits
# =============================================================================

#' Overlay Demand Curves from Multiple Models
#'
#' @description
#' Plots population-level demand curves from multiple fitted models on the same
#' axes for visual comparison.
#'
#' @param ... Fitted model objects (named or unnamed).
#' @param model_list Optional named list of models (combined with `...`).
#' @param labels Character vector of model labels for the legend.
#' @param prices Numeric vector of prices. Default uses the union of observed
#'   prices across all models.
#' @param n_points Integer; number of points for smooth curves (default 200).
#' @param x_trans Character; x-axis transformation (default `"log10"`).
#' @param free_trans Numeric; replacement for price = 0 on log scales.
#' @param y_min Numeric; minimum consumption value to display. Values below
#'   this floor are dropped to prevent extreme predictions (e.g., 1e-16 from
#'   hurdle models) from compressing the y-axis. Set to `NULL` to disable.
#'   Default is `0.001`.
#' @param inv_fun Function to back-transform consumption values (e.g.,
#'   [ll4_inv] for LL4-transformed models). Default is [identity].
#' @param x_lab,y_lab Axis labels.
#' @param style Character; `"modern"` or `"apa"`.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit1 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' # Compare with a 3-RE model:
#' # fit2 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
#' #   random_effects = c("zeros", "q0", "alpha"))
#' # plot_demand_overlay(fit1, fit2, labels = c("2-RE", "3-RE"))
#' plot_demand_overlay(fit1, labels = c("Hurdle"))
#' }
#'
#' @export
plot_demand_overlay <- function(
    ...,
    model_list = NULL,
    labels = NULL,
    prices = NULL,
    n_points = 200,
    x_trans = c("log10", "log", "linear", "pseudo_log"),
    free_trans = 0.01,
    y_min = 0.001,
    inv_fun = identity,
    x_lab = "Price",
    y_lab = "Consumption",
    style = c("modern", "apa")) {
  x_trans <- match.arg(x_trans)
  style <- match.arg(style)

  resolved <- .resolve_model_list(
    ...,
    model_list = model_list, labels = labels
  )
  models <- resolved$models
  labels <- resolved$labels

  # Determine price range from all models
  if (is.null(prices)) {
    all_prices <- numeric(0)
    for (m in models) {
      if (!is.null(m$data)) {
        x_var <- if (!is.null(m$param_info$x_var)) {
          m$param_info$x_var
        } else {
          "x"
        }
        if (x_var %in% names(m$data)) {
          all_prices <- c(all_prices, m$data[[x_var]])
        }
      }
    }
    if (length(all_prices) == 0) {
      stop("Cannot determine price range. Provide 'prices' explicitly.",
        call. = FALSE
      )
    }
    price_range <- range(all_prices, na.rm = TRUE)
    prices <- seq(
      max(price_range[1], 0),
      price_range[2],
      length.out = n_points
    )
  }

  # Get predictions from each model
  pred_list <- lapply(seq_along(models), function(i) {
    pred <- tryCatch(
      .get_demand_predictions(models[[i]], prices),
      error = function(e) {
        warning(
          "Predictions failed for model '", labels[i], "': ",
          e$message,
          call. = FALSE
        )
        NULL
      }
    )
    if (!is.null(pred)) {
      pred$consumption <- inv_fun(pred$consumption)
      pred$model <- labels[i]
    }
    pred
  })
  pred_all <- do.call(rbind, pred_list[!vapply(pred_list, is.null, logical(1))])

  if (is.null(pred_all) || nrow(pred_all) == 0) {
    stop("No predictions could be generated from the provided models.",
      call. = FALSE
    )
  }

  # Handle log scale
  if (beezdemand_is_log_scale(x_trans) && !is.null(free_trans)) {
    pred_all$price[pred_all$price == 0] <- free_trans
  }
  pred_all <- pred_all[pred_all$consumption > 0 & is.finite(pred_all$consumption), ]

  # Floor consumption to y_min to prevent extreme values (e.g., 1e-16 from
  # hurdle models) from crushing the log-scale axis
  if (!is.null(y_min) && y_min > 0) {
    pred_all <- pred_all[pred_all$consumption >= y_min, ]
  }

  pred_all$model <- factor(pred_all$model, levels = labels)

  # In APA mode, use linetypes in addition to greyscale so overlapping
  # curves remain distinguishable at print resolution
  if (identical(style, "apa") && length(labels) > 1) {
    p <- ggplot2::ggplot(
      pred_all,
      ggplot2::aes(
        x = .data$price,
        y = .data$consumption,
        color = .data$model,
        linetype = .data$model
      )
    ) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::labs(
        x = x_lab,
        y = y_lab,
        color = "Model",
        linetype = "Model",
        title = "Demand Curve Comparison"
      )
  } else {
    p <- ggplot2::ggplot(
      pred_all,
      ggplot2::aes(
        x = .data$price,
        y = .data$consumption,
        color = .data$model
      )
    ) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::labs(
        x = x_lab,
        y = y_lab,
        color = "Model",
        title = "Demand Curve Comparison"
      )
  }

  p <- p +
    ggplot2::scale_x_continuous(
      trans = beezdemand_get_trans(x_trans),
      labels = beezdemand_axis_labels()
    ) +
    ggplot2::scale_y_continuous(
      trans = beezdemand_get_trans("log10"),
      labels = beezdemand_axis_labels()
    ) +
    theme_beezdemand(style = style)

  p <- beezdemand_style_scales(p, style, color = TRUE, fill = FALSE)
  p
}


# =============================================================================
# plot_model_comparison() — Side-by-Side Parameter Comparison
# =============================================================================

#' Compare Parameter Estimates Across Models
#'
#' @description
#' Creates a forest plot (coefficient plot) comparing parameter estimates and
#' confidence intervals across multiple fitted demand models.
#'
#' @param ... Fitted model objects.
#' @param model_list Optional named list of models.
#' @param labels Character vector of model labels.
#' @param parameters Character vector of parameter names to compare
#'   (default `c("Q0", "alpha")`).
#' @param conf_level Numeric; confidence level for intervals (default 0.95).
#' @param style Character; `"modern"` or `"apa"`.
#'
#' @return A ggplot2 object.
#'
#' @details
#' Uses `tidy()` methods to extract parameter estimates and standard errors.
#' Confidence intervals are computed as estimate +/- z * SE. Parameters are
#' matched by the `term` column from `tidy()` output.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' plot_model_comparison(fit, labels = "Hurdle")
#' }
#'
#' @export
plot_model_comparison <- function(
    ...,
    model_list = NULL,
    labels = NULL,
    parameters = c("Q0", "alpha"),
    conf_level = 0.95,
    style = c("modern", "apa")) {
  style <- match.arg(style)

  resolved <- .resolve_model_list(
    ...,
    model_list = model_list, labels = labels
  )
  models <- resolved$models
  labels <- resolved$labels

  z_val <- stats::qnorm(1 - (1 - conf_level) / 2)

  tidy_list <- lapply(seq_along(models), function(i) {
    m <- models[[i]]
    tbl <- tryCatch(
      {
        if (inherits(m, "beezdemand_fixed")) {
          # Aggregate per-subject estimates
          t <- broom::tidy(m)
          agg <- stats::aggregate(
            t[c("estimate", "std.error")],
            by = list(term = t$term),
            FUN = mean,
            na.rm = TRUE
          )
          agg$term_display <- agg$term
          agg
        } else {
          broom::tidy(m)
        }
      },
      error = function(e) {
        warning(
          "tidy() failed for model '", labels[i], "': ", e$message,
          call. = FALSE
        )
        NULL
      }
    )
    if (!is.null(tbl)) {
      # Normalize term names
      term_col <- if ("term_display" %in% names(tbl)) "term_display" else "term"
      tbl$.term_match <- toupper(tbl[[term_col]])
      tbl$model <- labels[i]
    }
    tbl
  })
  tidy_all <- do.call(
    rbind,
    tidy_list[!vapply(tidy_list, is.null, logical(1))]
  )

  if (is.null(tidy_all) || nrow(tidy_all) == 0) {
    stop("No parameter estimates could be extracted.", call. = FALSE)
  }

  # Filter to requested parameters (prefix match for TMB "Q0:(Intercept)" etc.)
  params_upper <- toupper(parameters)
  match_idx <- vapply(tidy_all$.term_match, function(tm) {
    any(vapply(params_upper, function(p) {
      tm == p || startsWith(tm, paste0(p, ":"))
    }, logical(1)))
  }, logical(1))
  tidy_all <- tidy_all[match_idx, ]

  if (nrow(tidy_all) == 0) {
    stop(
      "None of the requested parameters (",
      paste(parameters, collapse = ", "),
      ") found in model output.",
      call. = FALSE
    )
  }

  # Compute confidence intervals
  tidy_all$lower <- tidy_all$estimate - z_val * tidy_all$std.error
  tidy_all$upper <- tidy_all$estimate + z_val * tidy_all$std.error
  term_col <- if ("term_display" %in% names(tidy_all)) "term_display" else "term"
  tidy_all$param <- tidy_all[[term_col]]
  # Normalize parameter names: strip ":(Intercept)" suffix so TMB and hurdle

  # models share the same facet panels for intercept-only comparisons
  tidy_all$param <- sub(":\\(Intercept\\)$", "", tidy_all$param)
  tidy_all$model <- factor(tidy_all$model, levels = labels)

  p <- ggplot2::ggplot(
    tidy_all,
    ggplot2::aes(
      x = .data$estimate,
      y = .data$model,
      xmin = .data$lower,
      xmax = .data$upper,
      color = .data$model
    )
  ) +
    ggplot2::geom_pointrange(size = 0.6) +
    ggplot2::facet_wrap(~param, scales = "free_x") +
    ggplot2::labs(
      x = "Estimate",
      y = NULL,
      color = "Model",
      title = "Parameter Comparison"
    ) +
    theme_beezdemand(style = style)

  p <- beezdemand_style_scales(p, style, color = TRUE, fill = FALSE)
  p
}


# =============================================================================
# plot_re_diagnostics() — Random Effects Diagnostics
# =============================================================================

#' Diagnostic Plots for Random Effects
#'
#' @description
#' Creates diagnostic panels for random effects: histogram and Q-Q plot for
#' each selected random effect, plus (for the zeros RE) a comparison of
#' observed vs predicted proportion of zeros across prices.
#'
#' @param object A fitted model object with random effects.
#' @param which Character; which random effects to diagnose. One of `"all"`
#'   (default), `"zeros"`, `"q0"`, or `"alpha"`.
#' @param style Character; `"modern"` or `"apa"`.
#' @param ... Additional arguments (ignored).
#'
#' @return A ggplot2/patchwork object, or a list of ggplot2 objects if
#'   patchwork is not installed.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' plot_re_diagnostics(fit)
#' plot_re_diagnostics(fit, which = "zeros")
#' }
#'
#' @export
plot_re_diagnostics <- function(object, ...) {
  UseMethod("plot_re_diagnostics")
}


#' @rdname plot_re_diagnostics
#' @export
plot_re_diagnostics.beezdemand_hurdle <- function(
    object,
    which = c("all", "zeros", "q0", "alpha"),
    style = c("modern", "apa"),
    ...) {
  which <- match.arg(which)
  style <- match.arg(style)

  subj_pars <- object$subject_pars
  if (is.null(subj_pars)) {
    stop("No subject-level parameters available.", call. = FALSE)
  }

  # Map RE names to display names and column names
  re_map <- list(
    zeros = list(col = "a_i", label = "Zeros (a_i)"),
    q0 = list(col = "b_i", label = "Q0 (b_i)"),
    alpha = list(col = "c_i", label = "Alpha (c_i)")
  )

  # Select which REs to diagnose
  if (which == "all") {
    selected <- names(re_map)[vapply(
      re_map,
      function(x) x$col %in% names(subj_pars),
      logical(1)
    )]
  } else {
    if (!re_map[[which]]$col %in% names(subj_pars)) {
      stop(
        "Random effect '", which, "' not found in model. ",
        "Available: ",
        paste(
          names(re_map)[vapply(
            re_map,
            function(x) x$col %in% names(subj_pars),
            logical(1)
          )],
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    selected <- which
  }

  plots <- list()

  for (re_name in selected) {
    col_name <- re_map[[re_name]]$col
    label <- re_map[[re_name]]$label
    vals <- subj_pars[[col_name]]
    vals <- vals[is.finite(vals)]

    df <- data.frame(value = vals)

    # Histogram
    p_hist <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value)) +
      ggplot2::geom_histogram(
        bins = 25,
        fill = beezdemand_style_color(style, "primary"),
        color = "white",
        alpha = 0.8
      ) +
      ggplot2::labs(
        x = label,
        y = "Count",
        title = paste0(label, " Distribution")
      ) +
      theme_beezdemand(style = style)

    # QQ plot
    p_qq <- ggplot2::ggplot(df, ggplot2::aes(sample = .data$value)) +
      ggplot2::geom_qq(alpha = 0.6) +
      ggplot2::geom_qq_line(color = "red", linetype = "dashed") +
      ggplot2::labs(
        x = "Theoretical Quantiles",
        y = "Sample Quantiles",
        title = paste0(label, " Q-Q Plot")
      ) +
      theme_beezdemand(style = style)

    plots <- c(plots, list(p_hist, p_qq))
  }

  # Add marginal P(zero) comparison for zeros RE
  if ("zeros" %in% selected) {
    pzero_plot <- .make_pzero_comparison_plot(object, style)
    if (!is.null(pzero_plot)) {
      plots <- c(plots, list(pzero_plot))
    }
  }

  if (requireNamespace("patchwork", quietly = TRUE)) {
    # Use wider layout for "all" panels to prevent cramping (M1 fix);
    # individual RE views (which="zeros"/"q0"/"alpha") are preferred for
    # publication quality
    ncol <- if (which == "all" && length(plots) > 4) 3 else min(2, length(plots))
    return(patchwork::wrap_plots(plots, ncol = ncol))
  }

  message(
    "Install 'patchwork' for combined diagnostic layout. ",
    "Returning list of individual plots."
  )
  class(plots) <- c("beezdemand_diagnostic_plots", "list")
  plots
}


#' @rdname plot_re_diagnostics
#' @export
plot_re_diagnostics.beezdemand_tmb <- function(
    object,
    which = c("all", "q0", "alpha"),
    style = c("modern", "apa"),
    ...) {
  which <- match.arg(which)
  style <- match.arg(style)

  subj_pars <- object$subject_pars
  if (is.null(subj_pars)) {
    stop("No subject-level parameters available.", call. = FALSE)
  }

  # TMB models: b_i (Q0), optionally c_i (alpha) — no zeros/a_i
  re_map <- list(
    q0 = list(col = "b_i", label = "Q0 (b_i)"),
    alpha = list(col = "c_i", label = "Alpha (c_i)")
  )

  if (which == "all") {
    selected <- names(re_map)[vapply(
      re_map,
      function(x) x$col %in% names(subj_pars),
      logical(1)
    )]
  } else {
    if (!re_map[[which]]$col %in% names(subj_pars)) {
      stop(
        "Random effect '", which, "' not found in model. ",
        "Available: ",
        paste(
          names(re_map)[vapply(
            re_map,
            function(x) x$col %in% names(subj_pars),
            logical(1)
          )],
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    selected <- which
  }

  plots <- list()

  for (re_name in selected) {
    col_name <- re_map[[re_name]]$col
    label <- re_map[[re_name]]$label
    vals <- subj_pars[[col_name]]
    vals <- vals[is.finite(vals)]

    df <- data.frame(value = vals)

    p_hist <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value)) +
      ggplot2::geom_histogram(
        bins = 25,
        fill = beezdemand_style_color(style, "primary"),
        color = "white",
        alpha = 0.8
      ) +
      ggplot2::labs(
        x = label,
        y = "Count",
        title = paste0(label, " Distribution")
      ) +
      theme_beezdemand(style = style)

    p_qq <- ggplot2::ggplot(df, ggplot2::aes(sample = .data$value)) +
      ggplot2::geom_qq(alpha = 0.6) +
      ggplot2::geom_qq_line(color = "red", linetype = "dashed") +
      ggplot2::labs(
        x = "Theoretical Quantiles",
        y = "Sample Quantiles",
        title = paste0(label, " Q-Q Plot")
      ) +
      theme_beezdemand(style = style)

    plots <- c(plots, list(p_hist, p_qq))
  }

  if (requireNamespace("patchwork", quietly = TRUE)) {
    ncol <- min(2, length(plots))
    return(patchwork::wrap_plots(plots, ncol = ncol))
  }

  message(
    "Install 'patchwork' for combined diagnostic layout. ",
    "Returning list of individual plots."
  )
  class(plots) <- c("beezdemand_diagnostic_plots", "list")
  plots
}


#' Create observed vs predicted P(zero) comparison plot
#' @noRd
.make_pzero_comparison_plot <- function(object, style) {
  data <- object$data
  x_var <- object$param_info$x_var %||% "x"
  y_var <- object$param_info$y_var %||% "y"

  # Observed proportion of zeros by price
  obs <- stats::aggregate(
    data[[y_var]] == 0,
    by = list(price = data[[x_var]]),
    FUN = mean,
    na.rm = TRUE
  )
  names(obs) <- c("price", "prop_zero")
  obs$source <- "Observed"

  # Model-predicted marginal P(zero) by price
  pred <- tryCatch(
    predict(
      object,
      newdata = stats::setNames(
        data.frame(obs$price),
        x_var
      ),
      type = "probability",
      marginal = TRUE,
      marginal_method = "kde"
    ),
    error = function(e) NULL
  )

  if (is.null(pred)) return(NULL)

  pred_df <- data.frame(
    price = obs$price,
    prop_zero = pred$.fitted,
    source = "Model (marginal)"
  )

  plot_df <- rbind(obs, pred_df)
  plot_df$source <- factor(plot_df$source)

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$price,
      y = .data$prop_zero,
      color = .data$source,
      shape = .data$source
    )
  ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::labs(
      x = "Price",
      y = "P(zero)",
      color = NULL,
      shape = NULL,
      title = "Marginal P(Zero): Observed vs Predicted"
    ) +
    ggplot2::ylim(0, 1) +
    theme_beezdemand(style = style)

  p <- beezdemand_style_scales(p, style, color = TRUE, fill = FALSE)
  p
}


# =============================================================================
# plot_expenditure() — Expenditure Curves
# =============================================================================

#' Plot Expenditure Curves
#'
#' @description
#' Plots the expenditure curve (Price x Consumption) derived from the fitted
#' demand model. Optionally overlays Pmax and Omax reference lines.
#'
#' @param object A fitted model object.
#' @param prices Numeric vector of prices. If `NULL`, uses a smooth grid
#'   spanning the observed price range.
#' @param n_points Integer; number of grid points (default 200).
#' @param show_pmax Logical; show Pmax vertical line (default `TRUE`).
#' @param show_omax Logical; show Omax horizontal line (default `TRUE`).
#' @param x_trans Character; x-axis transformation (default `"log10"`).
#' @param free_trans Numeric; replacement for price = 0 on log scales.
#' @param x_lab,y_lab Axis labels.
#' @param style Character; `"modern"` or `"apa"`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' plot_expenditure(fit)
#' }
#'
#' @export
plot_expenditure <- function(object, ...) {
  UseMethod("plot_expenditure")
}


#' @rdname plot_expenditure
#' @export
plot_expenditure.beezdemand_hurdle <- function(
    object,
    prices = NULL,
    n_points = 200,
    show_pmax = TRUE,
    show_omax = TRUE,
    x_trans = c("log10", "log", "linear", "pseudo_log"),
    free_trans = 0.01,
    x_lab = "Price",
    y_lab = "Expenditure (P \u00d7 Q)",
    style = c("modern", "apa"),
    ...) {
  x_trans <- match.arg(x_trans)
  style <- match.arg(style)

  if (is.null(prices)) {
    x_var <- object$param_info$x_var %||% "x"
    obs_prices <- sort(unique(object$data[[x_var]]))
    prices <- seq(
      max(min(obs_prices), 0.001),
      max(obs_prices),
      length.out = n_points
    )
  }

  pred <- .get_demand_predictions(object, prices)
  pred$expenditure <- pred$price * pred$consumption
  pred <- pred[pred$price > 0 & is.finite(pred$expenditure), ]

  # Handle log scale free_trans
  if (beezdemand_is_log_scale(x_trans) && !is.null(free_trans)) {
    pred$price[pred$price == 0] <- free_trans
  }

  p <- ggplot2::ggplot(
    pred,
    ggplot2::aes(x = .data$price, y = .data$expenditure)
  ) +
    ggplot2::geom_line(
      color = beezdemand_style_color(style, "primary"),
      linewidth = 1
    ) +
    ggplot2::scale_x_continuous(
      trans = beezdemand_get_trans(x_trans),
      labels = beezdemand_axis_labels()
    ) +
    ggplot2::labs(x = x_lab, y = y_lab, title = "Expenditure Curve") +
    theme_beezdemand(style = style)

  # Add Pmax/Omax
  if (show_pmax || show_omax) {
    metrics <- tryCatch(calc_group_metrics(object), error = function(e) NULL)
    if (!is.null(metrics)) {
      if (show_pmax && !is.null(metrics$Pmax) && is.finite(metrics$Pmax)) {
        p <- p + ggplot2::geom_vline(
          xintercept = metrics$Pmax,
          linetype = "dashed",
          color = beezdemand_style_color(style, "secondary"),
          linewidth = 0.6
        ) +
          ggplot2::annotate(
            "text",
            x = metrics$Pmax,
            y = max(pred$expenditure, na.rm = TRUE) * 0.95,
            label = paste0("Pmax = ", round(metrics$Pmax, 2)),
            hjust = -0.1,
            color = beezdemand_style_color(style, "secondary"),
            size = 3
          )
      }
      if (show_omax && !is.null(metrics$Omax) && is.finite(metrics$Omax)) {
        p <- p + ggplot2::geom_hline(
          yintercept = metrics$Omax,
          linetype = "dashed",
          color = beezdemand_style_color(style, "accent"),
          linewidth = 0.6
        ) +
          ggplot2::annotate(
            "text",
            x = max(pred$price) * 0.8,
            y = metrics$Omax,
            label = paste0("Omax = ", round(metrics$Omax, 2)),
            vjust = -0.5,
            color = beezdemand_style_color(style, "accent"),
            size = 3
          )
      }
    }
  }

  p
}


#' @rdname plot_expenditure
#' @export
plot_expenditure.beezdemand_tmb <- function(
    object,
    prices = NULL,
    n_points = 200,
    show_pmax = TRUE,
    show_omax = TRUE,
    x_trans = c("log10", "log", "linear", "pseudo_log"),
    free_trans = 0.01,
    x_lab = "Price",
    y_lab = "Expenditure (P \u00d7 Q)",
    style = c("modern", "apa"),
    ...) {
  x_trans <- match.arg(x_trans)
  style <- match.arg(style)

  if (is.null(prices)) {
    x_var <- object$param_info$x_var %||% "x"
    obs_prices <- sort(unique(object$data[[x_var]]))
    prices <- seq(
      max(min(obs_prices), 0.001),
      max(obs_prices),
      length.out = n_points
    )
  }

  pred <- .get_demand_predictions(object, prices)
  pred$expenditure <- pred$price * pred$consumption
  pred <- pred[pred$price > 0 & is.finite(pred$expenditure), ]

  # Enforce monotonic decline after peak expenditure to prevent rebound
  # artifacts from model extrapolation (TICKET-003 related)
  if (nrow(pred) > 0) {
    peak_idx <- which.max(pred$expenditure)
    if (peak_idx < nrow(pred)) {
      running_min <- pred$expenditure[peak_idx]
      for (i in (peak_idx + 1):nrow(pred)) {
        running_min <- min(running_min, pred$expenditure[i])
        pred$expenditure[i] <- running_min
      }
    }
  }

  if (beezdemand_is_log_scale(x_trans) && !is.null(free_trans)) {
    pred$price[pred$price == 0] <- free_trans
  }

  p <- ggplot2::ggplot(
    pred,
    ggplot2::aes(x = .data$price, y = .data$expenditure)
  ) +
    ggplot2::geom_line(
      color = beezdemand_style_color(style, "primary"),
      linewidth = 1
    ) +
    ggplot2::scale_x_continuous(
      trans = beezdemand_get_trans(x_trans),
      labels = beezdemand_axis_labels()
    ) +
    ggplot2::labs(x = x_lab, y = y_lab, title = "Expenditure Curve") +
    theme_beezdemand(style = style)

  if (show_pmax || show_omax) {
    metrics <- tryCatch(calc_group_metrics(object), error = function(e) NULL)
    if (!is.null(metrics)) {
      if (show_pmax && !is.null(metrics$Pmax) && is.finite(metrics$Pmax)) {
        p <- p + ggplot2::geom_vline(
          xintercept = metrics$Pmax,
          linetype = "dashed",
          color = beezdemand_style_color(style, "secondary"),
          linewidth = 0.6
        ) +
          ggplot2::annotate(
            "text",
            x = metrics$Pmax,
            y = max(pred$expenditure, na.rm = TRUE) * 0.95,
            label = paste0("Pmax = ", round(metrics$Pmax, 2)),
            hjust = -0.1,
            color = beezdemand_style_color(style, "secondary"),
            size = 3
          )
      }
      if (show_omax && !is.null(metrics$Omax) && is.finite(metrics$Omax)) {
        p <- p + ggplot2::geom_hline(
          yintercept = metrics$Omax,
          linetype = "dashed",
          color = beezdemand_style_color(style, "accent"),
          linewidth = 0.6
        ) +
          ggplot2::annotate(
            "text",
            x = max(pred$price) * 0.8,
            y = metrics$Omax,
            label = paste0("Omax = ", round(metrics$Omax, 2)),
            vjust = -0.5,
            color = beezdemand_style_color(style, "accent"),
            size = 3
          )
      }
    }
  }

  p
}


# =============================================================================
# plot_elasticity() — Elasticity Curves
# =============================================================================

#' Plot Own-Price Elasticity Curve
#'
#' @description
#' Computes and plots the own-price point elasticity of demand across prices.
#' Elasticity is computed numerically via central differences on the
#' unconditional demand curve.
#'
#' @inheritParams plot_expenditure
#' @param show_unit Logical; show unit elasticity reference line at -1
#'   (default `TRUE`).
#'
#' @return A ggplot2 object.
#'
#' @details
#' Point elasticity is computed as:
#' \deqn{\eta(P) = \frac{dQ}{dP} \cdot \frac{P}{Q(P)}}
#'
#' This uses the unconditional demand Q(P) for hurdle models (which includes
#' the probability of zero consumption), providing the economically relevant
#' total elasticity.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#' plot_elasticity(fit)
#' }
#'
#' @export
plot_elasticity <- function(object, ...) {
  UseMethod("plot_elasticity")
}


#' @rdname plot_elasticity
#' @export
plot_elasticity.beezdemand_hurdle <- function(
    object,
    prices = NULL,
    n_points = 200,
    show_unit = TRUE,
    x_trans = c("log10", "log", "linear", "pseudo_log"),
    free_trans = 0.01,
    x_lab = "Price",
    y_lab = "Elasticity",
    style = c("modern", "apa"),
    ...) {
  x_trans <- match.arg(x_trans)
  style <- match.arg(style)

  if (is.null(prices)) {
    x_var <- object$param_info$x_var %||% "x"
    obs_prices <- sort(unique(object$data[[x_var]]))
    min_p <- max(min(obs_prices[obs_prices > 0]), 0.001)
    prices <- seq(min_p, max(obs_prices), length.out = n_points)
  }
  # Remove zero prices (elasticity undefined at P=0)
  prices <- prices[prices > 0]

  # Numerical central difference
  h <- prices * 0.001
  h[h < 1e-8] <- 1e-8

  x_var <- object$param_info$x_var %||% "x"
  .pred_at <- function(p) {
    nd <- stats::setNames(data.frame(x = p), x_var)
    pred <- predict(object, newdata = nd, type = "demand")
    as.numeric(pred$.fitted)
  }

  Q <- .pred_at(prices)
  Q_plus <- .pred_at(prices + h)
  Q_minus <- .pred_at(prices - h)

  # Clamp predictions to a meaningful floor before computing derivatives
  # to prevent numerical instability when consumption approaches machine epsilon
  q_floor <- 1e-6
  Q_clamped <- pmax(Q, q_floor)
  Q_plus_clamped <- pmax(Q_plus, q_floor)
  Q_minus_clamped <- pmax(Q_minus, q_floor)

  dQdP <- (Q_plus_clamped - Q_minus_clamped) / (2 * h)
  elasticity <- dQdP * prices / Q_clamped

  # Truncate at prices where consumption has dropped below the floor
  # (elasticity values beyond this point are numerical artifacts)
  meaningful <- Q > q_floor

  df <- data.frame(price = prices, elasticity = elasticity)
  df <- df[meaningful & is.finite(df$elasticity), ]

  if (nrow(df) > 0) {
    # Trim elasticity values beyond 3x the interquartile range
    eq <- stats::quantile(df$elasticity, c(0.25, 0.75), na.rm = TRUE)
    iqr <- diff(eq)
    lower_fence <- eq[1] - 3 * iqr
    df <- df[df$elasticity >= lower_fence, ]
  }

  # Handle log scale
  if (beezdemand_is_log_scale(x_trans) && !is.null(free_trans)) {
    df$price[df$price == 0] <- free_trans
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$price, y = .data$elasticity)
  ) +
    ggplot2::geom_line(
      color = beezdemand_style_color(style, "primary"),
      linewidth = 0.9
    ) +
    ggplot2::scale_x_continuous(
      trans = beezdemand_get_trans(x_trans),
      labels = beezdemand_axis_labels()
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      title = "Elasticity of Demand"
    ) +
    theme_beezdemand(style = style)

  if (show_unit) {
    p <- p + ggplot2::geom_hline(
      yintercept = -1,
      linetype = "dashed",
      color = beezdemand_style_color(style, "secondary"),
      linewidth = 0.5
    ) +
      ggplot2::annotate(
        "text",
        x = max(df$price) * 0.9,
        y = -1,
        label = "Unit elasticity",
        vjust = -0.5,
        color = beezdemand_style_color(style, "secondary"),
        size = 3
      )
  }

  p
}


#' @rdname plot_elasticity
#' @export
plot_elasticity.beezdemand_tmb <- function(
    object,
    prices = NULL,
    n_points = 200,
    show_unit = TRUE,
    x_trans = c("log10", "log", "linear", "pseudo_log"),
    free_trans = 0.01,
    x_lab = "Price",
    y_lab = "Elasticity",
    style = c("modern", "apa"),
    ...) {
  x_trans <- match.arg(x_trans)
  style <- match.arg(style)

  if (is.null(prices)) {
    x_var <- object$param_info$x_var %||% "x"
    obs_prices <- sort(unique(object$data[[x_var]]))
    min_p <- max(min(obs_prices[obs_prices > 0]), 0.001)
    prices <- seq(min_p, max(obs_prices), length.out = n_points)
  }
  prices <- prices[prices > 0]

  # Numerical central difference using predict(type = "demand", scale = "natural")
  h <- prices * 0.001
  h[h < 1e-8] <- 1e-8

  .pred_at <- function(p) {
    pred <- predict(object, type = "demand", prices = p, scale = "natural")
    as.numeric(pred$.fitted)
  }

  Q <- .pred_at(prices)
  Q_plus <- .pred_at(prices + h)
  Q_minus <- .pred_at(prices - h)

  # Clamp predictions to a meaningful floor before computing derivatives
  q_floor <- 1e-6
  Q_clamped <- pmax(Q, q_floor)
  Q_plus_clamped <- pmax(Q_plus, q_floor)
  Q_minus_clamped <- pmax(Q_minus, q_floor)

  dQdP <- (Q_plus_clamped - Q_minus_clamped) / (2 * h)
  elasticity <- dQdP * prices / Q_clamped

  # Truncate at prices where consumption has dropped below the floor
  meaningful <- Q > q_floor

  df <- data.frame(price = prices, elasticity = elasticity)
  df <- df[meaningful & is.finite(df$elasticity), ]

  if (nrow(df) > 0) {
    eq <- stats::quantile(df$elasticity, c(0.25, 0.75), na.rm = TRUE)
    iqr <- diff(eq)
    lower_fence <- eq[1] - 3 * iqr
    df <- df[df$elasticity >= lower_fence, ]
  }

  if (beezdemand_is_log_scale(x_trans) && !is.null(free_trans)) {
    df$price[df$price == 0] <- free_trans
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$price, y = .data$elasticity)
  ) +
    ggplot2::geom_line(
      color = beezdemand_style_color(style, "primary"),
      linewidth = 0.9
    ) +
    ggplot2::scale_x_continuous(
      trans = beezdemand_get_trans(x_trans),
      labels = beezdemand_axis_labels()
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      title = "Elasticity of Demand"
    ) +
    theme_beezdemand(style = style)

  if (show_unit) {
    p <- p + ggplot2::geom_hline(
      yintercept = -1,
      linetype = "dashed",
      color = beezdemand_style_color(style, "secondary"),
      linewidth = 0.5
    ) +
      ggplot2::annotate(
        "text",
        x = max(df$price) * 0.9,
        y = -1,
        label = "Unit elasticity",
        vjust = -0.5,
        color = beezdemand_style_color(style, "secondary"),
        size = 3
      )
  }

  p
}


# =============================================================================
# plot_alpha_distribution() — Subject Alpha Distribution
# =============================================================================

#' Plot Distribution of Subject-Level Alpha Estimates
#'
#' @description
#' Visualizes the distribution of individual-level alpha (elasticity) estimates
#' from a fitted hurdle model, with the population MLE as a reference.
#'
#' @param object A fitted model object with subject-level parameters.
#' @param type Character; `"density"` (default) or `"histogram"`.
#' @param log_scale Logical; plot on log10 scale (default `TRUE`).
#' @param bins Integer; histogram bins (default 30).
#' @param style Character; `"modern"` or `"apa"`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' data(apt)
#' fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
#'   random_effects = c("zeros", "q0", "alpha"))
#' plot_alpha_distribution(fit)
#' }
#'
#' @export
plot_alpha_distribution <- function(object, ...) {
  UseMethod("plot_alpha_distribution")
}


#' @rdname plot_alpha_distribution
#' @export
plot_alpha_distribution.beezdemand_hurdle <- function(
    object,
    type = c("density", "histogram"),
    log_scale = TRUE,
    bins = 30,
    style = c("modern", "apa"),
    ...) {
  type <- match.arg(type)
  style <- match.arg(style)

  subj_pars <- object$subject_pars
  if (is.null(subj_pars) || !"alpha" %in% names(subj_pars)) {
    stop("No subject-level alpha estimates available.", call. = FALSE)
  }

  alpha_vals <- subj_pars$alpha
  alpha_vals <- alpha_vals[is.finite(alpha_vals) & alpha_vals > 0]

  if (length(alpha_vals) < 2) {
    stop("Need at least 2 valid alpha estimates for a distribution plot.",
      call. = FALSE
    )
  }

  df <- data.frame(alpha_plot = if (log_scale) log10(alpha_vals) else alpha_vals)
  x_label <- if (log_scale) {
    expression(log[10](alpha))
  } else {
    expression(alpha)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$alpha_plot))

  if (type == "histogram") {
    p <- p + ggplot2::geom_histogram(
      bins = bins,
      fill = beezdemand_style_color(style, "primary"),
      color = "white",
      alpha = 0.8
    )
  } else {
    p <- p + ggplot2::geom_density(
      fill = beezdemand_style_color(style, "primary"),
      alpha = 0.4,
      color = beezdemand_style_color(style, "primary")
    )
  }

  # Population MLE reference line
  mle <- .extract_hurdle_mle(object)
  mle_alpha <- if (log_scale) log10(mle$alpha) else mle$alpha

  p <- p +
    ggplot2::geom_vline(
      xintercept = mle_alpha,
      linetype = "dashed",
      color = beezdemand_style_color(style, "secondary"),
      linewidth = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x = mle_alpha,
      y = Inf,
      label = "Population MLE",
      hjust = -0.1,
      vjust = 1.5,
      color = beezdemand_style_color(style, "secondary"),
      size = 3
    ) +
    ggplot2::labs(
      x = x_label,
      y = if (type == "histogram") "Count" else "Density",
      title = "Distribution of Subject-Level Alpha"
    ) +
    theme_beezdemand(style = style)

  p
}


#' @rdname plot_alpha_distribution
#' @export
plot_alpha_distribution.beezdemand_tmb <- function(
    object,
    type = c("density", "histogram"),
    log_scale = TRUE,
    bins = 30,
    style = c("modern", "apa"),
    ...) {
  type <- match.arg(type)
  style <- match.arg(style)

  subj_pars <- object$subject_pars
  if (is.null(subj_pars) || !"alpha" %in% names(subj_pars)) {
    stop("No subject-level alpha estimates available.", call. = FALSE)
  }

  alpha_vals <- subj_pars$alpha
  alpha_vals <- alpha_vals[is.finite(alpha_vals) & alpha_vals > 0]

  if (length(alpha_vals) < 2) {
    stop("Need at least 2 valid alpha estimates for a distribution plot.",
      call. = FALSE
    )
  }

  df <- data.frame(alpha_plot = if (log_scale) log10(alpha_vals) else alpha_vals)
  x_label <- if (log_scale) {
    expression(log[10](alpha))
  } else {
    expression(alpha)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$alpha_plot))

  if (type == "histogram") {
    p <- p + ggplot2::geom_histogram(
      bins = bins,
      fill = beezdemand_style_color(style, "primary"),
      color = "white",
      alpha = 0.8
    )
  } else {
    p <- p + ggplot2::geom_density(
      fill = beezdemand_style_color(style, "primary"),
      alpha = 0.4,
      color = beezdemand_style_color(style, "primary")
    )
  }

  # Population MLE reference line
  mle <- .extract_tmb_mle(object)
  mle_alpha <- if (log_scale) log10(mle$alpha) else mle$alpha

  p <- p +
    ggplot2::geom_vline(
      xintercept = mle_alpha,
      linetype = "dashed",
      color = beezdemand_style_color(style, "secondary"),
      linewidth = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x = mle_alpha,
      y = Inf,
      label = "Population MLE",
      hjust = -0.1,
      vjust = 1.5,
      color = beezdemand_style_color(style, "secondary"),
      size = 3
    ) +
    ggplot2::labs(
      x = x_label,
      y = if (type == "histogram") "Count" else "Density",
      title = "Distribution of Subject-Level Alpha"
    ) +
    theme_beezdemand(style = style)

  p
}
