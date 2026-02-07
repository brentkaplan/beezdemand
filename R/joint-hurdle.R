#' @title Joint Hurdle Cross-Price Demand Model
#' @description Functions for fitting a joint three-part hurdle model that
#'   estimates zeros, target demand (alone and own), and cross-price (alt)
#'   consumption within a single likelihood.
#' @name joint-hurdle
NULL

#' Prepare Data for Joint Hurdle Model
#'
#' @description Transforms data from standard cross-price format to the
#'   long format required by the joint hurdle model.
#'
#' @param data A data frame containing cross-price demand data.
#' @param y_var Character. Name of the consumption variable.
#' @param price_var Character. Name of the target price variable.
#' @param id_var Character. Name of the subject identifier variable.
#' @param target_var Character. Name of the target/condition variable.
#'   Expected values: "alone", "own", "alt" (or similar).
#' @param commodity_var Character or NULL. Name of commodity variable if needed
#'   to distinguish target vs alternative commodity.
#' @param alone_value Character. Value in target_var indicating alone condition.
#' @param own_value Character. Value in target_var indicating own condition.
#' @param alt_value Character. Value in target_var indicating alt condition.
#'
#' @return A list with:
#'   \item{data}{Prepared data frame with stream column}
#'   \item{stream_counts}{Named vector of observation counts per stream}
#'   \item{subject_map}{Mapping from original IDs to 0-based indices}
#'   \item{warnings}{Character vector of any warnings generated}
#'
#' @keywords internal
prepare_joint_data <- function(
  data,
  y_var = "y",
  price_var = "x",
  id_var = "id",
  target_var = "target",
  commodity_var = NULL,
  alone_value = "alone",
  own_value = "own",
  alt_value = "alt"
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_cols <- c(y_var, price_var, id_var, target_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  warnings_list <- character(0)

  # Extract columns
  y <- data[[y_var]]
  price_T <- data[[price_var]]
  id <- data[[id_var]]
  target <- data[[target_var]]

  # Validate numeric columns

  if (!is.numeric(y)) {
    stop("y_var must be numeric")
  }
  if (!is.numeric(price_T)) {
    stop("price_var must be numeric")
  }
  if (any(y < 0, na.rm = TRUE)) {
    stop("Consumption (y) cannot be negative")
  }
  if (any(price_T < 0, na.rm = TRUE)) {
    stop("Price cannot be negative")
  }

  # Create stream variable
  # stream: 0 = alone.target, 1 = own.target, 2 = own.alt
  stream <- rep(NA_integer_, nrow(data))

  # Handle different target values
  is_alone <- target == alone_value
  is_own <- target == own_value
  is_alt <- target == alt_value

  # If commodity_var is provided, use it to distinguish
  if (!is.null(commodity_var)) {
    if (!(commodity_var %in% names(data))) {
      stop("commodity_var '", commodity_var, "' not found in data")
    }
    commodity <- data[[commodity_var]]
    # Assume "target" commodity for alone/own, "alt" commodity for alt
    stream[is_alone] <- 0L # alone.target
    stream[is_own] <- 1L # own.target (assuming target commodity)
    stream[is_alt] <- 2L # own.alt
  } else {
    # Simple mapping based on target_var only
    stream[is_alone] <- 0L
    stream[is_own] <- 1L
    stream[is_alt] <- 2L
  }

  # Check for unmapped observations
  unmapped <- is.na(stream)
  if (any(unmapped)) {
    unmapped_values <- unique(target[unmapped])
    stop(
      "Unrecognized target values: ",
      paste(unmapped_values, collapse = ", "),
      "\nExpected: '",
      alone_value,
      "', '",
      own_value,
      "', '",
      alt_value,
      "'"
    )
  }

  # Count observations per stream
  stream_counts <- c(
    alone.target = sum(stream == 0),
    own.target = sum(stream == 1),
    own.alt = sum(stream == 2)
  )

  # Check for missing streams (with warnings per spec)
  if (stream_counts["alone.target"] == 0) {
    warnings_list <- c(
      warnings_list,
      "alone.target not observed; baseline demand inferred indirectly"
    )
  }
  if (stream_counts["own.target"] == 0) {
    warnings_list <- c(
      warnings_list,
      "own.target not observed; own-condition demand not estimable"
    )
  }
  if (stream_counts["own.alt"] == 0) {
    warnings_list <- c(
      warnings_list,
      "own.alt not observed; cross-price parameters not estimable"
    )
  }

  # Create subject mapping (0-based for TMB)
  unique_ids <- unique(id)
  n_subjects <- length(unique_ids)
  subject_map <- stats::setNames(seq_along(unique_ids) - 1L, unique_ids)
  subject_id <- as.integer(subject_map[as.character(id)])

  # Edge case: single subject
  if (n_subjects == 1) {
    warnings_list <- c(
      warnings_list,
      "Single subject detected; random effects will shrink toward zero. Population-level inference is limited."
    )
  }

  # Edge case: check for constant price
  if (length(unique(price_T)) == 1) {
    warnings_list <- c(
      warnings_list,
      "Constant target price detected; alpha parameters not identifiable"
    )
  }

  # Edge case: check for all zeros in any stream
  for (s in 0:2) {
    stream_name <- c("alone.target", "own.target", "own.alt")[s + 1]
    stream_y <- y[stream == s]
    if (length(stream_y) > 0) {
      if (all(stream_y == 0)) {
        warnings_list <- c(
          warnings_list,
          paste0(
            "All zeros in ",
            stream_name,
            "; positive-consumption parameters for this stream not identifiable"
          )
        )
      }
      if (all(stream_y > 0)) {
        warnings_list <- c(
          warnings_list,
          paste0(
            "No zeros in ",
            stream_name,
            "; hurdle informed by other streams"
          )
        )
      }
    }
  }

  # Build output data frame
  out_data <- data.frame(
    id = id,
    subject_id = subject_id,
    price_T = price_T,
    y = y,
    stream = stream,
    stringsAsFactors = FALSE
  )

  # Emit warnings
  for (w in warnings_list) {
    warning(w, call. = FALSE)
  }

  list(
    data = out_data,
    stream_counts = stream_counts,
    n_subjects = n_subjects,
    subject_map = subject_map,
    warnings = warnings_list
  )
}


#' Fit Joint Hurdle Cross-Price Demand Model
#'
#' @description Fits a joint three-part hurdle model that estimates:
#'   \itemize{
#'     \item Part I: Shared zero-inflation process across all streams
#'     \item Part II (alone.target): Target demand when alternative absent
#'     \item Part II (own.target): Target demand when alternative present
#'     \item Part II (own.alt): Cross-price demand for alternative
#'   }
#'
#' @details
#' **Important:** The target price (\code{price_var}) is the predictor for
#' \strong{all} streams. Alternative price is assumed fixed and does not
#' enter the model as a predictor.
#'
#' @param data A data frame in cross-price format, or output from
#'   \code{prepare_joint_data()}.
#' @param y_var Character. Name of consumption variable.
#' @param price_var Character. Name of target price variable.
#' @param id_var Character. Name of subject identifier.
#' @param target_var Character. Name of target/condition variable.
#' @param alone_value,own_value,alt_value Character. Values in target_var
#'   identifying each condition.
#' @param joint_type Character. Model variant to fit:
#'   \itemize{
#'     \item "saturated": Stream-specific random effects (Variant 1)
#'     \item "latent": Latent-trait model with shared structure (Variant 2)
#'   }
#' @param k Numeric or `"estimate"`. If numeric, k is fixed at this value.
#'   If `"estimate"`, k is estimated as a free parameter (use with caution due
#'   to potential identifiability issues with alpha). Default is `2`.
#' @param epsilon Numeric. Small constant for log(price + epsilon).
#' @param start Named list. Starting values for parameters. If NULL, defaults
#'   are computed from data.
#' @param control List. Control parameters for \code{nlminb}.
#' @param verbose Integer. 0 = silent, 1 = progress, 2 = detailed.
#'
#' @return An object of class \code{beezdemand_joint_hurdle} containing:
#'   \item{coefficients}{Named vector of fixed effects}
#'   \item{random_effects}{Matrix of subject-level random effects}
#'   \item{vcov}{Variance-covariance matrix}
#'   \item{logLik}{Log-likelihood at convergence}
#'   \item{convergence}{Convergence status}
#'   \item{data}{Prepared data used for fitting}
#'   \item{call}{The matched call}
#'
#' @keywords internal
fit_joint_hurdle <- function(
  data,
  y_var = "y",
  price_var = "x",
  id_var = "id",
  target_var = "target",
  alone_value = "alone",
  own_value = "own",
  alt_value = "alt",
  joint_type = c("saturated", "latent"),
  k = 2,
  epsilon = 0.001,
  start = NULL,
  control = list(eval.max = 1000, iter.max = 1000),
  verbose = 1
) {
  joint_type <- match.arg(joint_type)
  call <- match.call()

 # =========================================================================
  # Validate k parameter
  # =========================================================================
  if (is.null(k)) {
    stop(
      "`k` cannot be NULL. Use a numeric value to fix k, or \"estimate\" to ",
      "estimate k as a free parameter.",
      call. = FALSE
    )
  }

  if (is.character(k)) {
    if (!identical(k, "estimate")) {
      stop(
        "`k` must be a numeric value or \"estimate\", not \"", k, "\".",
        call. = FALSE
      )
    }
    k_fixed <- FALSE
    k_start <- 2 # Starting value for optimization
    warning(
      "Estimating k as a free parameter. This may cause identifiability ",
      "issues with alpha parameters. Consider fixing k unless you have ",
      "strong theoretical justification.",
      call. = FALSE
    )
  } else if (is.numeric(k) && length(k) == 1) {
    k_fixed <- TRUE
    k_start <- k
  } else {
    stop(
      "`k` must be a single numeric value or \"estimate\".",
      call. = FALSE
    )
  }

  # =========================================================================
  # Prepare data
  # =========================================================================
  if (is.list(data) && "stream_counts" %in% names(data)) {
    # Already prepared
    prep <- data
  } else {
    prep <- prepare_joint_data(
      data = data,
      y_var = y_var,
      price_var = price_var,
      id_var = id_var,
      target_var = target_var,
      alone_value = alone_value,
      own_value = own_value,
      alt_value = alt_value
    )
  }

  dat <- prep$data
  n_subjects <- prep$n_subjects
  stream_counts <- prep$stream_counts

  if (verbose >= 1) {
    cat("Fitting joint hurdle model...\n")
    cat("  Variant:", joint_type, "\n")
    cat("  Subjects:", n_subjects, "\n")
    cat("  Observations by stream:\n")
    cat("    alone.target:", stream_counts["alone.target"], "\n")
    cat("    own.target:", stream_counts["own.target"], "\n")
    cat("    own.alt:", stream_counts["own.alt"], "\n")
  }

  # =========================================================================
  # Prepare TMB data and parameters (variant-specific)
  # =========================================================================
  if (joint_type == "saturated") {
    result <- fit_joint_saturated(
      dat,
      prep,
      n_subjects,
      stream_counts,
      k_start,
      k_fixed,
      start,
      epsilon,
      control,
      verbose,
      call
    )
  } else {
    result <- fit_joint_latent(
      dat,
      prep,
      n_subjects,
      stream_counts,
      k_start,
      k_fixed,
      start,
      epsilon,
      control,
      verbose,
      call
    )
  }

  result$joint_type <- joint_type
  result
}


#' Fit Saturated Joint Hurdle Model (Internal)
#' @keywords internal
fit_joint_saturated <- function(
  dat,
  prep,
  n_subjects,
  stream_counts,
  k_start,
  k_fixed,
  start,
  epsilon,
  control,
  verbose,
  call
) {
  # Compute starting values for saturated model
  if (is.null(start)) {
    start <- compute_joint_start_values_saturated(
      dat,
      k_start,
      epsilon,
      stream_counts
    )
  }

  if (k_fixed) {
    start$k <- k_start
  }

  tmb_data <- list(
    model = "JointHurdleSaturated",
    price_T = dat$price_T,
    y = dat$y,
    stream = dat$stream,
    subject_id = dat$subject_id,
    n_subjects = n_subjects,
    epsilon = epsilon,
    has_alone_target = as.integer(stream_counts["alone.target"] > 0),
    has_own_target = as.integer(stream_counts["own.target"] > 0),
    has_own_alt = as.integer(stream_counts["own.alt"] > 0)
  )

  # =========================================================================
  # Prepare TMB parameters
  # =========================================================================
  tmb_pars <- list(
    # Part I (hurdle)
    gamma0 = start$gamma0,
    gamma_own_target = start$gamma_own_target,
    gamma_own_alt = start$gamma_own_alt,
    gamma1 = start$gamma1,
    # Part II (target demand)
    logQ0_AT = start$logQ0_AT,
    alpha_AT = start$alpha_AT,
    logQ0_OT = start$logQ0_OT,
    alpha_OT = start$alpha_OT,
    k = start$k,
    # Part II (cross-price)
    logQalone_OA = start$logQalone_OA,
    I = start$I,
    log_beta = start$log_beta,
    # Variance parameters
    logsigma_a = start$logsigma_a,
    logsigma_b_AT = start$logsigma_b_AT,
    logsigma_b_OT = start$logsigma_b_OT,
    logsigma_b_OA = start$logsigma_b_OA,
    logsigma_e = start$logsigma_e,
    rho_AT_OT_raw = start$rho_AT_OT_raw,
    rho_AT_OA_raw = start$rho_AT_OA_raw,
    rho_OT_OA_raw = start$rho_OT_OA_raw,
    # Random effects (standardized)
    u = matrix(0, nrow = n_subjects, ncol = 4)
  )

  # =========================================================================
  # Build TMB objective
  # =========================================================================
  random_pars <- "u"
  map <- list()

  # Fix k if specified
  if (k_fixed) {
    map$k <- factor(NA)
  }

  obj <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_pars,
    random = random_pars,
    map = map,
    DLL = "beezdemand",
    silent = verbose < 2
  )

  # =========================================================================
  # Optimize
  # =========================================================================
  if (verbose >= 1) {
    cat("Optimizing...\n")
  }

  opt_warnings <- character(0)
  opt <- withCallingHandlers(
    stats::nlminb(
      start = obj$par,
      objective = obj$fn,
      gradient = obj$gr,
      control = control
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      opt_warnings <<- c(opt_warnings, msg)
      if (grepl("NA/NaN function evaluation|non-finite value supplied", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  if (verbose >= 1) {
    if (opt$convergence == 0) {
      cat("Converged successfully.\n")
    } else {
      cat(
        "Warning: Optimization did not converge (code:",
        opt$convergence,
        ")\n"
      )
    }
  }

  # =========================================================================
  # Extract results
  # =========================================================================
  # Standard errors via sdreport
  sdr <- TMB::sdreport(obj)

  # Fixed effects
  par_names <- names(obj$par)
  coefficients <- opt$par
  names(coefficients) <- par_names

  # Transform back to natural scale for reporting
  coefficients_natural <- coefficients
  coefficients_natural["beta"] <- exp(coefficients["log_beta"])

  # Random effects
  re_full <- obj$env$last.par.best
  u_idx <- which(names(re_full) == "u")
  u_matrix <- matrix(re_full[u_idx], nrow = n_subjects, ncol = 4)
  colnames(u_matrix) <- c("a", "b_AT", "b_OT", "b_OA")
  rownames(u_matrix) <- names(prep$subject_map)

  # Compute actual random effects (scaled)
  sigma_a <- exp(coefficients["logsigma_a"])
  sigma_b_AT <- exp(coefficients["logsigma_b_AT"])
  sigma_b_OT <- exp(coefficients["logsigma_b_OT"])
  sigma_b_OA <- exp(coefficients["logsigma_b_OA"])

  # Build correlation matrix for intensity REs
  rho_AT_OT <- tanh(coefficients["rho_AT_OT_raw"])
  rho_AT_OA <- tanh(coefficients["rho_AT_OA_raw"])
  rho_OT_OA <- tanh(coefficients["rho_OT_OA_raw"])

  Sigma_b <- matrix(0, 3, 3)
  Sigma_b[1, 1] <- sigma_b_AT^2
  Sigma_b[2, 2] <- sigma_b_OT^2
  Sigma_b[3, 3] <- sigma_b_OA^2
  Sigma_b[1, 2] <- Sigma_b[2, 1] <- sigma_b_AT * sigma_b_OT * rho_AT_OT
  Sigma_b[1, 3] <- Sigma_b[3, 1] <- sigma_b_AT * sigma_b_OA * rho_AT_OA
  Sigma_b[2, 3] <- Sigma_b[3, 2] <- sigma_b_OT * sigma_b_OA * rho_OT_OA

  L_b <- t(chol(Sigma_b))

  # Transform to actual scale
  random_effects <- matrix(0, nrow = n_subjects, ncol = 4)
  random_effects[, 1] <- sigma_a * u_matrix[, 1]
  for (i in seq_len(n_subjects)) {
    random_effects[i, 2:4] <- L_b %*% u_matrix[i, 2:4]
  }
  colnames(random_effects) <- c("a_i", "b_AT_i", "b_OT_i", "b_OA_i")
  rownames(random_effects) <- names(prep$subject_map)

  # Log-likelihood
  logLik <- -opt$objective

  # =========================================================================
  # Build output object
  # =========================================================================
  result <- list(
    coefficients = coefficients,
    coefficients_natural = c(
      coefficients[!names(coefficients) %in% c("log_beta")],
      beta = unname(exp(coefficients["log_beta"]))
    ),
    random_effects = random_effects,
    random_effects_standardized = u_matrix,
    vcov = if (!is.null(sdr$cov.fixed)) sdr$cov.fixed else NULL,
    logLik = logLik,
    AIC = -2 * logLik + 2 * length(opt$par),
    BIC = -2 * logLik + log(nrow(dat)) * length(opt$par),
    convergence = opt$convergence,
    message = opt$message,
    n_obs = nrow(dat),
    n_subjects = n_subjects,
    stream_counts = stream_counts,
    k_fixed = k_fixed,
    k_value = if (k_fixed) k_start else coefficients["k"],
    epsilon = epsilon,
    data = dat,
    subject_map = prep$subject_map,
    tmb_obj = obj,
    tmb_opt = opt,
    tmb_sdr = sdr,
    call = call,
    warnings = prep$warnings,
    optimizer_warnings = unique(opt_warnings)
  )

  class(result) <- "beezdemand_joint_hurdle"
  result
}


#' Compute Starting Values for Saturated Joint Hurdle Model
#'
#' @param dat Prepared data frame
#' @param k_start Starting value for k
#' @param epsilon Epsilon value
#' @param stream_counts Named vector of stream counts
#' @return Named list of starting values
#' @keywords internal
compute_joint_start_values_saturated <- function(
  dat,
  k_start,
  epsilon,
  stream_counts
) {
  # Separate by stream
  alone_dat <- dat[dat$stream == 0, ]
  own_dat <- dat[dat$stream == 1, ]
  alt_dat <- dat[dat$stream == 2, ]

  # Default starting values
  start <- list(
    # Part I
    gamma0 = 0,
    gamma_own_target = 0,
    gamma_own_alt = 0,
    gamma1 = -0.5,
    # Part II (target demand)
    logQ0_AT = 2,
    alpha_AT = 0.01,
    logQ0_OT = 2,
    alpha_OT = 0.01,
    k = k_start,
    # Part II (cross-price)
    logQalone_OA = 1,
    I = -0.5,
    log_beta = log(0.1),
    # Variance parameters
    logsigma_a = log(1),
    logsigma_b_AT = log(0.5),
    logsigma_b_OT = log(0.5),
    logsigma_b_OA = log(0.5),
    logsigma_e = log(0.5),
    rho_AT_OT_raw = 0,
    rho_AT_OA_raw = 0,
    rho_OT_OA_raw = 0
  )

  # Estimate from data if available
  # alone.target
  if (nrow(alone_dat) > 0) {
    pos_alone <- alone_dat[alone_dat$y > 0, ]
    if (nrow(pos_alone) > 0) {
      start$logQ0_AT <- log(max(pos_alone$y, na.rm = TRUE))
    }
  }

  # own.target
  if (nrow(own_dat) > 0) {
    pos_own <- own_dat[own_dat$y > 0, ]
    if (nrow(pos_own) > 0) {
      start$logQ0_OT <- log(max(pos_own$y, na.rm = TRUE))
    }
  }

  # own.alt
  if (nrow(alt_dat) > 0) {
    pos_alt <- alt_dat[alt_dat$y > 0, ]
    if (nrow(pos_alt) > 0) {
      start$logQalone_OA <- log(mean(pos_alt$y, na.rm = TRUE))
      # Estimate I from slope
      if (nrow(pos_alt) > 2) {
        low_price <- pos_alt[pos_alt$price_T <= median(pos_alt$price_T), ]
        high_price <- pos_alt[pos_alt$price_T > median(pos_alt$price_T), ]
        if (nrow(low_price) > 0 && nrow(high_price) > 0) {
          diff_y <- mean(log(high_price$y + 0.01)) -
            mean(log(low_price$y + 0.01))
          start$I <- diff_y # Negative if substitute, positive if complement
        }
      }
    }
  }

  # Estimate hurdle parameters from zero proportions
  zero_prop <- mean(dat$y == 0)
  if (zero_prop > 0 && zero_prop < 1) {
    start$gamma0 <- log(zero_prop / (1 - zero_prop))
  }

  start
}


#' Fit Latent-Trait Joint Hurdle Model (Internal)
#' @keywords internal
fit_joint_latent <- function(
  dat,
  prep,
  n_subjects,
  stream_counts,
  k_start,
  k_fixed,
  start,
  epsilon,
  control,
  verbose,
  call
) {
  # Compute starting values for latent model
  if (is.null(start)) {
    start <- compute_joint_start_values_latent(dat, k_start, epsilon, stream_counts)
  }

  if (k_fixed) {
    start$k <- k_start
  }

  tmb_data <- list(
    model = "JointHurdleLatent",
    price_T = dat$price_T,
    y = dat$y,
    stream = dat$stream,
    subject_id = dat$subject_id,
    n_subjects = n_subjects,
    epsilon = epsilon,
    has_alone_target = as.integer(stream_counts["alone.target"] > 0),
    has_own_target = as.integer(stream_counts["own.target"] > 0),
    has_own_alt = as.integer(stream_counts["own.alt"] > 0)
  )

  # =========================================================================
  # Prepare TMB parameters for latent model
  # =========================================================================
  tmb_pars <- list(
    # Part I (hurdle)
    gamma0 = start$gamma0,
    gamma_own_target = start$gamma_own_target,
    gamma_own_alt = start$gamma_own_alt,
    gamma1 = start$gamma1,
    # Part II fixed effects (population means on log scale)
    theta_Q0_AT = start$theta_Q0_AT,
    theta_alpha_AT = start$theta_alpha_AT,
    theta_Q0_OT = start$theta_Q0_OT,
    theta_alpha_OT = start$theta_alpha_OT,
    theta_Qalone_OA = start$theta_Qalone_OA,
    I = start$I,
    log_beta = start$log_beta,
    k = start$k,
    # Latent trait loadings
    lambda_sub_q0 = start$lambda_sub_q0,
    lambda_sub_alpha = start$lambda_sub_alpha,
    lambda_sub_alt = start$lambda_sub_alt,
    # Latent trait variances (log scale)
    logsigma_buy = start$logsigma_buy,
    logsigma_val = start$logsigma_val,
    logsigma_sens = start$logsigma_sens,
    logsigma_sub = start$logsigma_sub,
    # Latent trait correlations (raw scale)
    rho_buy_val_raw = start$rho_buy_val_raw,
    rho_buy_sens_raw = start$rho_buy_sens_raw,
    rho_buy_sub_raw = start$rho_buy_sub_raw,
    rho_val_sens_raw = start$rho_val_sens_raw,
    rho_val_sub_raw = start$rho_val_sub_raw,
    rho_sens_sub_raw = start$rho_sens_sub_raw,
    # Residual variance
    logsigma_e = start$logsigma_e,
    # Latent traits (standardized)
    u = matrix(0, nrow = n_subjects, ncol = 4)
  )

  # =========================================================================
  # Build TMB objective
  # =========================================================================
  random_pars <- "u"
  map <- list()

  # Fix k if specified
  if (k_fixed) {
    map$k <- factor(NA)
  }

  obj <- TMB::MakeADFun(
    data = tmb_data,
    parameters = tmb_pars,
    random = random_pars,
    map = map,
    DLL = "beezdemand",
    silent = verbose < 2
  )

  # =========================================================================
  # Optimize
  # =========================================================================
  if (verbose >= 1) {
    cat("Optimizing...\n")
  }

  opt_warnings <- character(0)
  opt <- withCallingHandlers(
    stats::nlminb(
      start = obj$par,
      objective = obj$fn,
      gradient = obj$gr,
      control = control
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      opt_warnings <<- c(opt_warnings, msg)
      if (grepl("NA/NaN function evaluation|non-finite value supplied", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  if (verbose >= 1) {
    if (opt$convergence == 0) {
      cat("Converged successfully.\n")
    } else {
      cat(
        "Warning: Optimization did not converge (code:",
        opt$convergence,
        ")\n"
      )
    }
  }

  # =========================================================================
  # Extract results
  # =========================================================================
  sdr <- TMB::sdreport(obj)

  # Fixed effects
  par_names <- names(obj$par)
  coefficients <- opt$par
  names(coefficients) <- par_names

  # Transform back to natural scale for reporting
  coefficients_natural <- coefficients
  coefficients_natural["beta"] <- exp(coefficients["log_beta"])

  # Latent traits (standardized)
  re_full <- obj$env$last.par.best
  u_idx <- which(names(re_full) == "u")
  u_matrix <- matrix(re_full[u_idx], nrow = n_subjects, ncol = 4)
  colnames(u_matrix) <- c("u_buy", "u_val", "u_sens", "u_sub")
  rownames(u_matrix) <- names(prep$subject_map)

  # Compute actual latent traits (scaled via Cholesky)
  sigma_buy <- exp(coefficients["logsigma_buy"])
  sigma_val <- exp(coefficients["logsigma_val"])
  sigma_sens <- exp(coefficients["logsigma_sens"])
  sigma_sub <- exp(coefficients["logsigma_sub"])

  # Build 4x4 covariance matrix
  rho_buy_val <- tanh(coefficients["rho_buy_val_raw"])
  rho_buy_sens <- tanh(coefficients["rho_buy_sens_raw"])
  rho_buy_sub <- tanh(coefficients["rho_buy_sub_raw"])
  rho_val_sens <- tanh(coefficients["rho_val_sens_raw"])
  rho_val_sub <- tanh(coefficients["rho_val_sub_raw"])
  rho_sens_sub <- tanh(coefficients["rho_sens_sub_raw"])

  Sigma <- matrix(0, 4, 4)
  Sigma[1, 1] <- sigma_buy^2
  Sigma[2, 2] <- sigma_val^2
  Sigma[3, 3] <- sigma_sens^2
  Sigma[4, 4] <- sigma_sub^2
  Sigma[1, 2] <- Sigma[2, 1] <- sigma_buy * sigma_val * rho_buy_val
  Sigma[1, 3] <- Sigma[3, 1] <- sigma_buy * sigma_sens * rho_buy_sens
  Sigma[1, 4] <- Sigma[4, 1] <- sigma_buy * sigma_sub * rho_buy_sub
  Sigma[2, 3] <- Sigma[3, 2] <- sigma_val * sigma_sens * rho_val_sens
  Sigma[2, 4] <- Sigma[4, 2] <- sigma_val * sigma_sub * rho_val_sub
  Sigma[3, 4] <- Sigma[4, 3] <- sigma_sens * sigma_sub * rho_sens_sub

  L <- t(chol(Sigma))

  # Transform to actual scale
  latent_traits <- matrix(0, nrow = n_subjects, ncol = 4)
  for (i in seq_len(n_subjects)) {
    latent_traits[i, ] <- L %*% u_matrix[i, ]
  }
  colnames(latent_traits) <- c("u_buy", "u_val", "u_sens", "u_sub")
  rownames(latent_traits) <- names(prep$subject_map)

  # Log-likelihood
  logLik <- -opt$objective

  # =========================================================================
  # Build output object
  # =========================================================================
  result <- list(
    coefficients = coefficients,
    coefficients_natural = c(
      coefficients[!names(coefficients) %in% c("log_beta")],
      beta = unname(exp(coefficients["log_beta"]))
    ),
    latent_traits = latent_traits,
    latent_traits_standardized = u_matrix,
    vcov = if (!is.null(sdr$cov.fixed)) sdr$cov.fixed else NULL,
    logLik = logLik,
    AIC = -2 * logLik + 2 * length(opt$par),
    BIC = -2 * logLik + log(nrow(dat)) * length(opt$par),
    convergence = opt$convergence,
    message = opt$message,
    n_obs = nrow(dat),
    n_subjects = n_subjects,
    stream_counts = stream_counts,
    k_fixed = k_fixed,
    k_value = if (k_fixed) k_start else coefficients["k"],
    epsilon = epsilon,
    data = dat,
    subject_map = prep$subject_map,
    tmb_obj = obj,
    tmb_opt = opt,
    tmb_sdr = sdr,
    call = call,
    warnings = prep$warnings,
    optimizer_warnings = unique(opt_warnings)
  )

  class(result) <- "beezdemand_joint_hurdle"
  result
}


#' Compute Starting Values for Latent-Trait Joint Hurdle Model
#'
#' @param dat Prepared data frame
#' @param k_start Starting value for k
#' @param epsilon Epsilon value
#' @param stream_counts Named vector of stream counts
#' @return Named list of starting values
#' @keywords internal
compute_joint_start_values_latent <- function(dat, k_start, epsilon, stream_counts) {
  # Separate by stream
  alone_dat <- dat[dat$stream == 0, ]
  own_dat <- dat[dat$stream == 1, ]
  alt_dat <- dat[dat$stream == 2, ]

  # Default starting values
  start <- list(
    # Part I (hurdle)
    gamma0 = 0,
    gamma_own_target = 0,
    gamma_own_alt = 0,
    gamma1 = -0.5,
    # Part II fixed effects (theta = population means)
    theta_Q0_AT = 2,
    theta_alpha_AT = log(0.01),
    theta_Q0_OT = 2,
    theta_alpha_OT = log(0.01),
    theta_Qalone_OA = 1,
    I = -0.5,
    log_beta = log(0.1),
    k = k_start,
    # Latent loadings (start at 0 = no differential substitution effect)
    lambda_sub_q0 = 0,
    lambda_sub_alpha = 0,
    lambda_sub_alt = 0,
    # Latent trait variances (log scale)
    logsigma_buy = log(0.5),
    logsigma_val = log(0.3),
    logsigma_sens = log(0.3),
    logsigma_sub = log(0.3),
    # Latent trait correlations (raw scale, start at 0 = uncorrelated)
    rho_buy_val_raw = 0,
    rho_buy_sens_raw = 0,
    rho_buy_sub_raw = 0,
    rho_val_sens_raw = 0,
    rho_val_sub_raw = 0,
    rho_sens_sub_raw = 0,
    # Residual variance
    logsigma_e = log(0.5)
  )

  # Estimate from data if available
  # alone.target
  if (nrow(alone_dat) > 0) {
    pos_alone <- alone_dat[alone_dat$y > 0, ]
    if (nrow(pos_alone) > 0) {
      start$theta_Q0_AT <- log(max(pos_alone$y, na.rm = TRUE))
    }
  }

  # own.target
  if (nrow(own_dat) > 0) {
    pos_own <- own_dat[own_dat$y > 0, ]
    if (nrow(pos_own) > 0) {
      start$theta_Q0_OT <- log(max(pos_own$y, na.rm = TRUE))
    }
  }

  # own.alt
  if (nrow(alt_dat) > 0) {
    pos_alt <- alt_dat[alt_dat$y > 0, ]
    if (nrow(pos_alt) > 0) {
      start$theta_Qalone_OA <- log(mean(pos_alt$y, na.rm = TRUE))
      # Estimate I from slope
      if (nrow(pos_alt) > 2) {
        low_price <- pos_alt[pos_alt$price_T <= median(pos_alt$price_T), ]
        high_price <- pos_alt[pos_alt$price_T > median(pos_alt$price_T), ]
        if (nrow(low_price) > 0 && nrow(high_price) > 0) {
          diff_y <- mean(log(high_price$y + 0.01)) -
            mean(log(low_price$y + 0.01))
          start$I <- diff_y
        }
      }
    }
  }

  # Estimate hurdle parameters from zero proportions
  zero_prop <- mean(dat$y == 0)
  if (zero_prop > 0 && zero_prop < 1) {
    start$gamma0 <- log(zero_prop / (1 - zero_prop))
  }

  start
}
