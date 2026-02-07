#' Fit Two-Part Mixed Effects Hurdle Cross-Price Demand Model
#'
#' @description
#' Fits a two-part mixed effects hurdle model for cross-price demand data
#' using Template Model Builder (TMB). This model is appropriate when cross-price
#' data contains many zeros representing true non-consumption.
#'
#' @param data Data frame containing cross-price demand data in long format.
#' @param y_var Column name for consumption (y). Values should be non-negative.
#' @param x_var Column name for alternative price (x).
#' @param id_var Column name for subject identifier.
#' @param random_effects Character vector: \code{"zeros"} (a_i), \code{"qalone"} (b_i),
#'   \code{"I"} (c_i). Default \code{c("zeros", "qalone", "I")} for 3-RE model.
#' @param epsilon Small constant for log(x + epsilon) in Part I. Default 0.001.
#' @param start_values Optional named list of starting values.
#' @param tmb_control List of TMB optimization controls.
#' @param verbose Verbosity level (0=silent, 1=progress, 2=detailed).
#' @param ... Additional arguments passed to nlminb.
#'
#' @return An object of class \code{beezdemand_cp_hurdle} containing model results.
#'
#' @keywords internal
fit_cp_hurdle <- function(
  data,
  y_var = "y",
  x_var = "x",
  id_var = "id",
  random_effects = c("zeros", "qalone", "I"),
  epsilon = 0.001,
  start_values = NULL,
  tmb_control = list(
    max_iter = 200,
    eval_max = 1000,
    trace = 0
  ),
  verbose = 1,
  ...
) {
  # Store the call

  call <- match.call()

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  required_cols <- c(y_var, x_var, id_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }

  # Validate random_effects
  valid_re <- c("zeros", "qalone", "I")
  invalid_re <- setdiff(random_effects, valid_re)
  if (length(invalid_re) > 0) {
    stop(
      "Invalid random_effects: ",
      paste(invalid_re, collapse = ", "),
      ". Valid options: ",
      paste(valid_re, collapse = ", ")
    )
  }

  if (!all(c("zeros", "qalone") %in% random_effects)) {
    stop("random_effects must include at least 'zeros' and 'qalone'")
  }

  n_re <- length(random_effects)
  if (n_re < 2 || n_re > 3) {
    stop("Number of random effects must be 2 or 3")
  }

  # Extract and prepare data
  consumption <- data[[y_var]]
  x <- data[[x_var]]
  id <- data[[id_var]]

  # Validate data values

  if (any(is.na(consumption)) || any(is.na(x)) || any(is.na(id))) {
    warning("NA values detected. Rows with NA will be removed.")
    complete_idx <- complete.cases(consumption, x, id)
    consumption <- consumption[complete_idx]
    x <- x[complete_idx]
    id <- id[complete_idx]
  }

  if (any(consumption < 0)) {
    stop("Consumption values must be non-negative")
  }

  if (any(x < 0)) {
    stop("Price values must be non-negative")
  }

  # Create subject mapping (0-indexed for TMB)
  unique_ids <- unique(id)
  n_subjects <- length(unique_ids)
  subject_id <- match(id, unique_ids) - 1L

  if (verbose >= 1) {
    message(sprintf("Fitting %d-RE hurdle cross-price model", n_re))
    message(sprintf("  Subjects: %d", n_subjects))
    message(sprintf("  Observations: %d", length(consumption)))
    message(sprintf(
      "  Zero consumption: %d (%.1f%%)",
      sum(consumption == 0),
      100 * mean(consumption == 0)
    ))
  }

  # Prepare response variables
  # delta = 1 if zero consumption, 0 if positive
  delta <- as.integer(consumption == 0)
  # logQ = log(consumption) for positive values, 0 otherwise
  logQ <- ifelse(consumption > 0, log(consumption), 0)

  # TMB data

  tmb_data <- list(
    model = if (n_re == 3) "HurdleCrossPrice3RE" else "HurdleCrossPrice2RE",
    x = x,
    logQ = logQ,
    delta = delta,
    subject_id = subject_id,
    n_subjects = n_subjects,
    epsilon = epsilon
  )

  # Default starting values
  if (is.null(start_values)) {
    # Estimate reasonable starting values from data
    pos_consumption <- consumption[consumption > 0]

    # Part I: simple logistic regression approximation
    p_zero <- mean(consumption == 0)
    beta0_start <- log(p_zero / (1 - p_zero + 1e-6))
    beta1_start <- 0.1

    # Part II: estimate from positive values
    logQalone_start <- mean(log(pos_consumption + 1e-6))
    I_start <- 0
    log_beta_start <- log(0.1) # beta = 0.1

    start_values <- list(
      beta0 = beta0_start,
      beta1 = beta1_start,
      logQalone = logQalone_start,
      I = I_start,
      log_beta = log_beta_start,
      logsigma_a = log(0.5),
      logsigma_b = log(0.5),
      logsigma_e = log(0.5),
      rho_ab_raw = 0
    )

    if (n_re == 3) {
      start_values$logsigma_c <- log(0.5)
      start_values$rho_ac_raw <- 0
      start_values$rho_bc_raw <- 0
    }
  }

  # Initialize random effects matrix
  u_init <- matrix(0, nrow = n_subjects, ncol = n_re)

  # TMB parameters
  tmb_pars <- c(
    start_values,
    list(u = u_init)
  )

  # Create TMB objective function
  if (verbose >= 2) {
    message("Creating TMB objective function...")
  }

  obj <- tryCatch(
    TMB::MakeADFun(
      data = tmb_data,
      parameters = tmb_pars,
      random = "u",
      DLL = "beezdemand",
      silent = verbose < 2
    ),
    error = function(e) {
      stop("Failed to create TMB objective: ", e$message)
    }
  )

  # Optimization
  if (verbose >= 1) {
    message("Optimizing...")
  }

  opt_warnings <- character(0)
  opt <- tryCatch(
    withCallingHandlers(
      nlminb(
        start = obj$par,
        objective = obj$fn,
        gradient = obj$gr,
        control = list(
          iter.max = tmb_control$max_iter,
          eval.max = tmb_control$eval_max,
          trace = tmb_control$trace
        ),
        ...
      ),
      warning = function(w) {
        msg <- conditionMessage(w)
        opt_warnings <<- c(opt_warnings, msg)
        if (grepl("NA/NaN function evaluation|non-finite value supplied", msg)) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(e) {
      stop("Optimization failed: ", e$message)
    }
  )

  converged <- opt$convergence == 0

  if (verbose >= 1) {
    if (converged) {
      message("Optimization converged successfully")
    } else {
      message(
        "Optimization may not have converged (code: ",
        opt$convergence,
        ")"
      )
    }
  }

  # Compute standard errors
  if (verbose >= 2) {
    message("Computing standard errors...")
  }

  sdr <- tryCatch(
    TMB::sdreport(obj),
    error = function(e1) {
      # Fallback: try without joint precision (more robust; may yield NA SEs).
      sdr2 <- tryCatch(
        TMB::sdreport(obj, getJointPrecision = FALSE),
        error = function(e2) NULL
      )
      if (is.null(sdr2) && verbose >= 1) {
        warning("Failed to compute standard errors: ", e1$message)
      }
      sdr2
    }
  )

  # Extract coefficients
  if (is.null(sdr)) {
    coefficients <- opt$par
    names(coefficients) <- names(obj$par)
    se <- setNames(rep(NA_real_, length(coefficients)), names(coefficients))
  } else {
    coef_summary <- summary(sdr, "fixed")
    coefficients <- coef_summary[, "Estimate"]
    names(coefficients) <- rownames(coef_summary)

    se <- coef_summary[, "Std. Error"]
    names(se) <- rownames(coef_summary)
  }

  # Transform log_beta to beta for reporting
  beta_val <- exp(coefficients["log_beta"])
  names(beta_val) <- "beta"

  # Get derived quantities
  derived <- if (is.null(sdr)) {
    NULL
  } else {
    tryCatch(summary(sdr, "report"), error = function(e) NULL)
  }

  # Extract random effects
  if (is.null(sdr)) {
    u_est <- matrix(NA_real_, nrow = n_subjects, ncol = n_re)
  } else {
    re_summary <- summary(sdr, "random")
    u_est <- matrix(re_summary[, "Estimate"], nrow = n_subjects, ncol = n_re)
  }

  # Get actual random effects (transformed)
  if (!is.null(derived) && "re" %in% rownames(derived)) {
    re_idx <- which(rownames(derived) == "re")
    re_vals <- derived[re_idx, "Estimate"]
    random_effects_mat <- matrix(re_vals, nrow = n_subjects, ncol = n_re)
  } else {
    random_effects_mat <- u_est
  }

  colnames(random_effects_mat) <- if (n_re == 3) {
    c("a_i", "b_i", "c_i")
  } else {
    c("a_i", "b_i")
  }

  # Calculate subject-specific parameters
  subject_pars <- data.frame(
    id = unique_ids,
    a_i = random_effects_mat[, "a_i"],
    b_i = random_effects_mat[, "b_i"],
    Qalone = exp(coefficients["logQalone"] + random_effects_mat[, "b_i"]),
    stringsAsFactors = FALSE
  )

  if (n_re == 3) {
    subject_pars$c_i <- random_effects_mat[, "c_i"]
    subject_pars$I_individual <- coefficients["I"] + random_effects_mat[, "c_i"]
  } else {
    subject_pars$I_individual <- rep(coefficients["I"], n_subjects)
  }

  # Determine relationship type based on I
  subject_pars$relationship <- ifelse(
    subject_pars$I_individual < 0,
    "substitute",
    "complement"
  )

  # Calculate breakpoint (price where P(zero) = 0.5)
  subject_pars$breakpoint <- exp(
    -(coefficients["beta0"] + random_effects_mat[, "a_i"]) /
      coefficients["beta1"]
  ) -
    epsilon

  # Extract variance components
  if (!is.null(derived)) {
    var_names <- c(
      "var_a",
      "var_b",
      "var_e",
      "cov_ab",
      "rho_ab",
      "sigma_a",
      "sigma_b",
      "sigma_e"
    )
    if (n_re == 3) {
      var_names <- c(
        var_names,
        "var_c",
        "cov_ac",
        "cov_bc",
        "rho_ac",
        "rho_bc",
        "sigma_c"
      )
    }
    variance_components <- list()
    for (vn in var_names) {
      if (vn %in% rownames(derived)) {
        variance_components[[vn]] <- derived[vn, "Estimate"]
      }
    }
  } else {
    variance_components <- NULL
  }

  # Log-likelihood and information criteria
  loglik <- -opt$objective
  n_fixed <- length(opt$par)
  n_obs <- length(consumption)
  AIC <- 2 * n_fixed - 2 * loglik
  BIC <- log(n_obs) * n_fixed - 2 * loglik

  # Build result object
  result <- list(
    model = list(
      coefficients = coefficients,
      se = se,
      beta = beta_val,
      variance_components = variance_components
    ),
    random_effects = random_effects_mat,
    subject_pars = subject_pars,
    tmb_obj = obj,
    opt = opt,
    sdr = sdr,
    call = call,
    data = data.frame(
      id = id,
      x = x,
      y = consumption,
      logQ = logQ,
      delta = delta
    ),
    param_info = list(
      y_var = y_var,
      x_var = x_var,
      id_var = id_var,
      n_subjects = n_subjects,
      n_obs = n_obs,
      n_random_effects = n_re,
      random_effects_spec = random_effects,
      epsilon = epsilon
    ),
    converged = converged,
    optimizer_warnings = unique(opt_warnings),
    loglik = loglik,
    AIC = AIC,
    BIC = BIC
  )

  class(result) <- "beezdemand_cp_hurdle"
  return(result)
}
