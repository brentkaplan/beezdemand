#' @useDynLib beezdemand, .registration = TRUE
#' @importFrom utils head modifyList
NULL

#' Fit Two-Part Mixed Effects Hurdle Demand Model
#'
#' @description
#' Fits a two-part hurdle model for demand data using TMB (Template Model Builder).
#' Part I models the probability of zero consumption using logistic regression.
#' Part II models log-consumption given positive response using a nonlinear
#' mixed effects model.
#'
#' @param data A data frame containing the demand data.
#' @param y_var Character string specifying the column name for consumption values.
#' @param x_var Character string specifying the column name for price.
#' @param id_var Character string specifying the column name for subject IDs.
#' @param random_effects Character vector specifying which random effects to include.
#'   Options are \code{"zeros"} (a_i for Part I), \code{"q0"} (b_i for intensity),
#'   and \code{"alpha"} (c_i for elasticity). Default is \code{c("zeros", "q0", "alpha")}
#'   for the full 3-random-effect model. Use \code{c("zeros", "q0")} for the
#'   simplified 2-random-effect model (fixed alpha across subjects).
#' @param param_space Character. Parameterization used for fitting core demand
#'   parameters. One of:
#'   - `"natural"`: optimize using natural `Q0`, `alpha`, `k` (internally mapped to the TMB working space)
#'   - `"log10"`: optimize using `log10(Q0)`, `log10(alpha)`, `log10(k)` (internally mapped)
#'
#'   This argument controls the optimization parameterization; model storage remains
#'   compatible with existing TMB internals and reporting can be controlled via
#'   `report_space` in `summary()`, `tidy()`, and `coef()`.
#' @param epsilon Small constant added to price before log transformation in Part I.
#'   Used to handle zero prices: \code{log(price + epsilon)}. Default is 0.001.
#' @param start_values Optional named list of starting values for optimization.
#'   If \code{NULL} (default), sensible defaults are used.
#' @param tmb_control List of control parameters for TMB optimization:
#'   \describe{
#'     \item{max_iter}{Maximum number of optimization iterations (default 200)}
#'     \item{eval_max}{Maximum number of function evaluations (default 1000)}
#'     \item{trace}{Print optimization trace: 0 = none, 1 = some (default 0)}
#'   }
#' @param verbose Integer controlling output verbosity: 0 = silent, 1 = progress
#'   messages, 2 = detailed optimization trace. Default is 1.
#' @param ... Additional arguments (reserved for future use).
#'
#' @return An object of class \code{beezdemand_hurdle} containing:
#' \describe{
#'   \item{model}{List with coefficients, se, variance_components, correlations}
#'   \item{random_effects}{Matrix of empirical Bayes random effect estimates}
#'   \item{subject_pars}{Data frame of subject-specific parameters including
#'     Q0, alpha, breakpoint, Pmax, Omax}
#'   \item{tmb_obj}{TMB objective function object}
#'   \item{opt}{Optimization result from \code{nlminb}}
#'   \item{sdr}{TMB sdreport object}
#'   \item{call}{The matched call}
#'   \item{data}{Original data used for fitting}
#'   \item{param_info}{List with y_var, x_var, id_var, n_subjects, n_obs, etc.}
#'   \item{converged}{Logical indicating convergence}
#'   \item{loglik}{Log-likelihood at convergence}
#'   \item{AIC, BIC}{Information criteria}
#'   \item{error_message}{Error message if fitting failed, NULL otherwise}
#' }
#'
#' @details
#' The model structure is:
#'
#' \strong{Part I (Binary - probability of zero consumption):}
#' \deqn{logit(\pi_{ij}) = \beta_0 + \beta_1 \cdot \log(price + \epsilon) + a_i}
#'
#' \strong{Part II (Continuous - log consumption given positive):}
#'
#' With 3 random effects (\code{random_effects = c("zeros", "q0", "alpha")}):
#' \deqn{\log(Q_{ij}) = (\log Q_0 + b_i) + k \cdot (\exp(-(\alpha + c_i) \cdot price) - 1) + \epsilon_{ij}}
#'
#' With 2 random effects (\code{random_effects = c("zeros", "q0")}):
#' \deqn{\log(Q_{ij}) = (\log Q_0 + b_i) + k \cdot (\exp(-\alpha \cdot price) - 1) + \epsilon_{ij}}
#'
#' Random effects follow a multivariate normal distribution with unstructured
#' covariance matrix. Use \code{\link{compare_hurdle_models}} for likelihood
#' ratio tests comparing nested models.
#'
#' @seealso \code{\link{summary.beezdemand_hurdle}}, \code{\link{predict.beezdemand_hurdle}},
#'   \code{\link{plot.beezdemand_hurdle}}, \code{\link{compare_hurdle_models}},
#'   \code{\link{simulate_hurdle_data}}
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(apt)
#'
#' # Fit full model with 3 random effects
#' fit3 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
#'                           random_effects = c("zeros", "q0", "alpha"))
#'
#' # Fit simplified model with 2 random effects (fixed alpha)
#' fit2 <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id",
#'                           random_effects = c("zeros", "q0"))
#'
#' # View results
#' summary(fit3)
#'
#' # Compare models with likelihood ratio test
#' compare_hurdle_models(fit3, fit2)
#' }
#'
#' @importFrom TMB MakeADFun sdreport
#' @importFrom stats nlminb na.omit setNames
#' @export
fit_demand_hurdle <- function(
  data,
  y_var,
  x_var,
  id_var,
  random_effects = c("zeros", "q0", "alpha"),
  param_space = c("natural", "log10"),
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
  # Capture call

  cl <- match.call()
  param_space <- match.arg(param_space)

  # Validate random_effects argument
  valid_re <- c("zeros", "q0", "alpha")
  if (!all(random_effects %in% valid_re)) {
    stop(
      "random_effects must be a subset of: ",
      paste(valid_re, collapse = ", ")
    )
  }
  if (!("zeros" %in% random_effects) || !("q0" %in% random_effects)) {
    stop("random_effects must include at least 'zeros' and 'q0'")
  }

  # Determine number of random effects
  n_re <- length(random_effects)
  if (!(n_re %in% c(2, 3))) {
    stop("random_effects must have 2 or 3 elements")
  }

  # Validate and prepare data
  data <- validate_hurdle_data(
    data = data,
    y_var = y_var,
    x_var = x_var,
    id_var = id_var
  )

  # Extract variables
  ids <- data[[id_var]]
  price <- as.numeric(data[[x_var]])
  consumption <- as.numeric(data[[y_var]])

  # Create derived variables
  delta <- as.integer(consumption == 0)
  logQ <- ifelse(consumption > 0, log(consumption), 0)

  # Create subject mapping (0-indexed for C++)
  subject_levels <- unique(ids)
  n_subjects <- length(subject_levels)
  subject_map <- setNames(
    seq_along(subject_levels) - 1L,
    as.character(subject_levels)
  )
  subject_id <- as.integer(subject_map[as.character(ids)])

  # Calculate number of parameters and check sample size
  n_fixed_params <- if (n_re == 3) 12 else 9
  min_recommended <- n_fixed_params * 5

  if (n_subjects < min_recommended && verbose >= 1) {
    message(sprintf(
      "Sample size may be too small for reliable estimation.\n  Subjects: %d, Parameters: %d, Recommended minimum: %d subjects.\n  Consider using more subjects or the simpler 2-RE model.",
      n_subjects,
      n_fixed_params,
      min_recommended
    ))
  }

  # Determine model name for TMB dispatcher
  model_name <- if (n_re == 3) "HurdleDemand3RE" else "HurdleDemand2RE"

  # Progress message
  if (verbose >= 1) {
    message(sprintf("Fitting %s model...", model_name))
    message(sprintf(
      "  Subjects: %d, Observations: %d",
      n_subjects,
      length(price)
    ))
    message(sprintf(
      "  Fixed parameters: %d, Random effects per subject: %d",
      n_fixed_params,
      n_re
    ))
  }

  # Fill in default tmb_control values
  default_control <- list(max_iter = 200, eval_max = 1000, trace = 0)
  tmb_control <- modifyList(default_control, tmb_control)

  # Prepare TMB data with model selector
  tmb_data <- list(
    model = model_name,
    price = price,
    logQ = logQ,
    delta = delta,
    subject_id = subject_id,
    n_subjects = n_subjects,
    epsilon = epsilon
  )

  # Default starting values - specific to each model
  if (is.null(start_values)) {
    mean_positive_consumption <- mean(
      consumption[consumption > 0],
      na.rm = TRUE
    )
    if (is.na(mean_positive_consumption) || mean_positive_consumption <= 0) {
      mean_positive_consumption <- 10
    }

    if (n_re == 3) {
      start_values <- list(
        beta0 = -2.5,
        beta1 = 1.0,
        logQ0 = log(mean_positive_consumption),
        k = 2.0,
        alpha = 0.5,
        logsigma_a = 0.5,
        logsigma_b = -0.5,
        logsigma_c = -1.5,
        logsigma_e = -0.5,
        rho_ab_raw = 0,
        rho_ac_raw = 0,
        rho_bc_raw = 0
      )
    } else {
      # 2 random effects model
      start_values <- list(
        beta0 = -2.5,
        beta1 = 1.0,
        logQ0 = log(mean_positive_consumption),
        k = 2.0,
        alpha = 0.5,
        logsigma_a = 0.5,
        logsigma_b = -0.5,
        logsigma_e = -0.5,
        rho_ab_raw = 0
      )
    }
  }
  start_values$u <- matrix(0, nrow = n_subjects, ncol = n_re)

  # Create TMB objective function
  obj <- tryCatch(
    {
      TMB::MakeADFun(
        data = tmb_data,
        parameters = start_values,
        random = "u",
        DLL = "beezdemand",
        silent = verbose < 2
      )
    },
    error = function(e) {
      stop(
        "TMB model creation failed: ",
        e$message,
        "\nMake sure the package was compiled with TMB support."
      )
    }
  )

  # Optimize
  if (verbose >= 1) {
    message("  Optimizing...")
  }

  wrap_tmb_param_space <- function(obj, param_space) {
    par_names <- names(obj$par)
    if (is.null(par_names) || !all(c("logQ0", "alpha", "k") %in% par_names)) {
      return(list(
        start = obj$par,
        fn = obj$fn,
        gr = obj$gr,
        to_internal = function(p) p,
        from_internal = function(p) p
      ))
    }

    ln10 <- log(10)

    to_internal <- function(p_work) {
      if (is.null(names(p_work))) names(p_work) <- par_names
      p <- p_work
      if (param_space == "natural") {
        p[["logQ0"]] <- log(p_work[["logQ0"]])
      } else if (param_space == "log10") {
        p[["logQ0"]] <- p_work[["logQ0"]] * ln10
        p[["alpha"]] <- 10^p_work[["alpha"]]
        p[["k"]] <- 10^p_work[["k"]]
      }
      p
    }

    from_internal <- function(p_int) {
      if (is.null(names(p_int))) names(p_int) <- par_names
      p <- p_int
      if (param_space == "natural") {
        p[["logQ0"]] <- exp(p_int[["logQ0"]])
      } else if (param_space == "log10") {
        p[["logQ0"]] <- p_int[["logQ0"]] / ln10
        p[["alpha"]] <- log10(p_int[["alpha"]])
        p[["k"]] <- log10(p_int[["k"]])
      }
      p
    }

    fn <- function(p_work) {
      if (is.null(names(p_work))) names(p_work) <- par_names
      obj$fn(to_internal(p_work))
    }
    gr <- function(p_work) {
      if (is.null(names(p_work))) names(p_work) <- par_names
      p_int <- to_internal(p_work)
      g_int <- obj$gr(p_int)
      if (is.null(names(g_int))) names(g_int) <- names(p_int)
      g <- g_int

      if (param_space == "natural") {
        g[["logQ0"]] <- g_int[["logQ0"]] / p_work[["logQ0"]]
      } else if (param_space == "log10") {
        g[["logQ0"]] <- g_int[["logQ0"]] * ln10
        g[["alpha"]] <- g_int[["alpha"]] * ln10 * p_int[["alpha"]]
        g[["k"]] <- g_int[["k"]] * ln10 * p_int[["k"]]
      }
      g
    }

    list(
      start = from_internal(obj$par),
      fn = fn,
      gr = gr,
      to_internal = to_internal,
      from_internal = from_internal
    )
  }

  wrapped <- wrap_tmb_param_space(obj, param_space)

  opt_warnings <- character(0)
  opt <- withCallingHandlers(
    nlminb(
      start = wrapped$start,
      objective = wrapped$fn,
      gradient = wrapped$gr,
      control = list(
        eval.max = tmb_control$eval_max,
        iter.max = tmb_control$max_iter,
        trace = if (verbose >= 2) 1 else tmb_control$trace
      )
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      opt_warnings <<- c(opt_warnings, msg)
      if (grepl("NA/NaN function evaluation|non-finite value supplied", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  converged <- opt$convergence == 0
  opt$par_internal <- wrapped$to_internal(opt$par)
  try(obj$fn(opt$par_internal), silent = TRUE)

  if (verbose >= 1) {
    if (converged) {
      message(sprintf("  Converged in %d iterations", opt$iterations))
    } else {
      message(sprintf(
        "  WARNING: Did not converge (code %d: %s)",
        opt$convergence,
        opt$message
      ))
    }
  }

  # Compute standard errors
  if (verbose >= 1) {
    message("  Computing standard errors...")
  }
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

  # Extract fixed effects (different for 2 vs 3 RE)
  if (n_re == 3) {
    fixed_names <- c(
      "beta0",
      "beta1",
      "logQ0",
      "k",
      "alpha",
      "logsigma_a",
      "logsigma_b",
      "logsigma_c",
      "logsigma_e",
      "rho_ab_raw",
      "rho_ac_raw",
      "rho_bc_raw"
    )
    var_names <- c(
      "var_a",
      "var_b",
      "var_c",
      "cov_ab",
      "cov_ac",
      "cov_bc",
      "var_e"
    )
    rho_names <- c("rho_ab", "rho_ac", "rho_bc")
  } else {
    fixed_names <- c(
      "beta0",
      "beta1",
      "logQ0",
      "k",
      "alpha",
      "logsigma_a",
      "logsigma_b",
      "logsigma_e",
      "rho_ab_raw"
    )
    var_names <- c("var_a", "var_b", "cov_ab", "var_e")
    rho_names <- c("rho_ab")
  }

  coefficients <- opt$par_internal[fixed_names]

  # Extract standard errors and derived quantities (handle NULL sdr)
  if (!is.null(sdr)) {
    se <- summary(sdr, "fixed")[fixed_names, "Std. Error"]
    adr <- summary(sdr, "report")
    variance_components <- adr[var_names, , drop = FALSE]
    correlations <- adr[rho_names, , drop = FALSE]
    re_summary <- summary(sdr, "random")
    u_hat <- matrix(re_summary[, "Estimate"], nrow = n_subjects, ncol = n_re)
  } else {
    se <- rep(NA_real_, length(fixed_names))
    names(se) <- fixed_names
    variance_components <- matrix(
      NA_real_,
      nrow = length(var_names),
      ncol = 2,
      dimnames = list(var_names, c("Estimate", "Std. Error"))
    )
    correlations <- matrix(
      NA_real_,
      nrow = length(rho_names),
      ncol = 2,
      dimnames = list(rho_names, c("Estimate", "Std. Error"))
    )
    u_hat <- matrix(0, nrow = n_subjects, ncol = n_re)
  }

  # Build covariance matrix to transform u -> actual random effects
  sigma_a <- exp(coefficients["logsigma_a"])
  sigma_b <- exp(coefficients["logsigma_b"])
  rho_ab <- tanh(coefficients["rho_ab_raw"])

  if (n_re == 3) {
    sigma_c <- exp(coefficients["logsigma_c"])
    rho_ac <- tanh(coefficients["rho_ac_raw"])
    rho_bc <- tanh(coefficients["rho_bc_raw"])

    Sigma <- matrix(
      c(
        sigma_a^2,
        sigma_a * sigma_b * rho_ab,
        sigma_a * sigma_c * rho_ac,
        sigma_a * sigma_b * rho_ab,
        sigma_b^2,
        sigma_b * sigma_c * rho_bc,
        sigma_a * sigma_c * rho_ac,
        sigma_b * sigma_c * rho_bc,
        sigma_c^2
      ),
      nrow = 3
    )

    L <- tryCatch(
      t(chol(Sigma)),
      error = function(e) {
        # Numeric stability: rho parameters can imply a non-PD correlation matrix.
        # Fall back to an uncorrelated covariance to allow downstream reporting.
        Sigma_fallback <- diag(c(sigma_a^2, sigma_b^2, sigma_c^2), nrow = 3)
        t(chol(Sigma_fallback))
      }
    )
    random_effects_mat <- t(L %*% t(u_hat))
    colnames(random_effects_mat) <- c("a_i", "b_i", "c_i")

    # Subject-specific parameters (with c_i)
    subj_Q0 <- exp(coefficients["logQ0"] + random_effects_mat[, "b_i"])
    subj_alpha <- coefficients["alpha"] + random_effects_mat[, "c_i"]

    # Calculate Omax and Pmax for each subject
    omax_pmax <- calc_omax_pmax_vec(
      Q0 = subj_Q0,
      k = coefficients["k"],
      alpha = subj_alpha
    )

    subject_pars <- data.frame(
      id = subject_levels,
      a_i = random_effects_mat[, "a_i"],
      b_i = random_effects_mat[, "b_i"],
      c_i = random_effects_mat[, "c_i"],
      Q0 = subj_Q0,
      alpha = subj_alpha,
      breakpoint = exp(
        -(coefficients["beta0"] + random_effects_mat[, "a_i"]) /
          coefficients["beta1"]
      ) -
        epsilon,
      Pmax = omax_pmax$Pmax,
      Omax = omax_pmax$Omax,
      stringsAsFactors = FALSE
    )
  } else {
    # 2 random effects
    Sigma <- matrix(
      c(
        sigma_a^2,
        sigma_a * sigma_b * rho_ab,
        sigma_a * sigma_b * rho_ab,
        sigma_b^2
      ),
      nrow = 2
    )

    L <- tryCatch(
      t(chol(Sigma)),
      error = function(e) {
        Sigma_fallback <- diag(c(sigma_a^2, sigma_b^2), nrow = 2)
        t(chol(Sigma_fallback))
      }
    )
    random_effects_mat <- t(L %*% t(u_hat))
    colnames(random_effects_mat) <- c("a_i", "b_i")

    # Subject-specific parameters (alpha is fixed, no c_i)
    subj_Q0 <- exp(coefficients["logQ0"] + random_effects_mat[, "b_i"])
    subj_alpha <- rep(coefficients["alpha"], n_subjects)

    # Calculate Omax and Pmax for each subject
    omax_pmax <- calc_omax_pmax_vec(
      Q0 = subj_Q0,
      k = coefficients["k"],
      alpha = subj_alpha
    )

    subject_pars <- data.frame(
      id = subject_levels,
      a_i = random_effects_mat[, "a_i"],
      b_i = random_effects_mat[, "b_i"],
      Q0 = subj_Q0,
      alpha = subj_alpha,
      breakpoint = exp(
        -(coefficients["beta0"] + random_effects_mat[, "a_i"]) /
          coefficients["beta1"]
      ) -
        epsilon,
      Pmax = omax_pmax$Pmax,
      Omax = omax_pmax$Omax,
      stringsAsFactors = FALSE
    )
  }
  rownames(random_effects_mat) <- as.character(subject_levels)
  names(subject_pars)[1] <- id_var

  # Compute fit statistics
  nll <- as.numeric(obj$fn(opt$par_internal))
  loglik <- -as.numeric(nll)
  n_fixed <- length(coefficients)
  AIC_val <- as.numeric(2 * nll + 2 * n_fixed)
  BIC_val <- as.numeric(2 * nll + log(nrow(data)) * n_fixed)

  # Build result object
  result <- list(
    model = list(
      coefficients = coefficients,
      se = se,
      variance_components = variance_components,
      correlations = correlations
    ),
    random_effects = random_effects_mat,
    subject_pars = subject_pars,
    tmb_obj = obj,
    opt = opt,
    sdr = sdr,
    call = cl,
    data = data,
    param_info = list(
      y_var = y_var,
      x_var = x_var,
      id_var = id_var,
      subject_levels = subject_levels,
      n_subjects = n_subjects,
      n_obs = nrow(data),
      n_random_effects = n_re,
      random_effects_spec = random_effects,
      epsilon = epsilon
    ),
    param_space = param_space,
    param_space_details = beezdemand_param_space_details_core(
      internal_names = list(Q0 = "logQ0", alpha = "alpha", k = "k"),
      internal_spaces = list(Q0 = "log", alpha = "natural", k = "natural")
    ),
    converged = converged,
    optimizer_warnings = unique(opt_warnings),
    loglik = loglik,
    AIC = AIC_val,
    BIC = BIC_val,
    error_message = NULL
  )

  class(result) <- "beezdemand_hurdle"

  if (verbose >= 1) {
    message(sprintf("Done. Log-likelihood: %.2f", loglik))
  }

  return(result)
}


#' Validate Hurdle Demand Data
#'
#' @description
#' Internal function to validate and prepare data for hurdle demand model fitting.
#'
#' @param data A data frame.
#' @param y_var Character string, name of consumption variable.
#' @param x_var Character string, name of price variable.
#' @param id_var Character string, name of subject ID variable.
#'
#' @return A cleaned data frame ready for model fitting.
#' @keywords internal
validate_hurdle_data <- function(data, y_var, x_var, id_var) {
  # Check data is a data frame
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  # Check required columns exist
  required_cols <- c(id_var, x_var, y_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }

  # Check numeric types
  if (!is.numeric(data[[x_var]])) {
    stop("'", x_var, "' must be numeric (price variable)")
  }
  if (!is.numeric(data[[y_var]])) {
    stop("'", y_var, "' must be numeric (consumption variable)")
  }

  # Check for negative values
  if (any(data[[y_var]] < 0, na.rm = TRUE)) {
    stop("Consumption values (", y_var, ") cannot be negative")
  }
  if (any(data[[x_var]] < 0, na.rm = TRUE)) {
    stop("Price values (", x_var, ") cannot be negative")
  }

  # Remove missing values
  data_complete <- na.omit(data[, required_cols, drop = FALSE])
  n_removed <- nrow(data) - nrow(data_complete)
  if (n_removed > 0) {
    warning(n_removed, " rows removed due to missing values")
  }

  # Check we have enough data
  if (nrow(data_complete) < 10) {
    stop("Insufficient data: need at least 10 complete observations")
  }

  # Check we have multiple subjects
  n_subjects <- length(unique(data_complete[[id_var]]))
  if (n_subjects < 2) {
    stop("Need at least 2 subjects for mixed effects model")
  }

  return(data_complete)
}
