#' @useDynLib beezdemand, .registration = TRUE
#' @importFrom utils head modifyList
NULL

# ==============================================================================
# Internal Helper Functions for Hurdle Models
# ==============================================================================

#' Prepare Hurdle Model Data
#'
#' @description
#' Internal function to prepare data structures for TMB hurdle model fitting.
#' Converts subject IDs to 0-indexed integers and creates derived variables.
#'
#' @param data A validated data frame.
#' @param y_var Character string, name of consumption variable.
#' @param x_var Character string, name of price variable.
#' @param id_var Character string, name of subject ID variable.
#'
#' @return A list containing:
#' \describe{
#'   \item{price}{Numeric vector of prices}
#'   \item{consumption}{Numeric vector of consumption values}
#'   \item{delta}{Integer vector (1 if consumption == 0, else 0)}
#'   \item{logQ}{Log consumption (0 for zeros)}
#'   \item{subject_id}{0-indexed subject IDs for C++}
#'   \item{subject_levels}{Unique subject IDs (original)}
#'   \item{n_subjects}{Number of unique subjects}
#' }
#' @keywords internal
.hurdle_prepare_data <- function(data, y_var, x_var, id_var) {
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

  list(
    price = price,
    consumption = consumption,
    delta = delta,
    logQ = logQ,
    subject_id = subject_id,
    subject_levels = subject_levels,
    n_subjects = n_subjects
  )
}


#' Build TMB Data List for Hurdle Model
#'
#' @description
#' Internal function to construct the TMB data list for hurdle demand models.
#'
#' @param prepared_data List from \code{.hurdle_prepare_data()}.
#' @param model_name Character string specifying the TMB model.
#' @param epsilon Small constant for log(price + epsilon) in Part I.
#'
#' @return A list suitable for \code{TMB::MakeADFun(data = ...)}.
#' @keywords internal
.hurdle_build_tmb_data <- function(prepared_data, model_name, epsilon) {
  list(
    model = model_name,
    price = prepared_data$price,
    logQ = prepared_data$logQ,
    delta = prepared_data$delta,
    subject_id = prepared_data$subject_id,
    n_subjects = prepared_data$n_subjects,
    epsilon = epsilon
  )
}


#' Generate Default Starting Values for Hurdle Model
#'
#' @description
#' Internal function to generate sensible default starting values for TMB
#' hurdle model optimization.
#'
#' @param consumption Numeric vector of consumption values.
#' @param n_re Number of random effects (2 or 3).
#' @param n_subjects Number of subjects.
#' @param has_k Logical, whether the model has a k parameter.
#'
#' @return A named list of starting values for TMB parameters.
#' @keywords internal
.hurdle_default_starts <- function(consumption, n_re, n_subjects, has_k) {
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
      log_q0 = log(mean_positive_consumption),
      log_alpha = log(0.5),
      logsigma_a = 0.5,
      logsigma_b = -0.5,
      logsigma_c = -1.5,
      logsigma_e = -0.5,
      rho_ab_raw = 0,
      rho_ac_raw = 0,
      rho_bc_raw = 0
    )
    if (has_k) {
      start_values$log_k <- log(2.0)
    }
  } else {
    # 2 random effects model
    start_values <- list(
      beta0 = -2.5,
      beta1 = 1.0,
      log_q0 = log(mean_positive_consumption),
      log_alpha = log(0.5),
      logsigma_a = 0.5,
      logsigma_b = -0.5,
      logsigma_e = -0.5,
      rho_ab_raw = 0
    )
    if (has_k) {
      start_values$log_k <- log(2.0)
    }
  }

  start_values$u <- matrix(0, nrow = n_subjects, ncol = n_re)
  start_values
}


#' Normalize User-Provided Starting Values
#'
#' @description
#' Internal function to handle backwards-compatibility and normalization of
#' user-provided starting values.
#'
#' @param start_values User-provided named list of starting values.
#' @param n_re Number of random effects (2 or 3).
#' @param has_k Logical, whether the model has a k parameter.
#'
#' @return Normalized starting values list.
#' @keywords internal
.hurdle_normalize_starts <- function(start_values, n_re, has_k) {
  # Backwards-compatibility for older parameter names

  if (!is.null(start_values$logQ0) && is.null(start_values$log_q0)) {
    start_values$log_q0 <- start_values$logQ0
    start_values$logQ0 <- NULL
  }
  if (!is.null(start_values$k) && is.null(start_values$log_k)) {
    if (!is.numeric(start_values$k) || length(start_values$k) != 1 ||
        !is.finite(start_values$k) || start_values$k <= 0) {
      stop("'start_values$k' must be a single positive numeric value.",
           call. = FALSE)
    }
    start_values$log_k <- log(start_values$k)
    start_values$k <- NULL
  }
  if (!is.null(start_values$alpha) && is.null(start_values$log_alpha)) {
    if (!is.numeric(start_values$alpha) || length(start_values$alpha) != 1 ||
        !is.finite(start_values$alpha) || start_values$alpha <= 0) {
      stop("'start_values$alpha' must be a single positive numeric value.",
           call. = FALSE)
    }
    start_values$log_alpha <- log(start_values$alpha)
    start_values$alpha <- NULL
  }

  if (!has_k && !is.null(start_values$log_k)) {
    warning(
      "Ignoring 'start_values$log_k' for simplified_exponential (SND) model.",
      call. = FALSE
    )
    start_values$log_k <- NULL
  }

  # Handle rho_bc parameterization for 3-RE models
  if (n_re == 3) {
    r_ab <- if (!is.null(start_values$rho_ab_raw)) {
      tanh(start_values$rho_ab_raw)
    } else {
      0
    }
    r_ac <- if (!is.null(start_values$rho_ac_raw)) {
      tanh(start_values$rho_ac_raw)
    } else {
      0
    }
    denom <- sqrt((1 - r_ab^2) * (1 - r_ac^2))

    rho_bc_target <- NULL
    if (!is.null(start_values$rho_bc)) {
      rho_bc_target <- start_values$rho_bc
      start_values$rho_bc <- NULL
    } else if (!is.null(start_values$rho_bc_raw)) {
      rho_bc_target <- tanh(start_values$rho_bc_raw)
    }

    if (!is.null(rho_bc_target)) {
      if (!is.numeric(rho_bc_target) || length(rho_bc_target) != 1 ||
          !is.finite(rho_bc_target)) {
        stop("'start_values$rho_bc' must be a single finite numeric value.",
             call. = FALSE)
      }
      if (rho_bc_target <= -1 || rho_bc_target >= 1) {
        stop("'start_values$rho_bc' must be in (-1, 1).", call. = FALSE)
      }

      rho_bc_partial <- if (is.finite(denom) && denom > 1e-8) {
        (rho_bc_target - r_ab * r_ac) / denom
      } else {
        0
      }
      rho_bc_partial <- max(min(rho_bc_partial, 0.999999), -0.999999)
      start_values$rho_bc_raw <- atanh(rho_bc_partial)
    }
  }

  start_values
}


#' Get Parameter Names for Hurdle Model
#'
#' @description
#' Internal function to get the names of fixed effect parameters and
#' variance component names based on model configuration.
#'
#' @param n_re Number of random effects (2 or 3).
#' @param has_k Logical, whether the model has a k parameter.
#'
#' @return A list with fixed_names, var_names, and rho_names.
#' @keywords internal
.hurdle_get_param_names <- function(n_re, has_k) {
  if (n_re == 3) {
    fixed_names <- c(
      "beta0", "beta1", "log_q0", "log_alpha",
      "logsigma_a", "logsigma_b", "logsigma_c", "logsigma_e",
      "rho_ab_raw", "rho_ac_raw", "rho_bc_raw"
    )
    if (has_k) {
      fixed_names <- append(fixed_names, "log_k", after = 3)
    }

    var_names <- c(
      "alpha", "var_a", "var_b", "var_c",
      "cov_ab", "cov_ac", "cov_bc", "var_e"
    )
    if (has_k) {
      var_names <- append(var_names, "k", after = 1)
    }

    rho_names <- c("rho_ab", "rho_ac", "rho_bc")
  } else {
    fixed_names <- c(
      "beta0", "beta1", "log_q0", "log_alpha",
      "logsigma_a", "logsigma_b", "logsigma_e", "rho_ab_raw"
    )
    if (has_k) {
      fixed_names <- append(fixed_names, "log_k", after = 3)
    }

    var_names <- c("alpha", "var_a", "var_b", "cov_ab", "var_e")
    if (has_k) {
      var_names <- append(var_names, "k", after = 1)
    }

    rho_names <- c("rho_ab")
  }

  list(fixed_names = fixed_names, var_names = var_names, rho_names = rho_names)
}


#' Extract Results from TMB Hurdle Fit
#'
#' @description
#' Internal function to extract coefficients, standard errors, and derived
#' quantities from a fitted TMB hurdle model.
#'
#' @param opt Optimization result from \code{nlminb()}.
#' @param obj TMB objective function object.
#' @param sdr TMB sdreport object (can be NULL).
#' @param param_names List from \code{.hurdle_get_param_names()}.
#' @param n_subjects Number of subjects.
#' @param n_re Number of random effects.
#'
#' @return A list with coefficients, se, variance_components, correlations,
#'   and random effects matrix.
#' @keywords internal
.hurdle_extract_estimates <- function(opt, obj, sdr, param_names, n_subjects,
                                      n_re) {
  fixed_names <- param_names$fixed_names
  var_names <- param_names$var_names
  rho_names <- param_names$rho_names

  coefficients <- opt$par_internal[fixed_names]

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

  list(
    coefficients = coefficients,
    se = se,
    variance_components = variance_components,
    correlations = correlations,
    u_hat = u_hat
  )
}


#' Transform Random Effects to Original Scale
#'
#' @description
#' Internal function to transform standardized random effects (u) to the
#' original scale using the Cholesky decomposition of the covariance matrix.
#'
#' @param u_hat Matrix of standardized random effects.
#' @param coefficients Named vector of fixed effect coefficients.
#' @param n_re Number of random effects.
#'
#' @return Matrix of random effects on original scale with named columns.
#' @keywords internal
.hurdle_transform_random_effects <- function(u_hat, coefficients, n_re) {
  sigma_a <- exp(coefficients["logsigma_a"])
  sigma_b <- exp(coefficients["logsigma_b"])
  rho_ab <- tanh(coefficients["rho_ab_raw"])

  if (n_re == 3) {
    sigma_c <- exp(coefficients["logsigma_c"])
    rho_ac <- tanh(coefficients["rho_ac_raw"])
    rho_bc_partial <- tanh(coefficients["rho_bc_raw"])
    rho_bc <- rho_ab * rho_ac +
      rho_bc_partial * sqrt((1 - rho_ab^2) * (1 - rho_ac^2))

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
        Sigma_fallback <- diag(c(sigma_a^2, sigma_b^2, sigma_c^2), nrow = 3)
        t(chol(Sigma_fallback))
      }
    )
    random_effects_mat <- t(L %*% t(u_hat))
    colnames(random_effects_mat) <- c("a_i", "b_i", "c_i")
  } else {
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
  }

  random_effects_mat
}


#' Compute Subject-Specific Parameters
#'
#' @description
#' Internal function to compute subject-specific demand parameters from
#' fixed effects and random effects.
#'
#' @param coefficients Named vector of fixed effect coefficients.
#' @param random_effects_mat Matrix of transformed random effects.
#' @param subject_levels Unique subject IDs.
#' @param n_re Number of random effects.
#' @param part2 Part II equation form.
#' @param price Numeric vector of prices.
#' @param subject_id 0-indexed subject IDs.
#' @param epsilon Epsilon for breakpoint calculation.
#' @param id_var Name of ID variable for output.
#'
#' @return Data frame of subject-specific parameters.
#' @keywords internal
.hurdle_compute_subject_pars <- function(coefficients, random_effects_mat,
                                         subject_levels, n_re, part2,
                                         price, subject_id, epsilon, id_var) {
  n_subjects <- length(subject_levels)

  if (n_re == 3) {
    subj_Q0 <- exp(coefficients["log_q0"] + random_effects_mat[, "b_i"])
    subj_alpha <- exp(coefficients["log_alpha"] + random_effects_mat[, "c_i"])
  } else {
    subj_Q0 <- exp(coefficients["log_q0"] + random_effects_mat[, "b_i"])
    subj_alpha <- rep(exp(coefficients["log_alpha"]), n_subjects)
  }

  # Calculate Omax and Pmax for each subject
  if (identical(part2, "zhao_exponential")) {
    omax_pmax <- calc_omax_pmax_vec(
      Q0 = subj_Q0,
      k = exp(coefficients["log_k"]),
      alpha = subj_alpha
    )
  } else if (identical(part2, "exponential")) {
    price_split <- split(price, subject_id)
    price_list <- lapply(seq_len(n_subjects), function(i) {
      price_split[[as.character(i - 1L)]]
    })

    omax_pmax <- beezdemand_calc_pmax_omax_vec(
      params_df = data.frame(
        alpha = subj_alpha,
        q0 = subj_Q0,
        k = rep(exp(coefficients["log_k"]), n_subjects)
      ),
      model_type = "hurdle_hs_stdq0",
      param_scales = list(alpha = "natural", q0 = "natural", k = "natural"),
      price_list = price_list,
      compute_observed = FALSE
    )
    omax_pmax <- list(Pmax = omax_pmax$pmax_model, Omax = omax_pmax$omax_model)
  } else if (identical(part2, "simplified_exponential")) {
    price_split <- split(price, subject_id)
    price_list <- lapply(seq_len(n_subjects), function(i) {
      price_split[[as.character(i - 1L)]]
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
    omax_pmax <- list(Pmax = omax_pmax$pmax_model, Omax = omax_pmax$omax_model)
  } else {
    stop("Internal error: unsupported part2: ", part2)
  }

  # Build subject parameters data frame
  if (n_re == 3) {
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
      ) - epsilon,
      Pmax = omax_pmax$Pmax,
      Omax = omax_pmax$Omax,
      stringsAsFactors = FALSE
    )
  } else {
    subject_pars <- data.frame(
      id = subject_levels,
      a_i = random_effects_mat[, "a_i"],
      b_i = random_effects_mat[, "b_i"],
      Q0 = subj_Q0,
      alpha = subj_alpha,
      breakpoint = exp(
        -(coefficients["beta0"] + random_effects_mat[, "a_i"]) /
          coefficients["beta1"]
      ) - epsilon,
      Pmax = omax_pmax$Pmax,
      Omax = omax_pmax$Omax,
      stringsAsFactors = FALSE
    )
  }

  names(subject_pars)[1] <- id_var
  subject_pars
}


# =============================================================================

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
#' @param part2 Character string selecting the Part II mean function. Options are
#'   \code{"zhao_exponential"} (default; no Q0 normalization in the exponent),
#'   \code{"exponential"} (HS-standardized; Q0 inside the exponent), and
#'   \code{"simplified_exponential"} (SND/log-linear; no \code{k} parameter).
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
#' \deqn{\log(Q_{ij}) = (\log Q_0 + b_i) + k \cdot (\exp(-\alpha_i \cdot price) - 1) + \epsilon_{ij}}
#' where \eqn{\alpha_i = \exp(\log(\alpha) + c_i)} and \eqn{k = \exp(\log(k))}.
#'
#' With 2 random effects (\code{random_effects = c("zeros", "q0")}):
#' \deqn{\log(Q_{ij}) = (\log Q_0 + b_i) + k \cdot (\exp(-\alpha \cdot price) - 1) + \epsilon_{ij}}
#' where \eqn{\alpha = \exp(\log(\alpha))} and \eqn{k = \exp(\log(k))}.
#'
#' Random effects follow a multivariate normal distribution with unstructured
#' covariance matrix. Use \code{\link{compare_hurdle_models}} for likelihood
#' ratio tests comparing nested models.
#'
#' @section Parameterization and comparability:
#' The TMB backend estimates positive-constrained parameters on the natural-log
#' scale: \eqn{\log(Q_0)}, \eqn{\log(\alpha)}, and \eqn{\log(k)}. Reporting methods
#' (`summary()`, `tidy()`, `coef()`) can back-transform to the natural scale or
#' present parameters on the \eqn{\log_{10}} scale.
#'
#' To compare \eqn{\alpha} estimates with models fit in \eqn{\log_{10}} space,
#' use:
#' \deqn{\log_{10}(\alpha) = \log(\alpha) / \log(10).}
#'
#' @seealso \code{\link{summary.beezdemand_hurdle}}, \code{\link{predict.beezdemand_hurdle}},
#'   \code{\link{plot.beezdemand_hurdle}}, \code{\link{compare_hurdle_models}},
#'   \code{\link{simulate_hurdle_data}}
#'
#' @examples
#' \donttest{
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
  epsilon = 0.001,
  start_values = NULL,
  tmb_control = list(
    max_iter = 200,
    eval_max = 1000,
    trace = 0
  ),
  verbose = 1,
  part2 = c("zhao_exponential", "exponential", "simplified_exponential"),
  ...
) {
  # Capture call

  cl <- match.call()

  # Normalize Part II equation selector (keep defaults behavior-preserving)
  part2 <- as.character(part2[1])
  part2 <- tolower(part2)
  part2 <- switch(part2,
    zhao = "zhao_exponential",
    zhao_exponential = "zhao_exponential",
    exponential = "exponential",
    hs_stdq0 = "exponential",
    snd = "simplified_exponential",
    simplified = "simplified_exponential",
    simplified_exponential = "simplified_exponential",
    stop(
      "Unsupported 'part2' value: ",
      part2,
      ". Supported: 'zhao_exponential' (alias: 'zhao'), 'exponential' (alias: 'hs_stdq0'), 'simplified_exponential' (alias: 'snd')."
    )
  )

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

  # Prepare data using helper function
  prepared <- .hurdle_prepare_data(data, y_var, x_var, id_var)
  price <- prepared$price
  consumption <- prepared$consumption
  delta <- prepared$delta
  logQ <- prepared$logQ
  subject_id <- prepared$subject_id
  subject_levels <- prepared$subject_levels
  n_subjects <- prepared$n_subjects

  # Calculate number of parameters and check sample size
  n_fixed_params <- if (identical(part2, "simplified_exponential")) {
    if (n_re == 3) 11 else 8
  } else {
    if (n_re == 3) 12 else 9
  }
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
  model_name <- switch(part2,
    zhao_exponential = if (n_re == 3) "HurdleDemand3RE" else "HurdleDemand2RE",
    exponential = if (n_re == 3) "HurdleDemand3RE_StdQ0" else "HurdleDemand2RE_StdQ0",
    simplified_exponential = if (n_re == 3) "HurdleDemand3RE_SND" else "HurdleDemand2RE_SND",
    stop("Internal error: unsupported part2: ", part2)
  )

  # Progress message
  if (verbose >= 1) {
    message(sprintf("Fitting %s model...", model_name))
    message(sprintf("  Part II: %s", part2))
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

  # Build TMB data using helper function
  tmb_data <- .hurdle_build_tmb_data(prepared, model_name, epsilon)

  # Determine if model has k parameter
  has_k <- !identical(part2, "simplified_exponential")

  # Default starting values - use helper function
  if (is.null(start_values)) {
    start_values <- .hurdle_default_starts(consumption, n_re, n_subjects, has_k)
  }

  # Normalize user-provided start values using helper function
  start_values <- .hurdle_normalize_starts(start_values, n_re, has_k)

  # Add random effects matrix
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

  opt_warnings <- character(0)
  opt <- withCallingHandlers(
    nlminb(
      start = obj$par,
      objective = obj$fn,
      gradient = obj$gr,
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
  opt$par_internal <- opt$par
  try(obj$fn(opt$par), silent = TRUE)

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

  # Extract fixed effects (different for 2 vs 3 RE; some Part II forms omit k)
  if (n_re == 3) {
    fixed_names <- c(
      "beta0",
      "beta1",
      "log_q0",
      "log_alpha",
      "logsigma_a",
      "logsigma_b",
      "logsigma_c",
      "logsigma_e",
      "rho_ab_raw",
      "rho_ac_raw",
      "rho_bc_raw"
    )
    if (has_k) fixed_names <- append(fixed_names, "log_k", after = 3)

    var_names <- c(
      "alpha",  # Natural-scale alpha from ADREPORT
      "var_a",
      "var_b",
      "var_c",
      "cov_ab",
      "cov_ac",
      "cov_bc",
      "var_e"
    )
    if (has_k) var_names <- append(var_names, "k", after = 1) # Natural-scale k from ADREPORT

    rho_names <- c("rho_ab", "rho_ac", "rho_bc")
  } else {
    fixed_names <- c(
      "beta0",
      "beta1",
      "log_q0",
      "log_alpha",
      "logsigma_a",
      "logsigma_b",
      "logsigma_e",
      "rho_ab_raw"
    )
    if (has_k) fixed_names <- append(fixed_names, "log_k", after = 3)

    var_names <- c("alpha", "var_a", "var_b", "cov_ab", "var_e")  # alpha from ADREPORT
    if (has_k) var_names <- append(var_names, "k", after = 1) # k from ADREPORT

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
    rho_bc_partial <- tanh(coefficients["rho_bc_raw"])
    # Match the TMB template's PD correlation parameterization:
    # rho_bc is defined via a partial correlation to ensure the 3x3 correlation
    # matrix is positive definite for all real-valued raw parameters.
    rho_bc <- rho_ab * rho_ac +
      rho_bc_partial * sqrt((1 - rho_ab^2) * (1 - rho_ac^2))

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

    # Subject-specific parameters (with multiplicative c_i for alpha)
    # Per EQUATIONS_CONTRACT.md: alpha_i = exp(log_alpha + c_i)
    subj_Q0 <- exp(coefficients["log_q0"] + random_effects_mat[, "b_i"])
    subj_alpha <- exp(coefficients["log_alpha"] + random_effects_mat[, "c_i"])

    # Calculate Omax and Pmax for each subject (Part II mean)
    if (identical(part2, "zhao_exponential")) {
      omax_pmax <- calc_omax_pmax_vec(
        Q0 = subj_Q0,
        k = exp(coefficients["log_k"]),
        alpha = subj_alpha
      )
    } else if (identical(part2, "exponential")) {
      price_split <- split(price, subject_id)
      price_list <- lapply(seq_len(n_subjects), function(i) {
        price_split[[as.character(i - 1L)]]
      })

      omax_pmax <- beezdemand_calc_pmax_omax_vec(
        params_df = data.frame(
          alpha = subj_alpha,
          q0 = subj_Q0,
          k = rep(exp(coefficients["log_k"]), n_subjects)
        ),
        model_type = "hurdle_hs_stdq0",
        param_scales = list(alpha = "natural", q0 = "natural", k = "natural"),
        price_list = price_list,
        compute_observed = FALSE
      )
      omax_pmax <- list(Pmax = omax_pmax$pmax_model, Omax = omax_pmax$omax_model)
    } else if (identical(part2, "simplified_exponential")) {
      price_split <- split(price, subject_id)
      price_list <- lapply(seq_len(n_subjects), function(i) {
        price_split[[as.character(i - 1L)]]
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
      omax_pmax <- list(Pmax = omax_pmax$pmax_model, Omax = omax_pmax$omax_model)
    } else {
      stop("Internal error: unsupported part2: ", part2)
    }

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
    # Per EQUATIONS_CONTRACT.md: alpha = exp(log_alpha)
    subj_Q0 <- exp(coefficients["log_q0"] + random_effects_mat[, "b_i"])
    subj_alpha <- rep(exp(coefficients["log_alpha"]), n_subjects)

    # Calculate Omax and Pmax for each subject (Part II mean)
    if (identical(part2, "zhao_exponential")) {
      omax_pmax <- calc_omax_pmax_vec(
        Q0 = subj_Q0,
        k = exp(coefficients["log_k"]),
        alpha = subj_alpha
      )
    } else if (identical(part2, "exponential")) {
      price_split <- split(price, subject_id)
      price_list <- lapply(seq_len(n_subjects), function(i) {
        price_split[[as.character(i - 1L)]]
      })

      omax_pmax <- beezdemand_calc_pmax_omax_vec(
        params_df = data.frame(
          alpha = subj_alpha,
          q0 = subj_Q0,
          k = rep(exp(coefficients["log_k"]), n_subjects)
        ),
        model_type = "hurdle_hs_stdq0",
        param_scales = list(alpha = "natural", q0 = "natural", k = "natural"),
        price_list = price_list,
        compute_observed = FALSE
      )
      omax_pmax <- list(Pmax = omax_pmax$pmax_model, Omax = omax_pmax$omax_model)
    } else if (identical(part2, "simplified_exponential")) {
      price_split <- split(price, subject_id)
      price_list <- lapply(seq_len(n_subjects), function(i) {
        price_split[[as.character(i - 1L)]]
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
      omax_pmax <- list(Pmax = omax_pmax$pmax_model, Omax = omax_pmax$omax_model)
    } else {
      stop("Internal error: unsupported part2: ", part2)
    }

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
      part2 = part2,
      subject_levels = subject_levels,
      n_subjects = n_subjects,
      n_obs = nrow(data),
      n_random_effects = n_re,
      random_effects_spec = random_effects,
      epsilon = epsilon
    ),
    param_space = "log",
    param_space_details = beezdemand_param_space_details_core(
      internal_names = list(
        Q0 = "log_q0",
        alpha = "log_alpha",
        k = if (has_k) "log_k" else NA_character_
      ),
      internal_spaces = list(Q0 = "log", alpha = "log", k = if (has_k) "log" else NA_character_)
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
