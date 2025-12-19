#' Calculate Omax and Pmax for Demand Curves
#'
#' @description
#' Calculates the maximum expenditure (Omax) and the price at maximum expenditure (Pmax)
#' for the exponential demand model used in the two-part hurdle model.
#'
#' @details
#' For the demand function:
#' \deqn{Q(p) = Q_0 \cdot \exp(k \cdot (\exp(-\alpha \cdot p) - 1))}
#'
#' Expenditure is E(p) = p * Q(p). Omax is the maximum of E(p) and Pmax is the
#' price at which this maximum occurs. These are found numerically.
#'
#' The search range is automatically adjusted based on alpha to ensure the
#' maximum is found. For small alpha values, Pmax can be quite large.
#'
#' @param Q0 Intensity parameter (consumption at price 0).
#' @param k Scaling parameter for the exponential decay.
#' @param alpha Elasticity parameter (rate of decay).
#' @param price_range Numeric vector of length 2 specifying the price range
#'   to search for Pmax. Default is \code{NULL}, which uses an adaptive range
#'   based on alpha (approximately 0 to 10/alpha).
#'
#' @return A named list with:
#' \describe{
#'   \item{Pmax}{Price at maximum expenditure}
#'   \item{Omax}{Maximum expenditure (price * quantity)}
#'   \item{Qmax}{Quantity at Pmax}
#' }
#'
#' @examples
#' # Calculate for group-level parameters
#' calc_omax_pmax(Q0 = 10, k = 2, alpha = 0.5)
#'
#' # With k >= e (~2.718), a local maximum exists
#' calc_omax_pmax(Q0 = 10, k = 3, alpha = 0.5)
#'
#' @seealso \code{\link{calc_group_metrics}}, \code{\link{fit_demand_hurdle}}
#' @export
calc_omax_pmax <- function(Q0, k, alpha, price_range = NULL) {
  # Handle edge cases
  if (is.na(alpha) || alpha <= 0 || is.na(k) || is.na(Q0)) {
    return(list(Pmax = NA_real_, Omax = NA_real_, Qmax = NA_real_))
  }

  # Demand function: Q(p) = Q0 * exp(k * (exp(-alpha * p) - 1))
  demand_fn <- function(p) {
    Q0 * exp(k * (exp(-alpha * p) - 1))
  }

  # Expenditure function: E(p) = p * Q(p)
  expenditure_fn <- function(p) {
    p * demand_fn(p)
  }

  # Derivative of expenditure: dE/dp = Q(p) * [1 - k * alpha * p * exp(-alpha * p)]
  # Local max when: k * alpha * p * exp(-alpha * p) = 1
  # The function g(p) = p * exp(-alpha * p) has max at p = 1/alpha with value 1/(alpha * e)
  # So local max exists only if k * alpha * (1/(alpha*e)) = k/e >= 1, i.e., k >= e

  e_const <- exp(1)

  if (k < e_const) {
    # No local maximum - expenditure is monotonically increasing
    # Return NA to indicate undefined Pmax for this parameter combination
    return(list(
      Pmax = NA_real_,
      Omax = NA_real_,
      Qmax = NA_real_,
      note = "k < e: no local maximum exists"
    ))
  }

  # Find Pmax by finding root of dE/dp = 0
  # Equation: k * alpha * p * exp(-alpha * p) = 1
  # Rearranging: p * exp(-alpha * p) = 1 / (k * alpha)

  target <- 1 / (k * alpha)

  # The function g(p) = p * exp(-alpha * p) increases from 0 to max at p = 1/alpha,
  # then decreases. The local max of expenditure is at the FIRST root (before 1/alpha).

  # Search for root in (0, 1/alpha)
  p_max_g <- 1 / alpha

  g_fn <- function(p) {
    p * exp(-alpha * p) - target
  }

  # Check if solution exists in (0, 1/alpha)
  g_at_max <- p_max_g * exp(-1) # = 1/(alpha * e)

  if (target > g_at_max) {
    # Target is above the max of g, so no solution exists
    return(list(
      Pmax = NA_real_,
      Omax = NA_real_,
      Qmax = NA_real_,
      note = "No local maximum in feasible range"
    ))
  }

  # Find the first root (local max of expenditure)
  root_result <- tryCatch(
    {
      uniroot(g_fn, interval = c(0.001, p_max_g - 0.001), tol = 1e-8)
    },
    error = function(e) NULL
  )

  if (is.null(root_result)) {
    # Fallback to optimization if root finding fails
    if (is.null(price_range)) {
      price_range <- c(0.001, min(50, 3 / alpha))
    }
    opt_result <- tryCatch(
      {
        optimize(f = function(p) -expenditure_fn(p), interval = price_range)
      },
      error = function(e) list(minimum = NA_real_, objective = NA_real_)
    )
    Pmax <- opt_result$minimum
    Omax <- -opt_result$objective
  } else {
    Pmax <- root_result$root
    Omax <- expenditure_fn(Pmax)
  }

  Qmax <- if (!is.na(Pmax)) demand_fn(Pmax) else NA_real_

  list(
    Pmax = Pmax,
    Omax = Omax,
    Qmax = Qmax
  )
}


#' Calculate Omax and Pmax for Multiple Subjects
#'
#' @description
#' Vectorized calculation of Omax and Pmax for multiple subjects with
#' individual-specific parameters.
#'
#' @param Q0 Numeric vector of intensity parameters.
#' @param k Numeric vector of scaling parameters (or single value).
#' @param alpha Numeric vector of elasticity parameters (or single value).
#' @param price_range Numeric vector of length 2 for search range. Default NULL
#'   uses adaptive range based on alpha.
#'
#' @return A data frame with columns Pmax, Omax, Qmax.
#'
#' @keywords internal
calc_omax_pmax_vec <- function(Q0, k, alpha, price_range = NULL) {
  n <- length(Q0)
  if (length(k) == 1) {
    k <- rep(k, n)
  }
  if (length(alpha) == 1) {
    alpha <- rep(alpha, n)
  }

  # Suppress warnings during vectorized calculation (will warn once at group level)
  results <- suppressWarnings(lapply(seq_len(n), function(i) {
    calc_omax_pmax(Q0[i], k[i], alpha[i], price_range)
  }))

  data.frame(
    Pmax = sapply(results, `[[`, "Pmax"),
    Omax = sapply(results, `[[`, "Omax"),
    Qmax = sapply(results, `[[`, "Qmax")
  )
}


#' Calculate Group-Level Demand Metrics
#'
#' @description
#' Calculates group-level (population) Omax and Pmax from a fitted hurdle demand model.
#'
#' @param object A fitted \code{beezdemand_hurdle} object.
#'
#' @return A named list with group-level Pmax, Omax, and Qmax.
#'
#' @examples
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
#' group_metrics <- calc_group_metrics(fit)
#' }
#'
#' @seealso \code{\link{calc_omax_pmax}}, \code{\link{fit_demand_hurdle}}
#' @export
calc_group_metrics <- function(object) {
  UseMethod("calc_group_metrics")
}

#' @export
calc_group_metrics.beezdemand_hurdle <- function(object) {
  # Extract group-level parameters
  coefs <- object$model$coefficients
  Q0 <- exp(coefs["logQ0"])
  k <- coefs["k"]
  alpha <- coefs["alpha"]

  calc_omax_pmax(Q0, k, alpha)
}


#' Get Subject-Specific Parameters
#'
#' @description
#' Convenience function to extract subject-specific demand parameters from
#' a fitted hurdle demand model. Equivalent to accessing \code{object$subject_pars}.
#'
#' @param object A fitted \code{beezdemand_hurdle} object.
#'
#' @return Data frame with subject-specific parameters including:
#' \describe{
#'   \item{id}{Subject identifier}
#'   \item{a_i}{Random effect for Part I (zeros)}
#'   \item{b_i}{Random effect for Part II (Q0)}
#'   \item{c_i}{Random effect for alpha (3-RE model only)}
#'   \item{Q0}{Subject-specific intensity (consumption at price 0)}
#'   \item{alpha}{Subject-specific elasticity}
#'   \item{breakpoint}{Price where P(quit) = 0.5}
#'   \item{Pmax}{Price at maximum expenditure}
#'   \item{Omax}{Maximum expenditure}
#' }
#'
#' @examples
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
#' pars <- get_subject_pars(fit)
#' head(pars)
#' }
#'
#' @seealso \code{\link{fit_demand_hurdle}}
#' @export
get_subject_pars <- function(object) {
  UseMethod("get_subject_pars")
}

#' @export
get_subject_pars.beezdemand_hurdle <- function(object) {
  object$subject_pars
}


#' Compare Nested Hurdle Demand Models
#'
#' @description
#' Performs a likelihood ratio test comparing two nested hurdle demand models.
#' Typically used to test whether adding the random effect on alpha (c_i)
#' significantly improves model fit (3-RE vs 2-RE models).
#'
#' @param model_full A \code{beezdemand_hurdle} object with 3 random effects.
#' @param model_reduced A \code{beezdemand_hurdle} object with 2 random effects.
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{lr_stat}{Likelihood ratio test statistic}
#'   \item{df}{Degrees of freedom}
#'   \item{p_value}{P-value from chi-squared distribution}
#'   \item{model_comparison}{Data frame with model comparison statistics}
#' }
#'
#' @examples
#' \dontrun{
#' fit3 <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id",
#'                           random_effects = c("zeros", "q0", "alpha"))
#' fit2 <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id",
#'                           random_effects = c("zeros", "q0"))
#' compare_hurdle_models(fit3, fit2)
#' }
#'
#' @importFrom stats pchisq
#' @seealso \code{\link{fit_demand_hurdle}}
#' @export
compare_hurdle_models <- function(model_full, model_reduced) {
  if (
    !inherits(model_full, "beezdemand_hurdle") ||
      !inherits(model_reduced, "beezdemand_hurdle")
  ) {
    stop("Both arguments must be beezdemand_hurdle objects")
  }

  ll_full <- model_full$loglik
  ll_reduced <- model_reduced$loglik
  df_full <- length(model_full$model$coefficients)
  df_reduced <- length(model_reduced$model$coefficients)

  # LR statistic
  lr_stat <- 2 * (ll_full - ll_reduced)

  # Degrees of freedom = difference in number of parameters
  df_diff <- df_full - df_reduced

  if (df_diff <= 0) {
    warning(
      "Full model does not have more parameters than reduced model. ",
      "Arguments may be in wrong order."
    )
  }

  # P-value
  p_value <- pchisq(lr_stat, df = abs(df_diff), lower.tail = FALSE)

  result <- data.frame(
    Model = c("Full (3 RE)", "Reduced (2 RE)"),
    n_RE = c(
      model_full$param_info$n_random_effects,
      model_reduced$param_info$n_random_effects
    ),
    LogLik = c(ll_full, ll_reduced),
    df = c(df_full, df_reduced),
    AIC = c(model_full$AIC, model_reduced$AIC),
    BIC = c(model_full$BIC, model_reduced$BIC)
  )

  cat("\nLikelihood Ratio Test\n")
  cat("=====================\n")
  print(result, row.names = FALSE)
  cat("\nLR statistic:", round(lr_stat, 4), "\n")
  cat("df:", abs(df_diff), "\n")
  cat("p-value:", format.pval(p_value), "\n")

  invisible(list(
    lr_stat = lr_stat,
    df = abs(df_diff),
    p_value = p_value,
    model_comparison = result
  ))
}


#' Get Hurdle Model Parameter Summary
#'
#' @description
#' Provides summary statistics for subject-level demand parameters from a
#' hurdle demand model. This is analogous to EMMs but based on empirical
#' Bayes estimates of subject-specific parameters.
#'
#' @param fit_obj A \code{beezdemand_hurdle} object.
#' @param ci_level Confidence level for intervals (default 0.95).
#'
#' @return A data frame with summary statistics for each parameter:
#' \describe{
#'   \item{parameter}{Parameter name}
#'   \item{mean}{Mean across subjects}
#'   \item{sd}{Standard deviation across subjects}
#'   \item{median}{Median across subjects}
#'   \item{lcl}{Lower confidence limit (based on percentiles)}
#'   \item{ucl}{Upper confidence limit (based on percentiles)}
#'   \item{min}{Minimum value}
#'   \item{max}{Maximum value}
#' }
#'
#' @examples
#' \dontrun{
#' fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
#' get_hurdle_param_summary(fit)
#' }
#'
#' @importFrom stats sd median quantile
#' @seealso \code{\link{fit_demand_hurdle}}, \code{\link{get_subject_pars}}
#' @export
get_hurdle_param_summary <- function(fit_obj, ci_level = 0.95) {
  if (!inherits(fit_obj, "beezdemand_hurdle")) {
    stop("Input must be a 'beezdemand_hurdle' object.")
  }

  pars <- fit_obj$subject_pars
  alpha_level <- (1 - ci_level) / 2

  # Parameters to summarize
  param_cols <- c("Q0", "alpha", "breakpoint", "Pmax", "Omax")

  summaries <- lapply(param_cols, function(p) {
    vals <- pars[[p]]
    vals <- vals[!is.na(vals) & is.finite(vals)]

    if (length(vals) == 0) {
      return(data.frame(
        parameter = p,
        mean = NA_real_,
        sd = NA_real_,
        median = NA_real_,
        lcl = NA_real_,
        ucl = NA_real_,
        min = NA_real_,
        max = NA_real_,
        n_valid = 0L
      ))
    }

    data.frame(
      parameter = p,
      mean = mean(vals),
      sd = sd(vals),
      median = median(vals),
      lcl = quantile(vals, alpha_level, names = FALSE),
      ucl = quantile(vals, 1 - alpha_level, names = FALSE),
      min = min(vals),
      max = max(vals),
      n_valid = length(vals)
    )
  })

  do.call(rbind, summaries)
}
