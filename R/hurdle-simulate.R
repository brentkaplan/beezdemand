#' Simulate Data from Two-Part Mixed Effects Hurdle Demand Model
#'
#' @description
#' Generates simulated demand data from the two-part hurdle model.
#' Useful for Monte Carlo simulation studies, power analyses, and model validation.
#'
#' @param n_subjects Number of subjects to simulate. Default is 100.
#' @param prices Numeric vector of prices at which to simulate consumption.
#'   Default is \code{seq(0, 11, by = 0.5)}.
#' @param beta0 Intercept for Part I (logistic). Default is -2.
#' @param beta1 Slope for Part I (on log(price + epsilon)). Default is 1.
#' @param log_q0 Log of intensity parameter (Q0). Default is log(10), meaning
#'   Q0 = 10. To specify Q0 directly, use \code{log_q0 = log(your_Q0)}.
#' @param logQ0 `r lifecycle::badge("deprecated")` Use `log_q0` instead.
#' @param k Scaling parameter for demand decay. Default is 2.
#' @param alpha Elasticity parameter controlling rate of demand decay. Default is 0.5.
#' @param sigma_a Standard deviation of random intercept for Part I. Default is 1.
#' @param sigma_b Standard deviation of random intercept for Part II. Default is 0.5.
#' @param sigma_c Standard deviation of random slope for alpha (only used if
#'   \code{n_random_effects = 3}). Default is 0.1.
#' @param rho_ab Correlation between a_i and b_i. Default is 0.3.
#' @param rho_ac Correlation between a_i and c_i. Default is 0.
#' @param rho_bc Correlation between b_i and c_i. Default is 0.
#' @param sigma_e Residual standard deviation. Default is 0.3.
#' @param epsilon Small constant for log(price + epsilon). Default is 0.001.
#' @param n_random_effects Number of random effects (2 or 3). Default is 2.
#' @param stop_at_zero Logical; if TRUE, stop generating observations for a
#'   subject once zero consumption is observed. This means subjects will have
#'   varying numbers of observations. Set to FALSE to generate all prices for
#'   all subjects. Default is TRUE.
#' @param seed Optional random seed for reproducibility.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{id}{Subject identifier}
#'   \item{x}{Price value}
#'   \item{y}{Simulated consumption (may include zeros)}
#'   \item{delta}{Indicator for zero consumption (1 = zero, 0 = positive)}
#'   \item{a_i}{Subject-specific random effect for Part I}
#'   \item{b_i}{Subject-specific random effect for Part II}
#'   \item{c_i}{Subject-specific random effect for alpha (if n_random_effects = 3)}
#' }
#'
#' @details
#' The simulation follows Zhao et al. (2016):
#'
#' \strong{Part I (Zero vs Positive):}
#' \deqn{logit(P(Y=0)) = \beta_0 + \beta_1 \cdot \log(price + \epsilon) + a_i}
#'
#' \strong{Part II (Positive Consumption):}
#' \deqn{\log(Y | Y > 0) = (\log Q_0 + b_i) + k \cdot (\exp(-(\alpha + c_i) \cdot price) - 1) + \epsilon}
#'
#' Random effects \eqn{(a_i, b_i)} or \eqn{(a_i, b_i, c_i)} are drawn from a
#' multivariate normal distribution with the specified variances and correlations.
#'
#' @examples
#' # Simulate with default parameters (2 RE model)
#' sim_data <- simulate_hurdle_data(n_subjects = 100, seed = 123)
#' head(sim_data)
#'
#' # Simulate with custom prices
#' apt_prices <- c(0, 0.25, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 6, 7, 8, 9, 10)
#' sim_apt <- simulate_hurdle_data(n_subjects = 100, prices = apt_prices, seed = 123)
#'
#' # Simulate with custom parameters (Q0 = 15, alpha = 0.1)
#' sim_custom <- simulate_hurdle_data(
#'   n_subjects = 100,
#'   log_q0 = log(15),
#'   alpha = 0.1,
#'   seed = 123
#' )
#'
#' # Simulate 3 RE model
#' sim_3re <- simulate_hurdle_data(
#'   n_subjects = 100,
#'   n_random_effects = 3,
#'   sigma_c = 0.1,
#'   seed = 456
#' )
#'
#' @seealso \code{\link{fit_demand_hurdle}}, \code{\link{run_hurdle_monte_carlo}}
#'
#' @importFrom stats rnorm rbinom
#' @export
simulate_hurdle_data <- function(
  n_subjects = 100,
  prices = seq(0, 11, by = 0.5),
  beta0 = -2,
  beta1 = 1,
  log_q0 = log(10),
  logQ0 = deprecated(),
  k = 2,
  alpha = 0.5,
  sigma_a = 1,
  sigma_b = 0.5,
  sigma_c = 0.1,
  rho_ab = 0.3,
  rho_ac = 0,
  rho_bc = 0,
  sigma_e = 0.3,
  epsilon = 0.001,
  n_random_effects = 2,
  stop_at_zero = TRUE,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Handle deprecated logQ0 argument
  if (lifecycle::is_present(logQ0)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "simulate_hurdle_data(logQ0)",
      "simulate_hurdle_data(log_q0)"
    )
    log_q0 <- logQ0
  }

  # Validate inputs
  if (!(n_random_effects %in% c(2, 3))) {
    stop("n_random_effects must be 2 or 3")
  }

  # Build covariance matrix for random effects
  if (n_random_effects == 2) {
    Sigma <- matrix(
      c(
        sigma_a^2,
        sigma_a * sigma_b * rho_ab,
        sigma_a * sigma_b * rho_ab,
        sigma_b^2
      ),
      nrow = 2
    )
  } else {
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
  }

  # Check positive definiteness
  eig <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
  if (any(eig <= 0)) {
    stop(
      "Covariance matrix is not positive definite. Check correlation parameters."
    )
  }

  # Generate random effects
  L <- t(chol(Sigma))
  u <- matrix(rnorm(n_subjects * n_random_effects), nrow = n_subjects)
  re <- t(L %*% t(u))

  a_i <- re[, 1]
  b_i <- re[, 2]
  c_i <- if (n_random_effects == 3) re[, 3] else rep(0, n_subjects)

  # Initialize results list
  results <- vector("list", n_subjects)

  for (i in seq_len(n_subjects)) {
    # Collect rows for this subject in a list
    subject_rows <- list()
    row_idx <- 0
    stopped <- FALSE

    for (p in prices) {
      if (stopped && stop_at_zero) {
        break
      }

      # Part I: Probability of zero
      log_price <- log(p + epsilon)
      eta <- beta0 + beta1 * log_price + a_i[i]
      prob_zero <- 1 / (1 + exp(-eta))

      # Generate zero/positive indicator
      is_zero <- rbinom(1, 1, prob_zero)

      if (is_zero == 1) {
        y <- 0
        stopped <- TRUE
      } else {
        # Part II: Positive consumption
        alpha_i <- alpha + c_i[i]
        mu <- (log_q0 + b_i[i]) + k * (exp(-alpha_i * p) - 1)
        log_y <- rnorm(1, mean = mu, sd = sigma_e)
        y <- exp(log_y)
      }

      row_idx <- row_idx + 1
      if (n_random_effects == 3) {
        subject_rows[[row_idx]] <- data.frame(
          id = i,
          x = p,
          y = y,
          delta = is_zero,
          a_i = a_i[i],
          b_i = b_i[i],
          c_i = c_i[i]
        )
      } else {
        subject_rows[[row_idx]] <- data.frame(
          id = i,
          x = p,
          y = y,
          delta = is_zero,
          a_i = a_i[i],
          b_i = b_i[i]
        )
      }
    }

    # Combine rows for this subject
    if (length(subject_rows) > 0) {
      results[[i]] <- do.call(rbind, subject_rows)
    }
  }

  # Combine all subjects
  sim_data <- do.call(rbind, results)
  sim_data$id <- factor(sim_data$id)

  # Add attributes with true parameters
  attr(sim_data, "true_params") <- list(
    beta0 = beta0,
    beta1 = beta1,
    log_q0 = log_q0,
    k = k,
    alpha = alpha,
    sigma_a = sigma_a,
    sigma_b = sigma_b,
    sigma_c = sigma_c,
    rho_ab = rho_ab,
    rho_ac = rho_ac,
    rho_bc = rho_bc,
    sigma_e = sigma_e,
    n_random_effects = n_random_effects
  )

  sim_data
}


#' Run Monte Carlo Simulation Study for Hurdle Demand Model
#'
#' @description
#' Runs a Monte Carlo simulation study to assess model performance,
#' including bias, standard error estimates, and confidence interval coverage.
#'
#' @param n_sim Number of simulated datasets. Default is 100.
#' @param n_subjects Number of subjects per dataset. Default is 100.
#' @param true_params Named list of true parameter values. If NULL, defaults
#'   are used from \code{\link{simulate_hurdle_data}}.
#' @param n_random_effects Number of random effects (2 or 3). Default is 2.
#' @param prices Numeric vector of prices. Default is seq(0, 11, by = 0.5).
#' @param stop_at_zero Logical; if TRUE in simulation, subjects stop after first
#'   zero. Default is TRUE.
#' @param verbose Logical; print progress. Default is TRUE.
#' @param seed Random seed for reproducibility.
#'
#' @return A list with:
#' \describe{
#'   \item{estimates}{Data frame of parameter estimates from each simulation}
#'   \item{true_params}{True parameter values used}
#'   \item{summary}{Summary statistics including bias, SE ratio, and coverage}
#'   \item{n_converged}{Number of simulations that converged}
#'   \item{n_sim}{Total number of simulations attempted}
#' }
#'
#' @examples
#' \dontrun{
#' # Run small simulation study (for demonstration)
#' mc_results <- run_hurdle_monte_carlo(n_sim = 10, n_subjects = 50, seed = 123)
#'
#' # View summary
#' print(mc_results$summary)
#'
#' # Check convergence rate
#' cat("Convergence rate:", mc_results$n_converged / mc_results$n_sim, "\n")
#' }
#'
#' @seealso \code{\link{simulate_hurdle_data}}, \code{\link{fit_demand_hurdle}}
#'
#' @importFrom stats sd pchisq
#' @export
run_hurdle_monte_carlo <- function(
  n_sim = 100,
  n_subjects = 100,
  true_params = NULL,
  n_random_effects = 2,
  prices = seq(0, 11, by = 0.5),
  stop_at_zero = TRUE,
  verbose = TRUE,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Set default true parameters
  if (is.null(true_params)) {
    true_params <- list(
      beta0 = -2,
      beta1 = 1,
      log_q0 = log(10),
      k = 2,
      alpha = 0.5,
      sigma_a = 1,
      sigma_b = 0.5,
      sigma_c = 0.1,
      rho_ab = 0.3,
      rho_ac = 0,
      rho_bc = 0,
      sigma_e = 0.3
    )
  }

  # Backwards compatibility: older code/tests used `logQ0`
  if (!is.null(true_params$logQ0) && is.null(true_params$log_q0)) {
    true_params$log_q0 <- true_params$logQ0
  }

  # Parameter names to track
  if (n_random_effects == 2) {
    param_names <- c(
      "beta0",
      "beta1",
      "log_q0",
      "k",
      "alpha",
      "logsigma_a",
      "logsigma_b",
      "logsigma_e",
      "rho_ab_raw"
    )
  } else {
    param_names <- c(
      "beta0",
      "beta1",
      "log_q0",
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
  }

  # Function to run one simulation
  run_one_sim <- function(sim_id) {
    # Simulate data
    sim_data <- simulate_hurdle_data(
      n_subjects = n_subjects,
      prices = prices,
      beta0 = true_params$beta0,
      beta1 = true_params$beta1,
      log_q0 = true_params$log_q0,
      k = true_params$k,
      alpha = true_params$alpha,
      sigma_a = true_params$sigma_a,
      sigma_b = true_params$sigma_b,
      sigma_c = true_params$sigma_c,
      rho_ab = true_params$rho_ab,
      rho_ac = true_params$rho_ac,
      rho_bc = true_params$rho_bc,
      sigma_e = true_params$sigma_e,
      n_random_effects = n_random_effects,
      stop_at_zero = stop_at_zero
    )

    # Fit model
    re_spec <- if (n_random_effects == 2) {
      c("zeros", "q0")
    } else {
      c("zeros", "q0", "alpha")
    }

    fit <- tryCatch(
      {
        fit_demand_hurdle(
          sim_data,
          y_var = "y",
          x_var = "x",
          id_var = "id",
          random_effects = re_spec,
          verbose = 0,
          tmb_control = list(max_iter = 300)
        )
      },
      error = function(e) NULL
    )

    if (is.null(fit) || !fit$converged) {
      return(NULL)
    }

    # Extract estimates
    est <- fit$model$coefficients[param_names]
    se <- fit$model$se[param_names]

    data.frame(
      sim_id = sim_id,
      parameter = param_names,
      estimate = as.numeric(est),
      se = as.numeric(se),
      converged = fit$converged,
      stringsAsFactors = FALSE
    )
  }

  # Run simulations
  if (verbose) {
    message(sprintf("Running %d Monte Carlo simulations...", n_sim))
  }

  results_list <- vector("list", n_sim)
  for (i in seq_len(n_sim)) {
    if (verbose && i %% 10 == 0) {
      message(sprintf("  Simulation %d/%d", i, n_sim))
    }
    results_list[[i]] <- run_one_sim(i)
  }

  # Combine results
  non_null_results <- results_list[!sapply(results_list, is.null)]
  n_converged <- length(non_null_results)

  if (n_converged == 0) {
    warning("No simulations converged. Check simulation parameters.")
    return(list(
      estimates = NULL,
      true_params = true_params,
      summary = NULL,
      n_converged = 0,
      n_sim = n_sim
    ))
  }

  estimates <- do.call(rbind, non_null_results)

  # Transform true parameters to estimation scale
  true_values <- c(
    beta0 = true_params$beta0,
    beta1 = true_params$beta1,
    log_q0 = true_params$log_q0,
    k = true_params$k,
    alpha = true_params$alpha,
    logsigma_a = log(true_params$sigma_a),
    logsigma_b = log(true_params$sigma_b),
    logsigma_c = log(true_params$sigma_c),
    logsigma_e = log(true_params$sigma_e),
    rho_ab_raw = atanh(true_params$rho_ab),
    rho_ac_raw = atanh(true_params$rho_ac),
    rho_bc_raw = atanh(true_params$rho_bc)
  )

  # Calculate summary statistics
  summary_df <- do.call(
    rbind,
    lapply(param_names, function(p) {
      est_p <- estimates[estimates$parameter == p, ]
      est_vals <- est_p$estimate
      se_vals <- est_p$se
      true_val <- true_values[p]

      # Remove NAs
      valid_idx <- !is.na(est_vals) & !is.na(se_vals)
      est_vals <- est_vals[valid_idx]
      se_vals <- se_vals[valid_idx]

      if (length(est_vals) == 0) {
        return(data.frame(
          parameter = p,
          true_value = true_val,
          mean_estimate = NA_real_,
          bias = NA_real_,
          relative_bias_pct = NA_real_,
          empirical_se = NA_real_,
          mean_se = NA_real_,
          se_ratio = NA_real_,
          coverage_95 = NA_real_,
          n_valid = 0L,
          stringsAsFactors = FALSE
        ))
      }

      bias <- mean(est_vals) - true_val
      rel_bias <- if (abs(true_val) > 1e-10) {
        bias / abs(true_val) * 100
      } else {
        NA_real_
      }
      emp_se <- sd(est_vals)
      mean_se <- mean(se_vals)

      # 95% CI coverage
      lower <- est_vals - 1.96 * se_vals
      upper <- est_vals + 1.96 * se_vals
      coverage <- mean(lower <= true_val & true_val <= upper)

      data.frame(
        parameter = p,
        true_value = true_val,
        mean_estimate = mean(est_vals),
        bias = bias,
        relative_bias_pct = rel_bias,
        empirical_se = emp_se,
        mean_se = mean_se,
        se_ratio = mean_se / emp_se,
        coverage_95 = coverage,
        n_valid = length(est_vals),
        stringsAsFactors = FALSE
      )
    })
  )

  if (verbose) {
    message(sprintf(
      "Done. %d/%d simulations converged (%.1f%%).",
      n_converged,
      n_sim,
      100 * n_converged / n_sim
    ))
  }

  list(
    estimates = estimates,
    true_params = true_params,
    summary = summary_df,
    n_converged = n_converged,
    n_sim = n_sim
  )
}


#' Print Monte Carlo Simulation Results
#'
#' @description
#' Prints a formatted summary of Monte Carlo simulation results.
#'
#' @param mc_results Output from \code{\link{run_hurdle_monte_carlo}}.
#' @param digits Number of digits to display. Default is 3.
#'
#' @examples
#' \dontrun{
#' mc_results <- run_hurdle_monte_carlo(n_sim = 50, n_subjects = 100, seed = 123)
#' print_mc_summary(mc_results)
#' }
#'
#' @export
print_mc_summary <- function(mc_results, digits = 3) {
  cat("\nMonte Carlo Simulation Summary\n")
  cat("==============================\n\n")

  cat(sprintf(
    "Simulations: %d attempted, %d converged (%.1f%%)\n",
    mc_results$n_sim,
    mc_results$n_converged,
    100 * mc_results$n_converged / mc_results$n_sim
  ))
  cat("\n")

  if (is.null(mc_results$summary)) {
    cat("No converged simulations to summarize.\n")
    return(invisible(mc_results))
  }

  # Format summary table
  summ <- mc_results$summary
  summ_display <- data.frame(
    Parameter = summ$parameter,
    True = round(summ$true_value, digits),
    Mean_Est = round(summ$mean_estimate, digits),
    Bias = round(summ$bias, digits),
    `Rel_Bias%` = round(summ$relative_bias_pct, 1),
    Emp_SE = round(summ$empirical_se, digits),
    Mean_SE = round(summ$mean_se, digits),
    SE_Ratio = round(summ$se_ratio, 2),
    `Coverage_95%` = round(summ$coverage_95 * 100, 1),
    N = summ$n_valid,
    check.names = FALSE
  )

  print(summ_display, row.names = FALSE)

  cat("\nInterpretation:\n")
  cat(
    "- SE Ratio close to 1.0 indicates well-calibrated SEs\
"
  )
  cat("- Coverage close to 95% indicates valid confidence intervals\n")
  cat("- Relative bias < 5% is generally acceptable\n")

  invisible(mc_results)
}
