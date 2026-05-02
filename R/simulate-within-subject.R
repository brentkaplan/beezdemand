#' Simulate within-subject demand data for TICKET-011 Phase 2 parity tests
#'
#' Generates long-format demand data where each subject is observed at every
#' price under every level of an in-subject `condition` factor. Used by the
#' Phase 2 parity tests to confirm that `fit_demand_tmb()` matches
#' `fit_demand_mixed()` on factor-expanded random-effects specifications
#' (`pdDiag(Q0+alpha~condition)`, `pdSymm(Q0+alpha~condition)` etc.).
#'
#' Data-generating process: each subject `i` at condition `c` and price
#' `p` has consumption
#' \deqn{y_{i,c,p} = Q_{0,i,c} \cdot \exp(-\alpha_{i,c} \cdot Q_{0,i,c} \cdot p) \cdot \exp(\epsilon)}
#' where
#' \deqn{\log Q_{0,i,c} = \log Q_{0,\text{pop}} + \delta^{Q_0}_c + b_{i,c}}
#' \deqn{\log \alpha_{i,c} = \log \alpha_{\text{pop}} + \delta^{\alpha}_c + d_{i,c}}
#' with per-condition shifts `delta_q0[c]`, `delta_alpha[c]` and per-subject
#' per-condition random deviations `(b_{i,c}, d_{i,c}) ~ N(0, Sigma)`.
#'
#' @param n_subjects Integer; number of subjects.
#' @param n_conditions Integer; number of within-subject condition levels
#'   (named `"C1"`, `"C2"`, ...).
#' @param prices Numeric vector of prices each subject sees at every condition.
#' @param log_q0_pop Numeric; population log-Q0.
#' @param log_alpha_pop Numeric; population log-alpha.
#' @param delta_q0 Numeric vector of length `n_conditions`; per-condition
#'   shifts on log-Q0. Defaults to 0 for all conditions.
#' @param delta_alpha Numeric vector of length `n_conditions`; per-condition
#'   shifts on log-alpha. Defaults to 0 for all conditions.
#' @param sigma_b Numeric; SD of per-(subject, condition) Q0 random deviation.
#' @param sigma_d Numeric; SD of per-(subject, condition) alpha random deviation.
#' @param rho_bd Numeric; correlation between b and d within (subject, condition).
#' @param sigma_e Numeric; residual SD on log-y.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A tibble with columns `id` (factor), `condition` (factor), `x`
#'   (price), and `y` (consumption). Long-format, one row per
#'   (subject, condition, price).
#'
#' @keywords internal
#' @importFrom stats rnorm
.simulate_within_subject_demand <- function(
  n_subjects = 30,
  n_conditions = 3,
  prices = c(0.1, 0.5, 1, 2, 5, 10, 20),
  log_q0_pop = log(20),
  log_alpha_pop = log(0.005),
  delta_q0 = NULL,
  delta_alpha = NULL,
  sigma_b = 0.3,
  sigma_d = 0.3,
  rho_bd = 0,
  sigma_e = 0.1,
  seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  if (is.null(delta_q0)) delta_q0 <- rep(0, n_conditions)
  if (is.null(delta_alpha)) delta_alpha <- rep(0, n_conditions)

  if (length(delta_q0) != n_conditions) {
    stop("`delta_q0` must have length `n_conditions`.")
  }
  if (length(delta_alpha) != n_conditions) {
    stop("`delta_alpha` must have length `n_conditions`.")
  }

  conditions <- paste0("C", seq_len(n_conditions))

  # Per-(subject, condition) random deviations from a 2x2 normal.
  Sigma <- matrix(
    c(sigma_b^2, rho_bd * sigma_b * sigma_d,
      rho_bd * sigma_b * sigma_d, sigma_d^2),
    nrow = 2
  )
  L <- t(chol(Sigma))

  rows <- vector("list", n_subjects * n_conditions * length(prices))
  k <- 1L
  for (i in seq_len(n_subjects)) {
    for (c_idx in seq_len(n_conditions)) {
      u <- rnorm(2)
      bd <- as.numeric(L %*% u)
      log_q0_ic <- log_q0_pop + delta_q0[c_idx] + bd[1]
      log_alpha_ic <- log_alpha_pop + delta_alpha[c_idx] + bd[2]
      q0_ic <- exp(log_q0_ic)
      alpha_ic <- exp(log_alpha_ic)
      for (p in prices) {
        log_y_mean <- log_q0_ic - alpha_ic * q0_ic * p
        y <- exp(log_y_mean + rnorm(1, sd = sigma_e))
        rows[[k]] <- list(
          id = i,
          condition = conditions[c_idx],
          x = p,
          y = y
        )
        k <- k + 1L
      }
    }
  }

  out <- do.call(rbind.data.frame, rows)
  out$id <- factor(out$id)
  out$condition <- factor(out$condition, levels = conditions)
  tibble::as_tibble(out)
}
