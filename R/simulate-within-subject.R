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


#' Simulate dose-response demand with a continuous within-subject random slope
#'
#' Sibling of [.simulate_within_subject_demand()] for TICKET-051: instead of a
#' within-subject *factor*, each subject is observed at several values of a
#' *continuous* within-subject covariate (e.g. centered `log10` drug dose), and
#' both intensity and elasticity change with that covariate at a
#' subject-specific rate (a continuous random slope).
#'
#' Data-generating process: for subject `i` at covariate value `d` (column
#' `dose_c`, assumed already centered) and price `p`,
#' \deqn{\log Q_{0,i}(d) = \log Q_{0,\text{pop}} + \beta^{Q_0}_1 d + u^{Q_0}_i + w^{Q_0}_i d}
#' \deqn{\log \alpha_i(d) = \log \alpha_{\text{pop}} + \beta^{\alpha}_1 d + u^{\alpha}_i + w^{\alpha}_i d}
#' with per-subject random intercepts/slopes
#' \eqn{(u^{Q_0}_i, w^{Q_0}_i, u^{\alpha}_i, w^{\alpha}_i) \sim \mathcal{N}(0, \Sigma)},
#' and SND mean consumption
#' \eqn{\mu = Q_{0,i}(d)\exp(-\alpha_i(d)\,Q_{0,i}(d)\,p)} with multiplicative
#' lognormal observation noise \eqn{y = \mu \cdot \exp(\epsilon)},
#' \eqn{\epsilon \sim \mathcal{N}(0, \sigma_e^2)}.
#'
#' \eqn{\Sigma} is block-diagonal across the Q0 and alpha parameter sides: the
#' Q0 intercept/slope pair correlates at `rho_q0`, the alpha pair at
#' `rho_alpha`, and the two sides are independent.
#'
#' **Noise caveat (TICKET-051 landmine #5).** The noise here is lognormal for
#' convenience (mirroring `test-tmb-recovery.R`), whereas the `simplified` /
#' `exponentiated` TMB likelihoods are Gaussian on raw Q. The fitted residual
#' SD therefore will *not* match `sigma_e`; only **mean-structure** recovery
#' (fixed slopes, RE intercept/slope SDs, and correlations) is a valid check.
#'
#' @param n_subjects Integer; number of subjects.
#' @param doses Numeric vector of within-subject covariate values seen by every
#'   subject. Assumed centered (mean ~ 0); for dose ladders typically
#'   `log10`-spaced then centered.
#' @param prices Numeric vector of prices each subject sees at every dose.
#' @param log_q0_pop,log_alpha_pop Numeric; population log-Q0 / log-alpha at the
#'   reference covariate value (`dose_c = 0`).
#' @param b1_q0,b1_alpha Numeric; fixed dose slopes on log-Q0 / log-alpha.
#' @param sd_u_q0,sd_w_q0 Numeric; SDs of the Q0 random intercept / slope.
#' @param sd_u_alpha,sd_w_alpha Numeric; SDs of the alpha random intercept / slope.
#' @param rho_q0,rho_alpha Numeric in (-1, 1); intercept-slope correlation on the
#'   Q0 and alpha sides respectively.
#' @param sigma_e Numeric; SD of the lognormal observation noise on log-y.
#' @param covariate_name Character; name of the emitted covariate column
#'   (default `"dose_c"`).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A tibble with columns `id` (factor), the covariate (`dose_c` by
#'   default, numeric), `x` (price), and `y` (consumption). Long-format, one row
#'   per (subject, dose, price). The true generating parameters are attached as
#'   `attr(., "truth")`.
#'
#' @keywords internal
#' @importFrom stats rnorm sd
.simulate_continuous_re_demand <- function(
  n_subjects = 60,
  doses = c(-2, -1, 0, 1, 2),
  prices = c(0, 1, 2, 4, 8, 16, 24, 36),
  log_q0_pop = log(20),
  log_alpha_pop = log(0.006),
  b1_q0 = 0.10,
  b1_alpha = -0.15,
  sd_u_q0 = 0.30,
  sd_w_q0 = 0.10,
  sd_u_alpha = 0.30,
  sd_w_alpha = 0.10,
  rho_q0 = 0.3,
  rho_alpha = 0.3,
  sigma_e = 0.05,
  covariate_name = "dose_c",
  seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  # 4x4 covariance of (u_q0, w_q0, u_alpha, w_alpha): two independent
  # intercept/slope blocks (Q0, alpha).
  Sigma <- matrix(0, nrow = 4, ncol = 4)
  Sigma[1, 1] <- sd_u_q0^2
  Sigma[2, 2] <- sd_w_q0^2
  Sigma[1, 2] <- Sigma[2, 1] <- rho_q0 * sd_u_q0 * sd_w_q0
  Sigma[3, 3] <- sd_u_alpha^2
  Sigma[4, 4] <- sd_w_alpha^2
  Sigma[3, 4] <- Sigma[4, 3] <- rho_alpha * sd_u_alpha * sd_w_alpha
  L <- t(chol(Sigma))

  rows <- vector("list", n_subjects * length(doses))
  k <- 1L
  for (i in seq_len(n_subjects)) {
    re <- as.numeric(L %*% rnorm(4))  # u_q0, w_q0, u_alpha, w_alpha
    for (d in doses) {
      log_q0_id <- log_q0_pop + b1_q0 * d + re[1] + re[2] * d
      log_alpha_id <- log_alpha_pop + b1_alpha * d + re[3] + re[4] * d
      q0 <- exp(log_q0_id)
      alpha <- exp(log_alpha_id)
      mu <- q0 * exp(-alpha * q0 * prices)
      y <- mu * exp(rnorm(length(prices), 0, sigma_e))
      rows[[k]] <- data.frame(
        id = i,
        dose_c = d,
        x = prices,
        y = y,
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }

  out <- do.call(rbind, rows)
  out$id <- factor(out$id)
  if (!identical(covariate_name, "dose_c")) {
    names(out)[names(out) == "dose_c"] <- covariate_name
  }
  out <- tibble::as_tibble(out)
  attr(out, "truth") <- list(
    log_q0_pop = log_q0_pop, log_alpha_pop = log_alpha_pop,
    b1_q0 = b1_q0, b1_alpha = b1_alpha,
    sd_u_q0 = sd_u_q0, sd_w_q0 = sd_w_q0,
    sd_u_alpha = sd_u_alpha, sd_w_alpha = sd_w_alpha,
    rho_q0 = rho_q0, rho_alpha = rho_alpha, sigma_e = sigma_e
  )
  out
}
