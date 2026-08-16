# Parametric Monte Carlo draws from a TMB fit's asymptotic posterior (TICKET-018)

#' Parametric Monte Carlo draws from a TMB fit's asymptotic posterior
#'
#' Draws `R` samples of the internal-scale fixed-effect parameter vector from
#' the joint asymptotic Gaussian posterior \eqn{N(\hat\beta, \hat\Sigma)}, where
#' \eqn{\hat\beta} is \code{object$model$coefficients} and \eqn{\hat\Sigma} is
#' \code{vcov(object)} (the TMB \code{sdreport} fixed-effect covariance,
#' \code{sdr$cov.fixed}). The mean vector and covariance are positionally
#' aligned because both derive from the optimizer's \code{opt$par}.
#'
#' This is the shared primitive behind
#' \code{confint(object, method = "simulate")} and is intended for reuse by
#' derived-metric bootstrap helpers. Draws are fixed-effect-only on the internal
#' scale; callers transform to the natural scale or evaluate derived metrics as
#' needed.
#'
#' The symmetric matrix square root is formed via an eigendecomposition with
#' negative eigenvalues clamped to zero, matching \code{MASS::mvrnorm}'s
#' robustness to a near-semidefinite covariance without taking a dependency.
#'
#' @param object A \code{beezdemand_tmb} object.
#' @param R Integer number of Monte Carlo draws.
#' @param seed Optional integer seed. When supplied, the caller's RNG state is
#'   saved and restored so the global \code{.Random.seed} stream is left
#'   unperturbed.
#' @return Numeric matrix of dimension \code{R x p}, with columns named after
#'   the internal coefficient vector (\code{names(object$model$coefficients)}).
#' @keywords internal
.tmb_parametric_draws <- function(object, R = 1000L, seed = NULL) {
  mu <- object$model$coefficients
  # vcov() errors with a clear message when cov.fixed is unavailable
  # (unconverged fit); the simulate path inherits that guard.
  Sigma <- vcov(object)
  if (length(mu) != nrow(Sigma)) {
    cli::cli_abort(
      "Coefficient vector ({length(mu)}) and covariance ({nrow(Sigma)} rows) are misaligned."
    )
  }
  # A non-PD / unconverged fit can leave NaN or Inf in the sdreport
  # covariance on some platforms; eigen() would then fail with an opaque
  # "infinite or missing values in 'x'". Refuse it with a clear message.
  if (!all(is.finite(Sigma))) {
    cli::cli_abort(
      c(
        "Cannot draw from the fixed-effect covariance: it contains non-finite values.",
        "i" = "This happens when the Hessian is not positive definite or the fit did not converge; see {.code check_demand_model(fit)} and {.code fit$hessian_pd}.",
        "i" = "Parametric-bootstrap intervals are unavailable for this fit; refit (more data, fewer random effects, better starts) or fall back to {.code confint(fit, method = \"wald\")} with the same caveat."
      ),
      class = "beezdemand_nonfinite_vcov_error"
    )
  }

  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    } else {
      # No RNG state existed on entry; set.seed() would create one. Remove it
      # on exit so the global stream is genuinely left unperturbed.
      on.exit(
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        },
        add = TRUE
      )
    }
    set.seed(seed)
  }

  p <- length(mu)
  e <- eigen(Sigma, symmetric = TRUE)
  root <- e$vectors %*% (t(e$vectors) * sqrt(pmax(e$values, 0)))
  z <- matrix(stats::rnorm(R * p), nrow = R, ncol = p)
  draws <- z %*% root
  draws <- sweep(draws, 2L, mu, "+")
  colnames(draws) <- names(mu)
  draws
}
