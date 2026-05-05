# Shared simulator for Hurdle Part II variant tests
# (test-hurdle_part2_variants.R, test-hurdle-golden-baseline.R).
#
# testthat auto-sources `helper-*.R` files before running any test, so
# this avoids the antipattern of test files sourcing each other.

simulate_hurdle_part2_data <- function(
  n_subjects,
  prices,
  part2 = c("exponential", "simplified_exponential"),
  seed = 123
) {
  part2 <- match.arg(part2)
  set.seed(seed)

  # Fixed effects (Part I)
  beta0 <- -3
  beta1 <- 2
  epsilon <- 0.001

  # Part II parameters (natural scale)
  log_q0 <- 0 # Q0 centered around 1
  alpha <- 0.8
  k <- 4

  # Random effects + residual
  sigma_a <- 0.6
  sigma_b <- 0.6
  sigma_e <- 0.15

  ids <- seq_len(n_subjects)
  a_i <- stats::rnorm(n_subjects, mean = 0, sd = sigma_a)
  b_i <- stats::rnorm(n_subjects, mean = 0, sd = sigma_b)

  grid <- expand.grid(
    id = ids,
    x = prices,
    stringsAsFactors = FALSE
  )

  y <- numeric(nrow(grid))

  for (row_idx in seq_len(nrow(grid))) {
    subj <- grid$id[[row_idx]]
    p <- grid$x[[row_idx]]

    eta <- beta0 + beta1 * log(p + epsilon) + a_i[[subj]]
    prob_zero <- stats::plogis(eta)
    delta <- stats::rbinom(1, size = 1, prob = prob_zero)

    if (delta == 1) {
      y[[row_idx]] <- 0
      next
    }

    Q0_i <- exp(log_q0 + b_i[[subj]])
    mu <- if (identical(part2, "simplified_exponential")) {
      (log_q0 + b_i[[subj]]) - alpha * Q0_i * p
    } else {
      (log_q0 + b_i[[subj]]) + k * (exp(-alpha * Q0_i * p) - 1)
    }

    logQ <- stats::rnorm(1, mean = mu, sd = sigma_e)
    y[[row_idx]] <- exp(logQ)
  }

  grid$y <- y
  grid
}
