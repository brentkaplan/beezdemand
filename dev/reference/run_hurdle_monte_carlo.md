# Run Monte Carlo Simulation Study for Hurdle Demand Model

Runs a Monte Carlo simulation study to assess model performance,
including bias, standard error estimates, and confidence interval
coverage.

## Usage

``` r
run_hurdle_monte_carlo(
  n_sim = 100,
  n_subjects = 100,
  true_params = NULL,
  n_random_effects = 2,
  prices = seq(0, 11, by = 0.5),
  stop_at_zero = TRUE,
  verbose = TRUE,
  seed = NULL
)
```

## Arguments

- n_sim:

  Number of simulated datasets. Default is 100.

- n_subjects:

  Number of subjects per dataset. Default is 100.

- true_params:

  Named list of true parameter values. If NULL, defaults are used from
  [`simulate_hurdle_data`](https://brentkaplan.github.io/beezdemand/reference/simulate_hurdle_data.md).

- n_random_effects:

  Number of random effects (2 or 3). Default is 2.

- prices:

  Numeric vector of prices. Default is seq(0, 11, by = 0.5).

- stop_at_zero:

  Logical; if TRUE in simulation, subjects stop after first zero.
  Default is TRUE.

- verbose:

  Logical; print progress. Default is TRUE.

- seed:

  Random seed for reproducibility.

## Value

A list with:

- estimates:

  Data frame of parameter estimates from each simulation

- true_params:

  True parameter values used

- summary:

  Summary statistics including bias, SE ratio, and coverage

- n_converged:

  Number of simulations that converged

- n_sim:

  Total number of simulations attempted

## See also

[`simulate_hurdle_data`](https://brentkaplan.github.io/beezdemand/reference/simulate_hurdle_data.md),
[`fit_demand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Run small simulation study (for demonstration)
mc_results <- run_hurdle_monte_carlo(n_sim = 10, n_subjects = 50, seed = 123)

# View summary
print(mc_results$summary)

# Check convergence rate
cat("Convergence rate:", mc_results$n_converged / mc_results$n_sim, "\n")
} # }
```
