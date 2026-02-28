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
# \donttest{
# Run small simulation study (for demonstration)
mc_results <- run_hurdle_monte_carlo(n_sim = 10, n_subjects = 50, seed = 123)
#> Running 10 Monte Carlo simulations...
#>   Simulation 10/10
#> Done. 10/10 simulations converged (100.0%).

# View summary
print(mc_results$summary)
#>             parameter true_value mean_estimate          bias relative_bias_pct
#> beta0           beta0 -2.0000000    -1.9943656  0.0056343897        0.28171948
#> beta1           beta1  1.0000000     0.9563567 -0.0436432966       -4.36432966
#> log_q0         log_q0  2.3025851     2.3060416  0.0034565568        0.15011636
#> k                   k  2.0000000            NA            NA                NA
#> alpha           alpha  0.5000000            NA            NA                NA
#> logsigma_a logsigma_a  0.0000000    -0.8292708 -0.8292707526                NA
#> logsigma_b logsigma_b -0.6931472    -0.7515787 -0.0584315206       -8.42988650
#> logsigma_e logsigma_e -1.2039728    -1.2031988  0.0007740471        0.06429108
#> rho_ab_raw rho_ab_raw  0.3095196     3.3914059  3.0818862996      995.69987094
#>            empirical_se      mean_se    se_ratio coverage_95 n_valid
#> beta0        0.17283314 2.928410e-01   1.6943567           1      10
#> beta1        0.32768830 4.440363e-01   1.3550569           1      10
#> log_q0       0.05897284 7.716464e-02   1.3084777           1      10
#> k                    NA           NA          NA          NA       0
#> alpha                NA           NA          NA          NA       0
#> logsigma_a   1.02444987 1.095008e+00   1.0688738           1      10
#> logsigma_b   0.07955811 1.110739e-01   1.3961355           1      10
#> logsigma_e   0.05163320 4.697075e-02   0.9097006           1      10
#> rho_ab_raw   4.07800960 1.152679e+03 282.6573410           1      10

# Check convergence rate
cat("Convergence rate:", mc_results$n_converged / mc_results$n_sim, "\n")
#> Convergence rate: 1 
# }
```
