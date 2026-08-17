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

  Data frame of parameter estimates from each converged simulation
  (includes non-PD-Hessian replicates, flagged via a `hessian_pd`
  column, for callers that want them)

- true_params:

  True parameter values used

- summary:

  Summary statistics including bias, SE ratio, and coverage, computed
  only from replicates that converged with a positive-definite Hessian
  (`diagnostics$status == "clean"`); converged-but-non-PD and
  converged-but-Hessian-unavailable replicates are excluded (TICKET-062)
  since their SEs are unreliable or unknown

- n_converged:

  Number of simulations that converged (regardless of Hessian
  positive-definiteness/availability; unchanged definition)

- n_sim:

  Total number of simulations attempted

- diagnostics:

  Data frame with one row per simulation: `sim_id`, `status` (`"error"`,
  `"nonconverged"`, `"converged_non_pd"`,
  `"converged_hessian_unavailable"`, or `"clean"`), `converged`,
  `hessian_pd` (`TRUE`/`FALSE`/`NA` – `NA` means `sdreport()` itself
  failed, a different condition from an explicit non-PD Hessian),
  `opt_convergence`, and `opt_message`

- n_hessian_not_pd:

  Number of converged replicates excluded from `summary` because
  `hessian_pd` was explicitly `FALSE`

- n_hessian_unavailable:

  Number of converged replicates excluded from `summary` because
  `hessian_pd` was `NA` (Hessian PD status unavailable)

## See also

[`simulate_hurdle_data`](https://brentkaplan.github.io/beezdemand/reference/simulate_hurdle_data.md),
[`fit_demand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)

## Examples

``` r
# \donttest{
# Tiny simulation study for demonstration (use n_sim >= 200 in practice)
mc_results <- run_hurdle_monte_carlo(n_sim = 5, n_subjects = 30, seed = 123,
                                     verbose = FALSE)

# View summary
print(mc_results$summary)
#>             parameter true_value mean_estimate        bias relative_bias_pct
#> beta0           beta0 -2.0000000    -2.2871469 -0.28714691        -14.357346
#> beta1           beta1  1.0000000     1.3025973  0.30259730         30.259730
#> log_q0         log_q0  2.3025851     2.3161793  0.01359416          0.590387
#> k                   k  2.0000000            NA          NA                NA
#> alpha           alpha  0.5000000            NA          NA                NA
#> logsigma_a logsigma_a  0.0000000    -0.6157705 -0.61577047                NA
#> logsigma_b logsigma_b -0.6931472    -0.6506238  0.04252333          6.134820
#> logsigma_e logsigma_e -1.2039728    -1.2691137 -0.06514086         -5.410493
#> rho_ab_raw rho_ab_raw  0.3095196     1.4204457  1.11092607        358.919452
#>            empirical_se      mean_se    se_ratio coverage_95 n_valid
#> beta0        0.30483277   0.50790202   1.6661661         1.0       5
#> beta1        0.34516913   0.79426872   2.3011001         1.0       5
#> log_q0       0.07302744   0.10642173   1.4572841         1.0       5
#> k                    NA           NA          NA          NA       0
#> alpha                NA           NA          NA          NA       0
#> logsigma_a   1.36074207   1.42004301   1.0435799         1.0       5
#> logsigma_b   0.09177586   0.13790528   1.5026314         1.0       5
#> logsigma_e   0.09265646   0.05896088   0.6363386         0.6       5
#> rho_ab_raw   3.00042161 834.73811464 278.2069401         1.0       5

# Check convergence rate
cat("Convergence rate:", mc_results$n_converged / mc_results$n_sim, "\n")
#> Convergence rate: 1 
# }
```
