# Print Monte Carlo Simulation Results

Prints a formatted summary of Monte Carlo simulation results.

## Usage

``` r
print_mc_summary(mc_results, digits = 3)
```

## Arguments

- mc_results:

  Output from
  [`run_hurdle_monte_carlo`](https://brentkaplan.github.io/beezdemand/reference/run_hurdle_monte_carlo.md).

- digits:

  Number of digits to display. Default is 3.

## Value

Invisibly returns the input `mc_results` object.

## Examples

``` r
# \donttest{
mc_results <- run_hurdle_monte_carlo(n_sim = 50, n_subjects = 100, seed = 123)
#> Running 50 Monte Carlo simulations...
#>   Simulation 10/50
#>   Simulation 20/50
#>   Simulation 30/50
#>   Simulation 40/50
#>   Simulation 50/50
#> Done. 50/50 simulations converged (100.0%).
print_mc_summary(mc_results)
#> 
#> Monte Carlo Simulation Summary
#> ==============================
#> 
#> Simulations: 50 attempted, 50 converged (100.0%)
#> 
#>   Parameter   True Mean_Est   Bias Rel_Bias% Emp_SE Mean_SE SE_Ratio
#>       beta0 -2.000   -1.997  0.003       0.1  0.179   0.204     1.14
#>       beta1  1.000    0.838 -0.162     -16.2  0.187   0.321     1.71
#>      log_q0  2.303    2.314  0.011       0.5  0.060   0.056     0.94
#>           k  2.000       NA     NA        NA     NA      NA       NA
#>       alpha  0.500       NA     NA        NA     NA      NA       NA
#>  logsigma_a  0.000   -0.420 -0.420        NA  0.515   0.713     1.39
#>  logsigma_b -0.693   -0.712 -0.019      -2.8  0.075   0.078     1.05
#>  logsigma_e -1.204   -1.210 -0.006      -0.5  0.034   0.032     0.95
#>  rho_ab_raw  0.310    1.055  0.745     240.8  2.117 168.570    79.63
#>  Coverage_95%  N
#>            98 50
#>            90 50
#>            92 50
#>            NA  0
#>            NA  0
#>            94 50
#>            98 50
#>            96 50
#>           100 50
#> 
#> Interpretation:
#> - SE Ratio close to 1.0 indicates well-calibrated SEs
#> - Coverage close to 95% indicates valid confidence intervals
#> - Relative bias < 5% is generally acceptable
# }
```
