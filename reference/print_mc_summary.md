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
# Tiny run for illustration (use n_sim >= 200 for a real calibration study)
mc_results <- run_hurdle_monte_carlo(n_sim = 5, n_subjects = 30, seed = 123,
                                     verbose = FALSE)
print_mc_summary(mc_results)
#> 
#> Monte Carlo Simulation Summary
#> ==============================
#> 
#> Simulations: 5 attempted, 5 converged (100.0%)
#> 
#>   Parameter   True Mean_Est   Bias Rel_Bias% Emp_SE Mean_SE SE_Ratio
#>       beta0 -2.000   -2.287 -0.287     -14.4  0.305   0.508     1.67
#>       beta1  1.000    1.303  0.303      30.3  0.345   0.794     2.30
#>      log_q0  2.303    2.316  0.014       0.6  0.073   0.106     1.46
#>           k  2.000       NA     NA        NA     NA      NA       NA
#>       alpha  0.500       NA     NA        NA     NA      NA       NA
#>  logsigma_a  0.000   -0.616 -0.616        NA  1.361   1.420     1.04
#>  logsigma_b -0.693   -0.651  0.043       6.1  0.092   0.138     1.50
#>  logsigma_e -1.204   -1.269 -0.065      -5.4  0.093   0.059     0.64
#>  rho_ab_raw  0.310    1.420  1.111     358.9  3.000 834.738   278.21
#>  Coverage_95% N
#>           100 5
#>           100 5
#>           100 5
#>            NA 0
#>            NA 0
#>           100 5
#>           100 5
#>            60 5
#>           100 5
#> 
#> Interpretation:
#> - SE Ratio close to 1.0 indicates well-calibrated SEs
#> - Coverage close to 95% indicates valid confidence intervals
#> - Relative bias < 5% is generally acceptable
# }
```
