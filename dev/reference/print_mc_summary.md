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

## Examples

``` r
if (FALSE) { # \dontrun{
mc_results <- run_hurdle_monte_carlo(n_sim = 50, n_subjects = 100, seed = 123)
print_mc_summary(mc_results)
} # }
```
