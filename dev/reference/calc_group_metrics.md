# Calculate Group-Level Demand Metrics

Calculates group-level (population) Omax and Pmax from a fitted hurdle
demand model.

## Usage

``` r
calc_group_metrics(object)
```

## Arguments

- object:

  A fitted `beezdemand_hurdle` object.

## Value

A named list with group-level Pmax, Omax, and Qmax.

## See also

[`calc_omax_pmax`](https://brentkaplan.github.io/beezdemand/reference/calc_omax_pmax.md),
[`fit_demand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#> Sample size may be too small for reliable estimation.
#>   Subjects: 10, Parameters: 12, Recommended minimum: 60 subjects.
#>   Consider using more subjects or the simpler 2-RE model.
#> Fitting HurdleDemand3RE model...
#>   Part II: zhao_exponential
#>   Subjects: 10, Observations: 160
#>   Fixed parameters: 12, Random effects per subject: 3
#>   Optimizing...
#>   Converged in 81 iterations
#>   Computing standard errors...
#> Done. Log-likelihood: 32.81
calc_group_metrics(fit)
#> $Pmax
#> [1] 11.04851
#> 
#> $Omax
#> [1] 23.82811
#> 
#> $Qmax
#>   log_q0 
#> 2.156681 
#> 
#> $method
#> [1] "analytic_lambert_w_hurdle"
#> 
#> $is_boundary
#> [1] FALSE
#> 
#> $elasticity_at_pmax
#> [1] -1
#> 
#> $unit_elasticity_pass
#> [1] TRUE
#> 
#> $note
#> NULL
#> 
#> $Pmax_unconditional
#> [1] 11.04851
#> 
#> $Omax_unconditional
#> [1] 23.82811
#> 
#> $Qmax_unconditional
#>   log_q0 
#> 2.156681 
#> 
#> $p_zero_at_pmax
#> [1] 8.473536e-20
#> 
#> $method_unconditional
#> [1] "numerical_optimize_observed_domain"
#> 
#> $is_boundary_unconditional
#> [1] FALSE
#> 
#> $note_unconditional
#> NULL
#> 
# }
```
