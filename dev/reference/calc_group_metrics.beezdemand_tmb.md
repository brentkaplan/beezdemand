# Calculate Population-Level Demand Metrics for TMB Model

Calculate Population-Level Demand Metrics for TMB Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
calc_group_metrics(object, ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- ...:

  Additional arguments (currently unused).

## Value

A list with Pmax, Omax, Qmax, elasticity_at_pmax, and method.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
calc_group_metrics(fit)
#> $Pmax
#> beta_alpha 
#>   11.23767 
#> 
#> $Omax
#> beta_alpha 
#>   23.89413 
#> 
#> $Qmax
#>  beta_q0 
#> 2.126252 
#> 
#> $elasticity_at_pmax
#> beta_alpha 
#>         -1 
#> 
#> $method
#> [1] "analytic_lambert_w"
#> 
# }
```
