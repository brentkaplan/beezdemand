# Glance at a beezdemand_tmb Model

Glance at a beezdemand_tmb Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
glance(x, ...)
```

## Arguments

- x:

  A `beezdemand_tmb` object.

- ...:

  Additional arguments.

## Value

A one-row tibble of model-level statistics.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
glance(fit)
#> # A tibble: 1 × 10
#>   model_class    backend   equation   nobs n_subjects n_random_effects converged
#>   <chr>          <chr>     <chr>     <int>      <int>            <int> <lgl>    
#> 1 beezdemand_tmb TMB_mixed exponent…   146         10                2 TRUE     
#> # ℹ 3 more variables: logLik <dbl>, AIC <dbl>, BIC <dbl>
# }
```
