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

A one-row tibble of model-level statistics with columns:

- `model_class`: `"beezdemand_tmb"`

- `backend`: `"TMB_mixed"`

- `equation_form`: The demand equation that was fitted

- `nobs`: Number of observations

- `n_subjects`: Number of subjects

- `n_random_effects`: Total number of random-effect columns per subject

- `converged`: Convergence status

- `logLik`, `AIC`, `BIC`: Model fit statistics

The canonical columns match
[`glance.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/glance.beezdemand_nlme.md),
so backend-agnostic code needs no dispatch glue.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
glance(fit)
#> # A tibble: 1 × 10
#>   model_class  backend equation_form  nobs n_subjects n_random_effects converged
#>   <chr>        <chr>   <chr>         <int>      <int>            <int> <lgl>    
#> 1 beezdemand_… TMB_mi… exponential     146         10                2 TRUE     
#> # ℹ 3 more variables: logLik <dbl>, AIC <dbl>, BIC <dbl>
# }
```
