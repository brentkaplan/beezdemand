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

A list with `Pmax`, `Omax`, `Qmax`, `elasticity_at_pmax`, `method`, and
(for covariate-adjusted fits) `conditioned_on` describing the reference
point used.

## Note

For fits that include `continuous_covariates`, the returned metrics are
computed from the intercept-only coefficients – i.e., the curve at every
covariate set to 0 – not at the training mean or any other defensible
population reference. A warning is emitted and the conditioning point is
reported in `conditioned_on`. Marginalization (or explicit `at`
conditioning) is planned for TICKET-011 Phase 5; the warn-and-label
behavior here mirrors the warning convention used by
`predict(type = "demand")`.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
calc_group_metrics(fit)
#> $Pmax
#> beta_alpha 
#>   11.23768 
#> 
#> $Omax
#> beta_alpha 
#>   23.89412 
#> 
#> $Qmax
#>  beta_q0 
#> 2.126251 
#> 
#> $elasticity_at_pmax
#> beta_alpha 
#>         -1 
#> 
#> $method
#> [1] "analytic_lambert_w"
#> 
#> $conditioned_on
#> NULL
#> 
# }
```
