# Extract Fixed Effects from TMB Model

Extract Fixed Effects from TMB Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
fixef(object, ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- ...:

  Additional arguments.

## Value

Named numeric vector of fixed effects.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
nlme::fixef(fit)
#>    beta_q0 beta_alpha   logsigma   logsigma logsigma_e    rho_raw 
#>  1.8799653 -5.5720123 -0.9506009 -0.7771868 -1.9469323 -0.4593234 
# }
```
