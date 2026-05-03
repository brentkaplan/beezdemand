# Extract Coefficients from TMB Model

Extract Coefficients from TMB Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
coef(object, ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- ...:

  Additional arguments (currently unused).

## Value

Named numeric vector of fixed effect coefficients.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
coef(fit)
#>    beta_q0 beta_alpha      log_k   logsigma   logsigma logsigma_e    rho_raw 
#>  1.8736539 -5.8010932  0.8954500 -0.9527944 -0.7797945 -1.9498223 -0.4674928 
# }
```
