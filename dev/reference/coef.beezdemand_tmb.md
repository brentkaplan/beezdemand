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
#>    beta_q0 beta_alpha      log_k logsigma_b logsigma_c logsigma_e rho_bc_raw 
#>  1.8736545 -5.8010926  0.8954493 -0.9527941 -0.7797958 -1.9498226 -0.4674918 
# }
```
