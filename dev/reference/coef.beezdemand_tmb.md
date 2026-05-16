# Extract Coefficients from TMB Model

Returns the optimizer's flat parameterization as a named numeric vector
(entries include `beta_q0`, `beta_alpha`, `logsigma_e`, and any random-
effect or covariance hyperparameters; intercepts are on log scale
because the optimizer works in unconstrained space).

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
coef(object, type = c("internal"), ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- type:

  Currently only `"internal"`. Reserved for the per-subject tibble
  outputs planned under TICKET-019.

- ...:

  Additional arguments (currently unused).

## Value

Named numeric vector of fixed-effect coefficients on the optimizer's
internal parameterization.

## Details

`type = "internal"` is the current and only supported value; it is
exposed as a forward-compatible alias for the per-subject tibble outputs
planned under TICKET-019 (where `coef(fit)` will default to a
per-subject tibble and `type = "internal"` will be preserved as the
numeric-vector escape hatch consumed by
[`car::deltaMethod`](https://rdrr.io/pkg/car/man/deltaMethod.html),
`multcomp::glht`, and similar tooling that expects a flat coefficient
vector).

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
coef(fit)
#>    beta_q0 beta_alpha      log_k   logsigma   logsigma logsigma_e    rho_raw 
#>  1.8736539 -5.8010932  0.8954500 -0.9527944 -0.7797945 -1.9498223 -0.4674928 
coef(fit, type = "internal")  # explicit equivalent
#>    beta_q0 beta_alpha      log_k   logsigma   logsigma logsigma_e    rho_raw 
#>  1.8736539 -5.8010932  0.8954500 -0.9527944 -0.7797945 -1.9498223 -0.4674928 
# }
```
