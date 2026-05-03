# Extract Random Effects from TMB Model

Returns subject-level random effect deviations on the natural (log)
scale. These are the Cholesky-transformed deviations (`b_i` for Q0,
`c_i` for alpha), not standardized scores. To obtain the standardized
random effects (`u` matrix), access `object$tmb_obj` directly.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
ranef(object, ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- ...:

  Additional arguments.

## Value

Data frame with subject-level random effects.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
head(nlme::ranef(fit))
#>    id        b_i        c_i
#> 1  19  0.4347399 -0.6136355
#> 2  30 -0.8308979  0.4420229
#> 3  38 -0.3588356  0.1280378
#> 4  60  0.3938846  0.1112281
#> 5  68  0.4514144 -0.3174826
#> 6 106 -0.1487057  0.4857013
# }
```
