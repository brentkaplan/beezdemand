# Extract Random Effects from TMB Model

Returns subject-level random effect deviations on the natural (log)
scale. These are the Cholesky-transformed deviations, not standardized
scores. To obtain the standardized random effects (`u` matrix), access
`object$tmb_obj` directly.

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

Data frame with subject-level random effects. Columns:

- `id`: subject identifier

- `b_i`, `c_i` (when present): first-column convenience aliases for
  `q0_(Intercept)` and `alpha_(Intercept)`. Preserved for backward
  compatibility with older callers.

- `q0_<term>`: per-block random-effect coefficients for log-Q0, one
  column per random-effects design column from the parsed block
  structure. For factor-expanded or multi-block fits, these expose the
  per-condition slope REs that `b_i` / `c_i` alone do not surface.

- `alpha_<term>`: analogous columns for log-alpha.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
head(nlme::ranef(fit))
#>    id        b_i        c_i q0_(Intercept) alpha_(Intercept)
#> 1  19  0.4351751 -0.6116759      0.4351751        -0.6116759
#> 2  30 -0.8297240  0.4438124     -0.8297240         0.4438124
#> 3  38 -0.3632195  0.1170514     -0.3632195         0.1170514
#> 4  60  0.3962798  0.1156735      0.3962798         0.1156735
#> 5  68  0.4495371 -0.3215051      0.4495371        -0.3215051
#> 6 106 -0.1525620  0.4772347     -0.1525620         0.4772347
# }
```
