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

- `id` — subject identifier

- `b_i`, `c_i` (when present) — first-column convenience aliases for
  `q0_(Intercept)` and `alpha_(Intercept)`. Preserved for backward
  compatibility with older callers.

- `q0_<term>` — per-block random-effect coefficients for log-Q0, one
  column per random-effects design column from the parsed block
  structure. For factor-expanded or multi-block fits, these expose the
  per-condition slope REs that `b_i` / `c_i` alone do not surface.

- `alpha_<term>` — analogous columns for log-alpha.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
head(nlme::ranef(fit))
#>    id        b_i        c_i q0_(Intercept) alpha_(Intercept)
#> 1  19  0.4347399 -0.6136355      0.4347399        -0.6136355
#> 2  30 -0.8308979  0.4420229     -0.8308979         0.4420229
#> 3  38 -0.3588356  0.1280378     -0.3588356         0.1280378
#> 4  60  0.3938846  0.1112281      0.3938846         0.1112281
#> 5  68  0.4514144 -0.3174826      0.4514144        -0.3174826
#> 6 106 -0.1487057  0.4857013     -0.1487057         0.4857013
# }
```
