# Confidence Intervals for TMB Model Parameters

Confidence Intervals for TMB Model Parameters

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
confint(
  object,
  parm = NULL,
  level = 0.95,
  report_space = c("internal", "natural"),
  ...
)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- parm:

  Character vector of parameter names.

- level:

  Confidence level (default 0.95).

- report_space:

  Character. `"internal"` or `"natural"`. When `"natural"`, `beta_q0`,
  `beta_alpha`, and `log_k` are exponentiated to the natural scale. For
  the intercept, this gives Q0 or alpha at the reference level. For
  non-intercept terms, the exponentiated value represents a
  **multiplicative fold-change** (ratio) relative to the reference
  level, not the absolute parameter value for that group. Variance
  parameters (`logsigma_*`, `rho_bc_raw`) remain on their internal
  scales; use [`summary()`](https://rdrr.io/r/base/summary.html) or
  `.tmb_format_variance_components()` for transformed variance
  components.

- ...:

  Additional arguments.

## Value

A tibble with term, estimate, conf.low, conf.high, level.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
confint(fit)
#> # A tibble: 7 × 5
#>   term              estimate conf.low conf.high level
#>   <chr>                <dbl>    <dbl>     <dbl> <dbl>
#> 1 Q0:(Intercept)       1.87    1.63       2.12   0.95
#> 2 alpha:(Intercept)   -5.80   -6.90      -4.70   0.95
#> 3 log_k                0.895  -0.0528     1.84   0.95
#> 4 logsigma            -0.953  -1.40      -0.504  0.95
#> 5 logsigma            -0.780  -1.23      -0.329  0.95
#> 6 logsigma_e          -1.95   -2.07      -1.83   0.95
#> 7 rho_raw             -0.467  -1.11       0.178  0.95
confint(fit, report_space = "natural")
#> # A tibble: 7 × 5
#>   term              estimate conf.low conf.high level
#>   <chr>                <dbl>    <dbl>     <dbl> <dbl>
#> 1 Q0:(Intercept)     6.51     5.10      8.31     0.95
#> 2 alpha:(Intercept)  0.00302  0.00101   0.00906  0.95
#> 3 log_k              2.45     0.949     6.32     0.95
#> 4 logsigma          -0.953   -1.40     -0.504    0.95
#> 5 logsigma          -0.780   -1.23     -0.329    0.95
#> 6 logsigma_e        -1.95    -2.07     -1.83     0.95
#> 7 rho_raw           -0.467   -1.11      0.178    0.95
# }
```
