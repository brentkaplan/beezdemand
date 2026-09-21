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
  method = c("wald", "simulate"),
  R = 1000L,
  seed = NULL,
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

- method:

  Character. `"wald"` (default) returns Hessian-based Wald intervals
  (`coef +/- z * se`). `"simulate"` draws `R` parametric Monte Carlo
  samples from the asymptotic Gaussian sampling approximation
  \\N(\hat\beta, \hat\Sigma)\\ (with \\\hat\Sigma = \\`vcov(object)`)
  and reports per-coefficient empirical quantiles.

- R:

  Integer. Number of Monte Carlo draws for `method = "simulate"`. Must
  be `>= 100`; `>= 1000` is recommended for stable quantiles. Ignored
  for `method = "wald"`.

- seed:

  Optional integer seed for `method = "simulate"` reproducibility. When
  supplied, the caller's RNG state is restored on exit so the global
  stream is left unperturbed.

- ...:

  Additional arguments.

## Value

A tibble with term, estimate, conf.low, conf.high, level.

## Details

`method = "simulate"` is Monte Carlo simulation from the asymptotic
Gaussian sampling approximation to the MLE (neither a Bayesian
posterior, nor a data-resampling bootstrap, nor a profile-likelihood
interval). Because the sampled distribution is the same Gaussian that
Wald assumes, the simulated per-coefficient quantiles converge to the
Wald intervals as `R -> Inf`; the method does **not** improve on Wald at
boundary cases and offers no positivity guarantee on the internal scale
(`logsigma_*` intervals can be negative). Its value is (a) a diagnostic
side-by-side check on the Gaussian approximation, and (b) a shared draw
primitive
([`.tmb_parametric_draws()`](https://brentkaplan.github.io/beezdemand/reference/dot-tmb_parametric_draws.md))
for derived-metric confidence intervals.

## See also

[`confint.beezdemand_nlme()`](https://brentkaplan.github.io/beezdemand/reference/confint.beezdemand_nlme.md),
[`vcov.beezdemand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/vcov.beezdemand_tmb.md).

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
confint(fit)
#> # A tibble: 6 × 5
#>   term              estimate conf.low conf.high level
#>   <chr>                <dbl>    <dbl>     <dbl> <dbl>
#> 1 Q0:(Intercept)       1.88      1.64     2.12   0.95
#> 2 alpha:(Intercept)   -5.57     -5.86    -5.28   0.95
#> 3 logsigma            -0.951    -1.40    -0.501  0.95
#> 4 logsigma            -0.777    -1.23    -0.326  0.95
#> 5 logsigma_e          -1.95     -2.07    -1.82   0.95
#> 6 rho_raw             -0.459    -1.10     0.186  0.95
confint(fit, report_space = "natural")
#> # A tibble: 6 × 5
#>   term              estimate conf.low conf.high level
#>   <chr>                <dbl>    <dbl>     <dbl> <dbl>
#> 1 Q0:(Intercept)     6.55     5.14      8.36     0.95
#> 2 alpha:(Intercept)  0.00380  0.00285   0.00508  0.95
#> 3 logsigma          -0.951   -1.40     -0.501    0.95
#> 4 logsigma          -0.777   -1.23     -0.326    0.95
#> 5 logsigma_e        -1.95    -2.07     -1.82     0.95
#> 6 rho_raw           -0.459   -1.10      0.186    0.95
# Diagnostic Monte Carlo intervals (asymptotically Wald-equivalent):
confint(fit, method = "simulate", R = 1000, seed = 42)
#> # A tibble: 6 × 5
#>   term              estimate conf.low conf.high level
#>   <chr>                <dbl>    <dbl>     <dbl> <dbl>
#> 1 Q0:(Intercept)       1.88      1.63     2.11   0.95
#> 2 alpha:(Intercept)   -5.57     -5.86    -5.30   0.95
#> 3 logsigma            -0.951    -1.42    -0.476  0.95
#> 4 logsigma            -0.777    -1.22    -0.320  0.95
#> 5 logsigma_e          -1.95     -2.07    -1.82   0.95
#> 6 rho_raw             -0.459    -1.10     0.202  0.95
# }
```
