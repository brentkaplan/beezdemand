# Random-Effect Variance Components for a TMB Demand Model

Extracts the random-effect variance components from a `beezdemand_tmb`
fit in the matrix layout produced by
[`nlme::VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html), so users
familiar with nlme or lme4 can introspect a TMB fit with the accessor
they already know. The reported values are the same ones returned by
[`summary.beezdemand_tmb`](https://brentkaplan.github.io/beezdemand/reference/summary.beezdemand_tmb.md):
the Q0 and alpha random-effect standard deviations on the **log10
scale** and the residual standard deviation on the model's likelihood
scale. This is a presentation shim — it formats already-computed values
and recomputes nothing.

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
VarCorr(x, sigma = 1, rdig = 3, ...)
```

## Arguments

- x:

  A `beezdemand_tmb` object.

- sigma:

  Present for signature compatibility with
  [`nlme::VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html). The
  TMB summary reports variance components as absolute standard
  deviations, so there is no residual scale factor to apply; any value
  other than the default (`1`) is an error.

- rdig:

  Integer. Number of significant digits used when formatting the
  displayed values. Default `3`.

- ...:

  Unused; present for generic compatibility.

## Value

A character matrix of class `"VarCorr.lme"` with one row per
random-effect term plus a final `"Residual"` row, columns `"Variance"`
and `"StdDev"`, and — for fits with correlated random effects (`pdSymm`)
— a `"Corr"` column. [`print()`](https://rdrr.io/r/base/print.html)
dispatches to `nlme`'s `print.VarCorr.lme()`.

## Note

The `Corr` column is placed using `nlme`'s convention — each correlation
on the row of its higher-indexed random effect. For multi-block
`pdBlocked` fits this assumes a single correlated block; consult
`summary(x)$correlations` for the authoritative values.

## See also

[`VarCorr`](https://rdrr.io/pkg/nlme/man/VarCorr.html),
[`summary.beezdemand_tmb`](https://brentkaplan.github.io/beezdemand/reference/summary.beezdemand_tmb.md)

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
VarCorr(fit)
#>          Variance StdDev Corr  
#> Q0       0.0281   0.167        
#> alpha    0.0397   0.199  -0.436
#> Residual 0.0202   0.142        
# }
```
