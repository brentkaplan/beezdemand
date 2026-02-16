# Confidence Intervals for Fixed-Effect Demand Model Parameters

Computes confidence intervals for Q0, alpha, and k parameters from
individual demand curve fits. Uses asymptotic normal approximation based
on standard errors when available.

## Usage

``` r
# S3 method for class 'beezdemand_fixed'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  A `beezdemand_fixed` object from
  [`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md).

- parm:

  Character vector of parameter names to compute CIs for. Default
  includes all available parameters.

- level:

  Confidence level (default 0.95).

- ...:

  Additional arguments (ignored).

## Value

A tibble with columns: `id`, `term`, `estimate`, `conf.low`,
`conf.high`, `level`.

## Details

For `beezdemand_fixed` objects, confidence intervals are computed using
the asymptotic normal approximation: estimate +/- z \* SE. If standard
errors are not available for a parameter, the confidence bounds will be
`NA`.

When the underlying NLS fit objects are available (from
`detailed = TRUE`), this method attempts to use
[`nlstools::confint2()`](https://rdrr.io/pkg/nlstools/man/confint2.html)
for more accurate profile-based intervals.

## Examples

``` r
# \donttest{
fit <- fit_demand_fixed(apt, equation = "hs", k = 2)
confint(fit)
#> # A tibble: 40 × 6
#>    id    term  estimate conf.low conf.high level
#>    <chr> <chr>    <dbl>    <dbl>     <dbl> <dbl>
#>  1 19    Q0       10.2      9.63     10.7   0.95
#>  2 30    Q0        2.81     2.36      3.25  0.95
#>  3 38    Q0        4.50     4.08      4.92  0.95
#>  4 60    Q0        9.92     9.02     10.8   0.95
#>  5 68    Q0       10.4      9.75     11.0   0.95
#>  6 106   Q0        5.68     5.10      6.27  0.95
#>  7 113   Q0        6.20     5.85      6.54  0.95
#>  8 142   Q0        6.17     4.92      7.43  0.95
#>  9 156   Q0        8.35     7.54      9.15  0.95
#> 10 188   Q0        6.30     5.20      7.41  0.95
#> # ℹ 30 more rows
confint(fit, level = 0.90)
#> # A tibble: 40 × 6
#>    id    term  estimate conf.low conf.high level
#>    <chr> <chr>    <dbl>    <dbl>     <dbl> <dbl>
#>  1 19    Q0       10.2      9.72     10.6    0.9
#>  2 30    Q0        2.81     2.44      3.18   0.9
#>  3 38    Q0        4.50     4.14      4.85   0.9
#>  4 60    Q0        9.92     9.17     10.7    0.9
#>  5 68    Q0       10.4      9.85     10.9    0.9
#>  6 106   Q0        5.68     5.19      6.18   0.9
#>  7 113   Q0        6.20     5.91      6.48   0.9
#>  8 142   Q0        6.17     5.12      7.23   0.9
#>  9 156   Q0        8.35     7.67      9.02   0.9
#> 10 188   Q0        6.30     5.38      7.23   0.9
#> # ℹ 30 more rows
confint(fit, parm = "Q0")
#> # A tibble: 10 × 6
#>    id    term  estimate conf.low conf.high level
#>    <chr> <chr>    <dbl>    <dbl>     <dbl> <dbl>
#>  1 19    Q0       10.2      9.63     10.7   0.95
#>  2 30    Q0        2.81     2.36      3.25  0.95
#>  3 38    Q0        4.50     4.08      4.92  0.95
#>  4 60    Q0        9.92     9.02     10.8   0.95
#>  5 68    Q0       10.4      9.75     11.0   0.95
#>  6 106   Q0        5.68     5.10      6.27  0.95
#>  7 113   Q0        6.20     5.85      6.54  0.95
#>  8 142   Q0        6.17     4.92      7.43  0.95
#>  9 156   Q0        8.35     7.54      9.15  0.95
#> 10 188   Q0        6.30     5.20      7.41  0.95
# }
```
