# Tidy a demand-parameter comparison into a flat contrasts frame

Backend-agnostic
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)
method for `beezdemand_comparison` objects (returned by
[`get_demand_comparisons()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_comparisons.md)
on both the NLME and TMB backends). This flat long tibble is the
cross-backend contract: identical column names and order regardless of
backend. The nested object itself keeps each backend's native dialect
(see
[`get_demand_comparisons()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_comparisons.md)).

## Usage

``` r
# S3 method for class 'beezdemand_comparison'
tidy(x, exponentiate = FALSE, ...)
```

## Arguments

- x:

  A `beezdemand_comparison` object.

- exponentiate:

  Logical. If `TRUE`, return base-invariant ratios
  (`estimate = 10^estimate`, CIs back-transformed); `std.error` becomes
  `NA` following broom's convention for exponentiated fits. Default
  `FALSE`.

- ...:

  Unused.

## Value

A tibble with columns `param`, `contrast`, `estimate`, `std.error`,
`statistic`, `df`, `conf.low`, `conf.high`, `p.value`. Estimates and CIs
are on the log10 scale (or ratios when `exponentiate = TRUE`).
`statistic` is a *t* ratio with finite `df` on the NLME backend and an
asymptotic *z* (`df = Inf`) on the TMB backend (the value differs by
backend, by design).

## Examples

``` r
# \donttest{
data(apt_full)
# 40 subjects per gender keep the example fast; use the full data in practice
ids <- unique(apt_full[c("id", "gender")])
ids <- ids[ids$gender %in% c("Male", "Female"), ]
keep <- unlist(lapply(split(ids$id, ids$gender), head, 40))
dat <- apt_full[apt_full$id %in% keep, ]
fit <- fit_demand_tmb(dat, equation = "exponential",
                      factors = "gender", verbose = 0)
#>   equation='exponential': Dropped 501 zero-consumption observations (859 remaining).
res <- get_demand_comparisons(fit, param = c("Q0", "alpha"))
tidy(res)
#> # A tibble: 2 × 9
#>   param contrast   estimate std.error statistic    df conf.low conf.high p.value
#>   <chr> <chr>         <dbl>     <dbl>     <dbl> <dbl>    <dbl>     <dbl>   <dbl>
#> 1 Q0    Female - …  -0.236     0.0686    -3.45    Inf   -0.371    -0.102 5.69e-4
#> 2 alpha Female - …   0.0358    0.0849     0.422   Inf   -0.131     0.202 6.73e-1
tidy(res, exponentiate = TRUE)
#> # A tibble: 2 × 9
#>   param contrast   estimate std.error statistic    df conf.low conf.high p.value
#>   <chr> <chr>         <dbl>     <dbl>     <dbl> <dbl>    <dbl>     <dbl>   <dbl>
#> 1 Q0    Female - …    0.580        NA    -3.45    Inf    0.426     0.791 5.69e-4
#> 2 alpha Female - …    1.09         NA     0.422   Inf    0.740     1.59  6.73e-1
# }
```
