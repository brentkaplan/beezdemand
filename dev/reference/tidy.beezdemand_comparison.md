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
dat <- apt_full[apt_full$gender %in% c("Male", "Female"), ]
fit <- fit_demand_tmb(dat, equation = "exponential",
                      factors = "gender", verbose = 0)
#>   equation='exponential': Dropped 5839 zero-consumption observations (12827 remaining).
#> Warning: NaNs produced
#> Warning: NaNs produced
res <- get_demand_comparisons(fit, param = c("Q0", "alpha"))
#> Warning: NaNs produced
tidy(res)
#> # A tibble: 2 × 9
#>   param contrast   estimate std.error statistic    df conf.low conf.high p.value
#>   <chr> <chr>         <dbl>     <dbl>     <dbl> <dbl>    <dbl>     <dbl>   <dbl>
#> 1 Q0    Female - … -0.105      0.0177    -5.89    Inf  -0.139    -0.0697 3.84e-9
#> 2 alpha Female - …  0.00626    0.0213     0.295   Inf  -0.0354    0.0480 7.68e-1
tidy(res, exponentiate = TRUE)
#> # A tibble: 2 × 9
#>   param contrast   estimate std.error statistic    df conf.low conf.high p.value
#>   <chr> <chr>         <dbl>     <dbl>     <dbl> <dbl>    <dbl>     <dbl>   <dbl>
#> 1 Q0    Female - …    0.786        NA    -5.89    Inf    0.726     0.852 3.84e-9
#> 2 alpha Female - …    1.01         NA     0.295   Inf    0.922     1.12  7.68e-1
# }
```
